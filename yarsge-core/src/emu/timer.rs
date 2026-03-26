use std::num::NonZero;

use crate::FallingEdge;
use crate::emu::InterruptFlags;

#[derive(Clone)]
struct TimaOverflow(u8);

impl TimaOverflow {
    #[must_use]
    #[inline(always)]
    const fn new() -> Self {
        Self(0)
    }

    /// Replace `self.prev()` with `self.current()` and replace `self.current()` with `false`
    #[must_use]
    #[inline(always)]
    fn tick(&mut self) -> bool {
        self.0 >>= 1;
        self.prev()
    }

    #[inline(always)]
    #[must_use]
    fn prev(&self) -> bool {
        self.0 & 0b1 == 0b1
    }

    #[inline(always)]
    #[must_use]
    fn current(&self) -> bool {
        self.0 & 0b10 == 0b10
    }

    #[inline(always)]
    fn set_current(&mut self, overflowing: bool) {
        self.0 |= u8::from(overflowing) << 1;
    }
}

pub(crate) struct Lazy {
    timer: Timer,
    cycles_to_next_interrupt: Option<NonZero<u32>>,
    banked_cycles: u32,
}

impl Lazy {
    pub const fn new() -> Self {
        Self {
            timer: Timer::new(),
            cycles_to_next_interrupt: None,
            banked_cycles: 0,
        }
    }

    fn force(&mut self) {
        if let Some(ticks) = NonZero::new(std::mem::replace(&mut self.banked_cycles, 0)) {
            let interrupt = self.timer.tick_many(ticks);
            debug_assert_eq!(interrupt, InterruptFlags::empty());
        }
    }

    #[must_use]
    pub fn read_reg(&mut self, addr: u8) -> u8 {
        match addr {
            Timer::ADDR_DIV => (self.timer.lazy_div(self.banked_cycles) >> 8) as u8,
            // this is invariant with time, it always reads back exactly what was written (sans unused bits).
            Timer::ADDR_TAC => self.timer.tac | 0xf8,
            _ => {
                self.force();

                self.cycles_to_next_interrupt = self.timer.next_interrupt();
                assert_eq!(
                    self.cycles_to_next_interrupt,
                    self.timer.next_interrupt_count()
                );
                self.timer.read_reg(addr)
            }
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        self.force();
        self.timer.write_reg(addr, val);
        self.cycles_to_next_interrupt = self.timer.next_interrupt();
        // assert_eq!(
        //     self.cycles_to_next_interrupt,
        //     self.timer.next_interrupt_count()
        // );
    }

    // clamp huge numbers of ticks into the range `0..(CLAMP_TICKS * 2 - 1)` while retaining all the smaller bits of precision.
    // this is a fairly rare(*) situation and doesn't even tick the `timer` because nothing has changed.
    //
    // (*): With SYS_TIMER's tick rate of 2^22 Hz it'll take (2^31 Hz / 2^22 Hz) seconds = 512 seconds for this to happen once,
    // assuming no timer register is ever written, and only DIV or TAC is read, which will almost certainly happen sooner.
    #[cold]
    #[inline(always)]
    fn tick_overflow(&mut self) {
        const CLAMP_TICKS: u32 = 1 << 16;
        const { assert!(CLAMP_TICKS.is_power_of_two()) };

        self.banked_cycles = (self.banked_cycles & CLAMP_TICKS - 1) | CLAMP_TICKS;
    }

    #[cold]
    #[inline(never)]
    fn tick_interrupt(&mut self, max: NonZero<u32>) -> InterruptFlags {
        let Some(banked) = self.banked_cycles.checked_sub(max.get()) else {
            return InterruptFlags::empty();
        };

        // an interrupt has occured,
        self.banked_cycles = banked;

        let interrupt = self.timer.tick_many(max);
        self.cycles_to_next_interrupt = self.timer.next_interrupt();
        assert_eq!(
            self.cycles_to_next_interrupt,
            self.timer.next_interrupt_count()
        );
        debug_assert_eq!(interrupt, InterruptFlags::TIMER);
        interrupt
    }

    pub fn tick(&mut self, ticks: u32) -> InterruptFlags {
        const MAX_TICKS: u32 = 1 << 31;
        // when there's never going to be an interrupt, we only need to keep track of "what do we turn the div to" and "do we overflow div at least once"
        // the second bit of information is important for the very specific situation of "the div doesn't change" or "the div changes by a small amount".

        // `ticks` is going to be relatively small currently.
        self.banked_cycles += ticks;

        match (self.banked_cycles, self.cycles_to_next_interrupt) {
            (..MAX_TICKS, None) => InterruptFlags::empty(),
            (MAX_TICKS.., None) => {
                self.tick_overflow();
                InterruptFlags::empty()
            }
            (_, Some(max)) => self.tick_interrupt(max),
        }
    }
}

#[derive(Clone)]
pub struct Timer {
    prev_timer: FallingEdge,
    tac: u8,
    tima: u8,
    tima_overflow: TimaOverflow,
    tma: u8,
    sys_timer: u16,
    tick: u8,
}

impl Timer {
    const ADDR_DIV: u8 = 0x04;
    const ADDR_TIMA: u8 = 0x05;
    const ADDR_TMA: u8 = 0x06;
    const ADDR_TAC: u8 = 0x07;

    #[must_use]
    pub const fn new() -> Self {
        Self {
            prev_timer: FallingEdge::new(false),
            tac: 0,
            tima: 0,
            tima_overflow: TimaOverflow::new(),
            tma: 0,
            // Gekkio got access to per clock timings!
            // On another note, yeah, the clock starts 8 t-cycles before
            // the first byte of the boot-rom is fetched.
            sys_timer: 8,
            tick: 0,
        }
    }

    #[inline(always)]
    #[must_use]
    fn tac_freq(tac: u8) -> u8 {
        tac & 0b11
    }

    #[inline(always)]
    #[must_use]
    fn tac_enable(tac: u8) -> bool {
        (tac >> 2) & 0b1 == 0b1
    }

    fn inc(&mut self) {
        let b = Self::tac_enable(self.tac) && self.has_timer_bit();
        if self.prev_timer.tick(b) {
            let tima_overflow;
            (self.tima, tima_overflow) = self.tima.overflowing_add(1);
            self.tima_overflow.set_current(tima_overflow);
        }
    }

    #[must_use]
    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            Self::ADDR_DIV => (self.sys_timer >> 8) as u8,
            Self::ADDR_TIMA => self.tima,
            Self::ADDR_TMA => self.tma,
            Self::ADDR_TAC => self.tac | 0xf8,
            _ => unreachable!(),
        }
    }

    fn lazy_div(&self, cycles: u32) -> u16 {
        let ticks =
            ((cycles + u32::from(self.tick)) / 4).saturating_sub(1) + u32::from(self.tick == 0);

        self.sys_timer.wrapping_add(ticks.wrapping_mul(4) as u16)
    }

    fn next_interrupt_count(&self) -> Option<NonZero<u32>> {
        let mut clone = self.clone();
        for cycle in 0..(1 << 22) {
            if clone.tick() == InterruptFlags::TIMER {
                return Some(NonZero::new(cycle + 1).unwrap());
            }
        }

        None
    }

    fn next_interrupt(&self) -> Option<NonZero<u32>> {
        // on top of any added delays, we have to align `self.tick` to zero and then have one more tick.
        fn extra_time(tick: u8) -> NonZero<u32> {
            // 4 cases (self.tick = x)
            // 0 => 1
            // 1 => 4
            // 2 => 3
            // 3 => 2
            if tick.is_multiple_of(4) {
                return const { NonZero::new(1).unwrap() };
            }

            NonZero::new(5 - u32::from(tick) % 4).unwrap()
        }

        if self.tima_overflow.current() {
            return Some(extra_time(self.tick));
        }

        if !Self::tac_enable(self.tac) {
            // if TAC isn't enabled and TIMA isn't at its max value, it's physically impossible for it to overflow.
            // likewise, if tima _is_ at it's max, but we wouldn't get a falling edge anyway...
            if self.tima < u8::MAX || !self.prev_timer.get() {
                return None;
            }

            return Some(extra_time(self.tick).checked_add(4).unwrap());
        }

        // calculation is `extra_time(tick) + 8 + cycles_to_next_tima_inc + (256 - tima - 1) * cycles_per_tima_inc`
        let extra_time = extra_time(self.tick);

        // the bit at `Self::timer_bit` needs to go from false -> true -> false for a tick.
        // as a sanity check: (2^9 * 2) / 4 = 256, which is how many M-cycles there are per TIMA tick with `tac_freq = 0`
        let cycles_per_tima_inc = (1_u32 << Self::timer_bit(self.tac)) * 2;

        // note: this one just assumes `self.tick % 4 == 0`, we already have tick adjustment.
        // if we have the timer bit already that's half the time, then we can just subtract `sys_timer`.
        let cycles_to_next_tima_inc = if u32::from(self.sys_timer) & (cycles_per_tima_inc - 1) == 0
            && self.prev_timer.get()
        {
            0
        } else {
            cycles_per_tima_inc - (u32::from(self.sys_timer) & (cycles_per_tima_inc - 1))
        };

        extra_time
            .checked_add(cycles_to_next_tima_inc)
            .and_then(|it| it.checked_add((256 - u32::from(self.tima) - 1) * cycles_per_tima_inc))
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        // the timer has a lot of "being able to use a value in the same M-cycle it's written", and due to sequencing,
        // that means we need to do a bunch of time-travel.
        // alternatively you can view this as happening on the falling adge of T-4
        match addr {
            Self::ADDR_DIV => self.sys_timer = 0,
            Self::ADDR_TIMA => {
                if !self.tima_overflow.prev() {
                    self.tima = val;
                    self.tima_overflow = TimaOverflow::new();
                }
            }
            Self::ADDR_TMA => {
                self.tma = val;

                if self.tima_overflow.prev() {
                    self.tima = self.tma;
                }
            }
            Self::ADDR_TAC => {
                self.tac = val & 0b111;
                // timetravel: if the TAC bits change in a way where the new value would lead to a TIMA increment, do the increment anyway.
                self.inc();
            }
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    fn timer_bit(tac: u8) -> u16 {
        let freq = Self::tac_freq(tac);
        if freq == 0b00 {
            9
        } else {
            ((u16::from(freq)) << 1) + 1
        }
    }

    #[must_use]
    fn has_timer_bit(&self) -> bool {
        let bit = Self::timer_bit(self.tac);

        ((self.sys_timer >> bit) & 1) > 0
    }

    fn tick_many(&mut self, cycles: NonZero<u32>) -> InterruptFlags {
        debug_assert!(self.tick & !0b11 == 0);

        // try to align to self.tick % 4 == 0

        let ticks_to_zero = if self.tick.is_multiple_of(4) {
            0
        } else {
            4 - self.tick % 4
        };

        // Note: if the remaining cycles are exactly zero we return early because it's the _next_ tick that makes work happen (does this ever happen? seems unlikely)
        let Some(cycles) = cycles
            .get()
            .checked_sub(u32::from(ticks_to_zero))
            .and_then(NonZero::new)
        else {
            self.tick = self.tick.wrapping_add(cycles.get() as u8) % 4;
            return InterruptFlags::empty();
        };

        let mut reg_if = InterruptFlags::empty();

        self.tick = 0;

        // help borrowck figure out that tac is never changed.
        let tac = self.tac;

        if !Self::tac_enable(tac) {
            let mut cycles = cycles.get();
            // tick invidiual cycles until the state stabilizes to something reasonable.
            // this should take no more than 12 t-cycles
            while cycles >= 4 && (self.tima_overflow.current() || self.prev_timer.get()) {
                cycles -= 4;
                let intr = self.tick_aligned();
                debug_assert!(
                    intr.is_empty() || cycles == 0,
                    "Interrupt must happen on final emulated cycle {intr:?}"
                );
                reg_if |= intr;
            }

            if cycles > 0 && (self.tima_overflow.current() || self.prev_timer.get()) {
                cycles -= 1;
                let intr = self.tick();
                debug_assert!(
                    intr.is_empty() || cycles == 0,
                    "Interrupt must happen on final emulated cycle {intr:?}"
                );
                reg_if |= intr;
            }

            if cycles == 0 {
                return reg_if;
            }

            // self.tick == 0 here because we never disaligned it.
            // debug_assert_eq!(self.tick, 0);

            // we need to calculate the number of times that `tick` was zero (counting if it started at zero, but not ending at zero)
            // examples: (t, c) = (self.tick % 4, cycles)
            // (0, 3) =>  (impossible in this case, but, still) 1
            // (0, 4) => 1
            // (1, 3) => 0
            // (1, 4) => 1
            // (1, 3) => 0
            let ticks = (cycles / 4).saturating_sub(1) + 1;

            // we need to calculate the final value for `tick` as well.
            self.tick = (cycles as u8) % 4;

            self.sys_timer = self.sys_timer.wrapping_add(ticks.wrapping_mul(4) as u16);
            return reg_if;
        }

        // tac is enabled, which means computing shennanigans, so, fixme: actually do that instead of a loop.
        let mut cycles = cycles.get();
        while cycles >= 4 {
            cycles -= 4;
            let intr = self.tick_aligned();
            debug_assert!(
                intr.is_empty() || cycles == 0,
                "Interrupt must happen on final emulated cycle"
            );
            reg_if |= intr;
        }

        if cycles > 0 {
            let intr = self.tick();
            debug_assert!(
                intr.is_empty() || cycles == 1,
                "Interrupt must happen on final emulated cycle"
            );

            reg_if |= intr;
        }

        self.tick = cycles as u8 % 4;

        reg_if
    }

    #[must_use]
    fn tick_aligned(&mut self) -> InterruptFlags {
        self.sys_timer = self.sys_timer.wrapping_add(4);

        if self.tima_overflow.tick() {
            self.tima = self.tma;

            return InterruptFlags::TIMER;
        }

        self.inc();

        InterruptFlags::empty()
    }

    #[must_use]
    pub fn tick(&mut self) -> InterruptFlags {
        debug_assert_eq!(self.tick & !0b11, 0);

        let tick = self.tick;
        self.tick = (self.tick + 1) % 4;

        if tick == 0 {
            return self.tick_aligned();
        }

        InterruptFlags::empty()
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::emu::InterruptFlags;
    use crate::emu::timer::Timer;

    #[track_caller]
    fn no_interrupt_cycle(timer: &mut Timer) {
        for _ in 0..4 {
            assert_eq!(timer.tick(), InterruptFlags::empty());
        }
    }

    #[track_caller]
    fn interrupt_cycle(timer: &mut Timer) {
        assert_eq!(timer.tick(), InterruptFlags::TIMER);
        assert_eq!(timer.tick(), InterruptFlags::empty());
        assert_eq!(timer.tick(), InterruptFlags::empty());
        assert_eq!(timer.tick(), InterruptFlags::empty());
    }

    #[test]
    fn tick_loop() {
        let mut timer = Timer::new();
        timer.sys_timer = 0;
        for t in 0..=0xffff_u16 {
            assert_eq!(timer.tick(), InterruptFlags::empty());
            assert_eq!(timer.sys_timer, (t / 4).wrapping_add(1).wrapping_mul(4));
        }
    }

    #[test]
    fn tick_reset() {
        let mut timer = Timer::new();

        no_interrupt_cycle(&mut timer);

        timer.write_reg(Timer::ADDR_DIV, 0xde);

        assert_eq!(timer.sys_timer, 0);
    }

    #[test]
    fn toggle_enable_inc() {
        let mut timer = Timer::new();
        timer.sys_timer = 0x200;

        for x in 0..0x40 {
            timer.tac ^= 0b100;
            assert_eq!(timer.tima, x / 2);

            no_interrupt_cycle(&mut timer);
        }

        assert_eq!(timer.tima, 0x20);
    }

    #[track_caller]
    fn overflow_tima(timer: &mut Timer) {
        timer.sys_timer = 0;
        timer.tac = 0b101;

        for _ in 0..0x400 {
            no_interrupt_cycle(timer);
        }
    }

    #[test]
    fn tima_overflow() {
        let mut timer = Timer::new();
        overflow_tima(&mut timer);

        interrupt_cycle(&mut timer);

        assert_eq!(timer.sys_timer, 0x401 * 4);
    }

    #[test]
    fn tima_tma_overflow_write() {
        const TIMA_WRITE: u8 = 0xc0;
        const TMA_A: u8 = 0xa0;
        const TMA_B: u8 = 0xb0;

        let mut timer = Timer::new();
        timer.tma = TMA_A;

        overflow_tima(&mut timer);

        // cancel the overflow with a write to TIMA
        timer.write_reg(Timer::ADDR_TIMA, TIMA_WRITE);
        assert_eq!(timer.tima, TIMA_WRITE);

        // since we wrote to TIMA, no interrupt should occur.
        no_interrupt_cycle(&mut timer);

        timer.tima = 0;
        overflow_tima(&mut timer);
        // since we _didn't_ write to TIMA, an interrupt should occur.
        interrupt_cycle(&mut timer);
        // ... and TIMA should have the value in TMA
        assert_eq!(timer.tima, timer.tma);

        // writing to TIMA shouldn't affect anything
        timer.write_reg(Timer::ADDR_TIMA, TIMA_WRITE);
        assert_eq!(timer.tima, timer.tma);

        // but writing to TMA should
        timer.write_reg(Timer::ADDR_TMA, TMA_B);
        assert_eq!(timer.tima, timer.tma);

        // wait a bit longer
        no_interrupt_cycle(&mut timer);

        // now writing to TMA shouldn't affect TIMA
        timer.write_reg(Timer::ADDR_TMA, TMA_A);
        assert_ne!(timer.tima, timer.tma);

        // but writing to TIMA should work again
        timer.write_reg(Timer::ADDR_TIMA, TIMA_WRITE);
        assert_eq!(timer.tima, TIMA_WRITE)
    }
}
