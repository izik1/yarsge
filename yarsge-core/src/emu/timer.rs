use crate::FallingEdge;
use crate::emu::InterruptFlags;

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
    fn prev(&self) -> bool {
        self.0 & 0b1 == 0b1
    }

    #[inline(always)]
    fn set_current(&mut self, overflowing: bool) {
        self.0 |= u8::from(overflowing) << 1;
    }
}

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

    #[must_use]
    fn has_timer_bit(&self) -> bool {
        let freq = Self::tac_freq(self.tac);
        let bit: u16 = if freq == 0b00 {
            9
        } else {
            ((u16::from(freq)) << 1) + 1
        };

        ((self.sys_timer >> bit) & 1) > 0
    }

    #[must_use]
    pub fn tick(&mut self) -> InterruptFlags {
        debug_assert!(self.tick & !0b11 == 0);

        let tick = self.tick;
        self.tick = (self.tick + 1) % 4;

        if tick != 0 {
            return InterruptFlags::empty();
        }

        self.sys_timer = self.sys_timer.wrapping_add(4);

        if self.tima_overflow.tick() {
            self.tima = self.tma;

            return InterruptFlags::TIMER;
        }

        self.inc();

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
