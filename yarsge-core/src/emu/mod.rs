use std::cmp;
use std::ops::{Add, AddAssign, Sub, SubAssign};
use std::time::{Duration, Instant};

use crate::Keys;
use crate::emu::apu::ApuSampler;
use crate::emu::cpu::Cpu;
use crate::emu::hardware::Hardware;
use crate::emu::ppu::DisplayPixel;

pub mod apu;
pub mod cpu;
pub mod dma;
pub mod ppu;
pub mod registers;
pub mod timer;

mod bus;
mod hardware;
mod memory;
mod pad;

bitflags::bitflags! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct InterruptFlags: u8 {
        const VBLANK = 0b0000_0001;
        const STAT = 0b0000_0010;
        const TIMER = 0b0000_0100;
        const SERIAL = 0b0000_1000;
        const JOYPAD = 0b0001_0000;
    }
}

pub mod flags {
    bitflags::bitflags! {
        #[derive(Copy, Clone)]
        pub struct CpuFlags: u8 {
            const Z = 0b1000_0000;
            const N = 0b0100_0000;
            const H = 0b0010_0000;
            const C = 0b0001_0000;
        }
    }
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Copy, Clone)]
pub struct TCycle(pub isize);

impl Add for TCycle {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub for TCycle {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}
impl AddAssign for TCycle {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}
impl SubAssign for TCycle {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

pub struct GameBoy<S: ApuSampler> {
    hw: Hardware<S>,
    cpu: Cpu,
    bank_ps: u64,
}

// const DMG_NOMINAL_CLOCK_FREQ: u64 = 4_194_304;
// const DMG_PHI_FREQ: u64 = DMG_NOMINAL_CLOCK_FREQ / 4;
// try to stay in the range such that 1/PS_PER_CLOCK = [4,194,304 Hz - 70ppm : 4,194,304 (Hz) - 50ppm]
const PS_PER_CLOCK: u64 = 238_420;

impl<S: ApuSampler> GameBoy<S> {
    #[must_use]
    pub fn new(boot_rom: Box<[u8]>, game_rom: Box<[u8]>, apu_sampler: S) -> Option<Self> {
        Some(Self {
            hw: Hardware::new(memory::Memory::new_detect(game_rom, boot_rom)?, apu_sampler),
            cpu: Cpu::new(),
            bank_ps: 0,
        })
    }

    #[must_use]
    #[inline]
    pub fn display(&self) -> impl IntoIterator<Item = DisplayPixel> {
        self.hw.display()
    }

    #[must_use]
    #[inline(always)]
    pub fn keys_mut(&mut self) -> &mut Keys {
        &mut self.hw.pad.keys
    }

    pub fn run(&mut self, elapsed: Duration) {
        self.bank_ps += u64::try_from(elapsed.as_nanos())
            .ok()
            .and_then(|it| it.checked_mul(1000))
            .unwrap_or(u64::MAX);

        // limit bank time to 10ms (so that if we start lagging we reach slowdown sooner than stuttering)
        // fixme: make this dynamic so that it's 10ms of _real time_, predicted based on how long the emulator runs.
        self.bank_ps = cmp::min(self.bank_ps, 10_000_000_000);

        // truncates
        let bankable_clocks: u64 = self.bank_ps / PS_PER_CLOCK;

        self.hw.cycle_counter += TCycle(bankable_clocks as isize);

        // any leftovers remains in the bank.
        self.bank_ps -= bankable_clocks * PS_PER_CLOCK;

        if self.hw.cycle_counter > TCycle(0) {
            self.hw.tick_pad();
        }

        while self.hw.cycle_counter > TCycle(0) {
            self.cpu.run(&mut self.hw);
        }
    }

    pub fn run_host_time(
        &mut self,
        start: Instant,
        duration: Duration,
        total_emulated_time: &mut Duration,
    ) {
        if duration == Duration::ZERO {
            return;
        }

        loop {
            let remaining = duration.saturating_sub(start.elapsed());

            if remaining == Duration::ZERO {
                return;
            }

            *total_emulated_time += remaining * 4;
            self.run(remaining * 4);
        }
    }

    #[must_use]
    pub fn sampler(&self) -> &S {
        self.hw.apu.sampler()
    }

    #[must_use]
    pub fn sampler_mut(&mut self) -> &mut S {
        self.hw.apu.sampler_mut()
    }
}
