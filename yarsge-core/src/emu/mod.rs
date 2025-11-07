use std::ops::{Add, AddAssign, Sub, SubAssign};
use std::time::Duration;

use crate::Keys;
use crate::emu::cpu::Cpu;
use crate::emu::hardware::Hardware;
use crate::emu::ppu::DisplayPixel;

pub mod cpu;
pub mod dma;
pub mod ppu;
pub mod registers;
pub mod timer;

mod bus;
mod hardware;
mod memory;
mod pad;

pub mod bits {
    #[inline(always)]
    #[must_use]
    pub fn get(num: u8) -> u8 {
        1 << num
    }

    #[inline(always)]
    #[must_use]
    pub fn has_bit(num: u8, bit: u8) -> bool {
        (num & get(bit)) == get(bit)
    }
}

bitflags::bitflags! {
    #[derive(Copy, Clone)]
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

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Mode {
    Step,
    Run,
}

pub struct GameBoy {
    hw: Hardware,
    cpu: Cpu,
    mode: Mode,
    bank_10ps: u64,
}

// const DMG_NOMINAL_CLOCK_FREQ: u64 = 4_194_304;
// const DMG_PHI_FREQ: u64 = DMG_NOMINAL_CLOCK_FREQ / 4;
const X10_PS_PER_CLOCK: u64 = 23_842;

impl GameBoy {
    pub fn register_breakpoint(&mut self, address: u16) {
        self.cpu.register_breakpoint(address);
    }

    #[must_use]
    pub fn new(boot_rom: Box<[u8]>, game_rom: Box<[u8]>) -> Option<Self> {
        Some(Self {
            hw: Hardware::new(memory::Memory::new(game_rom, boot_rom)?),
            cpu: Cpu::new(),
            mode: Mode::Run,
            bank_10ps: 0,
        })
    }

    #[must_use]
    pub fn get_display(&self) -> &[DisplayPixel] {
        self.hw.get_display()
    }

    pub fn run(&mut self, elapsed: Duration, pad: Keys) {
        self.hw.set_keys(pad);

        self.bank_10ps += u64::try_from(elapsed.as_nanos())
            .ok()
            .and_then(|it| it.checked_mul(100))
            .unwrap_or(u64::MAX);

        if self.bank_10ps >= 100_000_000_000 {
            self.bank_10ps = 100_000_000_000;
        }

        // truncates
        let bankable_clocks: u64 = self.bank_10ps / X10_PS_PER_CLOCK;

        self.hw.cycle_counter += TCycle(bankable_clocks as isize);

        // any leftovers remains in the bank.
        self.bank_10ps -= bankable_clocks * X10_PS_PER_CLOCK;

        while self.hw.cycle_counter > TCycle(0) {
            if let Some(new_mode) = self.cpu.run(&mut self.hw) {
                self.mode = new_mode;
            }

            if self.mode == Mode::Step {
                self.hw.cycle_counter = TCycle(0);
            }
        }
    }
}
