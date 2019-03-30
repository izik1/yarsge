// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use crate::emu::cpu::Cpu;
use crate::emu::hardware::Hardware;
use crate::emu::ppu::DisplayPixel;

pub mod cpu;
pub mod dma;
pub mod ppu;
pub mod registers;
pub mod timer;

mod hardware;
mod memory;

pub mod bits {
    pub fn get(num: u8) -> u8 {
        1 << num
    }

    pub fn has_bit(num: u8, bit: u8) -> bool {
        (num & get(bit)) == get(bit)
    }

    pub fn nget(num: u8) -> u8 {
        !(1 << num)
    }
}

pub mod flags {
    bitflags! {
        pub struct Flag: u8 {
            const Z = 0b1000_0000;
            const N = 0b0100_0000;
            const H = 0b0010_0000;
            const C = 0b0001_0000;
        }
    }
}

#[derive(Add, Sub, AddAssign, SubAssign, PartialOrd, Ord, Eq, PartialEq)]
pub struct TCycle(pub isize);

impl From<MCycle> for TCycle {
    fn from(m: MCycle) -> Self {
        Self(m.0 * 4)
    }
}

#[derive(Add, Sub)]
pub struct MCycle(pub isize);

impl From<TCycle> for (MCycle, TCycle) {
    fn from(t: TCycle) -> Self {
        (MCycle(t.0 / 4), TCycle(t.0 % 4))
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
}

impl GameBoy {
    pub fn new(boot_rom: Vec<u8>, game_rom: Vec<u8>) -> Option<Self> {
        Some(Self {
            hw: Hardware::new(memory::Memory::new(game_rom, boot_rom)?),
            cpu: Cpu::new(),
            mode: Mode::Run,
        })
    }

    pub fn get_display(&self) -> &[DisplayPixel] {
        self.hw.get_display()
    }

    pub fn run(&mut self, ticks: TCycle) {
        self.hw.cycle_counter += ticks;
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
