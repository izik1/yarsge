// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

pub mod cpu;
pub mod registers;
pub mod flags;
pub mod timer;
pub mod ppu;
pub mod dma;

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
