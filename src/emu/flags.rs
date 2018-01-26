// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

#[derive(Clone, Copy)]
pub enum Flag {
    Z,
    N,
    H,
    C,
}

impl Flag {
    pub fn to_mask(&self) -> u8 {
        match *self {
            Flag::Z => 0b1000_0000,
            Flag::N => 0b0100_0000,
            Flag::H => 0b0010_0000,
            Flag::C => 0b0001_0000,
        }
    }
}

pub fn get_hca(a: u8, b: u8) -> bool {
    (((a & 0xF) + (b & 0xF)) & 0x10) == 0x10
}
