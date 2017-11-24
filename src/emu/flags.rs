// Copyright Zachery Gyurkovitz 2017 MIT License, see lisence.md for more details.

pub enum Flag {
    Z,
    N,
    H,
    C,
}

impl Flag {    
    pub fn to_mask(&self) -> u8 {
        match self {
            &Flag::Z => 0b1000_0000,
            &Flag::N => 0b0100_0000,
            &Flag::H => 0b0010_0000,
            &Flag::C => 0b0001_0000,
        }
    }
}