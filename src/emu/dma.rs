// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

pub struct Dma {
    pub modulus: u8,
    pub time: usize,
    pub enabled: bool,
    pub addr: u16,
    pub ld_timer: i8,
    pub ld_addr: u16,
}

impl Dma {
    pub fn new() -> Self {
        Dma {
            modulus: 0,
            time: 0,
            enabled: false,
            addr: 0,
            ld_timer: -1,
            ld_addr: 0,
        }
    }
}
