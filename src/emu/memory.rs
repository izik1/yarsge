// Copyright Zachery Gyurkovitz 2017 MIT License, see lisence.md for more details.

pub struct Memory {
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
        }
    }
    
    pub fn update(&mut self, ticks: i64) {
        // TODO: STUB
    }
    
    pub fn read_byte(&self, address: u16) -> u8 {
        0xFF // TODO:  STUB
    }
    
    pub fn read_cycle(&mut self, address: u16) -> u8 {
        self.update(4); // Timing is supposed to be slightly different?
        self.read_byte(address)
    }
}
