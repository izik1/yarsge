// Copyright Zachery Gyurkovitz 2017 MIT License, see lisence.md for more details.

pub struct Memory {
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
        }
    }
    
    pub fn update(&mut self, _ticks: i64) {
        // TODO: STUB
    }
    
    pub fn read_byte(&self, _address: u16) -> u8 {
        0xFF // TODO:  STUB
    }
    
    pub fn read_cycle(&mut self, address: u16) -> u8 {
        self.update(4); // Timing is supposed to be slightly different?
        self.read_byte(address)
    }
    
    pub fn write_byte(&mut self, _address: u16, _value: u8) {
        // TODO: STUB.
    }
    
    pub fn write_cycle(&mut self, address: u16, value: u8) {
        self.update(4);
        self.write_byte(address, value);
    }
}
