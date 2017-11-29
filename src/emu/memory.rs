// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

pub struct Memory {
    wram: [u8; 0x2000],
    pub r_if: u8,
    pub r_ier: u8,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            r_ier: 0,
            wram: [0; 0x2000],
            r_if: 0,

        }
    }
    
    pub fn update(&mut self, _ticks: i64) {
        // TODO: STUB
    }
    
    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
        0x0000...0x3FFF => self.read_rom_low(address),
        0x4000...0x7FFF => self.read_rom_high(address),
        0xC000...0xDFFF => self.wram[(address as usize)-0xC000],
        _ => 0xFF    
        }
    }
    
    fn read_rom_low(&self, _address: u16) -> u8 {
        0xFF // TODO: STUB.
    }
    
    fn read_rom_high(&self, _address: u16) -> u8 {
        0xFF // TODO: STUB.
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
