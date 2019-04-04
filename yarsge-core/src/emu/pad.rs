use crate::emu::bits;

pub struct Pad {
    status: u8,
    keys: u8,
    prev_keys: u8,
}

impl Pad {
    pub fn new() -> Self {
        Self {
            status: 0x00,
            keys: 0xFF,
            prev_keys: 0xFF,
        }
    }

    pub fn set_status(&mut self, val: u8) {
        self.status = val & 0x30;
    }

    pub fn get_selected(&self) -> u8 {
        0xC0 | self.get_pad(self.keys)
    }

    pub fn update(&self) -> bool {
        self.get_pad(self.prev_keys) == 0xF && self.get_pad(self.keys) != 0xF
    }

    pub fn set_keys(&mut self, val: u8) {
        self.prev_keys = self.keys;
        self.keys = val;
    }

    fn get_pad(&self, p1: u8) -> u8 {
        if !bits::has_bit(self.status, 5) {
            p1 & 0xF
        } else if !bits::has_bit(self.status, 4) {
            p1 >> 4
        } else {
            0xF
        }
    }
}
