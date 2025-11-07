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
            keys: 0xff,
            prev_keys: 0xff,
        }
    }

    pub fn set_status(&mut self, val: u8) {
        self.status = val & 0x30;
    }

    #[must_use]
    pub fn get_selected(&self) -> u8 {
        0xc0 | self.get_pad(self.keys)
    }

    #[must_use]
    pub fn tick(&self) -> bool {
        self.get_pad(self.prev_keys) == 0xf && self.get_pad(self.keys) != 0xf
    }

    pub fn set_keys(&mut self, val: u8) {
        self.prev_keys = self.keys;
        self.keys = val;
    }

    fn get_pad(&self, p1: u8) -> u8 {
        if !bits::has_bit(self.status, 5) {
            p1 & 0xf
        } else if !bits::has_bit(self.status, 4) {
            p1 >> 4
        } else {
            0xf
        }
    }
}
