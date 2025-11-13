use crate::emu::bits;
use crate::{FallingEdge, Keys};

pub struct Pad {
    status: u8,
    keys: Keys,
    keys_interrupt: FallingEdge,
}

impl Pad {
    pub const fn new() -> Self {
        Self {
            status: 0x00,
            keys: Keys::empty(),
            keys_interrupt: FallingEdge::new(false),
        }
    }

    pub fn set_status(&mut self, val: u8) {
        self.status = val & 0x30;
    }

    #[must_use]
    pub const fn selected(&self) -> u8 {
        0xc0 | self.status | self.pad(self.keys)
    }

    #[must_use]
    pub fn tick(&mut self) -> bool {
        self.keys_interrupt.tick(self.pad(self.keys) != 0xf)
    }

    pub fn set_keys(&mut self, val: Keys) {
        self.keys = val;
    }

    const fn pad(&self, p1: Keys) -> u8 {
        if !bits::has_bit(self.status, 5) {
            !p1.bits() & 0xf
        } else if !bits::has_bit(self.status, 4) {
            !p1.bits() >> 4
        } else {
            0xf
        }
    }
}
