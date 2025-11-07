use std::mem;

use crate::emu::InterruptFlags;

use super::bits;

#[derive(Copy, Clone)]
struct FallingEdge(bool);

impl FallingEdge {
    fn tick(&mut self, new: bool) -> bool {
        let old = mem::replace(&mut self.0, new);

        old && !new
    }
}

pub struct Timer {
    prev_tima_overflow: i8,
    prev_timer: FallingEdge,
    tac: u8,
    tima: u8,
    tima_overflow: i8,
    tma: u8,
    sys_timer: u16,
}

impl Timer {
    #[must_use]
    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x04 => (self.sys_timer >> 8) as u8,
            0x05 => self.tima,
            0x06 => self.tma,
            0x07 => self.tac | 0xf8,
            _ => unreachable!(),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x04 => self.sys_timer = 0,
            0x05 => {
                self.tima_overflow = 1;
                self.tima = val;
            }
            0x06 => self.tma = val,
            0x07 => self.tac = val & 0b111,
            _ => unreachable!(),
        }
    }

    fn has_timer_bit(&self) -> bool {
        let bit = if self.tac & 0b11 == 0b00 {
            9
        } else {
            ((u16::from(self.tac) & 0b11) << 1) + 1
        };

        (self.sys_timer >> bit) & 1 > 0
    }

    pub fn tick(&mut self) -> InterruptFlags {
        let reg_if = 'block: {
            if self.tima_overflow == 0 {
                break 'block InterruptFlags::empty();
            }

            self.tima_overflow -= 1;

            if self.prev_tima_overflow != 0 && self.tima_overflow == 0 {
                self.tima = self.tma;
                InterruptFlags::TIMER
            } else {
                InterruptFlags::empty()
            }
        };

        self.prev_tima_overflow = self.tima_overflow;
        self.sys_timer = self.sys_timer.wrapping_add(1);
        let b = bits::has_bit(self.tac, 2) && self.has_timer_bit();
        if self.prev_timer.tick(b) {
            self.tima = self.tima.wrapping_add(1);
            if self.tima == 0 {
                self.tima_overflow = 5;
            }
        }

        reg_if
    }
}

impl Default for Timer {
    fn default() -> Self {
        Timer {
            prev_tima_overflow: 0,
            prev_timer: FallingEdge(false),
            tac: 0,
            tima: 0,
            tima_overflow: 0,
            tma: 0,
            // Gekkio got access to per clock timings!
            // On another note, yeah, the clock starts 8 t-cycles before
            // the first byte of the boot-rom is fetched.
            sys_timer: 8,
        }
    }
}
