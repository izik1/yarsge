// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

use super::bits;

pub struct Timer {
    prev_tima_overflow: i64,
    prev_timer_in: bool,
    tac: u8,
    tima: u8,
    tima_overflow: i64,
    tma: u8,
    sys_timer: u16,
}

impl Timer {
    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x04 => (self.sys_timer >> 8) as u8,
            0x05 => self.tima,
            0x06 => self.tma,
            0x07 => self.tac | 0xF8,
            _ => unreachable!(),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x04 => self.sys_timer = 0,
            0x05 => {
                self.tima_overflow = 1;
                self.tima = val
            }
            0x06 => self.tma = val,
            0x07 => self.tac = val & 0b111,
            _ => unreachable!(),
        }
    }

    pub fn new() -> Self {
        Timer {
            prev_tima_overflow: 0,
            prev_timer_in: false,
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

    fn has_timer_bit(&self) -> bool {
        let bit = if self.tac & 0b11 == 0b00 {
            9
        } else {
            ((u16::from(self.tac) & 0b11) << 1) + 1
        };

        (self.sys_timer & (1 << bit)) > 0
    }

    pub fn update(&mut self) -> u8 {
        let r_if = if self.tima_overflow > 0 {
            self.tima_overflow -= 1;
            if self.prev_tima_overflow != 0 && self.tima_overflow == 0 {
                self.tima = self.tma;
                bits::get(2)
            } else {
                0
            }
        } else {
            0
        };

        self.prev_tima_overflow = self.tima_overflow;
        self.sys_timer = self.sys_timer.wrapping_add(1);
        let b = bits::has_bit(self.tac, 2) && self.has_timer_bit();
        if self.prev_timer_in && !b {
            self.tima = self.tima.wrapping_add(1);
            if self.tima == 0 {
                self.tima_overflow = 5;
            }
        }

        self.prev_timer_in = b;
        r_if
    }
}
