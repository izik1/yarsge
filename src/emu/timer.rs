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
            0x05 => {self.tima_overflow = 1; self.tima = val}
            0x06 => self.tma = val,
            0x07 => self.tac = val & 7,
            _ => unreachable!()
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
            sys_timer: 0
        }
    }

    pub fn update(&mut self) -> u8 {
        let r_if = if self.tima_overflow > 0 {
            self.tima_overflow -= 1;
            if self.prev_tima_overflow != 0 && self.tima_overflow == 0 {
                self.tima = self.tma;
                bits::get(2)
            } else {0}
        } else {0};

        self.prev_tima_overflow = self.tima_overflow;
        self.sys_timer = self.sys_timer.wrapping_add(1);
        let b = bits::has_bit(self.tac, 2) && (self.sys_timer & (1 << if self.tac == 0b100 {9} else { (self.tac & 3) * 2 + 1}) as u16 > 0);
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
