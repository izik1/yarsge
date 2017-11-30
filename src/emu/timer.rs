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
    pub fn new() -> Timer {
        Timer{
            prev_tima_overflow: 0,
            prev_timer_in: false,
            tac: 0,
            tima: 0,
            tima_overflow: 0,
            tma: 0,
            sys_timer: 0
        }
    }

    pub fn update(&mut self, r_if: &mut u8) {
        if self.tima_overflow > 0 {
            self.tima_overflow -= 1;

            if self.prev_tima_overflow != 0 && self.tima_overflow == 0 {
                *r_if |= 4;
                self.tima = self.tma;
            }

            self.prev_tima_overflow = self.tima_overflow;
            self.sys_timer = self.sys_timer.wrapping_add(1);
            let b = (self.tac & 0b100) == 0b100 && (self.sys_timer & (1 << if self.tac == 0b100 {9} else { (self.tac & 3) * 2 + 1}) > 0);
            if self.prev_timer_in && !b {
                self.tima = self.tima.wrapping_add(1);
                if self.tima == 0 {
                    self.tima_overflow = 5;
                }
            }

            self.prev_timer_in = b;

        }
    }
}
