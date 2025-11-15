use crate::FallingEdge;
use crate::emu::InterruptFlags;

pub struct Timer {
    prev_tima_overflow: bool,
    prev_timer: FallingEdge,
    tac: u8,
    tima: u8,
    tima_overflow: bool,
    tma: u8,
    sys_timer: u16,
}

impl Timer {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            prev_timer: FallingEdge::new(false),
            tac: 0,
            tima: 0,
            prev_tima_overflow: false,
            tima_overflow: false,
            tma: 0,
            // Gekkio got access to per clock timings!
            // On another note, yeah, the clock starts 8 t-cycles before
            // the first byte of the boot-rom is fetched.
            sys_timer: 8,
        }
    }

    #[inline(always)]
    #[must_use]
    fn tac_freq(tac: u8) -> u8 {
        tac & 0b11
    }

    #[inline(always)]
    #[must_use]
    fn tac_enable(tac: u8) -> bool {
        (tac >> 2) & 0b1 == 0b1
    }

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
                if !self.prev_tima_overflow {
                    self.tima = val;
                    self.tima_overflow = false;
                }
            }
            0x06 => {
                self.tma = val;

                if self.prev_tima_overflow {
                    self.tima = self.tma;
                }
            }
            0x07 => self.tac = val & 0b111,
            _ => unreachable!(),
        }
    }

    #[must_use]
    fn has_timer_bit(&self) -> bool {
        let freq = Self::tac_freq(self.tac);
        let bit = if freq == 0b00 {
            9
        } else {
            ((u16::from(freq)) << 1) + 1
        };

        ((self.sys_timer >> bit) & 1) > 0
    }

    #[must_use]
    pub fn tick(&mut self) -> InterruptFlags {
        let tick = self.sys_timer & 0b11;
        self.sys_timer = self.sys_timer.wrapping_add(1);

        match tick {
            0 => {
                self.prev_tima_overflow = self.tima_overflow;
                if self.tima_overflow {
                    self.tima_overflow = false;
                    self.tima = self.tma;

                    return InterruptFlags::TIMER;
                }

                InterruptFlags::empty()
            }

            3 => {
                let b = Self::tac_enable(self.tac) && self.has_timer_bit();
                if self.prev_timer.tick(b) {
                    (self.tima, self.tima_overflow) = self.tima.overflowing_add(1);
                }

                InterruptFlags::empty()
            }

            _ => InterruptFlags::empty(),
        }
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self::new()
    }
}
