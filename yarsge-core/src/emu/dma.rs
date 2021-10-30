pub struct Dma {
    pub modulus: u8,
    pub time: usize,
    pub enabled: bool,
    pub addr: u16,
    pub ld_timer: i8,
    pub ld_addr: u16,
}

impl Default for Dma {
    fn default() -> Self {
        Dma {
            modulus: 0,
            time: 0,
            enabled: false,
            addr: 0,
            ld_timer: -1,
            ld_addr: 0,
        }
    }
}

impl Dma {
    pub fn update(&mut self) -> Option<(usize, u16)> {
        let mut return_val = None;

        if self.modulus == 0 {
            self.enabled = self.time > 0;
            if self.enabled {
                return_val = Some((160 - self.time, self.addr));
                self.time -= 1;
            }

            self.modulus = 3;
        } else {
            self.modulus -= 1;
        };

        if self.ld_timer > 0 {
            self.ld_timer -= 1;
            if self.ld_timer == 0 {
                self.time = 160;
                self.addr = self.ld_addr;
            }
        }

        return_val
    }
}
