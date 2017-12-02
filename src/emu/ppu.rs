// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

#[derive(Clone, Copy)]
pub enum DisplayPixel {
    White,
    LightGrey,
    DarkGrey,
    Black,
}

pub struct Ppu {
    display_memory: [DisplayPixel; 160*144],
    scx: u8,
    scy: u8,
    lcdc: u8,
    ly: u8,
    stat_upper: u8,
    cycle_mod: i32,
    visible_ly: u8,
    disabled: bool,
    pirq: bool,
}

impl Ppu {
    pub fn set_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x40 => self.lcdc = val,
            0x41 => self.scx = val,
            0x42 => self.scy = val,
            0x43 | 0x45...0x4B => eprintln!("Unimplemented PPU reg (write): (addr: 0xFF{:01$X} val: {2:03$X})", addr, 2, val, 2),
            0x44 => {}
            _ => unreachable!(),

        }
    }

    pub fn get_reg(&self, addr: u8) -> u8 {
        match addr {
            0x40 => self.lcdc,
            0x41 => self.scx,
            0x42 => self.scy,
            0x44 => self.visible_ly,
            0x43 | 0x45...0x4B => {eprintln!("Unimplemented PPU reg (read): (addr: 0xFF{:01$X})", addr, 2); 0xFF}
            _ => unreachable!(),
        }
    }

    fn disable(&mut self) {
        if !self.disabled {
            self.disabled = true;
        }
    }

    pub fn new() -> Ppu{
        Ppu {
            display_memory: [DisplayPixel::White; 160*144],
            scx: 0,
            scy: 0,
            lcdc: 0,
            ly: 0,
            stat_upper: 0,
            cycle_mod: 0,
            visible_ly: 0,
            disabled: false,
            pirq: false,
        }
    }

    fn update_line(&mut self) -> bool {
        false
    }

    fn update_vblank_start(&mut self) -> bool {
        false
    }

    fn ly_cp(&mut self) -> bool {
        false
    }

    pub fn update(&mut self, r_if: &mut u8) {
        use std::cmp::Ordering;

        if self.lcdc & 0x80 == 0 {
            self.disable();
        } else {
            let irq = match self.ly.cmp(&144) {
                Ordering::Less =>    self.update_line(),
                Ordering::Equal =>   self.update_vblank_start(),
                Ordering::Greater => self.ly_cp() || (self.stat_upper & 0x30) > 0,
            };

            self.cycle_mod += 1;
            if self.cycle_mod == 114 * 4 {
                self.cycle_mod = 0;
                if self.ly == 153 {
                    self.ly = 0;
                } else {
                    self.ly += 1;
                    self.visible_ly += 1;
                }

            }

            if self.ly == 153 && self.cycle_mod == 4 {
                self.disabled = false;
                self.visible_ly = 0;
            }

            if irq && !self.pirq {
                *r_if |= 2;
            }

            self.pirq = irq;
        }
    }

    pub fn display_to_bytes(&self) -> [u8; (160*144)/4] {
        let mut arr = [0; 160*144/4];
        for i in 0..160*144 {
            arr[i / 4] |= match self.display_memory[i] {
                DisplayPixel::White     => 0,
                DisplayPixel::LightGrey => 1,
                DisplayPixel::DarkGrey  => 2,
                DisplayPixel::Black     => 3,

            } << (i & 3);
        }

        arr
    }
}