// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use ::emu::bits;

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
    window_ly: u8,
    stat_upper: u8,
    stat_mode: u8,
    cycle_mod: i32,
    visible_ly: u8,
    disabled: bool,
    pirq: bool,
    lyc: u8,
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
            lyc: 0,
            window_ly: 0,
            stat_upper: 0,
            stat_mode: 0,
            cycle_mod: 0,
            visible_ly: 0,
            disabled: false,
            pirq: false,
        }
    }

    fn render_line(&mut self) {}

    fn update_line(&mut self) -> bool {
        match self.cycle_mod {
            0 | 212 => {self.stat_mode = 0; self.ly_cp() || (self.stat_upper & 4) == 4}
            04 => {self.stat_mode = 2;self.ly_cp() || bits::has_bit(self.stat_upper, 5)}
            40 => {self.stat_mode = 3;self.render_line(); self.ly_cp()}
            _ => self.ly_cp() || (self.stat_mode == 2 && bits::has_bit(self.stat_upper, 5)) || (self.stat_mode == 0 && bits::has_bit(self.stat_upper, 3))
        }

    }

    fn update_vblank_start(&mut self, r_ie: &mut u8) -> bool {
        if self.cycle_mod == 0 {
            self.window_ly = 0;
            self.ly_cp() || bits::has_bit(self.stat_upper, 3)
        } else {
            if self.cycle_mod == 4 {
                *r_ie |= bits::get(1);
                self.stat_mode = 1;
            }

            self.ly_cp() || bits::has_bit(self.stat_upper, 4) || bits::has_bit(self.stat_upper, 5)
        }
    }

    fn ly_cp(&mut self) -> bool {
        if (self.cycle_mod >= 4 && self.lyc == self.ly) || (self.cycle_mod < 4 && self.lyc == 0) {
            self.stat_upper |= bits::get(2);
            bits::has_bit(self.stat_upper, 6)
        } else {
            self.stat_upper &= bits::nget(2);
            false
        }
    }

    pub fn update(&mut self) -> u8 {
        use std::cmp::Ordering;

        if self.lcdc & bits::get(7) == 0 {
            self.disable();
            0
        } else {
            let mut vblank = 0;
            let irq = match self.ly.cmp(&144) {
                Ordering::Less =>    self.update_line(),
                Ordering::Equal =>   self.update_vblank_start(&mut vblank),
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

            let pirq = self.pirq;
            self.pirq = irq;
            vblank | if irq && !pirq {bits::get(1)} else {0}
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