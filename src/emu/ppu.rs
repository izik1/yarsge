// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use emu::bits;

#[derive(Clone, Copy)]
pub enum DisplayPixel {
    White,
    LightGrey,
    DarkGrey,
    Black,
}

impl DisplayPixel {
    fn from_num(num: u8) -> DisplayPixel {
        match num {
            0 => DisplayPixel::White,
            1 => DisplayPixel::LightGrey,
            2 => DisplayPixel::DarkGrey,
            3 => DisplayPixel::Black,
            _ => panic!(),
        }
    }
}

pub struct Ppu {
    vram: [u8; 0x2000],
    pub oam: [u8; 0xA0],
    display_memory: [DisplayPixel; 160 * 144],
    obj_pallet_a: u8,
    obj_pallet_b: u8,
    scx: u8,
    scy: u8,
    lcdc: u8,
    ly: u8,
    bg_pallet: u8,
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
    pub fn get_display(&self) -> &[DisplayPixel] {
        &self.display_memory
    }

    pub fn oam_blocked(&self) -> bool {
        self.stat_mode > 1
    }

    fn vram_blocked(&self) -> bool {
        self.stat_mode == 3
    }

    pub fn get_vram(&self, addr: u16) -> u8 {
        if self.vram_blocked() {0xFF}
        else {self.vram[addr as usize]}
    }

    pub fn set_vram(&mut self, addr: u16, val: u8) {
        if !self.vram_blocked() {self.vram[addr as usize] = val}
    }

    pub fn set_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x40 => self.lcdc = val,
            0x41 => self.stat_upper = val & 0x78,
            0x42 => self.scy = val,
            0x43 => self.scx = val,
            0x44 => {}
            0x45 => self.lyc = val,
            0x4A | 0x4B => eprintln!(
                "Unimplemented PPU reg (write): (addr: 0xFF{:01$X} val: {2:03$X})",
                addr, 2, val, 2
            ),
            0x47 => self.bg_pallet = val,
            0x48 => self.obj_pallet_a = val & 0xFC,
            0x49 => self.obj_pallet_b = val & 0xFC,
            _ => unreachable!(),
        }
    }

    pub fn get_reg(&self, addr: u8) -> u8 {
        match addr {
            0x40 => self.lcdc,
            0x41 => self.stat_upper | 0x80 | self.stat_mode,
            0x42 => self.scy,
            0x43 => self.scx,
            0x44 => self.visible_ly,
            0x45 => self.lyc,
            0x4A | 0x4B => {
                eprintln!("Unimplemented PPU reg (read): (addr: 0xFF{:01$X})", addr, 2);
                0xFF
            }
            0x47 => self.bg_pallet,
            0x48 => self.obj_pallet_a,
            0x49 => self.obj_pallet_b,
            _ => unreachable!(),
        }
    }

    fn disable(&mut self) {
        if !self.disabled {
            self.disabled = true;
            self.display_memory = [DisplayPixel::White; 160 * 144];
            self.ly = 0;
            self.visible_ly = 0;
            self.pirq = false;
            self.cycle_mod = 0;
        }
    }

    pub fn new() -> Ppu {
        Ppu {
            display_memory: [DisplayPixel::White; 160 * 144],
            obj_pallet_a: 0,
            obj_pallet_b: 0,
            vram: [0; 0x2000],
            oam: [0; 0xA0],
            scx: 0,
            scy: 0,
            lcdc: 0,
            ly: 0,
            lyc: 0,
            window_ly: 0,
            bg_pallet: 0,
            stat_upper: 0,
            stat_mode: 0,
            cycle_mod: 0,
            visible_ly: 0,
            disabled: false,
            pirq: false,
        }
    }

    fn get_pixel_index(&self, tile: usize, y: usize, x: u8) -> u8 {
        let lower = self.vram[tile * 16 + y + 0];
        let upper = self.vram[tile * 16 + y + 1];
        ((upper >> (7 - x) & 1) | ((lower >> (7 - x) & 1) * 2)) * 2
    }

    fn render_line_bg(&mut self) {
        let map_offset = if bits::has_bit(self.lcdc, 3) {
            0x1C00
        } else {
            0x1800
        } + (((self.scy as usize + self.ly as usize) & 0xFF) >> 3) * 32;

        let mut line_offset = self.scx as usize >> 3;
        let y = ((self.ly as usize + self.scy as usize) & 7) * 2;
        let mut x = self.scx & 7;

        let mut tile = self.vram[line_offset as usize + map_offset as usize] as usize;

        if !bits::has_bit(self.lcdc, 4) && tile < 128 {
            tile += 256;
        }

        for i in 0..160 {
            let index = self.get_pixel_index(tile, y, x);
            self.display_memory[((self.ly as usize) * 160) + i] =
                DisplayPixel::from_num((self.bg_pallet >> index) & 0b11);
            x += 1;
            if x == 8 {
                x = 0;
                line_offset = (line_offset + 1) & 31;
                tile = self.vram[line_offset + map_offset] as usize;
                if !bits::has_bit(self.lcdc, 4) && tile < 128 {
                    tile += 256;
                }
            }
        }
    }

    fn render_line(&mut self) {
        if bits::has_bit(self.lcdc, 0) {
            self.render_line_bg();
        }

        if bits::has_bit(self.lcdc, 5) {
            // TODO: window
        }

        if bits::has_bit(self.lcdc, 1) {
            // TODO: sprites
        }
    }

    fn update_line(&mut self) -> bool {
        match self.cycle_mod {
            0 | 212 => {
                self.stat_mode = 0;
                self.ly_cp() || (self.stat_upper & 4) == 4
            }
            04 => {
                self.stat_mode = 2;
                self.ly_cp() || bits::has_bit(self.stat_upper, 5)
            }
            40 => {
                self.stat_mode = 3;
                self.render_line();
                self.ly_cp()
            }
            _ => {
                self.ly_cp() || (self.stat_mode == 2 && bits::has_bit(self.stat_upper, 5))
                    || (self.stat_mode == 0 && bits::has_bit(self.stat_upper, 3))
            }
        }
    }

    fn update_vblank_start(&mut self) -> (u8, bool) {
        if self.cycle_mod == 0 {
            self.window_ly = 0;
            (0, self.ly_cp() || bits::has_bit(self.stat_upper, 3))
        } else {
            let vblank = if self.cycle_mod == 4 {
                self.stat_mode = 1;
                bits::get(0)
            } else {
                0
            };

            (
                vblank,
                self.ly_cp() || bits::has_bit(self.stat_upper, 4)
                    || bits::has_bit(self.stat_upper, 5),
            )
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
            self.disabled = false;
            let (vblank, irq) = match self.ly.cmp(&144) {
                Ordering::Less => (0, self.update_line()),
                Ordering::Equal => self.update_vblank_start(),
                Ordering::Greater => (0, self.ly_cp() || (self.stat_upper & 0x30) > 0),
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
            vblank | if irq && !pirq { bits::get(1) } else { 0 }
        }
    }

    pub fn display_to_bytes(&self) -> [u8; (160 * 144) / 4] {
        let mut arr = [0; 160 * 144 / 4];
        for i in 0..160 * 144 {
            arr[i / 4] |= match self.display_memory[i] {
                DisplayPixel::White => 0,
                DisplayPixel::LightGrey => 1,
                DisplayPixel::DarkGrey => 2,
                DisplayPixel::Black => 3,
            } << (i & 3);
        }

        arr
    }
}
