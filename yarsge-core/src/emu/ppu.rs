use crate::RisingEdge;
use crate::emu::{InterruptFlags, bits};

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

bitflags::bitflags! {
    #[derive(Copy, Clone)]
    struct Lcdc : u8 {
        const BG_WINDOW_ENABLE = 1 << 0;
        const OBJ_ENABLE = 1 << 1;
        const OBJ_SIZE = 1 << 2;
        const BG_TILE_MAP = 1 << 3;
        const BG_WINDOW_TILES = 1 << 4;
        const WINDOW_ENABLE = 1 << 5;
        const WINDOW_TILE_MAP = 1 << 6;
        const LCD_ENABLE = 1 << 7;
    }
}

pub struct Ppu {
    vram: [u8; 0x2000],
    pub oam: [u8; 0xa0],
    display_memory: [DisplayPixel; 160 * 144],
    obj_pallet_a: u8,
    obj_pallet_b: u8,
    scx: u8,
    scy: u8,
    lcdc: Lcdc,
    ly: u8,
    bg_pallet: u8,
    window_ly: u8,
    stat_upper: u8,
    stat_mode: u8,
    cycle_mod: i32,
    visible_ly: u8,
    disabled: bool,
    pirq: RisingEdge,
    lyc: u8,
}

impl Ppu {
    #[must_use]
    pub fn get_display(&self) -> &[DisplayPixel] {
        &self.display_memory
    }

    #[must_use]
    fn oam_blocked(&self) -> bool {
        self.stat_mode > 1
    }

    #[must_use]
    fn vram_blocked(&self) -> bool {
        self.stat_mode == 3
    }

    #[must_use]
    pub fn get_vram(&self, addr: u16) -> Option<u8> {
        (!self.vram_blocked()).then(|| self.vram[addr as usize])
    }

    pub fn set_vram(&mut self, addr: u16, val: u8) {
        if !self.vram_blocked() {
            self.vram[addr as usize] = val;
        }
    }

    #[must_use]
    pub fn read_oam(&self, addr: u16) -> Option<u8> {
        (!self.oam_blocked()).then(|| self.oam[addr as usize])
    }

    pub fn write_oam(&mut self, addr: u16, val: u8) {
        if !self.oam_blocked() {
            self.oam[addr as usize] = val;
        }
    }

    pub fn set_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x40 => self.lcdc = Lcdc::from_bits_retain(val),
            0x41 => self.stat_upper = val & 0x78,
            0x42 => self.scy = val,
            0x43 => self.scx = val,
            0x44 => {}
            0x45 => self.lyc = val,
            0x4a | 0x4b => log::warn!(
                "todo: unimplemented PPU reg write: (addr: 0xff{addr:02x} val: {val:#02x})",
            ),
            0x47 => self.bg_pallet = val,
            0x48 => self.obj_pallet_a = val & 0xfc,
            0x49 => self.obj_pallet_b = val & 0xfc,
            _ => log::error!("BUG: invalid PPU write (0xff{addr:02x} -> {val:#02x})"),
        }
    }

    #[must_use]
    pub fn get_reg(&self, addr: u8) -> u8 {
        match addr {
            0x40 => self.lcdc.bits(),
            0x41 => self.stat_upper | 0x80 | self.stat_mode,
            0x42 => self.scy,
            0x43 => self.scx,
            0x44 => self.visible_ly,
            0x45 => self.lyc,
            0x4a | 0x4b => {
                eprintln!("todo: unimplemented PPU reg (read): (addr: 0xff{addr:02x})",);
                0xff
            }
            0x47 => self.bg_pallet,
            0x48 => self.obj_pallet_a,
            0x49 => self.obj_pallet_b,
            _ => {
                log::error!("BUG: invalid PPU write (0xff{addr:02x} -> 0xff)");
                0xff
            }
        }
    }

    fn disable(&mut self) {
        if !self.disabled {
            self.disabled = true;
            self.display_memory = [DisplayPixel::White; 160 * 144];
            self.ly = 0;
            self.visible_ly = 0;
            self.pirq = RisingEdge::new(false);
            self.cycle_mod = 0;
            self.stat_mode = 0;
            
        }
    }

    #[must_use]
    fn get_pixel_index(&self, tile: usize, y: usize, x: u8) -> u8 {
        let lower = self.vram[tile * 16 + y];
        let upper = self.vram[tile * 16 + y + 1];
        ((upper >> (7 - x) & 1) | ((lower >> (7 - x) & 1) * 2)) * 2
    }

    fn render_line_bg(&mut self) {
        fn get_map_base(lcdc: Lcdc) -> usize {
            if lcdc.contains(Lcdc::BG_TILE_MAP) {
                0x1c00
            } else {
                0x1800
            }
        }

        let map_offset =
            get_map_base(self.lcdc) + (((self.scy as usize + self.ly as usize) & 0xff) >> 3) * 32;

        let mut line_offset = self.scx as usize >> 3;
        let y = ((self.ly as usize + self.scy as usize) & 7) * 2;
        let mut x = self.scx & 7;

        let mut tile = self.vram[line_offset + map_offset] as usize;

        if !self.lcdc.contains(Lcdc::BG_WINDOW_TILES) && tile < 128 {
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
                if !self.lcdc.contains(Lcdc::BG_WINDOW_TILES) && tile < 128 {
                    tile += 256;
                }
            }
        }
    }

    fn render_line(&mut self) {
        if self.lcdc.contains(Lcdc::BG_WINDOW_ENABLE) {
            self.render_line_bg();
        }

        if self.lcdc.contains(Lcdc::WINDOW_ENABLE) {
            // TODO: window
        }

        if self.lcdc.contains(Lcdc::OBJ_ENABLE) {
            // TODO: sprites
        }
    }

    fn update_line(&mut self) -> bool {
        match self.cycle_mod {
            0 | 212 => {
                self.stat_mode = 0;
                self.ly_cp() || bits::has_bit(self.stat_upper, 3)
            }

            4 => {
                self.stat_mode = 2;
                self.ly_cp() || bits::has_bit(self.stat_upper, 5)
            }

            40 => {
                self.stat_mode = 3;
                self.render_line();
                self.ly_cp()
            }

            _ => {
                self.ly_cp()
                    || (self.stat_mode == 2 && bits::has_bit(self.stat_upper, 5))
                    || (self.stat_mode == 0 && bits::has_bit(self.stat_upper, 3))
            }
        }
    }

    #[must_use]
    fn update_vblank_start(&mut self) -> (InterruptFlags, bool) {
        if self.cycle_mod == 0 {
            self.window_ly = 0;
            (
                InterruptFlags::empty(),
                self.ly_cp() || bits::has_bit(self.stat_upper, 3),
            )
        } else {
            let vblank = if self.cycle_mod == 4 {
                self.stat_mode = 1;
                InterruptFlags::VBLANK
            } else {
                InterruptFlags::empty()
            };

            (
                vblank,
                self.ly_cp()
                    || bits::has_bit(self.stat_upper, 4)
                    || bits::has_bit(self.stat_upper, 5),
            )
        }
    }

    // nonminimal_bool seems to be messed up here.
    #[allow(clippy::nonminimal_bool)]
    fn ly_cp(&mut self) -> bool {
        if (self.cycle_mod >= 4 && self.lyc == self.ly) || (self.cycle_mod < 4 && self.lyc == 0) {
            self.stat_upper |= bits::get(2);
            bits::has_bit(self.stat_upper, 6)
        } else {
            self.stat_upper &= !bits::get(2);
            false
        }
    }

    #[must_use]
    pub fn tick(&mut self) -> InterruptFlags {
        use std::cmp::Ordering;

        if !self.lcdc.contains(Lcdc::LCD_ENABLE) {
            self.disable();
            return InterruptFlags::empty();
        }

        self.disabled = false;
        let (vblank, irq) = match self.ly.cmp(&144) {
            Ordering::Less => (InterruptFlags::empty(), self.update_line()),
            Ordering::Equal => self.update_vblank_start(),
            Ordering::Greater => (
                InterruptFlags::empty(),
                self.ly_cp() || (self.stat_upper & 0x30) > 0,
            ),
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

        let edge = self.pirq.tick(irq);

        let stat = if edge {
            InterruptFlags::STAT
        } else {
            InterruptFlags::empty()
        };

        vblank | stat
    }
}

impl Default for Ppu {
    fn default() -> Ppu {
        Ppu {
            display_memory: [DisplayPixel::White; 160 * 144],
            obj_pallet_a: 0,
            obj_pallet_b: 0,
            vram: [0; 0x2000],
            oam: [0; 0xa0],
            scx: 0,
            scy: 0,
            lcdc: Lcdc::empty(),
            ly: 0,
            lyc: 0,
            window_ly: 0,
            bg_pallet: 0,
            stat_upper: 0,
            stat_mode: 0,
            cycle_mod: 0,
            visible_ly: 0,
            disabled: false,
            pirq: RisingEdge::new(false),
        }
    }
}
