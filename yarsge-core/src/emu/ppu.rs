use std::array;
use std::sync::atomic::{self, AtomicU64};

use crate::RisingEdge;
use crate::emu::{InterruptFlags, bits};

pub type Vram = [u8; 0x2000];
pub type Oam = [u8; 0xa0];

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum DisplayPixel {
    White = 0,
    LightGrey = 1,
    DarkGrey = 2,
    Black = 3,
}

impl DisplayPixel {
    fn from_bits_truncate(bits: u8) -> DisplayPixel {
        match bits & 0b11 {
            0 => DisplayPixel::White,
            1 => DisplayPixel::LightGrey,
            2 => DisplayPixel::DarkGrey,
            3 => DisplayPixel::Black,
            _ => unreachable!(),
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

// 160 / 4 = 40 is clean
struct DisplayMemory([u8; (160 / 4) * 144]);

impl DisplayMemory {
    const fn new() -> Self {
        Self([0; (160 / 4) * 144])
    }

    fn set4(&mut self, elem: usize, values: [DisplayPixel; 4]) {
        let value = {
            let mut value = 0_u8;
            for (idx, elem) in values.into_iter().enumerate() {
                value |= (elem as u8) << (idx * 2);
            }

            value
        };

        self.0[elem] = value;
    }

    fn set(&mut self, offset: usize, value: DisplayPixel) {
        let byte = &mut self.0[offset / 4];
        let idx = offset % 4;
        let mask = !(0b11_u8 << ((idx) * 2));

        *byte = (*byte & mask) | (value as u8) << ((idx) * 2);
    }

    fn iter(&self) -> impl IntoIterator<Item = DisplayPixel> {
        struct Iter<'a>(&'a [u8], u8);

        impl Iterator for Iter<'_> {
            type Item = DisplayPixel;

            fn next(&mut self) -> Option<Self::Item> {
                let (&first, rest) = self.0.split_first()?;

                let old = self.1;
                self.1 = (self.1 + 1) & 0b11;

                if self.1 == 0 {
                    self.0 = rest;
                }

                Some(DisplayPixel::from_bits_truncate(first >> (2 * old)))
            }

            fn nth(&mut self, n: usize) -> Option<Self::Item> {
                // pretend we've backtracked to bit 0 to make the math easy.
                let n = n + self.1 as usize;

                // skip n/4 bytes (fast)
                let (_, b) = self.0.split_at_checked(n / 4)?;
                self.0 = b;
                // then skip n % 4 bit pairs (also fast)
                self.1 = (n % 4) as u8;

                // then grab the value
                self.next()
            }
        }

        Iter(&self.0, 0)
    }
}

struct BgFifo {
    raw: u16,
    len: u8,
}

impl BgFifo {
    const fn new() -> Self {
        Self { raw: 0, len: 0 }
    }

    fn push8(&mut self, pxs: u16) -> bool {
        if self.len == 0 {
            self.raw = pxs;
            self.len = 8;
            true
        } else {
            false
        }
    }

    fn pop(&mut self) -> Option<u8> {
        self.len = self.len.checked_sub(1)?;
        let value = self.raw >> 14;
        self.raw <<= 2;

        Some((value as u8) & 0b11)
    }
}

fn interleave(hi: u8, lo: u8) -> u16 {
    let mut res = 0;
    let mut hi = hi.reverse_bits() as u16;
    let mut lo = lo.reverse_bits() as u16;

    for _ in 0..8 {
        res <<= 2;
        res |= ((hi & 1) << 1) | (lo & 1);
        hi >>= 1;
        lo >>= 1;
    }

    res
}
enum PixelFetcherState {
    GetTileD1,
    GetTileD2 { tile_index: u8 },
    TileDataLowD1 { tile_index: u8 },
    TileDataLowD2 { tile_index: u8, tile_low: u8 },
    TileDataHighD1 { tile_index: u8, tile_low: u8 },
    TileDataHighD2 { tile_low: u8, tile_high: u8 },
    Push { tile_low: u8, tile_high: u8 },
}

struct PixelFetcher {
    st: PixelFetcherState,
    first: bool,
    x: u8,
}

impl PixelFetcher {
    const fn new() -> Self {
        Self {
            st: PixelFetcherState::GetTileD1,
            first: true,
            x: 0,
        }
    }

    fn tick(&mut self, vram: &Vram, fifo: &mut BgFifo, scx: u8, lcdc: Lcdc, y: u8) {
        fn get_map_base(lcdc: Lcdc) -> usize {
            if lcdc.contains(Lcdc::BG_TILE_MAP) {
                0x1c00
            } else {
                0x1800
            }
        }

        fn get_tile<const OBJ: bool>(tile_index: u8, lcdc: Lcdc) -> usize {
            if OBJ || lcdc.contains(Lcdc::BG_WINDOW_TILES) {
                return tile_index as usize * 16;
            }

            // transform from 0x00..=0xff to [0x0000..=0x07f0] and [-0x0800..0x0000]
            let tile_index = usize::from(
                i16::from(tile_index.cast_signed())
                    .wrapping_mul(16)
                    .cast_unsigned(),
            );

            0x1000_usize.wrapping_add(tile_index)
        }

        match self.st {
            PixelFetcherState::GetTileD1 => {
                let x = if self.first {
                    self.first = false;
                    self.x
                } else {
                    let x = self.x;
                    self.x += 1;
                    x
                };

                let x = (x + (scx >> 3)) & 0x1f;
                let y = y >> 3;
                let tile_index = (y as usize) * 32 + x as usize;
                let offset = get_map_base(lcdc) + tile_index;

                let tile_index = vram[offset];

                self.st = PixelFetcherState::GetTileD2 { tile_index };
            }
            PixelFetcherState::GetTileD2 { tile_index } => {
                self.st = PixelFetcherState::TileDataLowD1 { tile_index };
            }
            PixelFetcherState::TileDataLowD1 { tile_index } => {
                let tile_base = get_tile::<false>(tile_index, lcdc);
                let tile_addr = tile_base + usize::from(y % 8) * 2;
                self.st = PixelFetcherState::TileDataLowD2 {
                    tile_index,
                    tile_low: vram[tile_addr],
                }
            }
            PixelFetcherState::TileDataLowD2 {
                tile_index,
                tile_low,
            } => {
                self.st = PixelFetcherState::TileDataHighD1 {
                    tile_index,
                    tile_low,
                }
            }
            PixelFetcherState::TileDataHighD1 {
                tile_index,
                tile_low,
            } => {
                let tile_base = get_tile::<false>(tile_index, lcdc);
                let tile_addr = tile_base + usize::from(y % 8) * 2 + 1;
                self.st = PixelFetcherState::TileDataHighD2 {
                    tile_low,
                    tile_high: vram[tile_addr],
                };
            }
            PixelFetcherState::TileDataHighD2 {
                tile_low,
                tile_high,
            }
            | PixelFetcherState::Push {
                tile_low,
                tile_high,
            } => {
                self.st = if fifo.push8(interleave(tile_high, tile_low)) {
                    PixelFetcherState::GetTileD1
                } else {
                    PixelFetcherState::Push {
                        tile_low,
                        tile_high,
                    }
                };
            }
        }
    }
}

enum PpuState {
    Disabled,
    OamScan,
    Draw {
        bg_fetcher: PixelFetcher,
        bg_fifo: BgFifo,
        screen_x: u8,
    },
    HBlank,
    Vblank,
}

bitflags::bitflags! {
    struct StatUpper : u8 {
        const LYC_INT_SELECT = 1 << 6;
        const MODE_2_INT_SELECT = 1 << 5;
        const MODE_1_INT_SELECT = 1 << 4;
        const MODE_0_INT_SELECT = 1 << 3;
        const LYC_LY_COINCIDENCE = 1 << 2;
    }
}

pub struct Ppu {
    vram: Vram,
    pub(crate) oam: Oam,
    display_memory: DisplayMemory,
    obj_pallet_a: u8,
    obj_pallet_b: u8,
    scx: u8,
    scy: u8,
    lcdc: Lcdc,
    ly: u8,
    // cmp_ly: u8,
    bg_pallet: u8,
    window_ly: u8,
    stat_upper: StatUpper,
    // visible value for `stat`'s lower two bits.
    stat_mode: u8,
    cycle_mod: i16,
    visible_ly: u8,
    state: PpuState,
    pirq: RisingEdge,
    lyc: u8,
}

impl Ppu {
    #[must_use]
    pub fn get_display(&self) -> impl IntoIterator<Item = DisplayPixel> {
        self.display_memory.iter()
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
            0x41 => self.stat_upper = StatUpper::from_bits_truncate(val),
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
            0x41 => self.stat_upper.bits() | 0x80 | self.stat_mode,
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
        if !matches!(self.state, PpuState::Disabled) {
            self.state = PpuState::Disabled;
            self.display_memory = DisplayMemory::new();
            self.ly = 0;
            self.visible_ly = 0;
            self.pirq = RisingEdge::new(false);
            self.cycle_mod = 0;
            self.stat_mode = 0;
        }
    }

    // nonminimal_bool seems to be messed up here.
    #[allow(clippy::nonminimal_bool)]
    fn ly_cp(&mut self) -> bool {
        if (self.cycle_mod >= 4 && self.lyc == self.ly) || (self.cycle_mod < 4 && self.lyc == 0) {
            self.stat_upper.insert(StatUpper::LYC_LY_COINCIDENCE);
            return self.stat_upper.contains(StatUpper::LYC_INT_SELECT);
        } else {
            self.stat_upper.remove(StatUpper::LYC_LY_COINCIDENCE);
            false
        }
    }

    // a lot of this comes from [pandocs](https://gbdev.io/pandocs/Rendering.html) and [gameroy](https://github.com/Rodrigodd/gameroy/blob/a5acdc921c0561ed93a077622b598df0e068583c/core/src/gameboy/ppu.rs#L936)
    fn tick_state(&mut self, cycle: i16, reg_if: &mut InterruptFlags) {
        let mut irq = false;
        irq |= self.ly_cp();

        match &mut self.state {
            PpuState::Disabled => unreachable!(),
            PpuState::OamScan => match cycle {
                0 => self.stat_mode = 0,
                1..3 => {}
                3 => {}
                4 => {
                    self.stat_mode = 2;
                }
                5..79 => {}
                79 => {
                    self.state = PpuState::Draw {
                        bg_fetcher: PixelFetcher::new(),
                        bg_fifo: BgFifo::new(),
                        // going to arbitrarily assume that "fine scx" is decided here until I have a better idea.
                        screen_x: 0_u8.wrapping_sub(8).wrapping_sub(self.scx % 8),
                    };
                }
                _ => unreachable!(),
            },
            PpuState::Draw {
                bg_fetcher,
                bg_fifo,
                screen_x,
            } => match cycle {
                0..80 => unreachable!("entered draw at line-cycle {cycle}"),
                80..84 => {}
                84 => {
                    self.stat_mode = 3;
                    bg_fetcher.tick(&self.vram, bg_fifo, self.scx, self.lcdc, self.ly + self.scy);
                }
                _ => {
                    // eprintln!("draw (screen_x={screen_x}, fifo={})", bg_fifo.len);

                    if let Some(px) = bg_fifo.pop() {
                        let old_x = *screen_x;
                        *screen_x += 1;
                        if old_x < 160 {
                            let px = (self.bg_pallet >> (px * 2)) & 0b11;

                            self.display_memory.set(
                                usize::from(self.ly) * 160 + usize::from(old_x),
                                DisplayPixel::from_bits_truncate(px),
                            );
                        }
                    }

                    bg_fetcher.tick(&self.vram, bg_fifo, self.scx, self.lcdc, self.ly + self.scy);

                    if *screen_x == 160 {
                        self.state = PpuState::HBlank;
                    }
                }
            },
            PpuState::HBlank => {
                self.stat_mode = 0;

                if cycle == 455 {
                    self.ly += 1;
                    self.visible_ly = self.ly;

                    self.state = if self.ly == 144 {
                        PpuState::Vblank
                    } else {
                        PpuState::OamScan
                    };
                }
            }
            PpuState::Vblank => match cycle {
                4 => {
                    self.stat_mode = 1;

                    if self.ly == 144 {
                        *reg_if |= InterruptFlags::VBLANK;
                    }

                    if self.ly == 153 {
                        self.visible_ly = 0;
                    }
                }
                455 => {
                    self.ly += 1;
                    if self.ly == 154 {
                        self.ly = 0;
                        self.state = PpuState::OamScan;
                    } else {
                        self.visible_ly = self.ly;
                    }
                }
                _ => {}
            },
        }

        irq |= match self.stat_mode {
            0 => self.stat_upper.contains(StatUpper::MODE_0_INT_SELECT),
            1 => self.stat_upper.contains(StatUpper::MODE_1_INT_SELECT),
            2 => self.stat_upper.contains(StatUpper::MODE_2_INT_SELECT),
            _ => false,
        };

        static TICKS: AtomicU64 = AtomicU64::new(0);
        let ticks = TICKS.fetch_add(1, atomic::Ordering::Relaxed);

        let edge = self.pirq.tick(irq);
        if edge && ticks < (2 << 22) * 12 {
            eprintln!("e=1;t={}", ticks);
        }

        if edge {
            *reg_if |= InterruptFlags::STAT;
        }
    }

    #[must_use]
    pub fn tick(&mut self) -> InterruptFlags {
        let mut reg_if = InterruptFlags::empty();

        if !self.lcdc.contains(Lcdc::LCD_ENABLE) {
            self.disable();
            return reg_if;
        }

        if matches!(self.state, PpuState::Disabled) {
            self.state = PpuState::OamScan;
        }

        let cycle = {
            let old = self.cycle_mod;
            self.cycle_mod = (old + 1) % 456;
            old
        };

        self.tick_state(cycle, &mut reg_if);

        reg_if
    }
}

impl Default for Ppu {
    fn default() -> Ppu {
        Ppu {
            display_memory: DisplayMemory::new(),
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
            stat_upper: StatUpper::empty(),
            stat_mode: 0,
            cycle_mod: 0,
            visible_ly: 0,
            pirq: RisingEdge::new(false),
            state: PpuState::Disabled,
        }
    }
}
