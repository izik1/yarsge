use std::cmp;

use arrayvec::ArrayVec;

use crate::RisingEdge;
use crate::emu::InterruptFlags;

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
    #[inline(always)]
    fn from_bits_truncate(bits: u8) -> Self {
        match bits & 0b11 {
            0 => Self::White,
            1 => Self::LightGrey,
            2 => Self::DarkGrey,
            3 => Self::Black,
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

    fn set(&mut self, offset: usize, value: DisplayPixel) {
        let byte = &mut self.0[offset / 4];
        let idx = offset % 4;
        let mask = !(0b11_u8 << ((idx) * 2));

        *byte = (*byte & mask) | (value as u8) << ((idx) * 2);
    }

    #[inline]
    const fn iter(&self) -> impl Iterator<Item = DisplayPixel> {
        struct Iter<'a>(&'a [u8], u8);

        impl Iterator for Iter<'_> {
            type Item = DisplayPixel;

            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                let &[first, ref rest @ ..] = self.0 else {
                    return None;
                };

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

struct SpritePixel {
    bg_over_sprite: bool,
    palette: bool,
    color: u8,
}

struct SpriteFifo {
    raw: u16,
    bg_over_sprite: u8,
    palette: u8,
    len: u8,
}

impl SpriteFifo {
    const fn new() -> Self {
        Self {
            raw: 0,
            bg_over_sprite: 0,
            palette: 0,

            len: 0,
        }
    }

    // always succeeds a push ("overlapping" pixels just get skipped)
    fn push8(&mut self, pxs: u16, bg_over_sprite: bool, palette: bool) {
        self.raw |= pxs & (u16::MAX >> (self.len * 2));

        self.bg_over_sprite |= u8::from(bg_over_sprite) * (u8::MAX >> self.len);
        self.palette |= u8::from(palette) * (u8::MAX >> self.len);

        self.len = 8;
    }

    fn pop(&mut self) -> SpritePixel {
        self.len = self.len.saturating_sub(1);
        let value = self.raw >> 14;
        let bg_over_sprite = self.bg_over_sprite >> 7 == 1;
        let palette = self.palette >> 7 == 1;
        self.raw <<= 2;
        self.bg_over_sprite <<= 1;
        self.palette <<= 1;

        SpritePixel {
            bg_over_sprite,
            palette,
            color: (value as u8) & 0b11,
        }
    }
}

fn interleave(hi: u8, lo: u8) -> u16 {
    let mut res = 0;
    let mut hi = u16::from(hi.reverse_bits());
    let mut lo = u16::from(lo.reverse_bits());

    for _ in 0..8 {
        res <<= 2;
        res |= ((hi & 1) << 1) | (lo & 1);
        hi >>= 1;
        lo >>= 1;
    }

    res
}

const fn get_map_base(lcdc: Lcdc) -> usize {
    0x1800 + (lcdc.contains(Lcdc::BG_TILE_MAP) as usize) * 400
}

fn get_tile<const OBJ: bool>(tile_index: u8, lcdc: Lcdc) -> usize {
    // conveinently, tall tiles are big-endian, so we can just mask out the bottom bit and still get the right math later.
    if OBJ && lcdc.contains(Lcdc::OBJ_SIZE) {
        return (tile_index & 0xfe) as usize * 16;
    }

    if OBJ || lcdc.contains(Lcdc::BG_WINDOW_TILES) {
        return tile_index as usize * 16;
    }

    // transform from 0x00..=0xff to [0x0000..=0x07f0] and [-0x0800..0x0000]
    let tile_index = isize::from(tile_index.cast_signed()).wrapping_mul(16);

    0x1000_usize.wrapping_add_signed(tile_index)
}

const fn is_sprite_onscreen(ly: u8, sy: u8, lcdc: Lcdc) -> bool {
    let height: u8 = if lcdc.contains(Lcdc::OBJ_SIZE) { 16 } else { 8 };

    let y = ly.wrapping_add(16).wrapping_sub(sy);

    y.cast_signed() >= 0 && y < height
}

fn get_sprite_y(ly: u8, sy: u8, lcdc: Lcdc, y_flip: bool) -> usize {
    let height: u8 = if lcdc.contains(Lcdc::OBJ_SIZE) { 16 } else { 8 };

    // we can already assume that if we're here the sprite is supposed to be drawn at all (ie,  the )

    // ly=0, sy=0, OBJ_SIZE=1 -> function should never be called (off the top of the screen)
    // ly=0, sy<=8, OBJ_SIZE=0 -> function should never be called (off the top of the screen)
    // ly=0, sy=17, OBJ_SIZE=? -> function should never be called (off the bottom of the screen)

    // ly=0, sy=1, OBJ_SIZE=1 -> `15 * 2` (vram[(base + 30)..][..2])
    // full math:
    // y = (h - 1) - ((sy + h) - (ly + 16 + 1))
    // y = (h - 1) - (sy + h) + (ly + 16 + 1)
    // y = (-1) - (sy) + (ly + 16 + 1)
    // y = -sy + ly + 16
    let y = ly + 16 - sy;
    debug_assert!(y.cast_signed() >= 0, "sprite off the top of the screen");
    debug_assert!(y < height, "sprite off the bottom of the screen");

    if y_flip {
        usize::from(height - y - 1)
    } else {
        usize::from(y)
    }
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
    sprite: Option<Sprite>,
    first: bool,
    x: u8,
}

impl PixelFetcher {
    const fn new() -> Self {
        Self {
            st: PixelFetcherState::GetTileD1,
            first: true,
            x: 0,
            sprite: None,
        }
    }

    const fn can_interrupt(&self, bg_fifo: &BgFifo) -> bool {
        matches!(self.st, PixelFetcherState::Push { .. }) && bg_fifo.len > 0
    }

    const fn can_obj_fetch_cancel(&self) -> bool {
        self.sprite.is_some() && !matches!(self.st, PixelFetcherState::TileDataHighD2 { .. })
    }

    fn tick(
        &mut self,
        vram: &Vram,
        bg_fifo: &mut BgFifo,
        sprite_fifo: &mut SpriteFifo,
        scx: u8,
        scy: u8,
        lcdc: Lcdc,
        ly: u8,
    ) {
        fn tile_addr(
            sprite: Option<&Sprite>,
            tile_index: u8,
            lcdc: Lcdc,
            ly: u8,
            scy: u8,
        ) -> usize {
            match sprite {
                Some(sprite) => {
                    let base = get_tile::<true>(tile_index, lcdc);
                    base + get_sprite_y(
                        ly,
                        sprite.sy,
                        lcdc,
                        sprite.flags.contains(SpriteFlags::Y_FLIP),
                    ) * 2
                }

                None => {
                    let base = get_tile::<false>(tile_index, lcdc);
                    base + usize::from(ly.wrapping_add(scy) % 8) * 2
                }
            }
        }

        match self.st {
            PixelFetcherState::GetTileD1 => {
                if let Some(sprite) = &self.sprite {
                    self.st = PixelFetcherState::GetTileD2 {
                        tile_index: sprite.tile,
                    };
                    return;
                }

                let x = self.x;

                let x = (x.wrapping_add(scx >> 3)) & 0x1f;
                let y = ly.wrapping_add(scy) >> 3;
                let tile_index = (y as usize) * 32 + x as usize;
                let offset = get_map_base(lcdc) + tile_index;

                let tile_index = vram[offset];

                self.st = PixelFetcherState::GetTileD2 { tile_index };
            }
            PixelFetcherState::GetTileD2 { tile_index } => {
                self.st = PixelFetcherState::TileDataLowD1 { tile_index };
            }
            PixelFetcherState::TileDataLowD1 { tile_index } => {
                let tile_addr = tile_addr(self.sprite.as_ref(), tile_index, lcdc, ly, scy);

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
                let tile_addr = tile_addr(self.sprite.as_ref(), tile_index, lcdc, ly, scy) + 1;

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
                if let Some(sprite) = self.sprite.take() {
                    let (tile_high, tile_low) = if sprite.flags.contains(SpriteFlags::X_FLIP) {
                        (tile_high.reverse_bits(), tile_low.reverse_bits())
                    } else {
                        (tile_high, tile_low)
                    };

                    sprite_fifo.push8(
                        interleave(tile_high, tile_low),
                        sprite.flags.contains(SpriteFlags::BG_OVER_SPRITE),
                        sprite.flags.contains(SpriteFlags::PALETTE),
                    );

                    self.st = PixelFetcherState::GetTileD1;
                    return;
                }

                self.st = if bg_fifo.push8(interleave(tile_high, tile_low)) {
                    if self.first {
                        self.first = false;
                    } else {
                        self.x += 1;
                    }

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

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    struct SpriteFlags : u8 {
        const BG_OVER_SPRITE = 1 << 7;
        const Y_FLIP = 1 << 6;
        const X_FLIP = 1 << 5;
        const PALETTE = 1 << 4;
    }
}

#[derive(Clone, Copy)]
struct Sprite {
    sx: u8,
    sy: u8,
    tile: u8,
    flags: SpriteFlags,
}

enum PpuState {
    Disabled,
    OamScan,
    Draw {
        px_fetcher: PixelFetcher,
        bg_fifo: BgFifo,
        sprite_fifo: SpriteFifo,
        sprites: ArrayVec<Sprite, 10>,
        screen_x: u8,
    },
    HBlank,
    Vblank,
}

impl PpuState {
    const fn draw(sprites: ArrayVec<Sprite, 10>, scx: u8) -> Self {
        Self::Draw {
            px_fetcher: const { PixelFetcher::new() },
            bg_fifo: const { BgFifo::new() },
            sprite_fifo: const { SpriteFifo::new() },
            sprites,
            // going to arbitrarily assume that "fine scx" is decided here until I have a better idea.
            screen_x: 0_u8.wrapping_sub(8).wrapping_sub(scx % 8),
        }
    }
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
    pub const fn new() -> Self {
        Self {
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

    #[must_use]
    pub const fn get_display(&self) -> impl IntoIterator<Item = DisplayPixel> {
        self.display_memory.iter()
    }

    #[must_use]
    const fn oam_blocked(&self) -> bool {
        self.stat_mode > 1
    }

    #[must_use]
    const fn vram_blocked(&self) -> bool {
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
        self.stat_upper.set(
            StatUpper::LYC_LY_COINCIDENCE,
            (self.cycle_mod >= 4 && self.lyc == self.ly) || (self.cycle_mod < 4 && self.lyc == 0),
        );
        self.stat_upper
            .contains(const { StatUpper::LYC_LY_COINCIDENCE.union(StatUpper::LYC_INT_SELECT) })
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
                    let mut count: usize = 0;
                    let mut sprites: ArrayVec<Sprite, 10> = ArrayVec::new_const();

                    for idx in 0_usize..40 {
                        if count >= 10 {
                            break;
                        }

                        let [sy, sx, tile, flags] = self.oam[idx * 4..][..4].try_into().unwrap();

                        let sprite = Sprite {
                            sy,
                            sx,
                            tile,
                            flags: SpriteFlags::from_bits_truncate(flags),
                        };

                        if is_sprite_onscreen(self.ly, sprite.sy, self.lcdc) {
                            count += 1;

                            if sprites.iter().any(|it| it.sx == sprite.sx) {
                                continue;
                            }

                            if sprites.try_push(sprite).is_err() {
                                break;
                            }
                        }
                    }

                    sprites.sort_by_key(|sprite| cmp::Reverse(sprite.sx));

                    self.state = PpuState::draw(sprites, self.scx);
                }
                _ => unreachable!(),
            },
            PpuState::Draw {
                px_fetcher,
                bg_fifo,
                sprite_fifo,
                sprites,
                screen_x,
            } => match cycle {
                0..80 => unreachable!("entered draw at line-cycle {cycle}"),
                80..84 => {}
                84 => {
                    self.stat_mode = 3;
                    px_fetcher.tick(
                        &self.vram,
                        bg_fifo,
                        sprite_fifo,
                        self.scx,
                        self.scy,
                        self.lcdc,
                        self.ly,
                    );
                }
                _ => {
                    // eprintln!("draw (screen_x={screen_x}, fifo={})", bg_fifo.len);

                    // sprites:
                    // if screen_x == (sprite.x - 8):
                    // - pause pixel pop
                    // - wait until fetcher is interruptable (fetcher just entered `GetTileD1`- hasn't _run that_ yet(?)- or fifo isn't empty and fetcher wants to push pixels)
                    // - run the fetcher for the sprite fifo
                    //   - read the tile index and attributes/flags from OAM
                    //   - grab tile data low
                    //   - grab tile data high
                    //   - push as many pixels to sprite fifo as would fit
                    //     - discard the leftmost pixels that don't fit (these are occupied by another sprite that won priority
                    // - pop pixel (merge fifos, screen_x += 1)

                    let allow_pop = 'b: {
                        if let Some(sprite) = sprites.last().copied()
                            && (*screen_x).wrapping_add(8) == sprite.sx
                        {
                            if !self.lcdc.contains(Lcdc::OBJ_ENABLE)
                                && px_fetcher.can_obj_fetch_cancel()
                            {
                                sprites.pop();
                                px_fetcher.sprite = None;
                                px_fetcher.st = PixelFetcherState::GetTileD1;

                                break 'b true;
                            }

                            if !self.lcdc.contains(Lcdc::OBJ_ENABLE) && px_fetcher.sprite.is_none()
                            {
                                sprites.pop();
                                break 'b true;
                            }

                            if sprite_fifo.len == 8 {
                                sprites.pop();
                                break 'b true;
                            }

                            if px_fetcher.can_interrupt(bg_fifo) {
                                px_fetcher.sprite = Some(sprite);
                                px_fetcher.st = PixelFetcherState::GetTileD1;
                            }

                            break 'b false;
                        }

                        true
                    };

                    px_fetcher.tick(
                        &self.vram,
                        bg_fifo,
                        sprite_fifo,
                        self.scx,
                        self.scy,
                        self.lcdc,
                        self.ly,
                    );

                    if allow_pop && let Some(px) = bg_fifo.pop() {
                        let px = if self.lcdc.contains(Lcdc::BG_WINDOW_ENABLE) {
                            px
                        } else {
                            0b00
                        };

                        let sprite_px = sprite_fifo.pop();

                        let old_x = *screen_x;
                        *screen_x = screen_x.wrapping_add(1);
                        if old_x < 160 {
                            // bg has priority if:
                            // 1. the sprite pixel is transparent or
                            // 2. the bg isn't transparent but the sprite's `low_priority` (bg_over_sprite) bit is set.
                            let px =
                                if sprite_px.color == 0 || (sprite_px.bg_over_sprite && px != 0) {
                                    (self.bg_pallet >> (px * 2)) & 0b11
                                } else {
                                    let palette = if sprite_px.palette {
                                        self.obj_pallet_b
                                    } else {
                                        self.obj_pallet_a
                                    };
                                    (palette >> (sprite_px.color * 2)) & 0b11
                                    // sprite has priority
                                };

                            self.display_memory.set(
                                usize::from(self.ly) * 160 + usize::from(old_x),
                                DisplayPixel::from_bits_truncate(px),
                            );
                        }
                    }

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

        if self.pirq.tick(irq) {
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
    fn default() -> Self {
        Self::new()
    }
}
