use std::{array, cmp};

use crate::FallingEdge;
use crate::util::FloatExt as _;

pub trait ApuSampler {
    fn push_samples(&mut self, samples: [f32; 2]);
    fn push_mute(&mut self, samples: usize) {
        for _ in 0..samples {
            self.push_samples([0.0; 2]);
        }
    }
}

struct Dac {
    capacitance: f32,
}

impl Dac {
    const fn new() -> Self {
        Self { capacitance: 0.0 }
    }

    fn tick(&mut self, enabled: bool, digital: u8) -> f32 {
        self.capacitance = if enabled {
            (15.0 - f32::from(digital)).mul_add_fast(const { 7.5f32.recip() }, -1.0)
        } else {
            // I assume this is how this one works, but I don't actually have numbers.
            self.capacitance * 0.999_958
        };

        self.capacitance
    }
}

struct LengthTimer {
    initial: u8,
    current: u8,
    enable: bool,
}

impl LengthTimer {
    const fn new() -> Self {
        Self {
            initial: 0,
            current: 0,
            enable: false,
        }
    }

    fn trigger<const OFFSET: u8>(&mut self) {
        self.current = OFFSET + self.initial;
    }

    fn div_apu_tick(&mut self) -> bool {
        !(self.enable && self.tick())
    }

    fn tick(&mut self) -> bool {
        self.current = self.current.wrapping_add(1);
        self.current == 0
    }
}

struct Envelope {
    direction: bool,
    volume: u8,
    sweep_pace: u8,
    sweep_step: u8,
    initial_volume: u8,
}

impl Envelope {
    const fn new() -> Self {
        Self {
            volume: 0,
            direction: false,
            sweep_pace: 0,
            sweep_step: 0,
            initial_volume: 0,
        }
    }

    #[must_use]
    fn dac_enabled(&self) -> bool {
        self.initial_volume != 0 || self.direction
    }

    fn trigger(&mut self) {
        self.volume = self.initial_volume;
        self.sweep_step = self.sweep_pace;
    }

    fn tick(&mut self) {
        self.sweep_step = self.sweep_step.wrapping_sub(1);
        if self.sweep_step == 0 {
            self.sweep_step = self.sweep_pace;
            let adjustment = (2 * i8::from(self.direction)) - 1;
            // self.volume = self.volume.wrapping_add_signed(adjustment) % 16;
            self.volume = cmp::min(15, self.volume.saturating_add_signed(adjustment));
        }
    }

    fn read(&self) -> u8 {
        (self.initial_volume << 4) | (u8::from(self.direction) << 3) | self.sweep_pace
    }

    fn write(&mut self, val: u8) {
        self.initial_volume = val >> 4;
        self.direction = (val & 0x08) == 0x08;
        self.sweep_pace = val & 0b111;
    }
}

struct Sweep {
    pace: u8,
    timer: u8,
    step: u8,
    direction: bool,
    enabled: bool,
}

impl Sweep {
    const fn new() -> Self {
        Self {
            pace: 0,
            timer: 0,
            step: 0,
            direction: false,
            enabled: false,
        }
    }

    fn write(&mut self, val: u8) {
        let pace = (val >> 4) & 0x7;

        if self.pace == 0 {
            self.timer = pace;
        }

        self.pace = pace;
        self.direction = val & 0b1000 == 0b1000;
        self.step = val & 0x7;
    }

    fn calc_period(&self, shadow_period: u16) -> Option<u16> {
        let sweep_sign = i16::from(!self.direction) * 2 - 1;

        let shadow = shadow_period
            .wrapping_add_signed((shadow_period >> self.step).cast_signed() * sweep_sign);

        (shadow < 0x800).then_some(shadow)
    }

    fn tick(&mut self, shadow_period: u16) -> (bool, u16) {
        self.timer -= 1;

        if self.timer != 0 {
            return (true, shadow_period);
        }

        self.timer = self.pace;
        let Some(shadow_period) = self.calc_period(shadow_period) else {
            return (false, shadow_period);
        };

        // we calculate again here just to see if we stop, for some reason.
        (self.calc_period(shadow_period).is_some(), shadow_period)
    }
}

// for pwm2 this is overly featured, but honestly, it's too much maintinence.
struct Pwm {
    wave_duty: u8,
    length: LengthTimer,
    envelope: Envelope,
    sweep: Sweep,
    period: u16,
    shadow_period: u16,
    period_div: u16,
    trigger: bool,
    ever_triggered: bool,
    dot: u8,
    sample_idx: u8,
    sample: bool,
    enabled: bool,
}

impl Pwm {
    const fn new() -> Self {
        Self {
            wave_duty: 0,
            length: LengthTimer::new(),
            envelope: Envelope::new(),
            sweep: Sweep::new(),
            period: 0,
            shadow_period: 0,
            period_div: 0,
            trigger: false,
            ever_triggered: false,
            dot: 0,
            sample_idx: 0,
            sample: false,
            enabled: false,
        }
    }

    #[must_use]
    fn dac_enabled(&self) -> bool {
        self.envelope.dac_enabled()
    }

    fn on_div_apu(&mut self, div_apu_mod: u8) {
        if self.envelope.sweep_pace != 0 && div_apu_mod % 8 == 0 {
            self.envelope.tick();
        }

        if self.sweep.enabled && div_apu_mod % 4 == 0 && self.sweep.timer > 0 {
            let (enabled, shadow_period) = self.sweep.tick(self.shadow_period);
            self.enabled &= enabled;

            if !self.enabled {
                self.envelope.volume = 0;
            }

            self.shadow_period = shadow_period;
            self.period = shadow_period;
        }
    }

    fn next_sample(wave_duty: u8, sample_idx: u8) -> bool {
        const DUTY: u32 = u32::from_be_bytes([
            0b1111_1110_u8,
            0b0111_1110_u8,
            0b0111_1000_u8,
            0b1000_0001_u8,
        ])
        .reverse_bits();

        ((DUTY >> (8 * wave_duty + sample_idx)) & 1) != 0
    }

    fn tick(&mut self) -> u8 {
        let dot = self.dot % 4;
        self.dot = (dot + 1) % 4;

        let trigger = self.trigger;

        if dot == 0 && trigger {
            self.trigger = false;
            self.length.trigger::<192>();
            self.shadow_period = self.period;
            self.period_div = self.shadow_period;
            self.envelope.trigger();
            self.sweep.timer = self.sweep.pace;
            self.sweep.enabled = self.sweep.pace != 0 || self.sweep.timer != 0;
            self.enabled = true;

            self.sample = if self.ever_triggered {
                Self::next_sample(self.wave_duty, self.sample_idx)
            } else {
                false
            };

            if self.sweep.timer != 0 {
                self.enabled &= self.sweep.calc_period(self.shadow_period).is_some();
            }

            // fixme: sweep the volume by 1 if it's time.
        }

        if !self.ever_triggered && !self.enabled {
            return 0;
        }

        if dot == 0 {
            self.period_div = (self.period_div + 1) % 2048;
            if self.period_div == 0 {
                self.period_div = self.period;
                self.sample_idx = (self.sample_idx + 1) % 8;
                self.sample = Self::next_sample(self.wave_duty, self.sample_idx);
            }
        }

        if !self.enabled {
            return 0;
        }

        u8::from(self.sample) * self.envelope.volume
    }
}

struct Wave {
    dac_enabled: bool,
    length: LengthTimer,
    // u2
    volume: u8,
    period: u16,
    period_div: u16,
    // u5
    sample_idx: u8,
    // u4
    sample: u8,
    pattern_ram: [u8; 0x10],
    enabled: bool,
    trigger: bool,
    dot: u8,
}

impl Wave {
    const fn new() -> Self {
        Self {
            dac_enabled: false,
            length: LengthTimer::new(),
            volume: 0,
            period: 0,
            period_div: 0,
            sample_idx: 0,
            sample: 0,
            pattern_ram: [0; 0x10],
            enabled: false,
            trigger: false,
            dot: 0,
        }
    }

    #[must_use]
    #[inline(always)]
    fn dac_enabled(&self) -> bool {
        self.dac_enabled
    }

    fn trigger(&mut self) {
        self.trigger = false;
        self.enabled = self.dac_enabled;
        self.period_div = self.period;
        self.length.trigger::<0>();
        self.sample_idx = 0;
        // sample is *not* cleared.
    }

    #[inline(always)]
    fn on_div_apu(&mut self, _div_apu_mod: u8) {}

    fn tick(&mut self) -> u8 {
        let dot = self.dot % 4;
        self.dot = (dot + 1) % 4;

        if dot == 0 && self.trigger {
            self.trigger();
        }

        if !self.enabled {
            return 0;
        }

        if self.dot.is_multiple_of(2) {
            self.period_div = (self.period_div + 1) % 2048;

            if self.period_div == 0 {
                self.period_div = self.period;
                self.sample_idx = (self.sample_idx + 1) % 32;
                self.sample = self.pattern_ram[(self.sample_idx / 2) as usize];
                if self.sample_idx.is_multiple_of(2) {
                    self.sample >>= 4;
                } else {
                    self.sample &= 0xf;
                }
            }
        }

        if !self.enabled || self.volume == 0 {
            return 0;
        }

        self.sample >> (self.volume - 1)
    }
}

struct Lsfr {
    register: u16,
    short: bool,
}

impl Lsfr {
    const fn new() -> Self {
        Self {
            register: 0,
            short: false,
        }
    }
    fn trigger(&mut self) {
        *self = Self {
            register: 0,
            short: self.short,
        };
    }

    fn tick(&mut self) {
        let bit = u16::from((self.register & 1) == ((self.register >> 1) & 1));
        let bit = 0_u16.wrapping_sub(bit);
        let mask = 0x8000 | (u16::from(self.short) << 7);
        self.register = (self.register & !mask) | (bit & mask);
    }

    fn current(&self) -> bool {
        self.register & 1 == 1
    }
}

struct Noise {
    length: LengthTimer,
    envelope: Envelope,
    lsfr: Lsfr,
    dot: u8,
    clock_shift: u8,
    clock_divider: u8,
    enabled: bool,
    trigger: bool,
    clock: u16,
}

impl Noise {
    const fn new() -> Self {
        Self {
            length: LengthTimer::new(),
            envelope: Envelope::new(),
            trigger: false,
            dot: 0,
            enabled: false,
            lsfr: Lsfr::new(),
            clock_shift: 0,
            clock_divider: 0,
            clock: 0,
        }
    }

    #[must_use]
    fn dac_enabled(&self) -> bool {
        self.envelope.dac_enabled()
    }

    fn on_div_apu(&mut self, div_apu_mod: u8) {
        if div_apu_mod.is_multiple_of(8) && self.envelope.sweep_pace != 0 {
            self.envelope.tick();
        }
    }

    fn tick(&mut self) -> u8 {
        let dot = self.dot % 4;
        self.dot = (dot + 1) % 4;

        if dot == 0 && self.trigger {
            self.trigger = false;
            self.length.trigger::<192>();
            self.envelope.trigger();
            self.lsfr.trigger();
            self.enabled = true;
        }

        if !self.enabled {
            return 0;
        }

        // lsfr ticks are a bit weird
        // I assume they can happen on any dot, but nothing actually says anything about that.

        if self.clock > 0 {
            self.clock -= 1;
        }

        if self.clock == 0 && self.clock_shift < 14 {
            self.lsfr.tick();
            let base = if self.clock_divider == 0 {
                8
            } else {
                16 * self.clock_divider
            };
            self.clock = u16::from(base) << self.clock_shift;
        }

        let digital = u8::from(self.lsfr.current());

        digital * self.envelope.volume
    }
}

bitflags::bitflags! {
    struct AudioMasterControl : u8 {
        const AUDIO_ENABLE = 1 << 7;
        const CH4_ENABLE = 1 << 3;
        const CH3_ENABLE = 1 << 2;
        const CH2_ENABLE = 1 << 1;
        const CH1_ENABLE = 1 << 0;
    }

    #[derive(Eq, PartialEq)]
    struct SoundPanning : u8 {
        const CH4_LEFT = 1 << 7;
        const CH3_LEFT = 1 << 6;
        const CH2_LEFT = 1 << 5;
        const CH1_LEFT = 1 << 4;
        const CH4_RIGHT = 1 << 3;
        const CH3_RIGHT = 1 << 2;
        const CH2_RIGHT = 1 << 1;
        const CH1_RIGHT = 1 << 0;
    }

    struct VinPanning : u8 {
        const LEFT = 1 << 7;
        const RIGHT = 1 << 3;
    }
}

struct Capacitor(f32);

impl Capacitor {
    fn sample(&mut self, sample: f32) -> f32 {
        let out = sample - self.0;
        // the simple version of this is self.0 = sample - (out * 0.999_958)
        self.0 = out.mul_add_fast(-0.999_958, sample);
        out
    }
}

pub(crate) struct Lazy<S> {
    banked_cycles: u32,
    div_apu: FallingEdge,
    apu: Apu<S>,
}

impl<S: ApuSampler> Lazy<S> {
    pub const fn new(sampler: S) -> Self {
        Self {
            banked_cycles: 0,
            div_apu: FallingEdge::new(false),
            apu: Apu::new(sampler),
        }
    }

    fn force<const DIV_APU: bool>(&mut self) {
        let banked_cycles = std::mem::take(&mut self.banked_cycles);

        if !self.apu.enabled {
            return self.apu.tick_disabled(banked_cycles, DIV_APU);
        }

        if !DIV_APU {
            return self.apu.tick_many(banked_cycles);
        }

        if banked_cycles > 1 {
            self.apu.tick_many(banked_cycles - 1);
        }

        self.apu.tick_div_apu();
    }

    #[cold]
    #[inline(never)]
    fn tick_force<const DIV_APU: bool>(&mut self) {
        self.force::<DIV_APU>();
    }

    pub fn tick(&mut self, div: u8) {
        // this is practically unreachable except in the very specific situation where:
        // - the APU isn't being actively used
        // - the audio sink is `mute`
        // - div keeps getting reset
        // but the APU isn't free to emulate so let's cap out the ticks at some point.
        const MAX_TICKS: u32 = 1 << (22 - 4);

        // fixme: how to lazy div_apu?

        self.banked_cycles += 1;

        let div_apu = self.div_apu.tick(div & 0b0001_0000 > 0);

        if div_apu {
            return self.tick_force::<true>();
        }

        if self.banked_cycles >= MAX_TICKS {
            return self.tick_force::<false>();
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        const AMC_ADDR: u8 = 0x26;

        // fixme: this is way too conservative (for enabled APU).
        // writes to AMC are the only times we ever might need to do a `force` while disabled, nothing visible is ticking otherwise.
        if self.apu.enabled || addr == AMC_ADDR {
            self.force::<false>();
        }

        if !self.apu.enabled {
            self.apu.write_disabled(addr, val);
            return;
        }

        self.apu.write_reg(addr, val);
    }

    #[must_use]
    pub fn read_reg(&mut self, addr: u8) -> u8 {
        // fixme: this is way too conservative.
        self.force::<false>();
        self.apu.read_reg(addr)
    }

    #[must_use]
    pub(crate) fn sampler_mut(&mut self) -> &mut S {
        self.force::<false>();
        &mut self.apu.sampler
    }
}

pub struct Apu<S> {
    sampler: S,
    panning: SoundPanning,
    vin_panning: VinPanning,
    left_volume: u8,
    right_volume: u8,
    pwm1: Pwm,
    pwm2: Pwm,
    wave: Wave,
    noise: Noise,
    div_apu_mod: u8,
    hpf: [Capacitor; 2],
    dacs: [Dac; 4],
    enabled: bool,
    panning_cvt: [f32; 8],
    dac_enabled: u8,
}

impl<S: ApuSampler> Apu<S> {
    pub const fn new(sampler: S) -> Self {
        Self {
            sampler,
            enabled: false,
            panning: SoundPanning::empty(),
            vin_panning: VinPanning::empty(),
            left_volume: 0,
            right_volume: 0,
            pwm1: Pwm::new(),
            pwm2: Pwm::new(),
            wave: Wave::new(),
            noise: Noise::new(),
            div_apu_mod: 0,
            hpf: [const { Capacitor(0.0) }; 2],
            dacs: [const { Dac::new() }; 4],
            panning_cvt: [0.0; 8],
            dac_enabled: 0,
        }
    }

    #[inline(never)]
    #[cold]
    fn clear(&mut self) {
        // innaccuracies around initial length timers around here.
        self.panning = SoundPanning::empty();
        self.vin_panning = VinPanning::empty();
        self.left_volume = 0;
        self.right_volume = 0;
        self.pwm1 = Pwm::new();
        self.pwm2 = Pwm::new();
        self.wave = Wave {
            pattern_ram: self.wave.pattern_ram,
            ..Wave::new()
        };
        self.noise = Noise::new();

        // just so that we know that the APU mod isn't reset, for whatever reason
        #[allow(clippy::unnecessary_operation, unused)]
        {
            self.div_apu_mod = self.div_apu_mod;
        }

        self.hpf = [const { Capacitor(0.0) }; 2];
        self.dacs = [const { Dac::new() }; 4];
        self.panning_cvt = [0.0; 8];
        self.dac_enabled = 0;
    }

    #[cold]
    #[inline(never)]
    fn write_disabled(&mut self, addr: u8, val: u8) {
        match addr {
            0x11 => {
                self.pwm1.wave_duty = val >> 6;
                self.pwm1.length.initial = val & 0x3f;
            }

            0x16 => {
                self.pwm2.wave_duty = val >> 6;
                self.pwm2.length.initial = val & 0x3f;
            }

            0x1b => {
                self.wave.length.initial = val;
            }

            0x26 => {
                self.enabled = val & 0x80 == 0x80;
            }

            0x30..0x40 => {
                // fixme: more precise timings.
                // self.wave can never be enabeld because the entire APU is disabled.
                self.wave.pattern_ram[(addr - 0x30) as usize] = val;
            }

            ..0x10 | 0x40.. => {
                log::error!("BUG: invalid APU write (0xff{addr:02x} -> {val:#02x})");
            }

            // all other writes are disabled
            0x10 | _ => {}
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x10 => {
                self.pwm1.sweep.write(val);
            }
            0x11 => {
                self.pwm1.wave_duty = val >> 6;
                self.pwm1.length.initial = val & 0x3f;
            }
            0x12 => {
                self.pwm1.envelope.write(val);
                self.dac_enabled = self.calc_dac_enabled();
            }
            0x13 => {
                self.pwm1.period = (self.pwm1.period & 0x300) | u16::from(val);
            }
            0x14 => {
                self.pwm1.trigger = val & 0x80 == 0x80 && self.pwm1.dac_enabled();
                self.pwm1.length.enable = val & 0x40 == 0x40;
                self.pwm1.period = (self.pwm1.period & 0x0ff) | (u16::from(val & 0x7) << 8);
            }

            0x16 => {
                self.pwm2.wave_duty = val >> 6;
                self.pwm2.length.initial = val & 0x3f;
            }
            0x17 => {
                self.pwm2.envelope.write(val);
                self.dac_enabled = self.calc_dac_enabled();
            }
            0x18 => {
                self.pwm2.period = (self.pwm2.period & 0x300) | u16::from(val);
            }
            0x19 => {
                self.pwm2.trigger = val & 0x80 == 0x80 && self.pwm2.dac_enabled();
                self.pwm2.length.enable = val & 0x40 == 0x40;
                self.pwm2.period = (self.pwm2.period & 0x0ff) | (u16::from(val & 0x7) << 8);
            }

            0x1a => {
                self.wave.dac_enabled = val & 0x80 == 0x80;
                self.wave.enabled &= self.wave.dac_enabled;
                self.dac_enabled = self.calc_dac_enabled();
            }

            0x1b => {
                self.wave.length.initial = val;
            }

            0x1c => {
                self.wave.volume = (val >> 5) & 0b11;
            }

            0x1d => {
                self.wave.period = (self.wave.period & 0x300) | u16::from(val);
            }

            0x1e => {
                self.wave.trigger = val & 0x80 == 0x80 && self.wave.dac_enabled();
                self.wave.length.enable = val & 0x40 == 0x40;
                self.wave.period = (self.wave.period & 0x0ff) | (u16::from(val & 0x7) << 8);
            }

            0x20 => {
                self.noise.length.initial = val & 0x3f;
            }

            0x21 => {
                self.noise.envelope.write(val);
                self.dac_enabled = self.calc_dac_enabled();
            }

            0x22 => {
                self.noise.clock_divider = val & 0x07;
                self.noise.lsfr.short = val & 0x08 != 0x08;
                self.noise.clock_shift = val >> 4;
            }

            0x23 => {
                self.noise.trigger = val & 0x80 == 0x80 && self.noise.dac_enabled();
                self.noise.length.enable = val & 0x40 == 0x40;
            }

            0x24 => {
                self.vin_panning = VinPanning::from_bits_truncate(val);
                self.left_volume = (val >> 4) & 0x7;
                self.right_volume = val & 0x7;
            }
            0x25 => {
                let new = SoundPanning::from_bits_retain(val);

                // in most games this address is rarely even accessed, (less than 1 time per second), tetris on the other hand constantly changes the panning to the same thing, so.
                if self.panning != new {
                    self.panning = SoundPanning::from_bits_retain(val);
                    self.panning_cvt = [
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH1_LEFT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH1_RIGHT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH2_LEFT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH2_RIGHT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH3_LEFT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH3_RIGHT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH4_LEFT))),
                        f32::from(u8::from(self.panning.contains(SoundPanning::CH4_RIGHT))),
                    ];
                }
            }
            0x26 => {
                self.enabled = val & 0x80 == 0x80;

                if !self.enabled {
                    self.clear();
                }
            }

            // nothing here (but it's a valid range, not a bug)
            0x15 | 0x1f | 0x27..0x30 => {}

            0x30..0x40 => {
                // fixme: more precise timings.
                if !self.wave.enabled {
                    self.wave.pattern_ram[(addr - 0x30) as usize] = val;
                }
            }

            ..0x10 | 0x40.. => log::error!("BUG: invalid APU write (0xff{addr:02x} -> {val:#02x})"),
        }
    }

    fn read_volume_vin_pan(&self) -> u8 {
        self.vin_panning.bits() | (self.left_volume << 4) | self.right_volume
    }

    #[must_use]
    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x12 => self.pwm1.envelope.read(),
            0x17 => self.pwm2.envelope.read(),
            0x21 => self.noise.envelope.read(),
            0x24 => self.read_volume_vin_pan(),
            0x25 => self.panning.bits(),
            0x26 => {
                let mut amc = AudioMasterControl::empty();
                amc.set(AudioMasterControl::AUDIO_ENABLE, self.enabled);
                amc.set(AudioMasterControl::CH1_ENABLE, self.pwm1.enabled);
                amc.set(AudioMasterControl::CH2_ENABLE, self.pwm2.enabled);
                amc.set(AudioMasterControl::CH3_ENABLE, self.wave.enabled);
                amc.set(AudioMasterControl::CH4_ENABLE, self.noise.enabled);
                amc.bits() | 0b0111_0000
            }

            // nothing here (but it's a valid range, not a bug)
            0x15 | 0x1f | 0x27..0x30 => 0xff,

            0x10..0x40 => {
                log::error!("BUG: unimplemented APU read (0xff{addr:02x} -> 0xff)");
                0xff
            }
            _ => {
                log::error!("BUG: invalid APU read (0xff{addr:02x} -> 0xff)");
                0xff
            }
        }
    }

    fn calc_dac_enabled(&self) -> u8 {
        u8::from(self.pwm1.dac_enabled())
            | (u8::from(self.pwm2.dac_enabled()) << 1)
            | (u8::from(self.wave.dac_enabled()) << 2)
            | (u8::from(self.noise.dac_enabled()) << 3)
    }

    fn calc_volume(&self) -> [f32; 2] {
        let clamp = |vol| match vol {
            0 => 1,
            7.. => 8,
            x => x,
        };

        // adjust sample volume by like, -20dB pls and ty (this isn't part of the emulation, it's just to prevent everything from blowing my ears out).
        let volume = [clamp(self.left_volume), clamp(self.right_volume)];
        volume.map(|it| f32::from(it) * const { 1.0 / 8.0 * 0.1 })
    }

    fn calc_dac(sample: [u8; 4], dacs: &mut [Dac; 4], dac_enabled: u8) -> [f32; 4] {
        array::from_fn(|idx| dacs[idx].tick(dac_enabled & (1 << idx) != 0, sample[idx]))
    }

    fn pan(sample: &[f32; 4], panning_cvt: &[f32; 8]) -> [f32; 2] {
        let pan = panning_cvt;
        array::from_fn(|i| {
            let a = pan[i].mul_add_fast(sample[0], pan[2 + i] * sample[1]);
            let b = pan[4 + i].mul_add_fast(sample[2], pan[6 + i] * sample[3]);

            a + b
        })
    }

    #[cold]
    fn tick_disabled(&mut self, ticks: u32, div_apu: bool) {
        if div_apu {
            let _ = self.apu_mod_tick();
        }

        self.sampler.push_mute(ticks as usize);
    }

    pub fn tick_many(&mut self, ticks: u32) {
        // never DIV APU here.

        // fixme: proper implementation (we have some more things to change about the APU first)
        if self.dac_enabled == 0 {
            for _ in 0..ticks {
                let _ = self.pwm1.tick();
                let _ = self.pwm2.tick();
                let _ = self.wave.tick();
                let _ = self.noise.tick();
            }

            self.sampler.push_mute(ticks as usize);

            return;
        }

        let volume = self.calc_volume();

        for _ in 0..ticks {
            let sample = [
                self.pwm1.tick(),
                self.pwm2.tick(),
                self.wave.tick(),
                self.noise.tick(),
            ];

            let sample = Self::calc_dac(sample, &mut self.dacs, self.dac_enabled);
            let sample = Self::pan(&sample, &self.panning_cvt);
            let sample: [_; 2] = array::from_fn(|idx| volume[idx] * sample[idx]);
            let sample = array::from_fn(|idx| self.hpf[idx].sample(sample[idx]));

            self.sampler.push_samples(sample);
        }
    }

    fn apu_mod_tick(&mut self) -> u8 {
        // 512 hz timer.
        self.div_apu_mod = (self.div_apu_mod + 1) % 8;
        self.div_apu_mod
    }

    fn on_div_apu(&mut self) {
        let div_apu_mod = self.apu_mod_tick();
        if !div_apu_mod.is_multiple_of(2) {
            return;
        }

        self.pwm1.enabled &= self.pwm1.length.div_apu_tick();
        self.pwm2.enabled &= self.pwm2.length.div_apu_tick();
        self.wave.enabled &= self.wave.length.div_apu_tick();
        self.noise.enabled &= self.noise.length.div_apu_tick();

        self.pwm1.on_div_apu(div_apu_mod);
        self.pwm2.on_div_apu(div_apu_mod);
        self.wave.on_div_apu(div_apu_mod);
        self.noise.on_div_apu(div_apu_mod);
    }

    pub fn tick_div_apu(&mut self) {
        // 512 hz timer.
        self.on_div_apu();

        let sample = [
            self.pwm1.tick(),
            self.pwm2.tick(),
            self.wave.tick(),
            self.noise.tick(),
        ];

        if self.dac_enabled == 0 {
            return self.sampler.push_samples([0.0; 2]);
        }

        let volume = self.calc_volume();

        let sample = Self::calc_dac(sample, &mut self.dacs, self.dac_enabled);
        let sample = Self::pan(&sample, &self.panning_cvt);
        let sample: [_; 2] = array::from_fn(|idx| volume[idx] * sample[idx]);
        let sample = array::from_fn(|idx| self.hpf[idx].sample(sample[idx]));

        self.sampler.push_samples(sample);
    }
}
