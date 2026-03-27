use std::ops::ControlFlow;
use std::{array, cmp};

use crate::FallingEdge;

pub trait ApuSampler {
    fn push_samples(&mut self, samples: [f32; 2]);
}

struct Dac {
    capacitance: f32,
}

impl Dac {
    fn tick(&mut self, enabled: bool, digital: u8) -> f32 {
        if enabled {
            return ((15.0 - f32::from(digital)) / 7.5) - 1.0;
        }

        // fixme: there's capacitance, samples don't magically teleport.
        0.0

        // self.capacitor *=
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

    fn tick(&mut self) {
        self.sweep_step = self.sweep_step.wrapping_sub(1);
        if self.sweep_step == 0 {
            self.sweep_step = self.sweep_pace;
            let adjustment = (2 * i8::from(self.direction)) - 1;
            // self.volume = self.volume.wrapping_add_signed(adjustment) % 16;
            self.volume = cmp::min(15, self.volume.saturating_add_signed(adjustment));
        }
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

    fn calc_period(&self, shadow_period: u16) -> Option<u16> {
        let sweep_sign = i16::from(!self.direction) * 2 - 1;

        let shadow = shadow_period
            .wrapping_add_signed((shadow_period >> self.step).cast_signed() * sweep_sign);

        (shadow < 0x800).then_some(shadow)
    }

    fn tick(&mut self, shadow_period: u16) -> (bool, u16) {
        eprintln!("period: 0x{shadow_period:03x}");
        self.timer = self.timer - 1;

        if self.timer != 0 {
            return (true, shadow_period);
        }

        self.timer = self.pace;
        let shadow_period = match self.calc_period(shadow_period) {
            Some(it) => it,
            None => return (false, shadow_period),
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
    dac: Dac,
    dot: u8,
    sample: u8,
    enabled: bool,
}

impl Pwm {
    const fn new() -> Self {
        Self {
            wave_duty: 0,
            dac: Dac { capacitance: 0.0 },
            length: LengthTimer::new(),
            envelope: Envelope::new(),
            sweep: Sweep::new(),
            period: 0,
            shadow_period: 0,
            period_div: 0,
            trigger: false,
            dot: 0,
            sample: 0,
            enabled: false,
        }
    }

    #[must_use]
    fn dac_enabled(&self) -> bool {
        self.envelope.initial_volume != 0 || self.envelope.direction
    }

    fn tick(&mut self, div_apu_mod: Option<u8>) -> f32 {
        const DUTY: u32 = u32::from_be_bytes([
            0b1111_1110_u8,
            0b0111_1110_u8,
            0b0111_1000_u8,
            0b1000_0001_u8,
        ])
        .reverse_bits();

        let dot = self.dot % 4;
        self.dot = (dot + 1) % 4;

        if dot == 0 && self.trigger {
            self.trigger = false;
            self.length.current = 192 + self.length.initial;
            self.shadow_period = self.period;
            self.period_div = self.shadow_period;
            self.envelope.volume = self.envelope.initial_volume;
            self.envelope.sweep_step = self.envelope.sweep_pace;
            self.sweep.timer = self.sweep.pace;
            self.sweep.enabled = self.sweep.pace != 0 || self.sweep.timer != 0;
            self.enabled = true;

            if self.sweep.timer != 0 {
                self.enabled &= self.sweep.calc_period(self.shadow_period).is_some();
            }

            // fixme: sweep the volume by 1 if it's time.

            return self.dac.tick(self.dac_enabled(), 0);
        }

        if dot == 0 {
            self.period_div = (self.period_div + 1) % 2048;
        }

        if self.period_div == 0 {
            self.period_div = self.period;
            self.sample = (self.sample + 1) % 8;
        }

        // only tick envelope and length if the channel is running.
        if let Some(div_apu_mod) = div_apu_mod
            && self.enabled
        {
            if self.length.enable && div_apu_mod % 2 == 0 && self.length.tick() {
                self.envelope.volume = 0;
                self.enabled = false;
            }

            if self.envelope.sweep_pace != 0 && div_apu_mod % 8 == 0 {
                self.envelope.tick();
                // if self.envelope.volume == 0 {
                //     self.enabled = false;
                // }
            }

            if self.sweep.enabled && div_apu_mod % 4 == 0 && self.sweep.timer > 0 {
                dbg!("hi");
                let (enabled, shadow_period) = self.sweep.tick(self.shadow_period);
                self.enabled &= enabled;

                if !self.enabled {
                    self.envelope.volume = 0;
                }

                self.shadow_period = shadow_period;
                self.period = shadow_period;
            }
        }

        let digital = ((DUTY >> (8 * self.wave_duty + self.sample)) & 1) as u8;

        self.dac
            .tick(self.dac_enabled(), digital * self.envelope.volume)
    }
}

struct Wave {}

struct Noise {}

bitflags::bitflags! {
    struct AudioMasterControl : u8 {
        const AUDIO_ENABLE = 1 << 7;
        const CH4_ENABLE = 1 << 3;
        const CH3_ENABLE = 1 << 2;
        const CH2_ENABLE = 1 << 1;
        const CH1_ENABLE = 1 << 0;
    }

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
        self.0 = sample - out * 0.999958;
        out
    }
}

pub struct Apu<S> {
    sampler: S,
    div_apu: FallingEdge,
    amc: AudioMasterControl,
    panning: SoundPanning,
    vin_panning: VinPanning,
    left_volume: u8,
    right_volume: u8,
    pwm1: Pwm,
    pwm2: Pwm,
    div_apu_mod: u8,
    capacitor: Capacitor,
}

impl<S: ApuSampler> Apu<S> {
    pub const fn new(sampler: S) -> Self {
        Self {
            sampler,
            div_apu: FallingEdge::new(false),
            amc: AudioMasterControl::empty(),
            panning: SoundPanning::empty(),
            vin_panning: VinPanning::empty(),
            left_volume: 0,
            right_volume: 0,
            pwm1: Pwm::new(),
            pwm2: Pwm::new(),
            div_apu_mod: 0,
            capacitor: Capacitor(0.0),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        // eprintln!("APU 0xff{addr:02x} = 0x{val:02x}");
        match addr {
            0x10 => {
                let pace = (val >> 4) & 0x7;

                if self.pwm1.sweep.pace == 0 {
                    self.pwm1.sweep.timer = pace;
                }

                self.pwm1.sweep.pace = pace;
                self.pwm1.sweep.direction = val & 0b1000 == 0b1000;
                self.pwm1.sweep.step = val & 0x7;
            }
            0x11 => {
                self.pwm1.wave_duty = val >> 6;
                self.pwm1.length.initial = val & 0x3f;
            }
            0x12 => {
                self.pwm1.envelope.initial_volume = val >> 4;
                self.pwm1.envelope.direction = val & 0x08 == 0x08;
                self.pwm1.envelope.sweep_pace = val & 0b111;
            }
            0x13 => {
                self.pwm1.period = (self.pwm1.period & 0x300) | u16::from(val);
            }
            0x14 => {
                self.pwm1.trigger = val & 0x80 == 0x80;
                self.pwm1.length.enable = val & 0x40 == 0x40;
                self.pwm1.period = (self.pwm1.period & 0x0ff) | (u16::from(val & 0x7) << 8);
            }

            0x16 => {
                self.pwm2.wave_duty = val >> 6;
                self.pwm2.length.initial = val & 0x3f;
            }
            0x17 => {
                self.pwm2.envelope.initial_volume = val >> 4;
                self.pwm2.envelope.direction = (val & 0xf) >> 3 > 0;
                self.pwm2.envelope.sweep_pace = val & 0b111;
            }
            0x18 => {
                self.pwm2.period = (self.pwm2.period & 0x300) | u16::from(val);
            }
            0x19 => {
                self.pwm2.trigger = val & 0x80 == 0x80;
                self.pwm2.length.enable = val & 0x40 == 0x40;
                self.pwm2.period = (self.pwm2.period & 0x0ff) | (u16::from(val & 0x7) << 8);
            }
            0x24 => {
                self.vin_panning = VinPanning::from_bits_truncate(val);
                self.left_volume = (val >> 4) & 0x7;
                self.right_volume = val & 0x7;
            }
            0x25 => self.panning = SoundPanning::from_bits_retain(val),
            0x10..0x40 => {
                log::error!("BUG: unimplemented APU write (0xff{addr:02x} -> {val:#02x})")
            }
            _ => log::error!("BUG: invalid APU write (0xff{addr:02x} -> {val:#02x})"),
        }
    }

    #[must_use]
    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x25 => self.panning.bits(),

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

    fn any_dac_enabled(&self) -> bool {
        self.pwm1.dac_enabled() || self.pwm2.dac_enabled()
    }

    pub fn tick(&mut self, div: u8) {
        // 512 hz timer.w
        let div_apu = self.div_apu.tick(div & 0b0001_0000 > 0);
        if div_apu {
            self.div_apu_mod = (self.div_apu_mod + 1) % 8;
        }

        let sample1 = {
            let sample = self.pwm1.tick(div_apu.then_some(self.div_apu_mod));

            let pan = [
                self.panning.contains(SoundPanning::CH1_LEFT) as u8 as f32,
                self.panning.contains(SoundPanning::CH1_RIGHT) as u8 as f32,
            ];

            let [left, right] = pan;
            [left * sample, right * sample]
        };

        let sample2 = {
            let sample = self.pwm2.tick(div_apu.then_some(self.div_apu_mod));

            let pan = [
                self.panning.contains(SoundPanning::CH1_LEFT) as u8 as f32,
                self.panning.contains(SoundPanning::CH1_RIGHT) as u8 as f32,
            ];

            let [left, right] = pan;
            [left * sample, right * sample]
        };

        let sample = if self.any_dac_enabled() {
            // adjust sample volume by like, -20dB pls and ty.
            array::from_fn(|idx| self.capacitor.sample(sample1[idx] + sample2[idx]) * 0.1)
        } else {
            [0.0; 2]
        };

        let sample = {
            let clamp = |vol| match vol {
                0 => 1,
                7.. => 8,
                x => x,
            };

            let [vl, vr] = [clamp(self.left_volume), clamp(self.right_volume)];
            let [sl, sr] = sample;
            [((vl as f32) / 8.0) * sl, (vr as f32) / 8.0 * sr]
        };

        self.sampler.push_samples(sample);
    }

    #[must_use]
    pub(crate) fn sampler(&self) -> &S {
        &self.sampler
    }

    #[must_use]
    pub(crate) fn sampler_mut(&mut self) -> &mut S {
        &mut self.sampler
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::emu::apu::Pwm2;

    #[test]
    fn test() {
        let mut pwm2_a = Pwm2::new();
        pwm2_a.initial_volume = 5;
        pwm2_a.period = 1750;
        pwm2_a.trigger = true;
        pwm2_a.wave_duty = 2;

        let mut pwm2_b = Pwm2::new();
        pwm2_b.initial_volume = 5;
        pwm2_b.period = 1812;
        pwm2_b.trigger = true;
        pwm2_b.wave_duty = 2;

        let mut pwm2_c = Pwm2::new();
        pwm2_c.initial_volume = 5;
        pwm2_c.period = 1849;
        pwm2_c.trigger = true;
        pwm2_c.wave_duty = 2;

        let mut in_samples = Vec::new();
        let mut out_samples = Vec::new();
        let mut capacitor = 0.0;
        for _ in 0..(1 << 22) {
            let sample_a = pwm2_a.tick();
            let sample_b = pwm2_b.tick();
            let sample_c = pwm2_c.tick();

            let sample = (sample_a + sample_b + sample_c);
            let mut out = 0.0;
            if pwm2_a.dac_enabled() || pwm2_b.dac_enabled() || pwm2_c.dac_enabled() {
                out = sample - capacitor;
                capacitor = sample - out * 0.999958;
            }

            in_samples.push(out);

            if in_samples.len() >= 1 {
                out_samples.push(in_samples.drain(..1).sum::<f32>() / 1.0);
            }
        }

        let spec = hound::WavSpec {
            channels: 1,
            sample_rate: 1 << 22,
            bits_per_sample: 32,
            sample_format: hound::SampleFormat::Float,
        };

        let mut writer = hound::WavWriter::create("tmp.wav", spec).unwrap();
        for &sample in &out_samples {
            writer.write_sample(sample).unwrap();
        }

        writer.finalize().unwrap();

        eprintln!("{:?}", &out_samples[12500..13500]);
        panic!("at the disco");
    }
}
