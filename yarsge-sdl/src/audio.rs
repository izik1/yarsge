use std::iter;
use std::num::NonZero;

use sdl3::AudioSubsystem;
use sdl3::audio::{AudioDevice, AudioSpec, AudioStream, AudioStreamOwner};
use yarsge_core::emu::apu::ApuSampler;
use yarsge_dsp::Fir;
use yarsge_math::FloatExt as _;

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
pub enum AudioSystem {
    Mute,
    NearestNeighbor,
    Mean,
    Fir,
}

#[inline(never)]
#[must_use]
pub fn audio_init(
    audio_subsystem: &AudioSubsystem,
    time_control_enabled: bool,
    audio_system: AudioSystem,
) -> Option<(AudioStreamOwner, FilteredSampler)> {
    if !time_control_enabled {
        return None;
    }

    // simplify 48000Hz / 4MiHz, we get 375/2^15.
    // The but the APU actually only outputs samples at 2MiHz, so, just divide that all by 2. Luckily we have a lot of factors of 2, so it doesn't really affect anything.
    let (expand, decimate) = (375, 32768 / 2);
    let filter = match audio_system {
        AudioSystem::Mute => return None,
        AudioSystem::NearestNeighbor => {
            AudioFilter::NearestNeighbor(NearestNeighborFilter::new(expand, decimate))
        }
        AudioSystem::Mean => AudioFilter::Mean(MeanFilter::new(expand, decimate)),
        AudioSystem::Fir => AudioFilter::Fir(FirFilter::new(expand, decimate)),
    };

    let sampler = FilteredSampler::new(filter);

    let device = AudioDevice::open_playback(
        audio_subsystem,
        None,
        &AudioSpec::new(
            Some(48000),
            Some(2),
            Some(sdl3::audio::AudioFormat::f32_sys()),
        ),
    )
    .unwrap();
    let stream = device.open_device_stream(None).unwrap();

    // arbitrarily put 0.03 seconds of audio to try to prevent pops?
    stream.put_data_f32(&[0.0; 480 * 2 * 3]).unwrap();

    Some((stream, sampler))
}

pub struct FilteredSampler {
    samples: Vec<[f32; 2]>,
    filter: AudioFilter,
}

impl FilteredSampler {
    #[must_use]
    pub const fn mute() -> Self {
        Self {
            samples: Vec::new(),
            filter: AudioFilter::Mute,
        }
    }

    #[must_use]
    pub fn new(filter: AudioFilter) -> Self {
        Self {
            // arbitrarily assume that we probably won't use more than 128 samples (~3ms)
            samples: Vec::with_capacity(128),
            filter,
        }
    }

    pub fn push_to_stream(&mut self, stream: &AudioStream) {
        if let AudioFilter::Fir(fir) = &mut self.filter {
            fir.filter_into(&mut self.samples);
        }

        let samples = self.samples.drain(..);
        let samples = samples.as_slice().as_flattened();
        stream.put_data_f32(samples).unwrap();
    }
}

impl ApuSampler for FilteredSampler {
    fn push_samples(&mut self, samples: [f32; 2]) {
        if let Some(sample) = self.filter.filter(samples) {
            self.samples.push(sample);
        }
    }

    fn push_mute(&mut self, samples: usize) {
        match &mut self.filter {
            AudioFilter::Mute => {}
            AudioFilter::NearestNeighbor(nn) => {
                self.samples.extend(
                    iter::repeat_n([0.0; 2], samples).filter_map(|sample| nn.filter(sample)),
                );
            }
            AudioFilter::Mean(mean) => {
                self.samples.extend(
                    iter::repeat_n([0.0; 2], samples).filter_map(|sample| mean.filter(sample)),
                );
            }
            AudioFilter::Fir(fir) => {
                fir.buf.extend(iter::repeat_n([0.0; 2], samples));
                fir.filter_into(&mut self.samples);
            }
        }
    }
}

// Technically this should have a more precise name, but w/e.
pub enum AudioFilter {
    // Deny all samples (different from no filter, which would just do nothing).
    Mute,
    NearestNeighbor(NearestNeighborFilter),
    Mean(MeanFilter),
    Fir(FirFilter),
}

impl AudioFilter {
    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        match self {
            Self::Mute => None,
            Self::NearestNeighbor(it) => it.filter(sample),
            Self::Mean(it) => it.filter(sample),
            Self::Fir(it) => it.filter(sample),
        }
    }
}

// this sampler isn't the best but having any is good.
pub struct NearestNeighborFilter {
    phase: u32,
    expansion_factor: u32,
    decimation_factor: u32,
}

impl NearestNeighborFilter {
    fn new(expansion_factor: u32, decimation_factor: u32) -> Self {
        // sure, we could support net expansion, but, meh.
        assert!(expansion_factor <= decimation_factor);
        // set the phase such that the first sample will be taken.
        let initial_phase = decimation_factor.checked_sub(expansion_factor).expect("`NearestNeighborFilter` only supports decimation, not expansion (expansion <= decimation)");

        Self {
            phase: initial_phase,
            expansion_factor,
            decimation_factor,
        }
    }

    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        self.phase += self.expansion_factor;
        self.phase = self.phase.checked_sub(self.decimation_factor)?;

        Some(sample)
    }
}

// An improvement of `NearestNeighbor` on account of not discading data.
pub struct MeanFilter {
    sample: [f32; 2],
    phase: u32,
    expansion_factor: u32,
    decimation_factor: u32,
    mean_recip_divisor: f32,
}

impl MeanFilter {
    const fn next_samples(expansion_factor: u32, decimation_factor: u32, phase: u32) -> u8 {
        (decimation_factor - phase).div_ceil(expansion_factor) as u8
    }

    fn new(expansion_factor: u32, decimation_factor: u32) -> Self {
        // sure, we could support net expansion, but, meh.
        assert!(expansion_factor <= decimation_factor);
        // set the phase such that the first sample will be taken.
        let _ = decimation_factor.checked_sub(expansion_factor).expect(
            "`MeanFilter` only supports decimation, not expansion (expansion <= decimation)",
        );

        Self {
            sample: [0.0; 2],
            phase: 0,
            expansion_factor,
            decimation_factor,
            mean_recip_divisor: f32::from(Self::next_samples(
                expansion_factor,
                decimation_factor,
                0,
            ))
            .recip(),
        }
    }

    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        self.sample = {
            let [bl, br] = self.sample;
            let [sl, sr] = sample;
            [
                sl.mul_add_fast(self.mean_recip_divisor, bl),
                sr.mul_add_fast(self.mean_recip_divisor, br),
            ]
        };

        self.phase += self.expansion_factor;
        self.phase = self.phase.checked_sub(self.decimation_factor)?;

        self.mean_recip_divisor = f32::from(Self::next_samples(
            self.expansion_factor,
            self.decimation_factor,
            self.phase,
        ))
        .recip();

        Some(std::mem::take(&mut self.sample))
    }
}

pub struct FirFilter {
    fir: Fir,
    buf: Vec<[f32; 2]>,
}

impl FirFilter {
    fn new(expansion_factor: u32, decimation_factor: u32) -> Self {
        let expansion_factor = NonZero::new(expansion_factor).unwrap().try_into().unwrap();
        let decimation_factor: NonZero<usize> =
            NonZero::new(decimation_factor).unwrap().try_into().unwrap();

        Self {
            fir: Fir::new(expansion_factor, decimation_factor),
            buf: Vec::with_capacity(expansion_factor.get()),
        }
    }

    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        self.buf.push(sample);

        None
    }

    fn filter_into(&mut self, out: &mut Vec<[f32; 2]>) {
        self.fir.filter(self.buf.as_slice(), out);
        self.buf.clear();
    }
}
