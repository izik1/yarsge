use std::mem::MaybeUninit;
use std::num::NonZero;
use std::{cmp, iter};

use sdl3::AudioSubsystem;
use sdl3::audio::{AudioDevice, AudioSpec, AudioStream, AudioStreamOwner};
use yarsge_core::emu::apu::ApuSampler;
use yarsge_core::util::FloatExt as _;

use crate::complex::Complex;

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
pub enum AudioSystem {
    Mute,
    NearestNeighbor,
    Mean,
    Fir,
}

#[inline(never)]
pub fn audio_init(
    audio_subsystem: &AudioSubsystem,
    time_control_enabled: bool,
    audio_system: AudioSystem,
) -> Option<(AudioStreamOwner, FilteredSampler)> {
    if !time_control_enabled {
        return None;
    }

    // simplify 48000Hz / 4MiHz, we get 375/2^15, but we technically use a slightly slower CPU frequency (fixme: do actual math to compute this).
    let (expand, decimate) = (375, 32768);
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
    pub const fn mute() -> Self {
        Self {
            samples: Vec::new(),
            filter: AudioFilter::Mute,
        }
    }

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
            AudioFilter::Mute => None,
            AudioFilter::NearestNeighbor(it) => it.filter(sample),
            AudioFilter::Mean(it) => it.filter(sample),
            AudioFilter::Fir(it) => it.filter(sample),
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
    fn next_samples(expansion_factor: u32, decimation_factor: u32, phase: u32) -> u8 {
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
        self.fir.filter(&self.buf[..], out);
        self.buf.clear();
    }
}

#[derive(Clone)]
struct Fir {
    expansion_factor: NonZero<usize>,
    decimation_factor: NonZero<usize>,
    taps: Vec<f64>,
    tap_idx: usize,
    delay: Vec<[f64; 2]>,
    delay_idx: usize,
    leftover: usize,
}

const GAINS: usize = 1 << 18;

impl Fir {
    fn new(expansion_factor: NonZero<usize>, decimation_factor: NonZero<usize>) -> Self {
        // sure, we could support net expansion, but, meh.
        assert!(expansion_factor <= decimation_factor);

        // make sure to retain at least 1 bucket (sadly our factors are uh, very big, so this filter won't be able to do enough)
        let pre_len = cmp::max(GAINS / cmp::max(decimation_factor, expansion_factor), 1);

        let gains = SparseVec {
            pre: vec![1.0; pre_len],
            zeros: GAINS - pre_len,
            post: Vec::new(),
        };

        // N-taps should be the largest multiple of `expansion_factor` that's smaller than `GAINS`
        let n_taps = GAINS
            .next_multiple_of(expansion_factor.get())
            .saturating_sub(expansion_factor.get());
        let n_taps = if n_taps == 0 {
            expansion_factor
        } else {
            NonZero::new(n_taps).unwrap()
        };

        let taps = firwin2_sparse_gains(n_taps, gains);

        let mut taps = quantize_taps(taps, 1.0);

        if !taps.len().is_multiple_of(expansion_factor.get()) {
            taps.resize(taps.len().next_multiple_of(expansion_factor.get()), 0.0);
        }

        let n_delay = taps.len() / expansion_factor.get();

        let delay = vec![[0.0; 2]; n_delay];

        Self {
            expansion_factor,
            decimation_factor,
            tap_idx: expansion_factor.get() - 1,
            taps,
            delay,
            delay_idx: 0,
            leftover: 0,
        }
    }

    pub fn filter(&mut self, x: &[[f32; 2]], y: &mut Vec<[f32; 2]>) {
        if x.is_empty() {
            return;
        }

        let y_extra = ((x.len() * self.expansion_factor.get()) + self.leftover)
            / self.decimation_factor.get();

        let y_init = y.len();

        y.reserve(y_extra);
        let yp = &mut y.spare_capacity_mut()[..y_extra];
        let mut yidx = 0;

        let mut x = x.iter().copied();

        let gain = (self.expansion_factor.get() as f64).recip();

        'outputs: loop {
            let until_expand = self.expansion_factor.get() - self.tap_idx;
            let until_decimate = self.decimation_factor.get() - self.leftover;

            if until_expand <= until_decimate {
                self.leftover += until_expand;
                let Some(x) = x.next() else {
                    self.tap_idx = self.expansion_factor.get() - 1;
                    break 'outputs;
                };

                self.tap_idx = 0;

                self.delay[self.delay_idx] = [x[0] as f64, x[1] as f64];

                if self.delay_idx == 0 {
                    self.delay_idx = self.delay.len();
                }

                self.delay_idx -= 1;
                continue;
            }

            self.tap_idx += until_decimate;
            self.leftover = 0;

            let mut acc = [0.0; 2];
            let mut tap_idx = self.tap_idx;

            for idx in ((self.delay_idx + 1)..self.delay.len()).chain(0..(self.delay_idx + 1)) {
                acc[0] += self.delay[idx][0] * self.taps[tap_idx];
                acc[1] += self.delay[idx][1] * self.taps[tap_idx];
                tap_idx += self.expansion_factor.get();
            }

            unsafe {
                *yp.get_unchecked_mut(yidx) =
                    MaybeUninit::new([(acc[0] / gain) as f32, (acc[1] / gain) as f32])
            };
            yidx += 1;
        }

        // Safety:
        // - yidx is only incremented after writing init to it
        // - yidx starts at zero
        // - there were already `y_init` elements
        // -> everything up to `y_init + yidx` is initialized.
        // yidx `<=` y.capacity() because otherwise the indexing would've failed.
        unsafe { y.set_len(y_init + yidx) };
    }
}

#[derive(Debug)]
pub struct SparseVec<T> {
    pre: Vec<T>,
    zeros: usize,
    post: Vec<T>,
}

impl<T> SparseVec<T> {
    fn len(&self) -> usize {
        self.pre.len() + self.zeros + self.post.len()
    }
}

// https://github.com/scipy/scipy/blob/8c75ae75176236f233824e9a0483c26a69e6dfec/scipy/signal/_fir_filter_design.py#L577-L777
pub fn firwin2_sparse_gains(n_taps: NonZero<usize>, gains: SparseVec<f64>) -> Vec<f64> {
    assert_eq!(gains.len(), GAINS);
    let n_taps = n_taps.get();
    let n_gains = gains.len();

    let pre = (n_taps as f64 - 1.0) * -std::f64::consts::FRAC_PI_2;

    let gains_pre_len = gains.pre.len();
    let gainsc = SparseVec {
        pre: gains
            .pre
            .into_iter()
            .enumerate()
            .map(|(idx, r)| {
                let r = r;
                let p = pre * (idx as f64) / (n_gains as f64);
                Complex::from_polar(r, p)
            })
            .collect(),
        zeros: gains.zeros,
        post: gains
            .post
            .into_iter()
            .enumerate()
            .map(|(idx, r)| {
                let r = r;
                let p = pre * (gains_pre_len + gains.zeros + idx) as f64 / n_gains as f64;
                Complex::from_polar(r, p)
            })
            .collect(),
    };

    let taps = irdft_sparse(gainsc);

    let w = window_hann(n_taps);
    taps.into_iter().zip(w).map(|(t, w)| t * w).collect()
}

pub fn dft_sparse(x: SparseVec<Complex>) -> impl IntoIterator<Item = Complex> {
    let big_n = x.len();
    (0..big_n).map(move |k| {
        if k % 100 == 0 {}

        let pre_sum = x
            .pre
            .iter()
            .copied()
            .enumerate()
            .fold(Complex::ZERO, |out, (n, x)| {
                let f = std::f64::consts::TAU * (k as f64) * (n as f64) / (big_n as f64);
                out + x * Complex {
                    re: f.cos(),
                    im: -f.sin(),
                }
            });

        let pre_n = x.pre.len() + x.zeros;

        x.post
            .iter()
            .copied()
            .enumerate()
            .fold(pre_sum, |out, (n, x)| {
                let f = std::f64::consts::TAU * (k as f64) * ((pre_n + n) as f64) / (big_n as f64);
                out + x * Complex {
                    re: f.cos(),
                    im: -f.sin(),
                }
            })
    })
}

pub fn idft_sparse(mut x: SparseVec<Complex>) -> impl IntoIterator<Item = Complex> {
    let n = x.len();

    x.pre.iter_mut().for_each(|x| *x = x.conj());
    x.post.iter_mut().for_each(|x| *x = x.conj());
    let y = dft_sparse(x);

    // n is almost always near a power of two,
    // and floats generally do great at multiplying by `near-power-of-2.recip()`
    let scale = (n as f64).recip();
    y.into_iter().map(move |y| y.conj().scale(scale))
}

fn irdft_sparse_pad(xc: &mut SparseVec<Complex>) {
    // fixme: handle the math for pushing post, then zeros, then pre.
    // currently we just assume post is empty (because we don't have a use for it)
    assert!(xc.post.is_empty());
    let no2p1 = xc.len();
    let no2 = no2p1 - 1;

    let added_items = no2.saturating_sub(1);
    let added_nonzeros = added_items.saturating_sub(xc.zeros.saturating_sub(1));
    let added_zeros = added_items - added_nonzeros;
    xc.zeros += added_zeros;

    {
        let tmp = if xc.zeros == 0 {
            &mut xc.pre
        } else {
            &mut xc.post
        };
        tmp.reserve(added_nonzeros);
    }

    for n in (1 + added_zeros)..no2 {
        let c = xc.pre[no2 - n].conj();

        let xc = if xc.zeros == 0 {
            &mut xc.pre
        } else {
            &mut xc.post
        };

        xc.push(c);
    }
}

pub fn irdft_sparse(mut xc: SparseVec<Complex>) -> impl IntoIterator<Item = f64> {
    irdft_sparse_pad(&mut xc);

    idft_sparse(xc).into_iter().map(|x| x.re)
}

pub fn window_hann(len: usize) -> Vec<f64> {
    assert!(len >= 2);

    // the window is symmetric so we can just mirror the second half from the first half
    let half_len = len.div_ceil(2);

    let scale = std::f64::consts::PI / ((len - 1) as f64);

    let mut window = vec![0.0; len];

    for (idx, elem) in window[..half_len].iter_mut().enumerate() {
        *elem = (scale * (idx as f64)).sin().powi(2);
    }

    let (orig, mirror) = window.split_at_mut(half_len);

    for (mirror, orig) in mirror.iter_mut().rev().zip(orig) {
        *mirror = *orig;
    }

    window
}

pub fn quantize_taps(taps: Vec<f64>, total: f64) -> Vec<f64> {
    let sum = taps.iter().fold(0.0, |acc, &x| acc + x);

    taps.into_iter().map(|t| t * (total / sum)).collect()
}

#[cfg(test)]
mod tests {
    use std::iter;

    use crate::audio::{dft_sparse, idft_sparse, irdft_sparse, irdft_sparse_pad, quantize_taps};
    use crate::complex::Complex;

    #[test]
    fn test_quantise_taps() {
        let taps = vec![0.5, 1.0, 0.5];
        assert_eq!(quantize_taps(taps, 1.0), vec![0.25, 0.5, 0.25]);
    }

    // https://docs.rs/sdr/0.7.0/src/sdr/fir.rs.html#800
    #[test]
    fn dft_1() {
        let x = vec![
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 0.0 },
        ];
        let y = dft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        });

        assert_eq!(
            y.into_iter().collect::<Vec<_>>(),
            vec![
                Complex { re: 1.0, im: 1.0 },
                Complex { re: 1.0, im: 1.0 },
                Complex { re: 1.0, im: 1.0 },
                Complex { re: 1.0, im: 1.0 },
            ]
        );
    }

    #[test]
    fn dft_2() {
        let x = vec![
            Complex { re: 0.0, im: 1.0 },
            Complex { re: 0.0, im: -1.0 },
            Complex { re: 0.0, im: 1.0 },
            Complex { re: 0.0, im: -1.0 },
        ];
        let y = dft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 0.0).abs() < 1e-8);
        assert!((y[1].re - 0.0).abs() < 1e-8);
        assert!((y[2].re - 0.0).abs() < 1e-8);
        assert!((y[3].re - 0.0).abs() < 1e-8);
        assert!((y[0].im - 0.0).abs() < 1e-8);
        assert!((y[1].im - 0.0).abs() < 1e-8);
        assert!((y[2].im - 4.0).abs() < 1e-8);
        assert!((y[3].im - 0.0).abs() < 1e-8);
    }

    #[test]
    fn dft_3() {
        let x = vec![
            Complex { re: 2.0, im: 1.0 },
            Complex { re: 0.0, im: 1.0 },
            Complex { re: -2.0, im: 1.0 },
            Complex { re: 0.0, im: 1.0 },
            Complex { re: 2.0, im: 1.0 },
            Complex { re: 0.0, im: 1.0 },
            Complex { re: -2.0, im: 1.0 },
            Complex { re: 0.0, im: 1.0 },
        ];
        let y = dft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 0.0).abs() < 1e-7);
        assert!((y[0].im - 8.0).abs() < 1e-7);
        assert!((y[1].re - 0.0).abs() < 1e-7);
        assert!((y[1].im - 0.0).abs() < 1e-7);
        assert!((y[2].re - 8.0).abs() < 1e-7);
        assert!((y[2].im - 0.0).abs() < 1e-7);
        assert!((y[3].re - 0.0).abs() < 1e-7);
        assert!((y[3].im - 0.0).abs() < 1e-7);
        assert!((y[4].re - 0.0).abs() < 1e-7);
        assert!((y[4].im - 0.0).abs() < 1e-7);
        assert!((y[5].re - 0.0).abs() < 1e-7);
        assert!((y[5].im - 0.0).abs() < 1e-7);
        assert!((y[6].re - 8.0).abs() < 1e-7);
        assert!((y[6].im - 0.0).abs() < 1e-7);
        assert!((y[7].re - 0.0).abs() < 1e-7);
        assert!((y[7].im - 0.0).abs() < 1e-7);
    }

    #[test]
    fn dft_4() {
        let x = vec![
            Complex { re: 1.0, im: 0.0 },
            Complex { re: -1.0, im: 0.0 },
            Complex { re: 2.0, im: 0.0 },
            Complex { re: -2.0, im: 0.0 },
        ];
        let y = dft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 0.0).abs() < 1e-8);
        assert!((y[0].im - 0.0).abs() < 1e-8);
        assert!((y[1].re - -1.0).abs() < 1e-8);
        assert!((y[1].im - -1.0).abs() < 1e-8);
        assert!((y[2].re - 6.0).abs() < 1e-8);
        assert!((y[2].im - 0.0).abs() < 1e-8);
        assert!((y[3].re - -1.0).abs() < 1e-8);
        assert!((y[3].im - 1.0).abs() < 1e-8);
    }

    #[test]
    fn dft_5() {
        let x = vec![
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 0.0, im: 0.0 },
        ];
        let y = dft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 2.0).abs() < 1e-8);
        assert!((y[0].im - 2.0).abs() < 1e-8);
        assert!((y[1].re - 0.0).abs() < 1e-8);
        assert!((y[1].im - 0.0).abs() < 1e-8);
        assert!((y[2].re - 2.0).abs() < 1e-8);
        assert!((y[2].im - 2.0).abs() < 1e-8);
        assert!((y[3].re - 0.0).abs() < 1e-8);
        assert!((y[3].im - 0.0).abs() < 1e-8);

        let y2 = dft_sparse(crate::audio::SparseVec {
            pre: vec![Complex { re: 1.0, im: 1.0 }],
            zeros: 1,
            post: vec![Complex { re: 1.0, im: 1.0 }, Complex { re: 0.0, im: 0.0 }],
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_eq!(y, y2);
    }

    #[test]
    fn test_idft_1() {
        let x = vec![
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 1.0, im: 1.0 },
        ];

        let y = idft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 1.0).abs() < 1e-8);
        assert!((y[1].re - 0.0).abs() < 1e-8);
        assert!((y[2].re - 0.0).abs() < 1e-8);
        assert!((y[3].re - 0.0).abs() < 1e-8);
        assert!((y[0].im - 1.0).abs() < 1e-8);
        assert!((y[1].im - 0.0).abs() < 1e-8);
        assert!((y[2].im - 0.0).abs() < 1e-8);
        assert!((y[3].im - 0.0).abs() < 1e-8);
    }

    #[test]
    fn test_idft_2() {
        let x = vec![
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 4.0 },
            Complex { re: 0.0, im: 0.0 },
        ];

        let y = idft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 0.0).abs() < 1e-8);
        assert!((y[1].re - 0.0).abs() < 1e-8);
        assert!((y[2].re - 0.0).abs() < 1e-8);
        assert!((y[3].re - 0.0).abs() < 1e-8);
        assert!((y[0].im - 1.0).abs() < 1e-8);
        assert!((y[1].im - -1.0).abs() < 1e-8);
        assert!((y[2].im - 1.0).abs() < 1e-8);
        assert!((y[3].im - -1.0).abs() < 1e-8);

        let y2 = idft_sparse(crate::audio::SparseVec {
            pre: vec![],
            zeros: 2,
            post: vec![Complex { re: 0.0, im: 4.0 }, Complex { re: 0.0, im: 0.0 }],
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_eq!(y, y2);
    }

    #[test]
    fn test_idft_3() {
        let x = vec![
            Complex { re: 0.0, im: 0.0 },
            Complex { re: -1.0, im: -1.0 },
            Complex { re: 6.0, im: 0.0 },
            Complex { re: -1.0, im: 1.0 },
        ];

        let y = idft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0].re - 1.0).abs() < 1e-8);
        assert!((y[1].re - -1.0).abs() < 1e-8);
        assert!((y[2].re - 2.0).abs() < 1e-8);
        assert!((y[3].re - -2.0).abs() < 1e-8);
        assert!((y[0].im - 0.0).abs() < 1e-8);
        assert!((y[1].im - 0.0).abs() < 1e-8);
        assert!((y[2].im - 0.0).abs() < 1e-8);
        assert!((y[3].im - 0.0).abs() < 1e-8);
    }

    #[test]
    fn test_irdft_1() {
        let x = vec![
            Complex { re: 1.0, im: 0.0 },
            Complex { re: 1.0, im: 0.0 },
            Complex { re: 1.0, im: 0.0 },
        ];

        let y = irdft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0] - 1.0).abs() < 1e-8);
        assert!((y[1] - 0.0).abs() < 1e-8);
        assert!((y[2] - 0.0).abs() < 1e-8);
        assert!((y[3] - 0.0).abs() < 1e-8);
    }

    #[test]
    fn test_irdft_2() {
        let x = vec![
            Complex { re: 0.0, im: 0.0 },
            Complex { re: -1.0, im: -1.0 },
            Complex { re: 6.0, im: 0.0 },
        ];

        let y = irdft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0] - 1.0).abs() < 1e-8);
        assert!((y[1] - -1.0).abs() < 1e-8);
        assert!((y[2] - 2.0).abs() < 1e-8);
        assert!((y[3] - -2.0).abs() < 1e-8);
    }

    #[test]
    fn test_irdft_3() {
        let x = vec![
            Complex { re: 1.0, im: 0.0 },
            Complex {
                re: 0.19509032201612833,
                im: 0.9807852804032304,
            },
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
        ];

        let y = irdft_sparse(crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert!((y[0] - 0.09929861743087548).abs() < 1e-8);
        assert!((y[1] - 0.03574621740468193).abs() < 1e-8);
        assert!((y[2] - -0.020738854791726727).abs() < 1e-8);
        assert!((y[3] - -0.05896903786590271).abs() < 1e-8);
        assert!((y[4] - -0.07137237519757898).abs() < 1e-8);
        assert!((y[5] - -0.05549223371126744).abs() < 1e-8);
        assert!((y[6] - -0.014473870185486475).abs() < 1e-8);
        assert!((y[7] - 0.04355852542626773).abs() < 1e-8);
        assert!((y[8] - 0.10711092545246059).abs() < 1e-8);
        assert!((y[9] - 0.16359599764886953).abs() < 1e-8);
        assert!((y[10] - 0.2018261807230456).abs() < 1e-8);
        assert!((y[11] - 0.21422951805472182).abs() < 1e-8);
        assert!((y[12] - 0.19834937656841023).abs() < 1e-8);
        assert!((y[13] - 0.15733101304262898).abs() < 1e-8);
        assert_eq!(y.len(), 14);

        let y2 = irdft_sparse(crate::audio::SparseVec {
            pre: vec![
                Complex { re: 1.0, im: 0.0 },
                Complex {
                    re: 0.19509032201612833,
                    im: 0.9807852804032304,
                },
            ],
            zeros: 6,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        for (idx, (y1, y2)) in y.into_iter().zip(y2).enumerate() {
            assert_eq!(y1, y2, "y[{idx}] != y2[{idx}]");
        }
    }

    #[test]
    fn test_irdft_pad_1() {
        let x = vec![
            Complex { re: 1.0, im: 0.0 },
            Complex {
                re: 0.19509032201612833,
                im: 0.9807852804032304,
            },
            Complex {
                re: -0.9238795325112867,
                im: -0.3826834323650899,
            },
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
        ];

        let mut y1 = crate::audio::SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        };

        irdft_sparse_pad(&mut y1);
        assert_eq!(y1.len(), 14);

        let mut y2 = crate::audio::SparseVec {
            pre: vec![
                Complex { re: 1.0, im: 0.0 },
                Complex {
                    re: 0.19509032201612833,
                    im: 0.9807852804032304,
                },
                Complex {
                    re: -0.9238795325112867,
                    im: -0.3826834323650899,
                },
            ],
            zeros: 5,
            post: Vec::new(),
        };

        irdft_sparse_pad(&mut y2);
        assert_eq!(y2.len(), y1.len());

        let y2 = y2
            .pre
            .into_iter()
            .chain(iter::repeat_n(Complex::ZERO, y2.zeros))
            .chain(y2.post);

        for (idx, (y1, y2)) in y1.pre.into_iter().zip(y2).enumerate() {
            assert_eq!(y1, y2, "y[{idx}] != y2[{idx}]");
        }
    }
}
