use std::cmp;
use std::mem::MaybeUninit;
use std::num::NonZero;

use yarsge_math::{Complex, FloatExt as _, RingBuf, SparseVec};

use crate::fourier::irdft_sparse;
use crate::window;

#[derive(Clone)]
pub struct Fir {
    expansion_factor: NonZero<usize>,
    decimation_factor: NonZero<usize>,
    taps: Vec<f64>,
    tap_idx: usize,
    delay: RingBuf<[f64; 2]>,
    leftover: usize,
}

const GAINS: usize = 1 << 18;

impl Fir {
    pub fn new(expansion_factor: NonZero<usize>, decimation_factor: NonZero<usize>) -> Self {
        // sure, we could support net expansion, but, meh.
        assert!(expansion_factor <= decimation_factor);

        // make sure to retain at least 1 bucket (our factors can be very big)
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

        let n_delay = taps.len() / expansion_factor;

        let taps = transpose_taps(taps, n_delay);

        let delay = vec![[0.0; 2]; n_delay].into();

        Self {
            expansion_factor,
            decimation_factor,
            tap_idx: expansion_factor.get() - 1,
            taps,
            delay,
            leftover: 0,
        }
    }

    pub fn filter(&mut self, x: &[[f32; 2]], y: &mut Vec<[f32; 2]>) {
        debug_assert_eq!(
            self.taps.len(),
            self.expansion_factor.get() * self.delay.len()
        );

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
                let until_decimate = self.decimation_factor.get() - self.leftover;

                let ticks = (until_decimate / self.expansion_factor) + 1;

                for t in 0..ticks {
                    let Some(x) = x.next() else {
                        self.tap_idx = self.expansion_factor.get() - 1;

                        // use this specific ordering so that we don't do integer overflow or underflow.
                        self.leftover = self.leftover + self.expansion_factor.get() * t - 1;

                        break 'outputs;
                    };

                    self.delay.push(x.map(f64::from));
                }

                self.tap_idx = until_decimate - (ticks - 1) * self.expansion_factor.get();
                self.leftover = 0;
            }

            debug_assert!(self.tap_idx < self.expansion_factor.get());
            let taps = &self.taps[self.delay.len() * self.tap_idx..][..self.delay.len()];
            let acc = accumulate(&self.delay, taps);

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

fn accumulate(delay: &RingBuf<[f64; 2]>, taps: &[f64]) -> [f64; 2] {
    // unfortunately `chain` is really unamenable to autovectorization :/
    let delay = delay.split();

    let taps = taps.split_at(delay.1.len());

    let acc = delay
        .1
        .iter()
        .zip(taps.0)
        .fold([0.0; 2], |dest, (delay, &tap)| {
            [
                delay[0].mul_add_fast(tap, dest[0]),
                delay[1].mul_add_fast(tap, dest[1]),
            ]
        });

    let acc = delay.0.iter().zip(taps.1).fold(acc, |dest, (delay, &tap)| {
        [
            delay[0].mul_add_fast(tap, dest[0]),
            delay[1].mul_add_fast(tap, dest[1]),
        ]
    });

    acc
}

// https://github.com/scipy/scipy/blob/8c75ae75176236f233824e9a0483c26a69e6dfec/scipy/signal/_fir_filter_design.py#L577-L777
fn firwin2_sparse_gains(n_taps: NonZero<usize>, gains: SparseVec<f64>) -> Vec<f64> {
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

    let w = window::hann(n_taps);
    taps.into_iter().zip(w).map(|(t, w)| t * w).collect()
}

fn quantize_taps(taps: Vec<f64>, total: f64) -> Vec<f64> {
    let sum = taps.iter().fold(0.0, |acc, &x| acc + x);

    taps.into_iter().map(|t| t * (total / sum)).collect()
}

fn transpose_taps<T: Copy>(taps: Vec<T>, n_delay: usize) -> Vec<T> {
    let other_dim = taps.len() / n_delay;
    assert!(other_dim > 0 && n_delay > 0);
    assert_eq!(taps.len() % n_delay, 0);

    let mut output = Vec::with_capacity(taps.len());

    {
        let output = &mut output.spare_capacity_mut()[..taps.len()];

        for (idx, output) in output.iter_mut().enumerate() {
            let y = idx % n_delay;
            let x = idx / n_delay;
            *output = MaybeUninit::new(taps[y * other_dim + x]);
        }
    }

    // Safety:
    // - output has capacity >= `taps.len()`, it was created with that capacity and no capacity decreasing operations have been taken.
    // - all elements of output up to this point are initialized (we go through each element of the output array trivially, since we simply iterate through it)
    unsafe {
        output.set_len(taps.len());
    }

    output
}

#[cfg(test)]
mod tests {
    use crate::fir::quantize_taps;

    #[test]
    fn quantize_taps_simple() {
        let taps = vec![0.5, 1.0, 0.5];
        assert_eq!(quantize_taps(taps, 1.0), vec![0.25, 0.5, 0.25]);
    }

    #[test]
    fn transpose_taps_square() {
        let taps = super::transpose_taps(vec![0_u8, 1, 2, 3], 2);

        assert_eq!(taps, &[0, 2, 1, 3])
    }

    #[test]
    fn transpose_taps_rectangle_a() {
        let taps = super::transpose_taps(vec![0_u8, 1, 2, 3, 4, 5], 3);

        assert_eq!(taps, &[0, 2, 4, 1, 3, 5])
    }

    #[test]
    fn transpose_taps_rectangle_b() {
        let taps = super::transpose_taps(vec![0_u8, 1, 2, 3, 4, 5], 2);

        assert_eq!(taps, &[0, 3, 1, 4, 2, 5])
    }
}
