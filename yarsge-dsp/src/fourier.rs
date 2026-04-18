use yarsge_math::{Complex, SparseVec};

pub fn dft_sparse(x: SparseVec<Complex>) -> impl IntoIterator<Item = Complex> {
    let big_n = x.len();
    let pre_n = x.pre.len() + x.zeros;
    let f_init = std::f64::consts::TAU / (big_n as f64);

    let f: Vec<_> = (0..big_n)
        .map(|k| {
            let f = f_init * (k as f64);
            Complex::cis(f).conj()
        })
        .collect();

    (0..big_n).map(move |k| {
        let pre_sum = x
            .pre
            .iter()
            .copied()
            .enumerate()
            .fold(Complex::ZERO, |out, (n, x)| {
                x.mul_add(f[(k * n) % big_n], out)
            });

        x.post
            .iter()
            .copied()
            .enumerate()
            .fold(pre_sum, |out, (n, x)| {
                x.mul_add(f[(k * (pre_n + n)) % big_n], out)
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
        let tmp: &mut Vec<Complex> = if xc.zeros == 0 {
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

#[cfg(test)]
mod tests {
    use core::fmt;
    use std::{cmp, iter};

    use yarsge_math::{Complex, SparseVec};

    use crate::fourier::{dft_sparse, idft_sparse, irdft_sparse, irdft_sparse_pad};

    fn complex_mse(h: &[Complex], e: &[Complex]) -> f64 {
        let len = cmp::min(h.len(), e.len()) as f64;
        let total = h
            .iter()
            .zip(e)
            .map(|(h, e)| (*e - *h).abs().powi(2))
            .sum::<f64>();

        total / len
    }

    macro_rules! assert_complex_mse {
        ($left:expr, $right:expr $(,)?) => {
            match (&$left, &$right) {
                (left_val, right_val) => {
                    assert_eq!(left_val.len(), right_val.len());
                    let mse = complex_mse(left_val, right_val);
                    assert!(
                        mse < 1e-29,
                        "{}",
                        fmt::from_fn(|f| assert_approx_eq_failed(f, mse, left_val, right_val))
                    );
                }
            }
        };
    }

    fn mse(h: &[f64], e: &[f64]) -> f64 {
        let len = cmp::min(h.len(), e.len()) as f64;
        let total = h
            .iter()
            .zip(e)
            .map(|(h, e)| (*e - *h).abs().powi(2))
            .sum::<f64>();

        total / len
    }

    macro_rules! assert_mse {
        ($left:expr, $right:expr $(,)?) => {
            match (&$left, &$right) {
                (left_val, right_val) => {
                    assert_eq!(left_val.len(), right_val.len());
                    let mse = mse(left_val, right_val);
                    assert!(
                        mse < 1e-31,
                        "{}",
                        fmt::from_fn(|f| assert_approx_eq_failed(f, mse, left_val, right_val))
                    );
                }
            }
        };
    }

    #[cold]
    #[inline(never)]
    fn assert_approx_eq_failed(
        f: &mut fmt::Formatter<'_>,
        mse: f64,
        left: &dyn fmt::Debug,
        right: &dyn fmt::Debug,
    ) -> fmt::Result {
        writeln!(f, "assertion `left` ~= `right` failed")?;
        writeln!(f, "left: {left:#?}")?;
        writeln!(f, "right: {right:#?}")?;
        writeln!(f, "mse: {mse}")
    }

    // https://docs.rs/sdr/0.7.0/src/sdr/fir.rs.html#800
    #[test]
    fn dft_1() {
        let x: Vec<Complex> = vec![
            Complex { re: 1.0, im: 1.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 0.0 },
        ];
        let y = dft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        });

        assert_eq!(
            &y.into_iter().collect::<Vec<_>>(),
            &[
                Complex { re: 1.0, im: 1.0 },
                Complex { re: 1.0, im: 1.0 },
                Complex { re: 1.0, im: 1.0 },
                Complex { re: 1.0, im: 1.0 },
            ]
        );
    }

    #[test]
    fn dft_2() {
        let x = vec![Complex::I, -Complex::I, Complex::I, -Complex::I];
        let y = dft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        let expected = [
            Complex::ZERO,
            Complex::ZERO,
            Complex { re: 0.0, im: 4.0 },
            Complex::ZERO,
        ];

        assert_complex_mse!(expected, y);
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
        let y = dft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_complex_mse!(
            y,
            [
                Complex { re: 0.0, im: 8.0 },
                Complex { re: 0.0, im: 0.0 },
                Complex { re: 8.0, im: 0.0 },
                Complex { re: 0.0, im: 0.0 },
                Complex { re: 0.0, im: 0.0 },
                Complex { re: 0.0, im: 0.0 },
                Complex { re: 8.0, im: 0.0 },
                Complex { re: 0.0, im: 0.0 },
            ]
        );
    }

    #[test]
    fn dft_4() {
        let x = vec![
            Complex { re: 1.0, im: 0.0 },
            Complex { re: -1.0, im: 0.0 },
            Complex { re: 2.0, im: 0.0 },
            Complex { re: -2.0, im: 0.0 },
        ];
        let y = dft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_complex_mse!(
            y,
            [
                Complex::ZERO,
                Complex { re: -1.0, im: -1.0 },
                Complex { re: 6.0, im: 0.0 },
                Complex { re: -1.0, im: 1.0 },
            ]
        );
    }

    #[test]
    fn dft_5() {
        let x = vec![
            Complex { re: 1.0, im: 1.0 },
            Complex::ZERO,
            Complex { re: 1.0, im: 1.0 },
            Complex::ZERO,
        ];
        let y = dft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        let expected = [
            Complex { re: 2.0, im: 2.0 },
            Complex::ZERO,
            Complex { re: 2.0, im: 2.0 },
            Complex::ZERO,
        ];

        assert_complex_mse!(y, expected);

        let y2 = dft_sparse(SparseVec {
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

        let y = idft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        let expected = [
            Complex { re: 1.0, im: 1.0 },
            Complex::ZERO,
            Complex::ZERO,
            Complex::ZERO,
        ];

        assert_complex_mse!(y, expected);
    }

    #[test]
    fn test_idft_2() {
        let x = vec![
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 0.0 },
            Complex { re: 0.0, im: 4.0 },
            Complex { re: 0.0, im: 0.0 },
        ];

        let y = idft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_complex_mse!(y, [Complex::I, -Complex::I, Complex::I, -Complex::I]);

        let y2 = idft_sparse(SparseVec {
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

        let y = idft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_complex_mse!(
            y,
            [
                Complex { re: 1.0, im: 0.0 },
                Complex { re: -1.0, im: 0.0 },
                Complex { re: 2.0, im: 0.0 },
                Complex { re: -2.0, im: 0.0 },
            ]
        );
    }

    #[test]
    fn test_irdft_1() {
        let x = vec![
            Complex { re: 1.0, im: 0.0 },
            Complex { re: 1.0, im: 0.0 },
            Complex { re: 1.0, im: 0.0 },
        ];

        let y = irdft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_mse!(y, [1.0, 0.0, 0.0, 0.0]);
    }

    #[test]
    fn test_irdft_2() {
        let x = vec![
            Complex { re: 0.0, im: 0.0 },
            Complex { re: -1.0, im: -1.0 },
            Complex { re: 6.0, im: 0.0 },
        ];

        let y = irdft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_mse!(y, [1.0, -1.0, 2.0, -2.0]);
    }

    #[test]
    fn test_irdft_3() {
        let x = vec![
            Complex::ONE,
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

        let y = irdft_sparse(SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        })
        .into_iter()
        .collect::<Vec<_>>();

        assert_mse!(
            y,
            [
                0.09929861743087548,
                0.03574621740468193,
                -0.020738854791726727,
                -0.05896903786590271,
                -0.07137237519757898,
                -0.05549223371126744,
                -0.014473870185486475,
                0.04355852542626773,
                0.10711092545246059,
                0.16359599764886953,
                0.2018261807230456,
                0.21422951805472182,
                0.19834937656841023,
                0.15733101304262898,
            ]
        );

        let y2 = irdft_sparse(SparseVec {
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

        let mut y1 = SparseVec {
            pre: x,
            zeros: 0,
            post: Vec::new(),
        };

        irdft_sparse_pad(&mut y1);
        assert_eq!(y1.len(), 14);

        let mut y2 = SparseVec {
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
