use yarsge_math::{Complex, SparseVec};

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

#[cfg(test)]
mod tests {
    use std::iter;

    use yarsge_math::{Complex, SparseVec};

    use crate::fourier::{dft_sparse, idft_sparse, irdft_sparse, irdft_sparse_pad};

    // https://docs.rs/sdr/0.7.0/src/sdr/fir.rs.html#800
    #[test]
    fn dft_1() {
        let x = vec![
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
        let y = dft_sparse(SparseVec {
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
        let y = dft_sparse(SparseVec {
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
        let y = dft_sparse(SparseVec {
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
        let y = dft_sparse(SparseVec {
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

        let y = idft_sparse(SparseVec {
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

        let y = irdft_sparse(SparseVec {
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

        let y = irdft_sparse(SparseVec {
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

        let y = irdft_sparse(SparseVec {
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
