use core::fmt;

use yarsge_core::util::FloatExt as _;

#[derive(Copy, Clone, PartialEq)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

impl fmt::Debug for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Complex")
            .field(&fmt::from_fn(|f| {
                write!(
                    f,
                    r#""{} {} {}i""#,
                    self.re,
                    if self.im.is_sign_positive() || self.im == 0.0 {
                        "+"
                    } else {
                        "-"
                    },
                    self.im.abs()
                )
            }))
            .finish()
    }
}

impl Complex {
    pub const ZERO: Self = Self { re: 0.0, im: 0.0 };
    pub const ONE: Self = Self { re: 1.0, im: 0.0 };
    pub const I: Self = Self { re: 0.0, im: 1.0 };

    pub fn cis(theta: f64) -> Self {
        let (sin, cos) = theta.sin_cos();
        Self { re: cos, im: sin }
    }

    pub fn to_polar(self) -> (f64, f64) {
        (f64::hypot(self.re, self.im), f64::atan2(self.im, self.re))
    }

    pub fn from_polar(r: f64, p: f64) -> Self {
        Self::cis(p).scale(r)
    }

    pub fn conj(self) -> Self {
        Self {
            re: self.re,
            im: -self.im,
        }
    }

    pub fn scale(self, t: f64) -> Self {
        Self {
            re: self.re * t,
            im: self.im * t,
        }
    }

    pub fn unscale(self, t: f64) -> Self {
        Self {
            re: self.re / t,
            im: self.im / t,
        }
    }

    pub fn mul_add(self, x: Complex, y: Complex) -> Self {
        Self {
            re: self
                .re
                .mul_add_fast(x.re, -self.im.mul_add_fast(x.im, y.re)),
            im: self.re.mul_add_fast(x.im, self.im.mul_add_fast(x.re, y.im)),
        }
    }
}

impl std::ops::Mul for Complex {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        Self {
            re: self.re.mul_add_fast(other.re, -(self.im * other.im)),
            im: self.re.mul_add_fast(other.im, self.im * other.re),
        }
    }
}

impl std::ops::Add for Complex {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        Self {
            re: self.re + other.re,
            im: self.im + other.im,
        }
    }
}
