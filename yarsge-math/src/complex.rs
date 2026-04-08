use core::fmt;

use crate::util::FloatExt as _;

#[derive(Copy, Clone, PartialEq)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

impl fmt::Debug for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {}i)",
            self.re,
            if self.im.is_sign_positive() || self.im == 0.0 {
                "+"
            } else {
                "-"
            },
            self.im.abs()
        )
    }
}

impl Complex {
    pub const ZERO: Self = Self { re: 0.0, im: 0.0 };
    pub const ONE: Self = Self { re: 1.0, im: 0.0 };
    pub const I: Self = Self { re: 0.0, im: 1.0 };

    #[inline]
    #[must_use]
    pub fn cis(theta: f64) -> Self {
        let (sin, cos) = theta.sin_cos();
        Self { re: cos, im: sin }
    }

    #[inline(always)]
    #[must_use]
    pub fn abs(self) -> f64 {
        f64::hypot(self.re, self.im)
    }

    #[inline]
    #[must_use]
    pub fn to_polar(self) -> (f64, f64) {
        (self.abs(), f64::atan2(self.im, self.re))
    }

    #[inline]
    #[must_use]
    pub fn from_polar(r: f64, p: f64) -> Self {
        Self::cis(p).scale(r)
    }

    #[inline]
    #[must_use]
    pub fn conj(self) -> Self {
        Self {
            re: self.re,
            im: -self.im,
        }
    }

    #[inline]
    #[must_use]
    pub fn scale(self, t: f64) -> Self {
        Self {
            re: self.re * t,
            im: self.im * t,
        }
    }

    #[inline]
    #[must_use]
    pub fn unscale(self, t: f64) -> Self {
        Self {
            re: self.re / t,
            im: self.im / t,
        }
    }

    #[inline]
    #[must_use]
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
    #[inline]
    fn mul(self, other: Self) -> Self::Output {
        Self {
            re: self.re.mul_add_fast(other.re, -(self.im * other.im)),
            im: self.re.mul_add_fast(other.im, self.im * other.re),
        }
    }
}

impl std::ops::Add for Complex {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self::Output {
        Self {
            re: self.re + other.re,
            im: self.im + other.im,
        }
    }
}

impl std::ops::Sub for Complex {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        Self {
            re: self.re - other.re,
            im: self.im - other.im,
        }
    }
}

impl std::ops::Neg for Complex {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            re: -self.re,
            im: -self.im,
        }
    }
}
