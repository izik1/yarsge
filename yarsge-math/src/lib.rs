#![warn(clippy::pedantic)]
#![allow(
    clippy::verbose_bit_mask,
    clippy::inline_always,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap
)]

mod complex;
mod ring_buf;
mod sparse;

mod util {
    mod sealed {
        pub trait Sealed {}

        impl Sealed for f32 {}
        impl Sealed for f64 {}
    }

    pub trait FloatExt: sealed::Sealed {
        /// Picks the faster implementation of `self * y + z` for the target.
        ///
        /// # Output
        /// This will either output exactly `self * y + z` or `self.mul_add(y, z)`,
        /// it's unspecified which one will be chosen, but one of them will be.
        #[must_use]
        fn mul_add_fast(self, y: Self, z: Self) -> Self;
    }

    impl FloatExt for f32 {
        #[inline(always)]
        fn mul_add_fast(self, y: Self, z: Self) -> Self {
            #[cfg(all(target_arch = "x86_64", target_feature = "fma"))]
            {
                self.mul_add(y, z)
            }

            #[cfg(not(all(target_arch = "x86_64", target_feature = "fma")))]
            {
                self * y + z
            }
        }
    }

    impl FloatExt for f64 {
        #[inline(always)]
        fn mul_add_fast(self, y: Self, z: Self) -> Self {
            #[cfg(all(target_arch = "x86_64", target_feature = "fma"))]
            {
                self.mul_add(y, z)
            }

            #[cfg(not(all(target_arch = "x86_64", target_feature = "fma")))]
            {
                self * y + z
            }
        }
    }
}

pub mod bmi {
    #[cfg(all(
        any(target_arch = "x86", target_arch = "x86_64"),
        target_feature = "bmi2"
    ))]
    #[cfg_attr(target_feature = "bmi2", target_feature(enable = "bmi2"))]
    #[inline]
    fn interleave_bmi2(hi: u8, lo: u8) -> u16 {
        #[cfg(target_arch = "x86")]
        use std::arch::x86 as arch;
        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64 as arch;

        let hi = arch::_pdep_u32(u32::from(hi), 0x0000_aaaa);
        let lo = arch::_pdep_u32(u32::from(lo), 0x0000_5555);
        (hi | lo) as u16
    }

    #[inline]
    pub fn interleave(hi: u8, lo: u8) -> u16 {
        #[cfg(all(
            any(target_arch = "x86", target_arch = "x86_64"),
            target_feature = "bmi2"
        ))]
        {
            // Safety: requires BMI2, but BMI2 is right here
            unsafe { interleave_bmi2(hi, lo) }
        }
        #[cfg(not(all(
            any(target_arch = "x86", target_arch = "x86_64"),
            target_feature = "bmi2"
        )))]
        {
            // https://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN
            // because we're interleaving bytes we can do `hi` and `lo` at the same time.
            let hi = u32::from(hi);
            let lo = u32::from(lo);

            let res = (hi << 16) | lo;

            let res = (res | (res << 4)) & 0x0f0f_0f0f;
            let res = (res | (res << 2)) & 0x3333_3333;
            let res = (res | (res << 1)) & 0x5555_5555;

            let res = res | ((res >> 16) << 1);
            res as u16
        }
    }

    #[cfg(test)]
    mod test {
        #[test]
        fn interleave_compare() {
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

            for hi in 0..=u8::MAX {
                for lo in 0..=u8::MAX {
                    assert_eq!(super::interleave(hi, lo), interleave(hi, lo));
                }
            }
        }
    }
}

pub use complex::Complex;
pub use ring_buf::RingBuf;
pub use sparse::SparseVec;
pub use util::FloatExt;
