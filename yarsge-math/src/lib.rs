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

pub use complex::Complex;
pub use ring_buf::RingBuf;
pub use sparse::SparseVec;
pub use util::FloatExt;
