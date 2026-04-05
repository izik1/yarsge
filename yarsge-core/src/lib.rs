#![warn(clippy::pedantic)]
#![allow(
    clippy::verbose_bit_mask,
    clippy::inline_always,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap
)]

use std::mem;

pub mod emu;

#[derive(Copy, Clone)]
struct EdgeDetector<const EDGE: bool>(bool);

impl<const EDGE: bool> EdgeDetector<EDGE> {
    #[must_use]
    const fn new(value: bool) -> Self {
        Self(value)
    }

    const fn get(self) -> bool {
        self.0
    }

    const fn tick(&mut self, new: bool) -> bool {
        let old = mem::replace(&mut self.0, new);

        if EDGE { !old && new } else { old && !new }
    }
}

type RisingEdge = EdgeDetector<true>;

type FallingEdge = EdgeDetector<false>;

bitflags::bitflags! {

    #[derive(Clone, Copy, Eq, PartialEq, Debug)]
    pub struct Keys : u8 {
        // buttons
        const A = 1 << 0;
        const B = 1 << 1;
        const SELECT = 1 << 2;
        const START = 1 << 3;

        const RIGHT = 1 << 4;
        const LEFT = 1 << 5;
        const UP = 1 << 6;
        const DOWN = 1 << 7;
    }
}

pub mod util {
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
