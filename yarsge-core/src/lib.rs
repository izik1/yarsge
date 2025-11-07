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

    fn tick(&mut self, new: bool) -> bool {
        let old = mem::replace(&mut self.0, new);

        if EDGE { !old && new } else { old && !new }
    }
}

// type RisingEdge = EdgeDetector<true>;

type FallingEdge = EdgeDetector<false>;
