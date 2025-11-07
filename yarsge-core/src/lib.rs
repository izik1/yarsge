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

bitflags::bitflags! {
        // if !bits::has_bit(self.status, 5) {
        //     p1 & 0xf
        // } else if !bits::has_bit(self.status, 4) {
        //     p1 >> 4
        // } else {
        //     0xf
        // }


    #[derive(Clone, Copy)]
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
