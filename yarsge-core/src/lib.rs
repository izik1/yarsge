// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

#![feature(nll)]
#![cfg_attr(feature = "cargo-clippy", allow(verbose_bit_mask))]

#[macro_use]
extern crate bitflags;

extern crate failure;

pub mod emu;
