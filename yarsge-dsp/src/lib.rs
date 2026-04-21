#![warn(clippy::must_use_candidate)]

mod fir;

pub mod fourier;
pub mod window;

pub use fir::Fir;
