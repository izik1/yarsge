#![warn(clippy::pedantic, clippy::nursery)]

pub mod audio;
pub mod input;

use std::marker::PhantomData;
use std::time::{Duration, Instant};

// Hack to work around a lack of const generics :/
pub trait Period {
    const PERIOD: Duration;
}

#[macro_export]
macro_rules! make_period_tys {
    ($(struct $id:ident($e:expr));*$(;)?) => {
        $(
            struct $id;

            impl $id {
                pub const PERIOD: ::core::time::Duration = $e;
            }

            impl $crate::Period for $id {
                const PERIOD: ::core::time::Duration = Self::PERIOD;
            }
        )*
    };
}

pub struct Statistics {
    pub next_report: Instant,
    pub total_microsleep_time: Duration,
    /// Amount of time past when we asked to sleep that we ended up sleeping.
    pub microsleep_slack_time: Duration,
    pub total_emulated_time: Duration,
    pub subframe: u64,
    pub display_frame: u64,
}

impl Statistics {
    const PERIOD: Duration = Duration::from_secs(1);

    #[must_use]
    pub fn new(start: Instant) -> Self {
        Self {
            next_report: start + Self::PERIOD,
            total_microsleep_time: Duration::ZERO,
            microsleep_slack_time: Duration::ZERO,
            total_emulated_time: Duration::ZERO,
            subframe: 0,
            display_frame: 0,
        }
    }
}

#[non_exhaustive]
pub struct Interval<P> {
    pub next: Instant,
    mark_period: PhantomData<fn(P)>,
}

impl<P> Interval<P> {
    #[must_use]
    pub const fn at(start: Instant) -> Self {
        Self {
            next: start,
            mark_period: PhantomData,
        }
    }
}

impl<P: Period> Interval<P> {
    pub fn tick(&mut self, now: Instant) -> Option<Duration> {
        let elapsed = now.checked_duration_since(self.next)?;

        self.next = if elapsed > P::PERIOD {
            now + P::PERIOD
        } else {
            self.next + P::PERIOD
        };

        Some(elapsed)
    }
}

#[cold]
#[inline(never)]
pub fn report_statistics(
    stats: &mut Statistics,
    audio_bytes_ahead: Option<i32>,
    current_frame: Instant,
    start: Instant,
) {
    // if we've lapsed, just reset the clock
    if stats.next_report + Statistics::PERIOD < current_frame {
        stats.next_report = current_frame + Statistics::PERIOD;
    } else {
        stats.next_report += Statistics::PERIOD;
    }

    let elapsed = start.elapsed();

    log::debug!(
        target: "statistics",
        "emu-time (factor: {:.6})",
        stats.total_emulated_time.div_duration_f64(elapsed),
    );

    // should be very close to 1 unless `no-time-control` is set`
    log::debug!(
        target: "statistics",
        "microsleep (factor: {:.3}, average: {:?}, slack: {:?})",
        stats.total_microsleep_time.div_duration_f64(elapsed),
        stats.total_microsleep_time.div_f64(stats.subframe as f64),
        stats.microsleep_slack_time.div_f64(stats.subframe as f64),
    );
    log::debug!(
        target: "statistics",
        "UPS: {:.2}, FPS: {:.2}",
        (stats.subframe as f64) / elapsed.as_secs_f64(),
        (stats.display_frame as f64) / elapsed.as_secs_f64(),
    );

    if let Some(audio_bytes_ahead) = audio_bytes_ahead {
        let bytes = audio_bytes_ahead.cast_unsigned();
        log::debug!(
            target: "statistics",
            "Audio buffer (bytes: {bytes}, samples: {samples}, duration: {duration:.03}s)",
            // 2 channels (f32) * 4 bytes per float.
            samples = bytes / 8,
            duration = f64::from(bytes / 8) * const { 48000.0f64.recip() },
        );
    }
}
