use core::fmt;
use std::cmp;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::{Duration, Instant};

use anyhow::Context;
use clap::Parser;
use rgb::RGB8;
use sdl3::audio::{AudioDevice, AudioSpec, AudioStreamOwner};
use sdl3::render::Canvas;
use sdl3::video::Window;
use sdl3::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};
use sdl3::{event::Event, keyboard::Keycode, pixels::Color};
use yarsge_core::emu::apu::ApuSampler;
use yarsge_core::util::FloatExt as _;
use yarsge_core::{Keys, emu};

const NAME: &str = env!("CARGO_PKG_NAME");

struct HexColor(RGB8);

impl fmt::Display for HexColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let RGB8 { r, g, b } = self.0;
        write!(f, "#{r:02x}{g:02x}{b:02x}")
    }
}

impl FromStr for HexColor {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let suffix = s.strip_prefix('#').ok_or("invalid palette color")?;

        if suffix.len() != 6 {
            return Err("invalid palette color");
        }

        let value = u32::from_str_radix(suffix, 16).map_err(|_| "invalid palette color")?;

        let [_, r, g, b] = value.to_be_bytes();

        Ok(Self(RGB8 { r, g, b }))
    }
}

#[derive(Clone, Debug)]
struct Palette([RGB8; 4]);

impl Palette {
    const DEFAULT: Self = Self([
        RGB8 {
            r: 0x9b,
            g: 0xbc,
            b: 0x0f,
        },
        RGB8 {
            r: 0x8b,
            g: 0xac,
            b: 0x0f,
        },
        RGB8 {
            r: 0x30,
            g: 0x62,
            b: 0x30,
        },
        RGB8 {
            r: 0x0f,
            g: 0x38,
            b: 0x0f,
        },
    ]);
}

impl FromStr for Palette {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let palette = s
            .split(',')
            .map(HexColor::from_str)
            .map(|it| it.map(|it| it.0))
            .collect::<Result<Vec<_>, _>>()?;

        palette.try_into().map(Self).map_err(|_| "invalid palette")
    }
}

impl fmt::Display for Palette {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [white, light, dark, black] = self.0.map(HexColor);
        write!(f, "{white},{light},{dark},{black}")
    }
}

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
enum AudioSystem {
    Mute,
    NearestNeighbor,
    Mean,
}

#[derive(Parser, Debug)]
#[clap(about = "Emulates GameBoy games.", author)]
struct Opt {
    #[clap(
        short = 's',
        long = "scale",
        help = "Screen scale size factor",
        default_value = "1"
    )]
    scale: u32,

    #[clap(help = "Path to the boot rom")]
    boot_rom: PathBuf,

    #[clap(help = "Path to the game rom")]
    game_rom: PathBuf,

    #[clap(short = 'p', long, default_value_t = Palette::DEFAULT)]
    palette: Palette,

    #[clap(long, value_enum, default_value_t = AudioSystem::Mean)]
    audio_system: AudioSystem,

    #[clap(
        long,
        help = r#"overclock the CPU to run as fast as possible"#,
        conflicts_with = "audio_system"
    )]
    no_time_control: bool,
}

fn lookup_key(map: &[(Keycode, Keys)], code: Keycode) -> Option<Keys> {
    map.iter().find_map(|map| (map.0 == code).then_some(map.1))
}

fn poll_inputs(
    event_pump: &mut EventPump,
    keymap: &[(Keycode, Keys)],
    key_state: &mut Keys,
) -> ControlFlow<()> {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => return ControlFlow::Break(()),

            Event::KeyDown {
                keycode: Some(code),
                ..
            } => {
                if let Some(key) = lookup_key(keymap, code) {
                    key_state.insert(key);
                }
            }

            Event::KeyUp {
                keycode: Some(code),
                ..
            } => {
                if let Some(key) = lookup_key(keymap, code) {
                    key_state.remove(key);
                }
            }

            _ => {}
        }
    }

    ControlFlow::Continue(())
}

fn microsleep(start: Instant, time: Duration) {
    {
        let remaining = time.saturating_sub(start.elapsed());
        // windows moment
        let thread_sleep_time = remaining.saturating_sub(Duration::from_millis(15));
        if thread_sleep_time > Duration::ZERO {
            // sleep most of the way there with the big imprecise sleep
            std::thread::sleep(thread_sleep_time);
        }
    }

    // then use libc nano sleep (and windows high precision timers) to cut down what we can.
    #[cfg(target_os = "linux")]
    loop {
        let remaining = time.saturating_sub(start.elapsed());
        if remaining < Duration::from_micros(1500) {
            break;
        }

        unsafe {
            let _ = libc::nanosleep(
                std::ptr::from_ref(&libc::timespec {
                    tv_sec: 0,
                    tv_nsec: i64::from(
                        remaining
                            .saturating_sub(Duration::from_micros(500))
                            .subsec_nanos()
                            .cast_signed(),
                    ),
                }),
                std::ptr::null_mut(),
            );
        }
    }

    {
        let mut ticks: usize = 7;

        loop {
            ticks += 1;
            if !ticks.is_multiple_of(8) {
                continue;
            }

            let remaining = time.saturating_sub(start.elapsed());

            if remaining < Duration::from_micros(50) {
                break;
            }

            std::thread::yield_now();
            std::hint::spin_loop();
        }
    }

    // fixme: tsc based sleep
    // need to actually figure out how to do it, rpcs3 _has_ an implementation, but I can't use it (or presumably study it) due to license mismatches.

    let mut ticks: usize = 0;
    while !ticks.is_multiple_of(8) || start.elapsed() < time {
        ticks += 1;
        std::hint::spin_loop();
    }
}

// pc = 16 bits
// a = 8 bits
// ir = 8 bits
// continue flag ()

// rax:63 = continue, eax:31:16 = pc, eax:8:0 = a, eax:15:8 = ir

struct SdlContext {
    sdl: Sdl,
    video_subsystem: VideoSubsystem,
    audio_subsystem: AudioSubsystem,
    canvas: Canvas<Window>,
    event_pump: EventPump,
    draw_buffer: Box<[u8; Self::BUFFER_SIZE]>,
}

impl SdlContext {
    const WIDTH: usize = 160;
    const HEIGHT: usize = 144;
    const WIDTH_32X: u32 = 160;
    const HEIGHT_32X: u32 = 144;
    const BUFFER_SIZE: usize = Self::WIDTH * Self::HEIGHT * 3;
}

#[inline(never)]
fn sdl_init(scale: u32) -> SdlContext {
    let sdl = sdl3::init().unwrap();
    let video_subsystem = sdl.video().unwrap();
    let window = video_subsystem
        .window(
            NAME,
            SdlContext::WIDTH_32X * scale,
            SdlContext::HEIGHT_32X * scale,
        )
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let audio_subsystem = sdl.audio().unwrap();

    let mut canvas = window.into_canvas();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    let event_pump = sdl.event_pump().unwrap();

    SdlContext {
        sdl,
        video_subsystem,
        audio_subsystem,
        canvas,
        event_pump,
        draw_buffer: Box::new([0; SdlContext::BUFFER_SIZE]),
    }
}

#[inline(never)]
fn audio_init(
    audio_subsystem: &AudioSubsystem,
    time_control_enabled: bool,
    audio_system: AudioSystem,
) -> Option<(AudioStreamOwner, FilteredSampler)> {
    if !time_control_enabled {
        return None;
    }

    // simplify 48000Hz / 4MiHz, we get 375/2^15, but we technically use a slightly slower CPU frequency (fixme: do actual math to compute this).
    let (expand, decimate) = (35763, 3125000);
    let filter = match audio_system {
        AudioSystem::Mute => return None,
        AudioSystem::NearestNeighbor => {
            AudioFilter::NearestNeighbor(NearestNeighborFilter::new(expand, decimate))
        }
        AudioSystem::Mean => AudioFilter::Mean(MeanFilter::new(expand, decimate)),
    };

    let sampler = FilteredSampler::new(filter);

    let device = AudioDevice::open_playback(
        audio_subsystem,
        None,
        &AudioSpec::new(
            Some(48000),
            Some(2),
            Some(sdl3::audio::AudioFormat::f32_sys()),
        ),
    )
    .unwrap();
    let stream = device.open_device_stream(None).unwrap();

    // arbitrarily put 0.03 seconds of audio to try to prevent pops?
    stream.put_data_f32(&[0.0; 480 * 2 * 3]).unwrap();

    Some((stream, sampler))
}

struct FilteredSampler {
    samples: Vec<[f32; 2]>,
    filter: AudioFilter,
}

impl FilteredSampler {
    const fn mute() -> Self {
        Self {
            samples: Vec::new(),
            filter: AudioFilter::Mute,
        }
    }

    fn new(filter: AudioFilter) -> Self {
        Self {
            // arbitrarily assume that we probably won't use more than 128 samples (~3ms)
            samples: Vec::with_capacity(128),
            filter,
        }
    }
}

impl ApuSampler for FilteredSampler {
    fn push_samples(&mut self, samples: [f32; 2]) {
        if let Some(sample) = self.filter.filter(samples) {
            self.samples.push(sample);
        }
    }
}

// Technically this should have a more precise name, but w/e.
enum AudioFilter {
    // Deny all samples (different from no filter, which would just do nothing).
    Mute,
    NearestNeighbor(NearestNeighborFilter),
    Mean(MeanFilter),
}

impl AudioFilter {
    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        match self {
            AudioFilter::Mute => None,
            AudioFilter::NearestNeighbor(it) => it.filter(sample),
            AudioFilter::Mean(it) => it.filter(sample),
        }
    }
}

// this sampler isn't the best but having any is good.
struct NearestNeighborFilter {
    phase: u32,
    expansion_factor: u32,
    decimation_factor: u32,
}

impl NearestNeighborFilter {
    fn new(expansion_factor: u32, decimation_factor: u32) -> Self {
        // sure, we could support net expansion, but, meh.
        assert!(expansion_factor <= decimation_factor);
        // set the phase such that the first sample will be taken.
        let initial_phase = decimation_factor.checked_sub(expansion_factor).expect("`NearestNeighborFilter` only supports decimation, not expansion (expansion <= decimation)");

        Self {
            phase: initial_phase,
            expansion_factor,
            decimation_factor,
        }
    }

    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        self.phase += self.expansion_factor;
        self.phase = self.phase.checked_sub(self.decimation_factor)?;

        Some(sample)
    }
}

// Theoretically an improvement of `NearestNeighbor` on account of
struct MeanFilter {
    sample: [f32; 2],
    phase: u32,
    expansion_factor: u32,
    decimation_factor: u32,
    mean_recip_divisor: f32,
}

impl MeanFilter {
    fn next_samples(expansion_factor: u32, decimation_factor: u32, phase: u32) -> u8 {
        (decimation_factor - phase).div_ceil(expansion_factor) as u8
    }

    fn new(expansion_factor: u32, decimation_factor: u32) -> Self {
        // sure, we could support net expansion, but, meh.
        assert!(expansion_factor <= decimation_factor);
        // set the phase such that the first sample will be taken.
        let _ = decimation_factor.checked_sub(expansion_factor).expect(
            "`MeanFilter` only supports decimation, not expansion (expansion <= decimation)",
        );

        Self {
            sample: [0.0; 2],
            phase: 0,
            expansion_factor,
            decimation_factor,
            mean_recip_divisor: f32::from(Self::next_samples(
                expansion_factor,
                decimation_factor,
                0,
            ))
            .recip(),
        }
    }

    fn filter(&mut self, sample: [f32; 2]) -> Option<[f32; 2]> {
        self.sample = {
            let [bl, br] = self.sample;
            let [sl, sr] = sample;
            [
                sl.mul_add_fast(self.mean_recip_divisor, bl),
                sr.mul_add_fast(self.mean_recip_divisor, br),
            ]
        };

        self.phase += self.expansion_factor;
        self.phase = self.phase.checked_sub(self.decimation_factor)?;

        self.mean_recip_divisor = f32::from(Self::next_samples(
            self.expansion_factor,
            self.decimation_factor,
            self.phase,
        ))
        .recip();

        Some(std::mem::take(&mut self.sample))
    }
}

struct Statistics {
    next_report: Instant,
    total_microsleep_time: Duration,
    total_emulated_time: Duration,
    subframe: u64,
    display_frame: u64,
}

impl Statistics {
    const PERIOD: Duration = Duration::from_secs(1);

    fn new(start: Instant) -> Self {
        Self {
            next_report: start + Self::PERIOD,
            total_microsleep_time: Duration::ZERO,
            total_emulated_time: Duration::ZERO,
            subframe: 0,
            display_frame: 0,
        }
    }
}

#[cold]
#[inline(never)]
fn report_statistics(
    stats: &mut Statistics,
    audio_bytes_ahead: Option<i32>,
    current_frame: Instant,
    start: Instant,
) {
    // if we've lapsed, just reset the clock
    if stats.next_report + Duration::from_secs(1) < current_frame {
        stats.next_report = current_frame + Duration::from_secs(1);
    } else {
        stats.next_report += Duration::from_secs(1);
    }

    let elapsed = start.elapsed();

    // should be very close to 1 unless `no-time-control` is set`
    log::debug!(
        target: "statistics",
        "micro_sleep_factor: {:.3}, emu time factor: {:.6}",
        stats.total_microsleep_time.as_secs_f64() / elapsed.as_secs_f64(),
        stats.total_emulated_time.as_secs_f64() / elapsed.as_secs_f64(),
    );
    log::debug!(
        target: "statistics",
        "UPS: {:.2}, FPS: {:.2}",
        (stats.subframe as f64) / elapsed.as_secs_f64(),
        (stats.display_frame as f64) / elapsed.as_secs_f64(),
    );

    if let Some(audio_bytes_ahead) = audio_bytes_ahead {
        let bytes = audio_bytes_ahead as u32;
        log::debug!(
            target: "statistics",
            "Audio buffer (bytes: {bytes}, samples: {samples}, duration: {duration:.03}s)",
            // 2 channels (f32) * 4 bytes per float.
            samples = bytes / 8,
            duration = f64::from(bytes / 8) * const { 48000.0f64.recip() },
        );
    }
}

#[inline(never)]
fn run(opt: &Opt) -> anyhow::Result<()> {
    const INPUT_POLL_PERIOD: Duration = Duration::from_micros(500);
    const DISPLAY_PERIOD: Duration = Duration::from_micros(500);
    const AUDIO_SINK_PERIOD: Duration = Duration::from_millis(3);

    let SdlContext {
        sdl: _sdl,
        video_subsystem: _video_subsystem,
        audio_subsystem,
        mut canvas,
        mut event_pump,
        mut draw_buffer,
    } = sdl_init(opt.scale);

    let texcr = canvas.texture_creator();
    let mut tex = texcr
        .create_texture_streaming(
            sdl3::pixels::PixelFormat::RGB24,
            SdlContext::WIDTH_32X,
            SdlContext::HEIGHT_32X,
        )
        .unwrap();

    tex.set_scale_mode(sdl3::render::ScaleMode::Nearest);

    let boot_rom = std::fs::read(&opt.boot_rom)
        .context("Failed to open the boot rom")?
        .into_boxed_slice();
    let game_rom = std::fs::read(&opt.game_rom)
        .context("Failed to open the game rom")?
        .into_boxed_slice();

    let keymap = [
        (Keycode::A, Keys::A),
        (Keycode::S, Keys::B),
        (Keycode::Space, Keys::SELECT),
        (Keycode::Return, Keys::START),
        (Keycode::Right, Keys::RIGHT),
        (Keycode::Left, Keys::LEFT),
        (Keycode::Up, Keys::UP),
        (Keycode::Down, Keys::DOWN),
    ];

    let (stream, sampler) =
        audio_init(&audio_subsystem, !opt.no_time_control, opt.audio_system).unzip();
    let sampler = sampler.unwrap_or(const { FilteredSampler::mute() });

    let mut gb = emu::GameBoy::new(boot_rom, game_rom, sampler)
        .ok_or_else(|| anyhow::anyhow!("Error loading cpu"))?;

    let start = Instant::now();
    let mut last_subframe = start;

    let mut next_display_frame = start;
    let mut next_poll_inputs = start + INPUT_POLL_PERIOD;
    let mut next_audio_sink = stream.is_some().then(|| start + AUDIO_SINK_PERIOD);

    let mut stats = Statistics::new(start);

    if let Some(stream) = &stream {
        stream.resume().unwrap();
    }

    'running: loop {
        stats.subframe += 1;

        let current_frame = std::time::Instant::now();

        let current_frame = {
            // fixme: technically we should also consider framerate report timing, but, eh.
            let time_until_display = next_display_frame.saturating_duration_since(current_frame);
            let micro_sleep_time = cmp::min(
                next_poll_inputs.saturating_duration_since(current_frame),
                next_audio_sink.map_or(time_until_display, |it| {
                    cmp::min(
                        it.saturating_duration_since(current_frame),
                        time_until_display,
                    )
                }),
            );

            // assume we can run at 4x speed.
            // in practice this should work with up to _28x_ speed according to my own CPU.
            let micro_sleep_time: Duration = micro_sleep_time.saturating_sub(micro_sleep_time / 4);

            match opt.no_time_control {
                true => {
                    gb.run_host_time(
                        current_frame,
                        micro_sleep_time,
                        &mut stats.total_emulated_time,
                    );
                    std::time::Instant::now()
                }
                false => {
                    if micro_sleep_time >= Duration::from_nanos(500) {
                        microsleep(current_frame, micro_sleep_time);
                        stats.total_microsleep_time += micro_sleep_time;
                        std::time::Instant::now()
                    } else {
                        current_frame
                    }
                }
            }
        };

        let delta_time: Duration = current_frame.duration_since(last_subframe);
        gb.run(delta_time);

        stats.total_emulated_time += delta_time;

        last_subframe = current_frame;

        if let Some(elapsed) = current_frame.checked_duration_since(next_poll_inputs) {
            next_poll_inputs = if elapsed > INPUT_POLL_PERIOD {
                if elapsed > Duration::from_millis(10) {
                    log::warn!("input unresponsive! (time since last poll: {elapsed:.3?})");
                }

                current_frame + INPUT_POLL_PERIOD
            } else {
                next_poll_inputs + INPUT_POLL_PERIOD
            };

            match poll_inputs(&mut event_pump, &keymap, gb.keys_mut()) {
                ControlFlow::Continue(()) => {}
                ControlFlow::Break(()) => break 'running Ok(()),
            }
        }

        if let Some(stream) = &stream
            && let Some(next) = next_audio_sink
            && let Some(elapsed) = current_frame.checked_duration_since(next)
        {
            next_audio_sink = Some(if elapsed > AUDIO_SINK_PERIOD {
                current_frame + AUDIO_SINK_PERIOD
            } else {
                next + AUDIO_SINK_PERIOD
            });

            let samples = gb.sampler_mut().samples.drain(..);
            let samples = samples.as_slice().as_flattened();
            stream.put_data_f32(samples).unwrap();
        }

        let Some(elapsed) = current_frame.checked_duration_since(next_display_frame) else {
            continue;
        };

        next_display_frame = if elapsed > DISPLAY_PERIOD {
            current_frame + DISPLAY_PERIOD
        } else {
            next_display_frame + DISPLAY_PERIOD
        };

        stats.display_frame += 1;

        let disp = gb.display();

        for (px, elems) in disp.into_iter().zip(draw_buffer.as_chunks_mut::<3>().0) {
            let px = opt.palette.0[px as usize];

            *elems = px.into();
        }

        tex.update(None, &*draw_buffer, SdlContext::WIDTH * 3)
            .unwrap();
        canvas.copy(&tex, None, None).unwrap();
        canvas.present();

        if log::log_enabled!(target: "statistics", log::Level::Debug)
            && current_frame >= stats.next_report
        {
            report_statistics(
                &mut stats,
                stream.as_ref().map(|it| it.available_bytes().unwrap()),
                current_frame,
                start,
            );
        }
    }
}

pub fn main() {
    env_logger::init();

    if let Err(e) = run(&Opt::parse()) {
        log::error!("fatal error: {:#?}", e);
        std::process::exit(1)
    }
}
