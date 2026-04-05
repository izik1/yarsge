use core::fmt;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::{Duration, Instant};

use anyhow::Context;
use clap::Parser;
use rgb::RGB8;
use sdl3::render::Canvas;
use sdl3::video::Window;
use sdl3::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};
use sdl3::{keyboard::Keycode, pixels::Color};
use yarsge_core::{Keys, emu};
use yarsge_sdl::audio::{AudioSystem, FilteredSampler, audio_init};
use yarsge_sdl::input::poll_inputs;
use yarsge_sdl::{Interval, make_period_tys, report_statistics};

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

#[derive(Parser, Debug)]
#[clap(about = "Emulates GameBoy games.", author)]
struct Opt {
    #[clap(
        short = 's',
        long = "scale",
        help = "Screen scale size factor",
        default_value_t = 1
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

make_period_tys! {
    struct InputPoll(Duration::from_micros(500));
    struct Display(Duration::from_micros(500));
    struct AudioSink(Duration::from_millis(3));
}

struct Intervals {
    input_poll: Interval<InputPoll>,
    display: Interval<Display>,
    audio_sink: Option<Interval<AudioSink>>,
}

impl Intervals {
    fn new(now: Instant, poll_audio: bool) -> Self {
        Self {
            input_poll: Interval::at(now + InputPoll::PERIOD),
            display: Interval::at(now),
            audio_sink: poll_audio.then(|| Interval::at(now + AudioSink::PERIOD)),
        }
    }

    fn next(&self) -> Instant {
        [self.input_poll.next, self.display.next]
            .into_iter()
            .chain(self.audio_sink.as_ref().map(|it| it.next))
            .min()
            .expect("`Intervals` should have at least one interval")
    }
}

#[inline(never)]
fn run(opt: &Opt) -> anyhow::Result<()> {
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

    let mut intervals = Intervals::new(start, stream.is_some());

    let mut stats = yarsge_sdl::Statistics::new(start);

    if let Some(stream) = &stream {
        stream.resume().unwrap();
    }

    'running: loop {
        stats.subframe += 1;

        let current_frame = std::time::Instant::now();

        let current_frame = {
            let micro_sleep_time = intervals.next().saturating_duration_since(current_frame);

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
        gb.run(delta_time, &mut stats.total_emulated_time);

        last_subframe = current_frame;

        if let Some(elapsed) = intervals.input_poll.tick(current_frame) {
            if elapsed > Duration::from_millis(10) {
                log::warn!("input unresponsive! (time since last poll: {elapsed:.3?})");
            }

            match poll_inputs(&mut event_pump, &keymap, gb.keys_mut()) {
                ControlFlow::Continue(()) => {}
                ControlFlow::Break(()) => {
                    break 'running Ok(());
                }
            }
        }

        if let Some(stream) = &stream
            && let Some(audio_sink) = intervals.audio_sink.as_mut()
            && let Some(_) = audio_sink.tick(current_frame)
        {
            gb.sampler_mut().push_to_stream(stream)
        }

        let Some(_) = intervals.display.tick(current_frame) else {
            continue;
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
