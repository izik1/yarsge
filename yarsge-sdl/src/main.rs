use core::fmt;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::{Duration, Instant};

use anyhow::Context;

use sdl3::EventPump;
use yarsge_core::{Keys, emu};

use sdl3::{event::Event, keyboard::Keycode, pixels::Color};

use clap::Parser;
use rgb::RGB8;

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
        default_value = "1"
    )]
    scale: u32,

    #[clap(help = "Path to the boot rom")]
    boot_rom: PathBuf,

    #[clap(help = "Path to the game rom")]
    game_rom: PathBuf,

    #[clap(short = 'p', long = "palette", default_value_t = Palette::DEFAULT)]
    palette: Palette,
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

fn run(opt: &Opt) -> anyhow::Result<()> {
    let sdl_context = sdl3::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    const WIDTH: usize = 160;
    const HEIGHT: usize = 144;
    const WIDTH_32X: u32 = 160;
    const HEIGHT_32X: u32 = 144;

    let window = video_subsystem
        .window(NAME, WIDTH_32X * opt.scale, HEIGHT_32X * opt.scale)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    const BUFFER_SIZE: usize = WIDTH * HEIGHT * 3;
    let mut array: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];

    let texcr = canvas.texture_creator();
    let mut tex = texcr
        .create_texture_streaming(sdl3::pixels::PixelFormat::RGB24, WIDTH_32X, HEIGHT_32X)
        .unwrap();

    tex.set_scale_mode(sdl3::render::ScaleMode::Nearest);

    let mut event_pump = sdl_context.event_pump().unwrap();

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

    let mut gb = emu::GameBoy::new(boot_rom, game_rom)
        .ok_or_else(|| anyhow::anyhow!("Error loading cpu"))?;

    let start = Instant::now();
    let mut last_subframe = start;
    let mut last_display_frame = start;
    let mut last_time_report = start;
    let mut last_poll_inputs = start;

    let mut total_subframes = 0_u64;
    let mut total_display_frames = 0_u64;

    // gb.register_breakpoint(0x0c);

    'running: loop {
        let subframe = total_subframes;
        total_subframes += 1;

        let current_frame = std::time::Instant::now();
        let delta_time = current_frame.duration_since(last_subframe);
        last_subframe = current_frame;

        if current_frame.duration_since(last_poll_inputs) >= Duration::from_micros(500) {
            last_poll_inputs = current_frame;
            match poll_inputs(&mut event_pump, &keymap, gb.keys_mut()) {
                ControlFlow::Continue(()) => {}
                ControlFlow::Break(()) => break 'running Ok(()),
            }
        }

        gb.run(delta_time);

        if current_frame.duration_since(last_display_frame) < Duration::from_micros(200) {
            let mut ticks: usize = 0;
            while !ticks.is_multiple_of(8) || current_frame.elapsed() < Duration::from_micros(5) {
                ticks += 1;
                std::hint::spin_loop();
            }

            continue;
        }

        let display_frame = total_display_frames;
        total_display_frames += 1;

        if current_frame.duration_since(last_time_report) >= Duration::from_secs(1) {
            last_time_report = current_frame;
            log::debug!(
                target: "framerate",
                "UPS: {:.2}, FPS: {:.2}",
                (subframe as f64) / start.elapsed().as_secs_f64(),
                (display_frame as f64) / start.elapsed().as_secs_f64(),
            );
        }

        last_display_frame = current_frame;

        let disp = gb.display();

        for (px, elems) in disp.into_iter().zip(array.as_chunks_mut::<3>().0) {
            let px = opt.palette.0[px as usize];

            *elems = px.into();
        }

        tex.update(None, &array, WIDTH * 3).unwrap();
        canvas.copy(&tex, None, None).unwrap();
        canvas.present();
    }
}

pub fn main() {
    env_logger::init();

    if let Err(e) = run(&Opt::parse()) {
        log::error!("fatal error: {:#?}", e);
        std::process::exit(1)
    }
}
