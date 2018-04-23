// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

#![feature(nll)]
#![cfg_attr(feature = "cargo-clippy", allow(verbose_bit_mask))]

extern crate rgb;
extern crate sdl2;

#[macro_use]
extern crate structopt;

#[macro_use]
extern crate bitflags;

mod emu;

use sdl2::{event::Event, keyboard::Keycode, pixels::Color};

use emu::cpu;

use std::{fs::File, io::{self, prelude::*}};

use rgb::RGB8;
use structopt::StructOpt;

fn load_file(path: &str) -> io::Result<Vec<u8>> {
    let mut buf = Vec::new();
    File::open(path)?.read_to_end(&mut buf)?;
    Ok(buf)
}

fn load_rom(path: &str, name: &str) -> Result<Vec<u8>, String> {
    match load_file(path) {
        Ok(v) => Ok(v),
        Err(e) => Err(format!(
            "Failed to open the \"{}\" ({}) because: {:?}",
            name,
            path,
            e.kind()
        )),
    }
}

const NAME: &str = env!("CARGO_PKG_NAME");

#[derive(StructOpt, Debug)]
#[structopt(about = "Emulates GameBoy games.")]
struct Opt {
    #[structopt(short = "s", long = "scale", help = "Screen scale size", default_value = "1")]
    scale: u32,

    #[structopt(help = "The path to the boot rom")]
    boot_rom: String,

    #[structopt(help = "The path to the game rom")]
    game_rom: String,
}

fn run(opt: &Opt) -> Result<(), String> {
    let sdl_context = sdl2::init().unwrap();
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

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    const BUFFER_SIZE: usize = WIDTH * HEIGHT * 3;
    let mut array: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];

    let texcr = canvas.texture_creator();
    let mut tex = texcr
        .create_texture_streaming(sdl2::pixels::PixelFormatEnum::RGB24, WIDTH_32X, HEIGHT_32X)
        .unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let boot_rom = load_rom(&opt.boot_rom, "boot_rom")?;
    let game_rom = load_rom(&opt.game_rom, "game_rom")?;

    let mut gb = match cpu::Cpu::new(boot_rom, game_rom) {
        Some(c) => c,
        None => return Err("Error loading cpu".to_string()),
    };

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running Ok(()),
                _ => {}
            }
        }

        gb.run(0x4_0000);
        let disp = gb.ppu.get_display();
        for i in 0..WIDTH * HEIGHT {
            use emu::ppu::DisplayPixel;
            let px = match disp[i] {
                DisplayPixel::White => RGB8 {
                    r: 0x9B,
                    g: 0xBC,
                    b: 0x0F,
                },
                DisplayPixel::LightGrey => RGB8 {
                    r: 0x8B,
                    g: 0xAC,
                    b: 0x0F,
                },
                DisplayPixel::DarkGrey => RGB8 {
                    r: 0x30,
                    g: 0x62,
                    b: 0x30,
                },
                DisplayPixel::Black => RGB8 {
                    r: 0x0F,
                    g: 0x38,
                    b: 0x0F,
                },
            };

            array[i * 3] = px.r;
            array[i * 3 + 1] = px.g;
            array[i * 3 + 2] = px.b;
        }

        tex.update(None, &array, WIDTH * 3).unwrap();
        canvas.copy(&tex, None, None).unwrap();
        canvas.present();
    }
}

pub fn main() {
    if let Err(e) = run(&Opt::from_args()) {
        eprintln!("{}", e);
        std::process::exit(1)
    }
}
