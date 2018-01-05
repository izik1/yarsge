// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

#![feature(nll)]

extern crate clap;
extern crate rgb;
extern crate sdl2;
mod emu;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use emu::cpu;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use clap::{App, Arg};
use rgb::RGB8;

fn load_rom(path: String) -> io::Result<Vec<u8>> {
    let mut buf = Vec::new();
    File::open(path)?.read_to_end(&mut buf)?;
    Result::Ok(buf)
}

fn unwrap_rom(vec: io::Result<Vec<u8>>) -> Vec<u8> {
    if let Result::Ok(rom) = vec {
        rom
    } else {
        println!("Error loading rom");
        std::process::exit(1)
    }
}

pub fn main() {
    const VERSION: &str = env!("CARGO_PKG_VERSION");
    const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
    const NAME: &str = env!("CARGO_PKG_NAME");
    let matches = App::new(NAME)
        .version(VERSION)
        .author(AUTHORS)
        .about("Emulates GameBoy games")
        .arg(
            Arg::with_name("boot_rom")
                .help("The path to the boot rom")
                .required(true),
        )
        .arg(
            Arg::with_name("game_rom")
                .help("The path to the game rom")
                .required(true),
        )
        .arg(
            Arg::with_name("scale")
                .help("Screen scale size")
                .takes_value(true)
                .short("s")
                .long("scale"),
        )
        .get_matches();

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let scale = match matches.value_of("scale") {
        Some(num) => if let Ok(n) = num.to_string().parse() {
            n
        } else {
            eprintln!("Couldn't convert scale to number ({})", num);
            std::process::exit(1)
        },
        None => 1,
    };
    const WIDTH: u32 = 160;
    const HEIGHT: u32 = 144;
    let window = video_subsystem
        .window(NAME, WIDTH * scale, HEIGHT * scale)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    const BUFFER_SIZE: usize = 160 * 144 * 3;
    let mut array: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];

    let texcr = canvas.texture_creator();
    let mut tex = texcr
        .create_texture_streaming(sdl2::pixels::PixelFormatEnum::RGB24, 160, 144)
        .unwrap();
    tex.update(None, &array, 160 * 3).unwrap();
    canvas.copy(&tex, None, None).unwrap();
    canvas.present();
    let mut event_pump = sdl_context.event_pump().unwrap();

    let boot_rom = unwrap_rom(load_rom(matches.value_of("boot_rom").unwrap().to_string()));
    let game_rom = unwrap_rom(load_rom(matches.value_of("game_rom").unwrap().to_string()));
    let mut gb = if let Some(c) = cpu::Cpu::new(boot_rom, game_rom) {
        c
    } else {
        println!("Error loading cpu");
        std::process::exit(1)
    };

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        gb.run(0x40000);
        let disp = gb.ppu.get_display();
        for i in 0..160 * 144 {
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

            array[i * 3 + 0] = px.r;
            array[i * 3 + 1] = px.g;
            array[i * 3 + 2] = px.b;
        }

        tex.update(None, &array, 160 * 3).unwrap();
        canvas.copy(&tex, None, None).unwrap();
        canvas.present();
    }
}
