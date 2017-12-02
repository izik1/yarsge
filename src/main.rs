// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

extern crate sdl2;
extern crate clap;
mod emu;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use emu::cpu;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use clap::{App, Arg};
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
    let matches = App::new("yarsge")
        .version("0.1")
        .author("Zachery Gyurkovitz <zgyurkovitz@gmail.com>")
        .about("Emulate GameBoy games")
        .arg(Arg::with_name("boot_rom")
            .help("The path to the boot rom")
            .required(true))
        .arg(Arg::with_name("game_rom")
            .help("The path to the game rom")
            .required(true))
        .get_matches();

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    const WIDTH: u32 = 160;
    const HEIGHT: u32 = 144;
    let window = video_subsystem
        .window("yarsge", WIDTH, HEIGHT)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    const BUFFER_SIZE: usize = 160 * 144 * 3;
    let mut val: u8 = 0;
    let mut array: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];

    let texcr = canvas.texture_creator();
    let mut tex = texcr
        .create_texture_streaming(sdl2::pixels::PixelFormatEnum::BGR24, 160, 144)
        .unwrap();
    tex.update(None, &array, 144).unwrap();
    canvas.copy(&tex, None, None).unwrap();
    canvas.present();
    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut dir = true;

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
                Event::Quit { .. } |
                Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        gb.run(0x4000);


        for n in 0..BUFFER_SIZE {

            dir = match val {
                0x00...0x20 => true,
                0x80...0xFF => false,
                _ => dir,
            };

            val = if dir { val + 10 } else { val - 3 };
            array[n] = val;
        }

        tex.update(None, &array, 144).unwrap();
        canvas.copy(&tex, None, None).unwrap();
        canvas.present();

    }
}
