use anyhow::Context;

use yarsge_core::emu;

use sdl2::{event::Event, keyboard::Keycode, pixels::Color};

use clap::Parser;
use rgb::RGB8;
use yarsge_core::emu::TCycle;

const NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Parser, Debug)]
#[clap(about = "Emulates GameBoy games.", author)]
struct Opt {
    #[clap(
        short = 's',
        long = "scale",
        about = "Screen scale size",
        default_value = "1"
    )]
    scale: u32,

    #[clap(about = "Path to the boot rom")]
    boot_rom: String,

    #[clap(about = "Path to the game rom")]
    game_rom: String,
}

fn index_of_key(keys: &[Keycode], code: Keycode) -> Option<usize> {
    keys.iter().position(|key| *key == code)
}

fn run(opt: &Opt) -> anyhow::Result<()> {
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

    let boot_rom = std::fs::read(&opt.boot_rom).context("Failed to open the boot rom")?;
    let game_rom = std::fs::read(&opt.game_rom).context("Failed to open the game rom")?;

    let keymap = [
        Keycode::A,
        Keycode::S,
        Keycode::Space,
        Keycode::Return,
        Keycode::Right,
        Keycode::Left,
        Keycode::Up,
        Keycode::Down,
    ];

    let mut gb = emu::GameBoy::new(boot_rom, game_rom)
        .ok_or_else(|| anyhow::anyhow!("Error loading cpu"))?;

    let mut key_state = 0xFF;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running Ok(()),

                Event::KeyDown {
                    keycode: Some(code),
                    ..
                } => {
                    if let Some(pos) = index_of_key(&keymap, code) {
                        key_state &= !(1 << pos) as u8;
                    }
                }

                Event::KeyUp {
                    keycode: Some(code),
                    ..
                } => {
                    if let Some(pos) = index_of_key(&keymap, code) {
                        key_state |= (1 << pos) as u8;
                    }
                }

                _ => {}
            }
        }

        gb.run(TCycle(0x4_0000), key_state);

        let disp = gb.get_display();
        for i in 0..WIDTH * HEIGHT {
            use yarsge_core::emu::ppu::DisplayPixel;
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
    env_logger::init();

    if let Err(e) = run(&Opt::parse()) {
        log::error!("fatal error: {:#?}", e);
        std::process::exit(1)
    }
}
