// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

#![recursion_limit = "2048"]

#[macro_use]
extern crate stdweb;

extern crate yarsge_core;

use std::cell::RefCell;
use std::rc::Rc;

use stdweb::{
    traits::*,
    unstable::TryInto,
    web::{
        self,
        event::{ChangeEvent, ProgressLoadEvent},
        html_element::InputElement,
        Element, FileList, FileReader, FileReaderResult,
    },
    UnsafeTypedArray, Value,
};

use yarsge_core::emu;
use yarsge_core::emu::{TCycle, GameBoy};
use stdweb::web::event::{ClickEvent, KeyDownEvent, KeyUpEvent};

struct Emulator {
    core: Option<emu::GameBoy>,
    ctx: Value,
    framebuffer: [u32; 160 * 144],
    boot_rom: Option<Vec<u8>>,
    rom: Option<Vec<u8>>,
    keys: u8,
}

impl Emulator {
    fn new(canvas: &Element) -> Self {
        let ctx = js! {
            var c = {};
            var canvas = @{canvas};
            var new_canvas = canvas.cloneNode( true );
            canvas.parentNode.replaceChild( new_canvas, canvas );
            canvas = new_canvas;

            c.ctx = canvas.getContext("2d");
            c.img = c.ctx.createImageData(160, 144);
            c.buffer = new Uint32Array(c.img.data.buffer);
            return c;
        };

        Emulator {
            core: None,
            ctx,
            framebuffer: [0xFFFF_FFFF; 160 * 144],
            boot_rom: None,
            rom: None,
            keys: 0xFF,
        }
    }

    fn run_frame(&mut self) {
        // todo: this has timing offset issues.

        if let Some(core) = &mut self.core {
            core.run(TCycle(4_194_304 / 60), self.keys)
        }
    }

    fn draw(&mut self) {
        use yarsge_core::emu::ppu::DisplayPixel;

        if let Some(core) = &self.core {
            let disp = core.get_display();
            for (buffer_pixel, display_pixel) in self.framebuffer.iter_mut().zip(disp.iter()) {
                // framebuffer is abgr... or big endian.
                *buffer_pixel = match display_pixel {
                    DisplayPixel::White => 0xFF0F_BC9B,
                    DisplayPixel::LightGrey => 0xFF0FAC8B,
                    DisplayPixel::DarkGrey => 0xFF30_6230,
                    DisplayPixel::Black => 0xFF0F_380F,
                }
            }
        } else {
            self.framebuffer.iter_mut().for_each(|x| *x = 0xFF0F_BC9B);
        }

        js! {
             var c = @{&self.ctx};
             var framebuffer = @{unsafe {UnsafeTypedArray::new(&self.framebuffer)}};
             c.buffer.set(framebuffer);
             c.ctx.putImageData(c.img, 0, 0);
        }
    }
}

fn load_boot_rom(emulator: Rc<RefCell<Emulator>>) {
    let load_boot_rom_button = web::document().get_element_by_id("load-boot_rom").unwrap();
    load_boot_rom_button.add_event_listener(move |event: ChangeEvent| {
        let input: InputElement = event.target().unwrap().try_into().unwrap();
        let files: FileList = js!( return @{input}.files ).try_into().unwrap();
        let file = match files.iter().next() {
            Some(file) => file,
            None => return,
        };

        let reader = FileReader::new();

        reader.add_event_listener({
            let reader = reader.clone();
            let emulator = emulator.clone();
            move |_event: ProgressLoadEvent| {
                emulator.borrow_mut().boot_rom = reader.clone().result().map(|res| {
                    match res {
                        FileReaderResult::ArrayBuffer(buffer) => buffer,
                        _ => unreachable!(),
                    }
                    .into()
                });
            }
        });

        reader.read_as_array_buffer(&file).unwrap();
    });
}

fn load_rom(emulator: Rc<RefCell<Emulator>>) {
    let load_rom_button = web::document().get_element_by_id("load-rom").unwrap();
    load_rom_button.add_event_listener(move |event: ChangeEvent| {
        let input: InputElement = event.target().unwrap().try_into().unwrap();
        let files: FileList = js!( return @{input}.files ).try_into().unwrap();
        let file = match files.iter().next() {
            Some(file) => file,
            None => return,
        };

        let reader = FileReader::new();

        reader.add_event_listener({
            let reader = reader.clone();
            let emulator = emulator.clone();
            move |_event: ProgressLoadEvent| {
                emulator.borrow_mut().rom = reader.clone().result().map(|res| {
                    match res {
                        FileReaderResult::ArrayBuffer(buffer) => buffer,
                        _ => unreachable!(),
                    }
                        .into()
                });
            }
        });

        reader.read_as_array_buffer(&file).unwrap();
    });
}

fn reload(emulator: Rc<RefCell<Emulator>>) {
    let reload_button = web::document().get_element_by_id("reload").unwrap();
    reload_button.add_event_listener(move |_event: ClickEvent | {
        let mut emulator = emulator.borrow_mut();

        if let (Some(boot_rom), Some(rom)) = (emulator.boot_rom.clone(), emulator.rom.clone()) {
            emulator.core = Some(GameBoy::new(boot_rom, rom).unwrap())
        }
    });
}

fn main_loop(emulator: Rc<RefCell<Emulator>>) {
    emulator.borrow_mut().run_frame();
    emulator.borrow_mut().draw();
    web::window().request_animation_frame(move |_| {
        main_loop(emulator);
    });
}

fn input(emulator: Rc<RefCell<Emulator>>) {
    input_keydown(emulator.clone());
    input_keyup(emulator);
}

fn input_key(key: &str) -> u8 {
    match key {
        "a" => 0b0000_0001,
        "s" => 0b0000_0010,
        " " => 0b0000_0100,
        "Enter" => 0b0000_1000,
        "ArrowRight" => 0b0001_0000,
        "ArrowLeft" => 0b0010_0000,
        "ArrowUp" => 0b0100_0000,
        "ArrowDown" => 0b1000_0000,
        _ => 0x00,
    }
}

fn input_keydown(emulator: Rc<RefCell<Emulator>>) {
    web::document().add_event_listener(move |event: KeyDownEvent| {
        emulator.borrow_mut().keys &= !input_key(&event.key());
        js! {
            console.log(@{&event.key()});
        };

    });
}

fn input_keyup(emulator: Rc<RefCell<Emulator>>) {
    web::document().add_event_listener(move |event: KeyUpEvent| {
        emulator.borrow_mut().keys |= input_key(&event.key());
    });
}

fn main() {
    stdweb::initialize();
    let canvas = web::document().get_element_by_id("viewport").unwrap();
    let emulator = Rc::new(RefCell::new(Emulator::new(&canvas)));

    load_boot_rom(emulator.clone());
    load_rom(emulator.clone());
    reload(emulator.clone());
    input(emulator.clone());


    web::window().request_animation_frame(move |_| {
        main_loop(emulator);
    });

    stdweb::event_loop();
}
