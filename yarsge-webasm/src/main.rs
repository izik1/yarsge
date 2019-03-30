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
use yarsge_core::emu::TCycle;

macro_rules! enclose {
    ( ($( $x:ident ),*) $y:expr ) => {
        {
            $(let $x = $x.clone();)*
            $y
        }
    };
}

struct Emulator {
    core: emu::GameBoy,
    ctx: Value,
    framebuffer: [u32; 160 * 144],
}

impl Emulator {
    fn new(canvas: &Element) -> Self {
        let core = emu::GameBoy::new(vec![0x00u8; 0x100], vec![0x00u8; 0x150]).unwrap();

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
            core,
            ctx,
            framebuffer: [0xFFFF_FFFF; 160 * 144],
        }
    }

    fn run_frame(&mut self) {
        // todo: this has timing offset issues.
        self.core.run(TCycle(4_194_304 / 60));
    }

    fn draw(&mut self) {
        use yarsge_core::emu::ppu::DisplayPixel;

        let disp = self.core.get_display();
        for (buffer_pixel, display_pixel) in self.framebuffer.iter_mut().zip(disp.iter()) {
            // framebuffer is abgr... or big endian.
            *buffer_pixel = match display_pixel {
                DisplayPixel::White => 0xFF0F_BC9B,
                DisplayPixel::LightGrey => 0xFFC_0BA8B,
                DisplayPixel::DarkGrey => 0xFF30_6230,
                DisplayPixel::Black => 0xFF0F_380F,
            }
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
        reader.add_event_listener(enclose!( (emulator, reader) move |_: ProgressLoadEvent| {
            let data: Vec< u8 > = match reader.result().unwrap() {
                FileReaderResult::ArrayBuffer( buffer ) => buffer,
                _ => unreachable!()
            }.into();

            emulator.borrow_mut().core = emu::GameBoy::new(data, vec![0x00u8; 0x150]).unwrap();
        }));

        reader.read_as_array_buffer(&file).unwrap();
    });
}

fn main_loop(emulator: Rc<RefCell<Emulator>>) {
    emulator.borrow_mut().run_frame();
    emulator.borrow_mut().draw();
    web::window().request_animation_frame(move |_| {
        main_loop(emulator);
    });
}

fn main() {
    stdweb::initialize();
    let canvas = web::document().get_element_by_id("viewport").unwrap();
    let emulator = Rc::new(RefCell::new(Emulator::new(&canvas)));

    load_boot_rom(emulator.clone());

    web::window().request_animation_frame(move |_| {
        main_loop(emulator);
    });

    stdweb::event_loop();
}
