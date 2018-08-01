// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

#![recursion_limit="2048"]

#[macro_use]
extern crate stdweb;

extern crate yarsge_core;

use std::cell::RefCell;
use std::ops::DerefMut;
use std::rc::Rc;

use stdweb::traits::*;
use stdweb::unstable::TryInto;
use stdweb::web::{self, document, event::{ChangeEvent, ProgressLoadEvent}, Element, HtmlElement, WebSocket, FileList, FileReader, FileReaderResult};

use stdweb::{Once, UnsafeTypedArray, Value};

use stdweb::web::html_element::InputElement;

use yarsge_core::emu::{self, cpu::Cpu};

macro_rules! enclose {
    ( ($( $x:ident ),*) $y:expr ) => {
        {
            $(let $x = $x.clone();)*
            $y
        }
    };
}

struct Emulator {
    core: Cpu,
    ctx: Value,
    framebuffer: [u32; 160 * 144],
}

impl Emulator {
    fn new(canvas: &Element) -> Self {
        let core = Cpu::new(vec![0x00u8; 0x100], vec![0x00u8; 0x150]).unwrap();

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
            framebuffer: [0xFFFFFFFF; 160 * 144],
        }
    }

    fn run_frame(&mut self) {
        // todo: this has timing offset issues.
        self.core.run(4194304 / 60);
    }

    fn draw(&mut self) {
        let disp = self.core.ppu.get_display();
        for i in 0..160 * 144 {
            use emu::ppu::DisplayPixel;
            // framebuffer is abgr... or big endian.
            self.framebuffer[i] = match disp[i] {
                DisplayPixel::White => 0xFF0FBC9B,
                DisplayPixel::LightGrey => 0xFFC0BA8B,
                DisplayPixel::DarkGrey => 0xFF306230,
                DisplayPixel::Black => 0xFF0F380F,
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
            Some( file ) => file,
            None => return
        };

        let reader = FileReader::new();
        reader.add_event_listener(enclose!( (emulator, reader) move |_: ProgressLoadEvent| {
            let data: Vec< u8 > = match reader.result().unwrap() {
                FileReaderResult::ArrayBuffer( buffer ) => buffer,
                _ => unreachable!()
            }.into();

            emulator.borrow_mut().core = Cpu::new(data, vec![0x00u8; 0x150]).unwrap();
        }));

        reader.read_as_array_buffer( &file ).unwrap();
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
