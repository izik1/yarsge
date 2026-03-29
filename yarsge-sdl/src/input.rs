use std::ops::ControlFlow;

use sdl3::EventPump;
use sdl3::event::Event;
use sdl3::keyboard::Keycode;
use yarsge_core::Keys;

fn lookup_key(map: &[(Keycode, Keys)], code: Keycode) -> Option<Keys> {
    map.iter().find_map(|map| (map.0 == code).then_some(map.1))
}

pub fn poll_inputs(
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
