#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate yarsge_core;

use yarsge_core::emu::cpu;

fuzz_target!(|data: &[u8]| {
        if data.len() < 0x100 + 0x150 {
            return;
        }

        let mut boot_rom = Vec::new();
        let mut game_rom = Vec::new();
        if data.len() >= 0x100 {
            boot_rom = (data[0..0x100].iter().cloned().collect());
            if data.len() > 0x100 {
                game_rom = data[0x100..(data.len()-0x100)].iter().cloned().collect();
            }
        }

        if let Some(mut cpu) = cpu::Cpu::new(boot_rom, game_rom) {
            cpu.run(0x10_000);
        }
});
