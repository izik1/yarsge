use std::path::PathBuf;
use std::time::{Duration, Instant};

use anyhow::Context;
use clap::Parser;
use yarsge_core::emu;
use yarsge_core::emu::apu::ApuSampler;
use yarsge_core::emu::registers::Reg;

#[derive(Parser, Debug)]
#[clap(about = "Emulates GameBoy test roms", author)]
struct Opt {
    #[clap(help = "Path to the boot rom")]
    boot_rom: PathBuf,

    #[clap(help = "Path to the game rom")]
    game_rom: PathBuf,
}

// pc = 16 bits
// a = 8 bits
// ir = 8 bits
// continue flag ()

// rax:63 = continue, eax:31:16 = pc, eax:8:0 = a, eax:15:8 = ir

struct NullSampler;

impl ApuSampler for NullSampler {
    #[inline(always)]
    fn push_samples(&mut self, _samples: [f32; 2]) {}
}

#[inline(never)]
fn run(opt: &Opt) -> anyhow::Result<()> {
    let start = Instant::now();
    let boot_rom = std::fs::read(&opt.boot_rom)
        .context("Failed to open the boot rom")?
        .into_boxed_slice();
    let game_rom = std::fs::read(&opt.game_rom)
        .context("Failed to open the game rom")?
        .into_boxed_slice();

    log::info!("roms loaded at {:?}", start.elapsed());

    let mut gb = emu::GameBoy::new_break_on_ld_b_b(boot_rom, game_rom, NullSampler)
        .ok_or_else(|| anyhow::anyhow!("Error loading cpu"))?;

    log::info!("system init at {:?}", start.elapsed());

    let mut total_emulated_time = Duration::ZERO;
    let hit_breakpoint = gb.run_host_time(
        Instant::now(),
        Duration::from_secs(5),
        &mut total_emulated_time,
    );

    log::info!("emulated for {:?} in-system", total_emulated_time);

    if !hit_breakpoint {
        let regs = gb.read_registers();

        let [b, c, d, e, h, l] = [
            regs.reg(Reg::B),
            regs.reg(Reg::C),
            regs.reg(Reg::D),
            regs.reg(Reg::E),
            regs.reg(Reg::H),
            regs.reg(Reg::L),
        ];

        eprintln!("{b}/{c}/{d}/{e}/{h}/{l}");
        anyhow::bail!("emulator timed out");
    }

    log::info!("break at {:?}", start.elapsed());

    let regs = gb.read_registers();

    let [b, c, d, e, h, l] = [
        regs.reg(Reg::B),
        regs.reg(Reg::C),
        regs.reg(Reg::D),
        regs.reg(Reg::E),
        regs.reg(Reg::H),
        regs.reg(Reg::L),
    ];

    println!("{b}/{c}/{d}/{e}/{h}/{l}");

    Ok(())
}

pub fn main() {
    env_logger::init();

    if let Err(e) = run(&Opt::parse()) {
        log::error!("fatal error: {:#?}", e);
        std::process::exit(1)
    }
}
