// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use crate::emu::cpu::Cpu;
use crate::emu::ppu::DisplayPixel;

pub mod cpu;
pub mod dma;
pub mod ppu;
pub mod registers;
pub mod timer;

pub mod bits {
    pub fn get(num: u8) -> u8 {
        1 << num
    }

    pub fn has_bit(num: u8, bit: u8) -> bool {
        (num & get(bit)) == get(bit)
    }

    pub fn nget(num: u8) -> u8 {
        !(1 << num)
    }
}

pub mod flags {
    bitflags! {
        pub struct Flag: u8 {
            const Z = 0b1000_0000;
            const N = 0b0100_0000;
            const H = 0b0010_0000;
            const C = 0b0001_0000;
        }
    }
}

pub struct Hardware {
    ppu: ppu::Ppu,
    timer: timer::Timer,
    dma: dma::Dma,
    memory: memory::Memory,
    r_if: u8,
    r_ier: u8,
    cycle_counter: i64,
    mid_check: bool,
}

impl Hardware {
    fn new(memory: memory::Memory) -> Self {
        Self {
            cycle_counter: 0,
            ppu: Default::default(),
            memory,
            timer: Default::default(),
            r_if: 0,
            r_ier: 0,
            mid_check: false,
            dma: Default::default(),
        }
    }

    fn read_cycle(&mut self, addr: u16) -> u8 {
        self.update(4);
        self.read_byte(addr)
    }

    fn stall(&mut self, m_cycles: i64) {
        self.update(m_cycles * 4);
    }

    fn update(&mut self, mut cycles: i64) {
        if self.mid_check {
            cycles += 2;
            self.mid_check = false;
        }

        for _ in 0..cycles {
            self.r_if |= (self.timer.update() | self.ppu.update()) & 0x1F;
            if let Some((oam_offset, addr)) = self.dma.update() {
                self.ppu.oam[oam_offset] = self.read_byte(addr)
            }
        }

        self.cycle_counter -= cycles;
    }

    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => self.memory.read_rom_low(addr),
            0x4000..=0x7FFF => self.memory.read_rom_high(addr - 0x4000),
            0x8000..=0x9FFF => self.ppu.get_vram(addr - 0x8000),
            0xC000..=0xDFFF => self.memory.wram[(addr - 0xC000) as usize],
            0xE000..=0xFDFF => self.memory.wram[(addr - 0xE000) as usize],
            0xFE00..=0xFE9F => self.ppu.read_oam(addr - 0xFE00).unwrap_or(0xFF),
            0xFF00..=0xFF7F => self.read_io(addr as u8),
            0xFEA0..=0xFEFF => 0,
            0xFF80..=0xFFFE => self.memory.hram[addr as usize - 0xFF80],
            0xFFFF => self.r_ier,
            _ => unimplemented!(
                "Unimplemented address range (read): (addr: {:01$X})",
                addr,
                4
            ),
        }
    }

    fn fetch(&mut self, addr: u16) -> u8 {
        self.update(3);
        let val = self.read_byte(addr);
        self.update(1);
        val
    }

    fn interrupt_check(&mut self) {
        self.update(2);
        self.mid_check = true;
    }

    fn read_io(&self, addr: u8) -> u8 {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x00 => 0xFF, // joypad, not implemented, 0xFF = no buttons pressed down...
            0x04..=0x07 => self.timer.read_reg(addr),
            0x08..=0x0E => 0xFF, // Empty range.
            0x0F => self.r_if | 0xE0,
            0x10..=0x3F => 0xFF, // TODO: APU, silently ignore
            0x46 => (self.dma.addr >> 8) as u8,
            0x40..=0x45 | 0x47..=0x4B => self.ppu.get_reg(addr),
            0x4C..=0x7F => 0xFF, // Empty range.
            0x80..=0xFF => unreachable!("Invalid address range for IO regs! (read)"),
            _ => {
                eprintln!("Unimplemented IO reg (read): (addr: 0xFF{:01$X})", addr, 2);
                0xFF
            }
        }
    }

    fn write_u16_cycle(&mut self, address: u16, value: u16) {
        self.write_cycle(address, value as u8);
        self.write_cycle(address.wrapping_add(1), (value >> 8) as u8)
    }

    fn write_cycle(&mut self, addr: u16, val: u8) {
        self.update(4); // TODO (TEST): Hardware timing might be different.
        match addr {
            0x0000..=0x7FFF => self.memory.mbc_write(addr, val),
            0x8000..=0x9FFF => self.ppu.set_vram(addr - 0x8000, val),
            0xC000..=0xDFFF => self.memory.wram[(addr - 0xC000) as usize] = val,
            0xE000..=0xFDFF => self.memory.wram[(addr - 0xE000) as usize] = val,
            0xFE00..=0xFE9F => self.ppu.write_oam(addr - 0xFE00, val),
            0xFEA0..=0xFEFF => {}
            0xFF00..=0xFF7F => self.write_io(addr as u8, val),
            0xFF80..=0xFFFE => self.memory.hram[addr as usize - 0xFF80] = val,
            0xFFFF => self.r_ier = val,
            _ => unimplemented!(
                "Unimplemented address range (write): (addr: {:01$X} val: {2:03$X})",
                addr,
                4,
                val,
                2
            ),
        }
    }

    fn write_io(&mut self, addr: u8, val: u8) {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x01 | 0x02 => {} // TODO: serial, silently ignore
            0x04..=0x07 => self.timer.write_reg(addr, val),
            0x08..=0x0E => {} // Empty range.
            0x0F => self.r_if = val & 0x1F,
            0x10..=0x3F => {} // TODO: APU, silently ignore
            0x46 => {
                self.dma.ld_addr = u16::from(val) << 8;
                self.dma.ld_timer = 4
            }
            0x40..=0x45 | 0x47..=0x4B => self.ppu.set_reg(addr, val),
            0x50 => self.memory.disable_boot_rom(),
            0x4C..=0x4F | 0x51..=0x7F => {}
            0x80..=0xFF => unreachable!("Invalid address range for IO regs! (write)"),
            _ => eprintln!(
                "Unimplemented IO reg (write): (addr: 0xFF{:01$X} val: {2:03$X})",
                addr, 2, val, 2
            ),
        }
    }
}

mod memory {
    pub struct Memory {
        pub wram: [u8; 0x2000],
        pub hram: [u8; 0x007F],
        game_rom: Vec<u8>,
        boot_rom: Vec<u8>,
        boot_rom_enabled: bool,
        mbc: Mbc,
    }

    impl Memory {
        pub fn new(game_rom: Vec<u8>, boot_rom: Vec<u8>) -> Option<Self> {
            if game_rom.len() < 0x150 || boot_rom.len() != 0x100 {
                None
            } else {
                let mbc = Mbc::new(game_rom[0x147], game_rom[0x148], game_rom[0x149])?;
                Some(Self {
                    wram: [0; 0x2000],
                    hram: [0; 0x007F],
                    game_rom,
                    boot_rom,
                    boot_rom_enabled: true,
                    mbc,
                })
            }
        }

        pub fn read_rom_low(&self, addr: u16) -> u8 {
            if self.boot_rom_enabled && addr < 0x100 {
                self.boot_rom[addr as usize]
            } else {
                let addr = self.mbc.get_physical_addr_low(addr);
                if addr < self.game_rom.len() {
                    self.game_rom[addr]
                } else {
                    0xFF
                }
            }
        }

        pub fn read_rom_high(&self, addr: u16) -> u8 {
            let addr = self.mbc.get_physical_addr_high(addr);
            if addr < self.game_rom.len() {
                self.game_rom[addr]
            } else {
                0xFF
            }
        }

        pub fn mbc_write(&mut self, addr: u16, val: u8) {
            match self.mbc {
                Mbc::Mbc0 => {}
                Mbc::Mbc1(ref mut desc) => match addr {
                    0x0000..=0x1FFF => unimplemented!(),
                    0x2000..=0x3FFF => desc.rom_bank = if val & 0x1F == 0 { 1 } else { val & 0x1F },
                    0x4000..=0x5FFF => desc.ram_bank = val & 0b11,
                    0x6000..=0x7FFF => desc.ram_bank_mode = super::bits::has_bit(val, 0),
                    _ => unreachable!(),
                },
            }
        }

        pub fn disable_boot_rom(&mut self) {
            self.boot_rom_enabled = false;
        }
    }

    #[derive(Debug)]
    struct MbcDescriptor {
        banks_rom: u8,
        banks_ram: u8,
        ram_bank_mode: bool,
        ram_bank: u8,
        rom_bank: u8,
    }

    impl MbcDescriptor {
        fn get_real_bank_count(&self) -> usize {
            2 << (self.banks_rom as usize)
        }
    }

    #[derive(Debug)]
    enum Mbc {
        Mbc0,
        Mbc1(MbcDescriptor),
    }

    impl Mbc {
        fn new(cart_type: u8, banks_rom: u8, banks_ram: u8) -> Option<Mbc> {
            match cart_type {
                0x00 => Some(Mbc::Mbc0),
                0x01 => Some(Mbc::Mbc1(MbcDescriptor {
                    banks_rom,
                    banks_ram,
                    ram_bank_mode: false,
                    rom_bank: 1,
                    ram_bank: 0,
                })),
                _ => None,
            }
        }

        fn get_physical_addr_low(&self, addr: u16) -> usize {
            match *self {
                Mbc::Mbc0 => addr as usize,
                Mbc::Mbc1(ref desc) => {
                    if !desc.ram_bank_mode {
                        (addr as usize).wrapping_add(
                            ((desc.get_real_bank_count() - 1) & ((desc.ram_bank << 5) as usize))
                                * 0x4000,
                        )
                    } else {
                        addr as usize
                    }
                }
            }
        }

        fn get_physical_addr_high(&self, addr: u16) -> usize {
            match *self {
                Mbc::Mbc0 => addr.wrapping_add(0x4000) as usize,
                Mbc::Mbc1(ref desc) => (addr as usize).wrapping_add(
                    ((desc.get_real_bank_count() - 1)
                        & (desc.rom_bank | (desc.ram_bank << 5)) as usize)
                        * 0x4000,
                ),
            }
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Mode {
    Step,
    Run,
}

pub struct GameBoy {
    hw: Hardware,
    cpu: Cpu,
    mode: Mode,
}

impl GameBoy {
    pub fn new(boot_rom: Vec<u8>, game_rom: Vec<u8>) -> Option<Self> {
        Some(Self {
            hw: Hardware::new(memory::Memory::new(game_rom, boot_rom)?),
            cpu: Cpu::new(),
            mode: Mode::Run,
        })
    }

    pub fn get_display(&self) -> &[DisplayPixel] {
        self.hw.ppu.get_display()
    }

    pub fn run(&mut self, ticks: i64) {
        self.hw.cycle_counter += ticks;
        while self.hw.cycle_counter > 0 {
            if let Some(new_mode) = self.cpu.run(&mut self.hw) {
                self.mode = new_mode;
            }

            if self.mode == Mode::Step {
                self.hw.cycle_counter = 0;
            }
        }
    }
}
