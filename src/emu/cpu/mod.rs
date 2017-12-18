// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

mod instr;
use std::vec::*;
use super::registers;
use super::registers::*;
use super::flags::*;
use super::timer::Timer;
use super::ppu::Ppu;
use super::dma::Dma;

#[derive(Clone, Copy)]
pub enum State {
    Okay,
    Halt,
    Stop,
    Hang,
}

#[derive(Debug)]
struct MbcDescriptor {
    banks_rom: u8,
    banks_ram: u8,

}

#[derive(Debug)]
enum Mbc {
    Mbc0,
    Mbc1(MbcDescriptor)
}

impl Mbc {
    fn new (cart_type: u8, banks_rom: u8, banks_ram: u8) -> Option<Mbc> {
        match cart_type {
            0x00 => Some(Mbc::Mbc0),
            0x01 => Some(Mbc::Mbc1(MbcDescriptor{banks_rom, banks_ram})),
            _    => None,
        }

    }
}

pub struct Cpu {
    cycle_counter: i64,
    wram: [u8; 0x2000],
    hram: [u8; 0x007F],
    pub regs: registers::Registers,
    pub status: State,
    ime: bool,
    ei: bool,
    halt_bugged: bool,
    r_if: u8,
    r_ier: u8,
    tim: Timer,
    game_rom: Vec<u8>,
    boot_rom: Vec<u8>,
    mbc: Mbc,
    boot_rom_enabled: bool,
    pub ppu: Ppu,
    dma: Dma,
}

impl Cpu {

    // TODO: This is a terrible function. How do I make it not bother the borrow checker in impl Dma?
    fn update_dma(&mut self) {
        self.dma.modulus = if self.dma.modulus == 0 {
            self.dma.enabled = if self.dma.time == 0 { false } else {
                self.ppu.oam[160 - self.dma.time] = self.read_byte(self.dma.addr);
                self.dma.time -= 1;
                true
            };

            3
        } else {
            self.dma.modulus - 1
        };

        if self.dma.ld_timer > 0 {
            self.dma.ld_timer -= 1;
            if self.dma.ld_timer == 0 {
                self.dma.time = 160;
                self.dma.addr = self.dma.ld_addr;
                self.dma.ld_timer = -1;
            }
        }
    }


    fn update(&mut self, cycles: i64) {
        for _ in 0..cycles {
            self.r_if |= (self.tim.update() | self.ppu.update()) & 0x1F;
            self.update_dma();
        }

        self.cycle_counter -= cycles;
    }

    fn read_rom_low(&self, addr: u16) -> u8
    {
        let addr = addr as usize;
        if self.boot_rom_enabled && addr < 0x100 {
            self.boot_rom[addr as usize]
        } else {
            match self.mbc {
                Mbc::Mbc0 => if addr < self.game_rom.len() { self.game_rom[addr as usize] } else { 0xFF },
                Mbc::Mbc1(ref a) => if addr < self.game_rom.len() { self.game_rom[addr as usize] } else { 0xFF },
                _         => unimplemented!("Unimplemented MBC mode: {:?}", self.mbc) // FIXME: stub
            }
        }
    }

    fn read_rom_high(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        match self.mbc {
            Mbc::Mbc0 => if addr < self.game_rom.len() { self.game_rom[addr as usize + 0x4000] } else { 0xFF },
            Mbc::Mbc1(ref a) => if addr < self.game_rom.len() { self.game_rom[addr as usize + 0x4000] } else { 0xFF },
            _         => unimplemented!("Unimplemented MBC mode: {:?}", self.mbc) // FIXME: stub
        }
    }

    fn read_io(&self, addr: u8) -> u8 {
        // TODO: Most (all) of this function.
        match addr {
            0x04...0x07 => self.tim.read_reg(addr),
            0x0F        => self.r_if | 0xE0,
            0x10...0x3F => 0xFF, // TODO: APU, silently ignore
            0x46        => (self.dma.addr >> 8) as u8,
            0x40...0x4B => self.ppu.get_reg(addr),
            0x4C...0x7F => 0xFF, // Empty range.
            0x80...0xFF => unreachable!("Invalid address range for IO regs! (read)"),
            _ => {eprintln!("Unimplemented IO reg (read): (addr: 0xFF{:01$X})", addr, 2); 0xFF}
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        // println!("Reading (addr: {:01$X})", addr, 4);
        match addr {
            0x0000...0x3FFF => self.read_rom_low(addr),
            0x4000...0x7FFF => self.read_rom_high(addr - 0x4000),
            0x8000...0x9FFF => self.ppu.get_vram(addr - 0x8000),
            0xC000...0xDFFF => self.wram[(addr - 0xC000) as usize],
            0xE000...0xFDFF => self.wram[(addr - 0xE000) as usize],
            0xFF00...0xFF7F => self.read_io(addr as u8),
            0xFF80...0xFFFE => self.hram[addr as usize - 0xFF80],
            0xFFFF          => self.r_ier,
            _ => unimplemented!("Unimplemented address range (read): (addr: {:01$X})", addr, 4),
        }
    }

    fn read_cycle(&mut self, addr: u16) -> u8 {
        self.update(4);
        self.read_byte(addr)
    }

    fn read_pc(&self) -> u8 {self.read_byte(self.regs.pc)}
    
    fn read_ipc(&mut self) -> u8 {
        let val = self.read_pc();
        self.regs.pc = self.regs.pc.wrapping_add(1);
        val
    }
    
    fn read_u16_cycle(&mut self) -> u16 {
         (self.read_ipc_cycle() as u16) | ((self.read_ipc_cycle() as u16) << 8)
    }
    
    fn write_u16_cycle(&mut self, address: u16, value: u16) {
        self.write_cycle(address, value as u8);
        self.write_cycle(address.wrapping_add(1), (value >> 8) as u8)
    }

    fn read_ipc_cycle(&mut self) -> u8 {
        let pc = self.regs.pc;
        let val = self.read_cycle(pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        val
    }

    fn write_oam(&mut self, addr: u16, val: u8) {
        // TODO: PPU OAM blocking.
        self.ppu.oam[addr as usize] = val;
    }

    fn write_io(&mut self, addr: u8, val: u8) {
        match addr {
            0x01 | 0x02 => {} // TODO: serial, silently ignore
            0x04...0x07 => self.tim.write_reg(addr, val),
            0x0F        => self.r_if = val & 0x1F,
            0x10...0x3F => {} // TODO: APU, silently ignore
            0x46        => {self.dma.ld_addr = (val as u16) << 8; self.dma.ld_timer = 4}
            0x40...0x45 | 0x47...0x4B => self.ppu.set_reg(addr, val),
            0x50        => self.boot_rom_enabled = false,
            0x4C...0x7F => {},
            0x80...0xFF => unreachable!("Invalid address range for IO regs! (write)"),
            _ => eprintln!("Unimplemented IO reg (write): (addr: 0xFF{:01$X} val: {2:03$X})", addr, 2, val, 2)
        }
    }

    fn mbc_write(&mut self, _addr: u16, _val: u8) {
        match self.mbc {
            Mbc::Mbc0 => {},
            _         => unimplemented!("Unimplemented MBC mode: {:?}", self.mbc) // FIXME: stub
        }
    }

    fn write_cycle(&mut self, addr: u16, val: u8) {
        self.update(4); // TODO (TEST): Hardware timing might be different.
        match addr {
            0x0000...0x7FFF => self.mbc_write(addr, val),
            0x8000...0x9FFF => self.ppu.set_vram(addr - 0x8000, val),
            0xC000...0xDFFF => self.wram[(addr - 0xC000) as usize] = val,
            0xE000...0xFDFF => self.wram[(addr - 0xE000) as usize] = val,
            0xFE00...0xFE9F => self.write_oam(addr - 0xFE00, val),
            0xFEA0...0xFEFF => {}
            0xFF00...0xFF7F => self.write_io(addr as u8, val),
            0xFF80...0xFFFE => self.hram[addr as usize - 0xFF80] = val,
            0xFFFF          => self.r_ier = val,
            _ => unimplemented!("Unimplemented address range (write): (addr: {:01$X} val: {2:03$X})", addr, 4, val, 2)
        }
    }

    fn write_hl_cycle(&mut self, val: u8) {
        let hl = self.regs.hl;
        self.write_cycle(hl, val);
    }
    
    fn read_hl_cycle(&mut self) -> u8 {
        let hl = self.regs.hl;
        self.read_cycle(hl)
    }
    
    fn read_pop_cycle(&mut self) -> u8 {
        let sp = self.regs.sp;
        let val = self.read_cycle(sp);
        self.regs.sp = sp.wrapping_add(1);
        val
    }
    
    fn write_push_cycle(&mut self, val: u8) {
        self.regs.sp = self.regs.sp.wrapping_sub(1);
        let sp = self.regs.sp;
        self.write_cycle(sp, val);
    }
    
    fn read_pop_16_cycle(&mut self) -> u16 {
        self.read_pop_cycle() as u16 | ((self.read_pop_cycle() as u16) << 8)
    }
    
    fn write_push_16_cycle(&mut self, val: u16) {
        self.write_push_cycle((val >> 8) as u8);
        self.write_push_cycle(val as u8);
    }

    fn run_instruction(&mut self) {
        use self::instr::MathReg;
        self.update(1);

        if self.regs.pc == 0x100 {
            println!("0x100!");
        }

        //println!("i:{:01$X}", self.regs.pc, 4);
        let op = self.read_ipc();
        //println!("v:{:01$X}", op, 2);
        self.update(1);

        if self.halt_bugged {
            self.regs.pc = self.regs.pc.wrapping_sub(1);
            self.halt_bugged = false;
        }

        match op {
            0x00 => {}
            0x01 => instr::ld_r16_d16(self, R16::BC),
            0x02 => instr::ld_r16_a(self, R16::BC),
            0x03 => instr::inc_16(self, R16::BC),
            0x04 => instr::inc_8(self, Reg::B),
            0x05 => instr::dec_8(self, Reg::B),
            0x06 => instr::ld_r8_d8(self, Reg::B),
            0x07 => instr::rlca(self),
            0x08 => instr::ld_a16_sp(self),
            0x09 => instr::add_hl_reg16(self, R16::BC),
            0x0A => instr::ld_a_r16(self, R16::BC),
            0x0B => instr::dec_16(self, R16::BC),
            0x0C => instr::inc_8(self, Reg::C),
            0x0D => instr::dec_8(self, Reg::C),
            0x0E => instr::ld_r8_d8(self, Reg::C),
            0x0F => instr::rrca(self),
            0x10 => instr::stop(self),
            0x11 => instr::ld_r16_d16(self, R16::DE),
            0x12 => instr::ld_r16_a(self, R16::DE),
            0x13 => instr::inc_16(self, R16::DE),
            0x14 => instr::inc_8(self, Reg::D),
            0x15 => instr::dec_8(self, Reg::D),
            0x16 => instr::ld_r8_d8(self, Reg::D),
            0x17 => instr::rla(self),
            0x18 => instr::jr(self, true),
            0x19 => instr::add_hl_reg16(self, R16::DE),
            0x1A => instr::ld_a_r16(self, R16::DE),
            0x1B => instr::dec_16(self, R16::DE),
            0x1C => instr::inc_8(self, Reg::E),
            0x1D => instr::dec_8(self, Reg::E),
            0x1E => instr::ld_r8_d8(self, Reg::E),
            0x1F => instr::rra(self),

            0x20 => {let j = !self.regs.get_flag(Flag::Z); instr::jr(self, j)}
            0x21 => instr::ld_r16_d16(self, R16::HL),
            0x22 => instr::ld_r16_a(self, R16::HL),
            0x23 => instr::inc_16(self, R16::HL),
            0x24 => instr::inc_8(self, Reg::H),
            0x25 => instr::dec_8(self, Reg::H),
            0x26 => instr::ld_r8_d8(self, Reg::H),
            0x27 => instr::daa(self),
            0x28 => {let j =  self.regs.get_flag(Flag::Z); instr::jr(self, j)}
            0x29 => instr::add_hl_reg16(self, R16::HL),
            0x2A => instr::ld_a_r16(self, R16::HL),
            0x2B => instr::dec_16(self, R16::HL),
            0x2C => instr::inc_8(self, Reg::L),
            0x2D => instr::dec_8(self, Reg::L),
            0x2E => instr::ld_r8_d8(self, Reg::L),
            0x2F => instr::cpl(self),
            0x30 => {let j = !self.regs.get_flag(Flag::C); instr::jr(self, j)}
            0x31 => instr::ld_r16_d16(self, R16::SP),
            0x32 => instr::ld_r16_a(self, R16::SP),
            0x33 => instr::inc_16(self, R16::SP),
            0x34 => instr::inc_8(self, Reg::HL),
            0x35 => instr::dec_8(self, Reg::HL),
            0x36 => instr::ld_r8_d8(self, Reg::HL),
            0x37 => instr::scf(self),
            0x38 => {let j =  self.regs.get_flag(Flag::C); instr::jr(self, j)}
            0x39 => instr::add_hl_reg16(self, R16::SP),
            0x3A => instr::ld_a_r16(self, R16::SP),
            0x3B => instr::dec_16(self, R16::SP),
            0x3C => instr::inc_8(self, Reg::A),
            0x3D => instr::dec_8(self, Reg::A),
            0x3E => instr::ld_r8_d8(self, Reg::A),
            0x3F => instr::ccf(self),

            0x40 => instr::ld(self, Reg::B , Reg::B), 0x41 => instr::ld(self, Reg::B , Reg::C), 0x42 => instr::ld(self, Reg::B , Reg::D) , 0x43 => instr::ld(self, Reg::B , Reg::E),
            0x44 => instr::ld(self, Reg::B , Reg::H), 0x45 => instr::ld(self, Reg::B , Reg::L), 0x46 => instr::ld(self, Reg::B , Reg::HL), 0x47 => instr::ld(self, Reg::B , Reg::A),
            0x48 => instr::ld(self, Reg::C , Reg::B), 0x49 => instr::ld(self, Reg::C , Reg::C), 0x4A => instr::ld(self, Reg::C , Reg::D) , 0x4B => instr::ld(self, Reg::C , Reg::E),
            0x4C => instr::ld(self, Reg::C , Reg::H), 0x4D => instr::ld(self, Reg::C , Reg::L), 0x4E => instr::ld(self, Reg::C , Reg::HL), 0x4F => instr::ld(self, Reg::C , Reg::A),
            0x50 => instr::ld(self, Reg::D , Reg::B), 0x51 => instr::ld(self, Reg::D , Reg::C), 0x52 => instr::ld(self, Reg::D , Reg::D) , 0x53 => instr::ld(self, Reg::D , Reg::E),
            0x54 => instr::ld(self, Reg::D , Reg::H), 0x55 => instr::ld(self, Reg::D , Reg::L), 0x56 => instr::ld(self, Reg::D , Reg::HL), 0x57 => instr::ld(self, Reg::D , Reg::A),
            0x58 => instr::ld(self, Reg::E , Reg::B), 0x59 => instr::ld(self, Reg::E , Reg::C), 0x5A => instr::ld(self, Reg::E , Reg::D) , 0x5B => instr::ld(self, Reg::E , Reg::E),
            0x5C => instr::ld(self, Reg::E , Reg::H), 0x5D => instr::ld(self, Reg::E , Reg::L), 0x5E => instr::ld(self, Reg::E , Reg::HL), 0x5F => instr::ld(self, Reg::E , Reg::A),
            0x60 => instr::ld(self, Reg::H , Reg::B), 0x61 => instr::ld(self, Reg::H , Reg::C), 0x62 => instr::ld(self, Reg::H , Reg::D) , 0x63 => instr::ld(self, Reg::H , Reg::E),
            0x64 => instr::ld(self, Reg::H , Reg::H), 0x65 => instr::ld(self, Reg::H , Reg::L), 0x66 => instr::ld(self, Reg::H , Reg::HL), 0x67 => instr::ld(self, Reg::H , Reg::A),
            0x68 => instr::ld(self, Reg::L , Reg::B), 0x69 => instr::ld(self, Reg::L , Reg::C), 0x6A => instr::ld(self, Reg::L , Reg::D) , 0x6B => instr::ld(self, Reg::L , Reg::E),
            0x6C => instr::ld(self, Reg::L , Reg::H), 0x6D => instr::ld(self, Reg::L , Reg::L), 0x6E => instr::ld(self, Reg::L , Reg::HL), 0x6F => instr::ld(self, Reg::L , Reg::A),
            0x70 => instr::ld(self, Reg::HL, Reg::B), 0x71 => instr::ld(self, Reg::HL, Reg::C), 0x72 => instr::ld(self, Reg::HL, Reg::D) , 0x73 => instr::ld(self, Reg::HL, Reg::E),
            0x74 => instr::ld(self, Reg::HL, Reg::H), 0x75 => instr::ld(self, Reg::HL, Reg::L), 0x76 => instr::halt(self)                , 0x77 => instr::ld(self, Reg::HL, Reg::A),
            0x78 => instr::ld(self, Reg::A , Reg::B), 0x79 => instr::ld(self, Reg::A , Reg::C), 0x7A => instr::ld(self, Reg::A , Reg::D) , 0x7B => instr::ld(self, Reg::A , Reg::E),
            0x7C => instr::ld(self, Reg::A , Reg::H), 0x7D => instr::ld(self, Reg::A , Reg::L), 0x7E => instr::ld(self, Reg::A , Reg::HL), 0x7F => instr::ld(self, Reg::A , Reg::A),

            0x80 => instr::add(self, MathReg::R(Reg::B )), 0x81 => instr::add(self, MathReg::R(Reg::C)),
            0x82 => instr::add(self, MathReg::R(Reg::D )), 0x83 => instr::add(self, MathReg::R(Reg::E)),
            0x84 => instr::add(self, MathReg::R(Reg::H )), 0x85 => instr::add(self, MathReg::R(Reg::L)),
            0x86 => instr::add(self, MathReg::R(Reg::HL)), 0x87 => instr::add(self, MathReg::R(Reg::A)),

            0x88 => instr::adc(self, MathReg::R(Reg::B )), 0x89 => instr::adc(self, MathReg::R(Reg::C)),
            0x8A => instr::adc(self, MathReg::R(Reg::D )), 0x8B => instr::adc(self, MathReg::R(Reg::E)),
            0x8C => instr::adc(self, MathReg::R(Reg::H )), 0x8D => instr::adc(self, MathReg::R(Reg::L)),
            0x8E => instr::adc(self, MathReg::R(Reg::HL)), 0x8F => instr::adc(self, MathReg::R(Reg::A)),

            0x90 => instr::sub(self, MathReg::R(Reg::B )), 0x91 => instr::sub(self, MathReg::R(Reg::C)),
            0x92 => instr::sub(self, MathReg::R(Reg::D )), 0x93 => instr::sub(self, MathReg::R(Reg::E)),
            0x94 => instr::sub(self, MathReg::R(Reg::H )), 0x95 => instr::sub(self, MathReg::R(Reg::L)),
            0x96 => instr::sub(self, MathReg::R(Reg::HL)), 0x97 => instr::sub(self, MathReg::R(Reg::A)),

            0x98 => instr::sbc(self, MathReg::R(Reg::B )), 0x99 => instr::sbc(self, MathReg::R(Reg::C)),
            0x9A => instr::sbc(self, MathReg::R(Reg::D )), 0x9B => instr::sbc(self, MathReg::R(Reg::E)),
            0x9C => instr::sbc(self, MathReg::R(Reg::H )), 0x9D => instr::sbc(self, MathReg::R(Reg::L)),
            0x9E => instr::sbc(self, MathReg::R(Reg::HL)), 0x9F => instr::sbc(self, MathReg::R(Reg::A)),

            0xA0 => instr::and(self, MathReg::R(Reg::B )), 0xA1 => instr::and(self, MathReg::R(Reg::C)),
            0xA2 => instr::and(self, MathReg::R(Reg::D )), 0xA3 => instr::and(self, MathReg::R(Reg::E)),
            0xA4 => instr::and(self, MathReg::R(Reg::H )), 0xA5 => instr::and(self, MathReg::R(Reg::L)),
            0xA6 => instr::and(self, MathReg::R(Reg::HL)), 0xA7 => instr::and(self, MathReg::R(Reg::A)),

            0xA8 => instr::xor(self, MathReg::R(Reg::B )), 0xA9 => instr::xor(self, MathReg::R(Reg::C)),
            0xAA => instr::xor(self, MathReg::R(Reg::D )), 0xAB => instr::xor(self, MathReg::R(Reg::E)),
            0xAC => instr::xor(self, MathReg::R(Reg::H )), 0xAD => instr::xor(self, MathReg::R(Reg::L)),
            0xAE => instr::xor(self, MathReg::R(Reg::HL)), 0xAF => instr::xor(self, MathReg::R(Reg::A)),

            0xB0 => instr::or (self, MathReg::R(Reg::B )), 0xB1 => instr::or (self, MathReg::R(Reg::C)),
            0xB2 => instr::or (self, MathReg::R(Reg::D )), 0xB3 => instr::or (self, MathReg::R(Reg::E)),
            0xB4 => instr::or (self, MathReg::R(Reg::H )), 0xB5 => instr::or (self, MathReg::R(Reg::L)),
            0xB6 => instr::or (self, MathReg::R(Reg::HL)), 0xB7 => instr::or (self, MathReg::R(Reg::A)),

            0xB8 => instr::cp (self, MathReg::R(Reg::B )), 0xB9 => instr::cp (self, MathReg::R(Reg::C)),
            0xBA => instr::cp (self, MathReg::R(Reg::D )), 0xBB => instr::cp (self, MathReg::R(Reg::E)),
            0xBC => instr::cp (self, MathReg::R(Reg::H )), 0xBD => instr::cp (self, MathReg::R(Reg::L)),
            0xBE => instr::cp (self, MathReg::R(Reg::HL)), 0xBF => instr::cp (self, MathReg::R(Reg::A)),

            0xC0 => {let j = !self.regs.get_flag(Flag::Z); instr::retc(self, j)}
            0xC1 => instr::pop(self, R16::BC),
            0xC2 => {let j = !self.regs.get_flag(Flag::Z); instr::jp  (self, j)}
            0xC3 => instr::jp (self, true ),
            0xC4 => {let j = !self.regs.get_flag(Flag::Z); instr::call(self, j)}
            0xC5 => instr::push(self, R16::BC),
            0xC6 => instr::add(self, MathReg::Imm),
            0xC7 => instr::rst(self, 0x00),
            0xC8 => {let j =  self.regs.get_flag(Flag::Z); instr::retc(self, j)}
            0xC9 => instr::ret(self, false),
            0xCA => {let j =  self.regs.get_flag(Flag::Z); instr::jp  (self, j)}
            0xCB => self.run_extended(),
            0xCC => {let j =  self.regs.get_flag(Flag::Z); instr::call(self, j)}
            0xCD => instr::call(self, true),
            0xCE => instr::adc(self, MathReg::Imm),
            0xCF => instr::rst(self, 0x08),
            0xD0 => {let j = !self.regs.get_flag(Flag::C); instr::retc(self, j)}
            0xD1 => instr::pop(self, R16::DE),
            0xD2 => {let j = !self.regs.get_flag(Flag::C); instr::jp  (self, j)}
            0xD3 => instr::invalid(self),
            0xD4 => {let j = !self.regs.get_flag(Flag::C); instr::call(self, j)}
            0xD5 => instr::push(self, R16::DE),
            0xD6 => instr::sub(self, MathReg::Imm),
            0xD7 => instr::rst(self, 0x10),
            0xD8 => {let j =  self.regs.get_flag(Flag::C); instr::retc(self, j)}
            0xD9 => instr::ret(self, true),
            0xDA => {let j =  self.regs.get_flag(Flag::C); instr::jp  (self, j)}
            0xDB => instr::invalid(self),
            0xDC => {let j =  self.regs.get_flag(Flag::C); instr::call(self, j)}
            0xDD => instr::invalid(self),
            0xDE => instr::sbc(self, MathReg::Imm),
            0xDF => instr::rst(self, 0x18),

            0xE0 => instr::ldh_a8_a(self),
            0xE1 => instr::pop(self, R16::HL),
            0xE2 => instr::ldh_c_a(self),
            0xE3 => instr::invalid(self),
            0xE4 => instr::invalid(self),
            0xE5 => instr::push(self, R16::HL),
            0xE6 => instr::and(self, MathReg::Imm),
            0xE7 => instr::rst(self, 0x20),
            0xE8 => instr::add_sp_r8(self),
            0xE9 => instr::jp_hl(self),
            0xEA => instr::ld_a16_a(self),
            0xEB => instr::invalid(self),
            0xEC => instr::invalid(self),
            0xED => instr::invalid(self),
            0xEE => instr::xor(self, MathReg::Imm),
            0xEF => instr::rst(self, 0x28),
            0xF0 => instr::ldh_a_a8(self),
            0xF1 => instr::pop(self, R16::SP),
            0xF2 => instr::ldh_a_c(self),
            0xF3 => instr::di(self),
            0xF4 => instr::invalid(self),
            0xF5 => instr::push(self, R16::SP),
            0xF6 => instr::or(self, MathReg::Imm),
            0xF7 => instr::rst(self, 0x30),
            0xF8 => instr::ld_hl_sp_r8(self),
            0xF9 => instr::ld_sp_hl(self),
            0xFA => instr::ld_a_a16(self),
            0xFB => instr::ei(self),
            0xFC => instr::invalid(self),
            0xFD => instr::invalid(self),
            0xFE => instr::cp(self, MathReg::Imm),
            0xFF => instr::rst(self, 0x38),
            _ => unreachable!(),
        }
    }

    fn run_extended(&mut self) {
        let op = self.read_ipc_cycle();
        match op {
            0x00 => instr::rlc (self, Reg::B), 0x01 => instr::rlc (self, Reg::C), 0x02 => instr::rlc (self, Reg::D) , 0x03 => instr::rlc (self, Reg::E),
            0x04 => instr::rlc (self, Reg::H), 0x05 => instr::rlc (self, Reg::L), 0x06 => instr::rlc (self, Reg::HL), 0x07 => instr::rlc (self, Reg::A),
            0x08 => instr::rrc (self, Reg::B), 0x09 => instr::rrc (self, Reg::C), 0x0A => instr::rrc (self, Reg::D) , 0x0B => instr::rrc (self, Reg::E),
            0x0C => instr::rrc (self, Reg::H), 0x0D => instr::rrc (self, Reg::L), 0x0E => instr::rrc (self, Reg::HL), 0x0F => instr::rrc (self, Reg::A),

            0x10 => instr::rl  (self, Reg::B), 0x11 => instr::rl  (self, Reg::C), 0x12 => instr::rl  (self, Reg::D) , 0x13 => instr::rl  (self, Reg::E),
            0x14 => instr::rl  (self, Reg::H), 0x15 => instr::rl  (self, Reg::L), 0x16 => instr::rl  (self, Reg::HL), 0x17 => instr::rl  (self, Reg::A),
            0x18 => instr::rr  (self, Reg::B), 0x19 => instr::rr  (self, Reg::C), 0x1A => instr::rr  (self, Reg::D) , 0x1B => instr::rr  (self, Reg::E),
            0x1C => instr::rr  (self, Reg::H), 0x1D => instr::rr  (self, Reg::L), 0x1E => instr::rr  (self, Reg::HL), 0x1F => instr::rr  (self, Reg::A),

            0x20 => instr::sla (self, Reg::B), 0x21 => instr::sla (self, Reg::C), 0x22 => instr::sla (self, Reg::D) , 0x23 => instr::sla (self, Reg::E),
            0x24 => instr::sla (self, Reg::H), 0x25 => instr::sla (self, Reg::L), 0x26 => instr::sla (self, Reg::HL), 0x27 => instr::sla (self, Reg::A),
            0x28 => instr::sra (self, Reg::B), 0x29 => instr::sra (self, Reg::C), 0x2A => instr::sra (self, Reg::D) , 0x2B => instr::sra (self, Reg::E),
            0x2C => instr::sra (self, Reg::H), 0x2D => instr::sra (self, Reg::L), 0x2E => instr::sra (self, Reg::HL), 0x2F => instr::sra (self, Reg::A),

            0x30 => instr::swap(self, Reg::B), 0x31 => instr::swap(self, Reg::C), 0x32 => instr::swap(self, Reg::D) , 0x33 => instr::swap(self, Reg::E),
            0x34 => instr::swap(self, Reg::H), 0x35 => instr::swap(self, Reg::L), 0x36 => instr::swap(self, Reg::HL), 0x37 => instr::swap(self, Reg::A),
            0x38 => instr::srl (self, Reg::B), 0x39 => instr::srl (self, Reg::C), 0x3A => instr::srl (self, Reg::D) , 0x3B => instr::srl (self, Reg::E),
            0x3C => instr::srl (self, Reg::H), 0x3D => instr::srl (self, Reg::L), 0x3E => instr::srl (self, Reg::HL), 0x3F => instr::srl (self, Reg::A),

            0x40 => instr::bit(self, Reg::B, 0x01), 0x41 => instr::bit(self, Reg::C, 0x01), 0x42 => instr::bit(self, Reg::D , 0x01), 0x43 => instr::bit(self, Reg::E, 0x01),
            0x44 => instr::bit(self, Reg::H, 0x01), 0x45 => instr::bit(self, Reg::L, 0x01), 0x46 => instr::bit(self, Reg::HL, 0x01), 0x47 => instr::bit(self, Reg::A, 0x01),
            0x48 => instr::bit(self, Reg::B, 0x02), 0x49 => instr::bit(self, Reg::C, 0x02), 0x4A => instr::bit(self, Reg::D , 0x02), 0x4B => instr::bit(self, Reg::E, 0x02),
            0x4C => instr::bit(self, Reg::H, 0x02), 0x4D => instr::bit(self, Reg::L, 0x02), 0x4E => instr::bit(self, Reg::HL, 0x02), 0x4F => instr::bit(self, Reg::A, 0x02),
            0x50 => instr::bit(self, Reg::B, 0x04), 0x51 => instr::bit(self, Reg::C, 0x04), 0x52 => instr::bit(self, Reg::D , 0x04), 0x53 => instr::bit(self, Reg::E, 0x04),
            0x54 => instr::bit(self, Reg::H, 0x04), 0x55 => instr::bit(self, Reg::L, 0x04), 0x56 => instr::bit(self, Reg::HL, 0x04), 0x57 => instr::bit(self, Reg::A, 0x04),
            0x58 => instr::bit(self, Reg::B, 0x08), 0x59 => instr::bit(self, Reg::C, 0x08), 0x5A => instr::bit(self, Reg::D , 0x08), 0x5B => instr::bit(self, Reg::E, 0x08),
            0x5C => instr::bit(self, Reg::H, 0x08), 0x5D => instr::bit(self, Reg::L, 0x08), 0x5E => instr::bit(self, Reg::HL, 0x08), 0x5F => instr::bit(self, Reg::A, 0x08),
            0x60 => instr::bit(self, Reg::B, 0x10), 0x61 => instr::bit(self, Reg::C, 0x10), 0x62 => instr::bit(self, Reg::D , 0x10), 0x63 => instr::bit(self, Reg::E, 0x10),
            0x64 => instr::bit(self, Reg::H, 0x10), 0x65 => instr::bit(self, Reg::L, 0x10), 0x66 => instr::bit(self, Reg::HL, 0x10), 0x67 => instr::bit(self, Reg::A, 0x10),
            0x68 => instr::bit(self, Reg::B, 0x20), 0x69 => instr::bit(self, Reg::C, 0x20), 0x6A => instr::bit(self, Reg::D , 0x20), 0x6B => instr::bit(self, Reg::E, 0x20),
            0x6C => instr::bit(self, Reg::H, 0x20), 0x6D => instr::bit(self, Reg::L, 0x20), 0x6E => instr::bit(self, Reg::HL, 0x20), 0x6F => instr::bit(self, Reg::A, 0x20),
            0x70 => instr::bit(self, Reg::B, 0x40), 0x71 => instr::bit(self, Reg::C, 0x40), 0x72 => instr::bit(self, Reg::D , 0x40), 0x73 => instr::bit(self, Reg::E, 0x40),
            0x74 => instr::bit(self, Reg::H, 0x40), 0x75 => instr::bit(self, Reg::L, 0x40), 0x76 => instr::bit(self, Reg::HL, 0x40), 0x77 => instr::bit(self, Reg::A, 0x40),
            0x78 => instr::bit(self, Reg::B, 0x80), 0x79 => instr::bit(self, Reg::C, 0x80), 0x7A => instr::bit(self, Reg::D , 0x80), 0x7B => instr::bit(self, Reg::E, 0x80),
            0x7C => instr::bit(self, Reg::H, 0x80), 0x7D => instr::bit(self, Reg::L, 0x80), 0x7E => instr::bit(self, Reg::HL, 0x80), 0x7F => instr::bit(self, Reg::A, 0x80),

            0x80 => instr::res(self, Reg::B, 0x01), 0x81 => instr::res(self, Reg::C, 0x01), 0x82 => instr::res(self, Reg::D , 0x01), 0x83 => instr::res(self, Reg::E, 0x01),
            0x84 => instr::res(self, Reg::H, 0x01), 0x85 => instr::res(self, Reg::L, 0x01), 0x86 => instr::res(self, Reg::HL, 0x01), 0x87 => instr::res(self, Reg::A, 0x01),
            0x88 => instr::res(self, Reg::B, 0x02), 0x89 => instr::res(self, Reg::C, 0x02), 0x8A => instr::res(self, Reg::D , 0x02), 0x8B => instr::res(self, Reg::E, 0x02),
            0x8C => instr::res(self, Reg::H, 0x02), 0x8D => instr::res(self, Reg::L, 0x02), 0x8E => instr::res(self, Reg::HL, 0x02), 0x8F => instr::res(self, Reg::A, 0x02),
            0x90 => instr::res(self, Reg::B, 0x04), 0x91 => instr::res(self, Reg::C, 0x04), 0x92 => instr::res(self, Reg::D , 0x04), 0x93 => instr::res(self, Reg::E, 0x04),
            0x94 => instr::res(self, Reg::H, 0x04), 0x95 => instr::res(self, Reg::L, 0x04), 0x96 => instr::res(self, Reg::HL, 0x04), 0x97 => instr::res(self, Reg::A, 0x04),
            0x98 => instr::res(self, Reg::B, 0x08), 0x99 => instr::res(self, Reg::C, 0x08), 0x9A => instr::res(self, Reg::D , 0x08), 0x9B => instr::res(self, Reg::E, 0x08),
            0x9C => instr::res(self, Reg::H, 0x08), 0x9D => instr::res(self, Reg::L, 0x08), 0x9E => instr::res(self, Reg::HL, 0x08), 0x9F => instr::res(self, Reg::A, 0x08),
            0xA0 => instr::res(self, Reg::B, 0x10), 0xA1 => instr::res(self, Reg::C, 0x10), 0xA2 => instr::res(self, Reg::D , 0x10), 0xA3 => instr::res(self, Reg::E, 0x10),
            0xA4 => instr::res(self, Reg::H, 0x10), 0xA5 => instr::res(self, Reg::L, 0x10), 0xA6 => instr::res(self, Reg::HL, 0x10), 0xA7 => instr::res(self, Reg::A, 0x10),
            0xA8 => instr::res(self, Reg::B, 0x20), 0xA9 => instr::res(self, Reg::C, 0x20), 0xAA => instr::res(self, Reg::D , 0x20), 0xAB => instr::res(self, Reg::E, 0x20),
            0xAC => instr::res(self, Reg::H, 0x20), 0xAD => instr::res(self, Reg::L, 0x20), 0xAE => instr::res(self, Reg::HL, 0x20), 0xAF => instr::res(self, Reg::A, 0x20),
            0xB0 => instr::res(self, Reg::B, 0x40), 0xB1 => instr::res(self, Reg::C, 0x40), 0xB2 => instr::res(self, Reg::D , 0x40), 0xB3 => instr::res(self, Reg::E, 0x40),
            0xB4 => instr::res(self, Reg::H, 0x40), 0xB5 => instr::res(self, Reg::L, 0x40), 0xB6 => instr::res(self, Reg::HL, 0x40), 0xB7 => instr::res(self, Reg::A, 0x40),
            0xB8 => instr::res(self, Reg::B, 0x80), 0xB9 => instr::res(self, Reg::C, 0x80), 0xBA => instr::res(self, Reg::D , 0x80), 0xBB => instr::res(self, Reg::E, 0x80),
            0xBC => instr::res(self, Reg::H, 0x80), 0xBD => instr::res(self, Reg::L, 0x80), 0xBE => instr::res(self, Reg::HL, 0x80), 0xBF => instr::res(self, Reg::A, 0x80),

            0xC0 => instr::set(self, Reg::B, 0x01), 0xC1 => instr::set(self, Reg::C, 0x01), 0xC2 => instr::set(self, Reg::D , 0x01), 0xC3 => instr::set(self, Reg::E, 0x01),
            0xC4 => instr::set(self, Reg::H, 0x01), 0xC5 => instr::set(self, Reg::L, 0x01), 0xC6 => instr::set(self, Reg::HL, 0x01), 0xC7 => instr::set(self, Reg::A, 0x01),
            0xC8 => instr::set(self, Reg::B, 0x02), 0xC9 => instr::set(self, Reg::C, 0x02), 0xCA => instr::set(self, Reg::D , 0x02), 0xCB => instr::set(self, Reg::E, 0x02),
            0xCC => instr::set(self, Reg::H, 0x02), 0xCD => instr::set(self, Reg::L, 0x02), 0xCE => instr::set(self, Reg::HL, 0x02), 0xCF => instr::set(self, Reg::A, 0x02),
            0xD0 => instr::set(self, Reg::B, 0x04), 0xD1 => instr::set(self, Reg::C, 0x04), 0xD2 => instr::set(self, Reg::D , 0x04), 0xD3 => instr::set(self, Reg::E, 0x04),
            0xD4 => instr::set(self, Reg::H, 0x04), 0xD5 => instr::set(self, Reg::L, 0x04), 0xD6 => instr::set(self, Reg::HL, 0x04), 0xD7 => instr::set(self, Reg::A, 0x04),
            0xD8 => instr::set(self, Reg::B, 0x08), 0xD9 => instr::set(self, Reg::C, 0x08), 0xDA => instr::set(self, Reg::D , 0x08), 0xDB => instr::set(self, Reg::E, 0x08),
            0xDC => instr::set(self, Reg::H, 0x08), 0xDD => instr::set(self, Reg::L, 0x08), 0xDE => instr::set(self, Reg::HL, 0x08), 0xDF => instr::set(self, Reg::A, 0x08),
            0xE0 => instr::set(self, Reg::B, 0x10), 0xE1 => instr::set(self, Reg::C, 0x10), 0xE2 => instr::set(self, Reg::D , 0x10), 0xE3 => instr::set(self, Reg::E, 0x10),
            0xE4 => instr::set(self, Reg::H, 0x10), 0xE5 => instr::set(self, Reg::L, 0x10), 0xE6 => instr::set(self, Reg::HL, 0x10), 0xE7 => instr::set(self, Reg::A, 0x10),
            0xE8 => instr::set(self, Reg::B, 0x20), 0xE9 => instr::set(self, Reg::C, 0x20), 0xEA => instr::set(self, Reg::D , 0x20), 0xEB => instr::set(self, Reg::E, 0x20),
            0xEC => instr::set(self, Reg::H, 0x20), 0xED => instr::set(self, Reg::L, 0x20), 0xEE => instr::set(self, Reg::HL, 0x20), 0xEF => instr::set(self, Reg::A, 0x20),
            0xF0 => instr::set(self, Reg::B, 0x40), 0xF1 => instr::set(self, Reg::C, 0x40), 0xF2 => instr::set(self, Reg::D , 0x40), 0xF3 => instr::set(self, Reg::E, 0x40),
            0xF4 => instr::set(self, Reg::H, 0x40), 0xF5 => instr::set(self, Reg::L, 0x40), 0xF6 => instr::set(self, Reg::HL, 0x40), 0xF7 => instr::set(self, Reg::A, 0x40),
            0xF8 => instr::set(self, Reg::B, 0x80), 0xF9 => instr::set(self, Reg::C, 0x80), 0xFA => instr::set(self, Reg::D , 0x80), 0xFB => instr::set(self, Reg::E, 0x80),
            0xFC => instr::set(self, Reg::H, 0x80), 0xFD => instr::set(self, Reg::L, 0x80), 0xFE => instr::set(self, Reg::HL, 0x80), 0xFF => instr::set(self, Reg::A, 0x80),
            _ => unreachable!(),
        }

    }

    fn handle_interrupts(&mut self) {
        if !self.ime || (self.r_ier & self.r_if & 0x1F) == 0 {
            self.ime |= self.ei;
        } else {
            self.ime = false;
            self.update(6);
            let old_pc = self.regs.pc;
            self.write_push_cycle((old_pc >> 8) as u8);
            let b = self.r_ier & self.r_if & 0x1F;
            self.regs.pc = 0;
            for i in 0..5 {
                if (b >> i) & 1 == 1 {
                    self.regs.pc = (i * 8 + 0x40) as u16;
                    self.r_if &= !(1 << i) as u8;
                    break;
                }
            }

            self.write_push_cycle(old_pc as u8);
            self.update(2);
        }

        self.ei = false;
    }
    
    fn handle_okay(&mut self) {
        self.update(2);
        self.handle_interrupts();
        self.run_instruction();
    }
    
    pub fn new(boot_rom: Vec<u8>, game_rom: Vec<u8>) -> Option<Self> {
        if game_rom.len() < 0x150 {
            None
        } else {
            let mbc = Mbc::new(game_rom[0x147], game_rom[0x148], game_rom[0x149])?;
            match boot_rom.len() {
                0x100 => Some(Cpu {
                    cycle_counter: 0,
                    wram: [0; 0x2000],
                    hram: [0; 0x007F],
                    regs: registers::Registers::new(),
                    status: State::Okay,
                    ime: false,
                    ei: false,
                    halt_bugged: false,
                    r_ier: 0,
                    r_if: 0,
                    tim: Timer::new(),
                    game_rom,
                    boot_rom,
                    mbc,
                    boot_rom_enabled: true,
                    ppu: Ppu::new(),
                    dma: Dma::new(),
                }),
                _ => None
            }
        }
    }
    
    pub fn run(&mut self, ticks: i64) {
        self.cycle_counter += ticks;
        while self.cycle_counter > 0 {
            match self.status {
                State::Okay => self.handle_okay(),
                State::Stop => unimplemented!("Implement CPU stop behavior!"),
                State::Halt => unimplemented!("Implement CPU halt behavior!"),
                State::Hang => unimplemented!("Implement CPU hung behavior!"),
            };
        }
    }
}
