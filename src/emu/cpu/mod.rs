// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

mod instr;
use std::vec::*;
use super::registers;
use super::registers::*;
use super::flags::*;
use super::timer::Timer;
use super::ppu::Ppu;
use super::dma::Dma;
use super::bits;
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
        match self {
            &Mbc::Mbc0 => addr as usize,
            &Mbc::Mbc1(ref desc) => if !desc.ram_bank_mode {
                (addr as usize).wrapping_add(
                    ((desc.get_real_bank_count() - 1) & ((desc.ram_bank << 5) as usize)) * 0x4000,
                )
            } else {
                addr as usize
            },
        }
    }

    fn get_physical_addr_high(&self, addr: u16) -> usize {
        match self {
            &Mbc::Mbc0 => addr.wrapping_add(0x4000) as usize,
            &Mbc::Mbc1(ref desc) => (addr as usize).wrapping_add(
                ((desc.get_real_bank_count() - 1) & (desc.rom_bank | (desc.ram_bank << 5)) as usize)
                    * 0x4000,
            ),
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
            self.dma.enabled = if self.dma.time == 0 {
                false
            } else {
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

    fn read_rom_low(&self, addr: u16) -> u8 {
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

    fn read_rom_high(&self, addr: u16) -> u8 {
        let addr = self.mbc.get_physical_addr_high(addr);
        if addr < self.game_rom.len() {
            self.game_rom[addr]
        } else {
            0xFF
        }
    }

    fn read_oam(&self, addr: u16) -> u8 {
        if self.ppu.oam_blocked() {
            0xFF
        } else {
            self.ppu.oam[addr as usize]
        }
    }

    fn read_io(&self, addr: u8) -> u8 {
        match addr {
            0x04...0x07 => self.tim.read_reg(addr),
            0x0F => self.r_if | 0xE0,
            0x10...0x3F => 0xFF, // TODO: APU, silently ignore
            0x46 => (self.dma.addr >> 8) as u8,
            0x40...0x4B => self.ppu.get_reg(addr),
            0x4C...0x7F => 0xFF, // Empty range.
            0x80...0xFF => unreachable!("Invalid address range for IO regs! (read)"),
            _ => {
                eprintln!("Unimplemented IO reg (read): (addr: 0xFF{:01$X})", addr, 2);
                0xFF
            }
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => self.read_rom_low(addr),
            0x4000...0x7FFF => self.read_rom_high(addr - 0x4000),
            0x8000...0x9FFF => self.ppu.get_vram(addr - 0x8000),
            0xC000...0xDFFF => self.wram[(addr - 0xC000) as usize],
            0xE000...0xFDFF => self.wram[(addr - 0xE000) as usize],
            0xFE00...0xFE9F => self.read_oam(addr - 0xFE00),
            0xFF00...0xFF7F => self.read_io(addr as u8),
            0xFF80...0xFFFE => self.hram[addr as usize - 0xFF80],
            0xFFFF => self.r_ier,
            _ => unimplemented!(
                "Unimplemented address range (read): (addr: {:01$X})",
                addr,
                4
            ),
        }
    }

    fn read_cycle(&mut self, addr: u16) -> u8 {
        self.update(4);
        self.read_byte(addr)
    }

    fn read_pc(&self) -> u8 {
        self.read_byte(self.regs.pc)
    }

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
        if !self.ppu.oam_blocked() {
            self.ppu.oam[addr as usize] = val
        };
    }

    fn write_io(&mut self, addr: u8, val: u8) {
        match addr {
            0x01 | 0x02 => {} // TODO: serial, silently ignore
            0x04...0x07 => self.tim.write_reg(addr, val),
            0x0F => self.r_if = val & 0x1F,
            0x10...0x3F => {} // TODO: APU, silently ignore
            0x46 => {
                self.dma.ld_addr = (val as u16) << 8;
                self.dma.ld_timer = 4
            }
            0x40...0x45 | 0x47...0x4B => self.ppu.set_reg(addr, val),
            0x50 => self.boot_rom_enabled = false,
            0x4C...0x7F => {}
            0x80...0xFF => unreachable!("Invalid address range for IO regs! (write)"),
            _ => eprintln!(
                "Unimplemented IO reg (write): (addr: 0xFF{:01$X} val: {2:03$X})",
                addr, 2, val, 2
            ),
        }
    }

    fn mbc_write(&mut self, addr: u16, val: u8) {
        match self.mbc {
            Mbc::Mbc0 => {}
            Mbc::Mbc1(ref mut desc) => match addr {
                0x0000...0x1FFF => unimplemented!(),
                0x2000...0x3FFF => desc.rom_bank = if val & 0x1F == 0 { 1 } else { val & 0x1F },
                0x4000...0x5FFF => desc.ram_bank = val & 0b11,
                0x6000...0x7FFF => desc.ram_bank_mode = bits::has_bit(val, 0),
                _ => unreachable!(),
            },
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

        let op = self.read_ipc();
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

            0x20 => instr::jr(self, !self.regs.get_flag(Flag::Z)),
            0x21 => instr::ld_r16_d16(self, R16::HL),
            0x22 => instr::ld_r16_a(self, R16::HL),
            0x23 => instr::inc_16(self, R16::HL),
            0x24 => instr::inc_8(self, Reg::H),
            0x25 => instr::dec_8(self, Reg::H),
            0x26 => instr::ld_r8_d8(self, Reg::H),
            0x27 => instr::daa(self),
            0x28 => instr::jr(self, self.regs.get_flag(Flag::Z)),
            0x29 => instr::add_hl_reg16(self, R16::HL),
            0x2A => instr::ld_a_r16(self, R16::HL),
            0x2B => instr::dec_16(self, R16::HL),
            0x2C => instr::inc_8(self, Reg::L),
            0x2D => instr::dec_8(self, Reg::L),
            0x2E => instr::ld_r8_d8(self, Reg::L),
            0x2F => instr::cpl(self),
            0x30 => instr::jr(self, !self.regs.get_flag(Flag::C)),
            0x31 => instr::ld_r16_d16(self, R16::SP),
            0x32 => instr::ld_r16_a(self, R16::SP),
            0x33 => instr::inc_16(self, R16::SP),
            0x34 => instr::inc_8(self, Reg::HL),
            0x35 => instr::dec_8(self, Reg::HL),
            0x36 => instr::ld_r8_d8(self, Reg::HL),
            0x37 => instr::scf(self),
            0x38 => instr::jr(self, self.regs.get_flag(Flag::C)),
            0x39 => instr::add_hl_reg16(self, R16::SP),
            0x3A => instr::ld_a_r16(self, R16::SP),
            0x3B => instr::dec_16(self, R16::SP),
            0x3C => instr::inc_8(self, Reg::A),
            0x3D => instr::dec_8(self, Reg::A),
            0x3E => instr::ld_r8_d8(self, Reg::A),
            0x3F => instr::ccf(self),

            0x40...0x7F => {
                let dest = Reg::from_num(op >> 3);
                let src = Reg::from_num(op);
                if op == 0x76 {
                    instr::halt(self);
                } else {
                    instr::ld(self, dest, src);
                }
            }

            op @ 0x40...0xBF => {
                let reg = MathReg::R(Reg::from_num(op));
                match (op >> 0b11) & 0b111 {
                    0b000 => instr::add(self, reg),
                    0b001 => instr::adc(self, reg),
                    0b010 => instr::sub(self, reg),
                    0b011 => instr::sbc(self, reg),
                    0b100 => instr::and(self, reg),
                    0b101 => instr::xor(self, reg),
                    0b110 => instr::or(self, reg),
                    0b111 => instr::cp(self, reg),
                    _ => unreachable!(),
                }
            }

            0xC0 => instr::retc(self, !self.regs.get_flag(Flag::Z)),
            0xC1 => instr::pop(self, R16::BC),
            0xC2 => instr::jp(self, !self.regs.get_flag(Flag::Z)),
            0xC3 => instr::jp(self, true),
            0xC4 => instr::call(self, !self.regs.get_flag(Flag::Z)),
            0xC5 => instr::push(self, R16::BC),
            0xC6 => instr::add(self, MathReg::Imm),
            0xC7 => instr::rst(self, 0x00),
            0xC8 => instr::retc(self, self.regs.get_flag(Flag::Z)),
            0xC9 => instr::ret(self, false),
            0xCA => instr::jp(self, self.regs.get_flag(Flag::Z)),
            0xCB => self.run_extended(),
            0xCC => instr::call(self, self.regs.get_flag(Flag::Z)),
            0xCD => instr::call(self, true),
            0xCE => instr::adc(self, MathReg::Imm),
            0xCF => instr::rst(self, 0x08),
            0xD0 => instr::retc(self, !self.regs.get_flag(Flag::C)),
            0xD1 => instr::pop(self, R16::DE),
            0xD2 => instr::jp(self, !self.regs.get_flag(Flag::C)),
            0xD3 => instr::invalid(self),
            0xD4 => instr::call(self, !self.regs.get_flag(Flag::C)),
            0xD5 => instr::push(self, R16::DE),
            0xD6 => instr::sub(self, MathReg::Imm),
            0xD7 => instr::rst(self, 0x10),
            0xD8 => instr::retc(self, self.regs.get_flag(Flag::C)),
            0xD9 => instr::ret(self, true),
            0xDA => instr::jp(self, self.regs.get_flag(Flag::C)),
            0xDB => instr::invalid(self),
            0xDC => instr::call(self, self.regs.get_flag(Flag::C)),
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
        let reg = Reg::from_num(op);

        match op >> 6 {
            0b00 => match op >> 3 {
                0b000 => instr::rlc(self, reg),
                0b001 => instr::rrc(self, reg),
                0b010 => instr::rl(self, reg),
                0b011 => instr::rr(self, reg),
                0b100 => instr::sla(self, reg),
                0b101 => instr::sra(self, reg),
                0b110 => instr::swap(self, reg),
                0b111 => instr::srl(self, reg),
                _ => unreachable!(),
            },

            0b01 => instr::bit(self, reg, 1 << ((op >> 3) & 0b111)),
            0b10 => instr::res(self, reg, 1 << ((op >> 3) & 0b111)),
            0b11 => instr::set(self, reg, 1 << ((op >> 3) & 0b111)),
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

    fn handle_halt(&mut self) {
        self.update(4);
        if self.r_if & self.r_ier & 0x1F > 0 {
            self.status = State::Okay;
        }
    }

    pub fn new(boot_rom: Vec<u8>, game_rom: Vec<u8>) -> Option<Self> {
        if game_rom.len() < 0x150 || boot_rom.len() != 0x100 {
            None
        } else {
            let mbc = Mbc::new(game_rom[0x147], game_rom[0x148], game_rom[0x149])?;
            Some(Cpu {
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
            })
        }
    }

    pub fn run(&mut self, ticks: i64) {
        self.cycle_counter += ticks;
        while self.cycle_counter > 0 {
            match self.status {
                State::Okay => self.handle_okay(),
                State::Stop => unimplemented!("Implement CPU stop behavior!"),
                State::Halt => self.handle_halt(),
                State::Hang => unimplemented!("Implement CPU hung behavior!"),
            };
        }
    }
}
