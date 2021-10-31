mod instr;
use super::registers::Registers;
use super::{
    flags::Flag,
    registers::{self, Reg, R16},
};

use crate::emu::registers::RegisterArg;
use crate::emu::{Hardware, MCycle, Mode};

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum State {
    Okay,
    Halt,
    Stop,
    Hang,
    HaltBug,
}

pub struct Cpu {
    pub regs: registers::Registers,
    pub status: State,
    ime: bool,
    ei: bool,
    halt_bugged: bool,
    break_point_addresses: Vec<u16>,
}

impl Cpu {
    pub fn register_breakpoint(&mut self, address: u16) {
        self.break_point_addresses.push(address);
    }

    fn fetch(&mut self, hw: &mut Hardware) -> u8 {
        let val = hw.fetch(self.regs.pc);

        if self.status == State::HaltBug {
            self.status = State::Okay;
        } else {
            self.regs.pc = self.regs.pc.wrapping_add(1);
        }

        val
    }

    fn read_u16_cycle(&mut self, hw: &mut Hardware) -> u16 {
        u16::from(self.read_ipc_cycle(hw)) | (u16::from(self.read_ipc_cycle(hw)) << 8)
    }

    fn read_ipc_cycle(&mut self, hw: &mut Hardware) -> u8 {
        let val = hw.read_cycle(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        val
    }

    fn read_pop_cycle(&mut self, hw: &mut Hardware) -> u8 {
        let val = hw.read_cycle(self.regs.sp);
        self.regs.sp = self.regs.sp.wrapping_add(1);
        val
    }

    fn write_push_cycle(&mut self, hw: &mut Hardware, val: u8) {
        self.regs.sp = self.regs.sp.wrapping_sub(1);
        hw.write_cycle(self.regs.sp, val);
    }

    fn read_pop_16_cycle(&mut self, hw: &mut Hardware) -> u16 {
        u16::from(self.read_pop_cycle(hw)) | (u16::from(self.read_pop_cycle(hw)) << 8)
    }

    fn write_push_16_cycle(&mut self, hw: &mut Hardware, val: u16) {
        self.write_push_cycle(hw, (val >> 8) as u8);
        self.write_push_cycle(hw, val as u8);
    }

    // there isn't much way to reduce the line count here,
    // we literally need to pick 1 out of 256 possibilites. (512 even)
    #[allow(clippy::too_many_lines)]
    fn run_instruction(&mut self, hw: &mut Hardware) {
        use self::instr::MathReg;
        let op = self.fetch(hw);

        match op {
            0x00 => {}
            0x01 => instr::ld_r16_d16(self, hw, R16::BC),
            0x02 => instr::ld_r16_a(self, hw, R16::BC),
            0x03 => instr::inc_16(self, hw, R16::BC),
            0x04 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::B)),
            0x05 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::B)),
            0x06 => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::B)),
            0x07 => instr::rlca(self),
            0x08 => instr::ld_a16_sp(self, hw),
            0x09 => instr::add_hl_reg16(self, hw, R16::BC),
            0x0A => instr::ld_a_r16(self, hw, R16::BC),
            0x0B => instr::dec_16(self, hw, R16::BC),
            0x0C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0E => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0F => instr::rrca(self),
            0x10 => instr::stop(self),
            0x11 => instr::ld_r16_d16(self, hw, R16::DE),
            0x12 => instr::ld_r16_a(self, hw, R16::DE),
            0x13 => instr::inc_16(self, hw, R16::DE),
            0x14 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::D)),
            0x15 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::D)),
            0x16 => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::D)),
            0x17 => instr::rla(self),
            0x18 => instr::jr(self, hw, true),
            0x19 => instr::add_hl_reg16(self, hw, R16::DE),
            0x1A => instr::ld_a_r16(self, hw, R16::DE),
            0x1B => instr::dec_16(self, hw, R16::DE),
            0x1C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1E => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1F => instr::rra(self),

            0x20 => instr::jr(self, hw, !self.regs.f.contains(Flag::Z)),
            0x21 => instr::ld_r16_d16(self, hw, R16::HL),
            0x22 => instr::ld_r16_a(self, hw, R16::HL),
            0x23 => instr::inc_16(self, hw, R16::HL),
            0x24 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::H)),
            0x25 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::H)),
            0x26 => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::H)),
            0x27 => instr::daa(self),
            0x28 => instr::jr(self, hw, self.regs.f.contains(Flag::Z)),
            0x29 => instr::add_hl_reg16(self, hw, R16::HL),
            0x2A => instr::ld_a_r16(self, hw, R16::HL),
            0x2B => instr::dec_16(self, hw, R16::HL),
            0x2C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2E => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2F => instr::cpl(self),
            0x30 => instr::jr(self, hw, !self.regs.f.contains(Flag::C)),
            0x31 => instr::ld_r16_d16(self, hw, R16::SP),
            0x32 => instr::ld_r16_a(self, hw, R16::SP),
            0x33 => instr::inc_16(self, hw, R16::SP),
            0x34 => instr::inc_8(self, hw, RegisterArg::Indirect),
            0x35 => instr::dec_8(self, hw, RegisterArg::Indirect),
            0x36 => instr::ld_r8_d8(self, hw, RegisterArg::Indirect),
            0x37 => instr::scf(self),
            0x38 => instr::jr(self, hw, self.regs.f.contains(Flag::C)),
            0x39 => instr::add_hl_reg16(self, hw, R16::SP),
            0x3A => instr::ld_a_r16(self, hw, R16::SP),
            0x3B => instr::dec_16(self, hw, R16::SP),
            0x3C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3E => instr::ld_r8_d8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3F => instr::ccf(self),

            0x40..=0x7F => {
                let dest = RegisterArg::from_num(op >> 3);
                let src = RegisterArg::from_num(op);
                instr::ld(self, hw, dest, src);
            }

            op @ 0x40..=0xBF => {
                let reg = MathReg::R(RegisterArg::from_num(op));
                match (op >> 0b11) & 0b111 {
                    0b000 => instr::add(self, hw, reg),
                    0b001 => instr::adc(self, hw, reg),
                    0b010 => instr::sub(self, hw, reg),
                    0b011 => instr::sbc(self, hw, reg),
                    0b100 => instr::and(self, hw, reg),
                    0b101 => instr::xor(self, hw, reg),
                    0b110 => instr::or(self, hw, reg),
                    0b111 => instr::cp(self, hw, reg),
                    _ => unreachable!(),
                }
            }

            0xC0 => instr::retc(self, hw, !self.regs.f.contains(Flag::Z)),
            0xC1 => instr::pop(self, hw, R16::BC),
            0xC2 => instr::jp(self, hw, !self.regs.f.contains(Flag::Z)),
            0xC3 => instr::jp(self, hw, true),
            0xC4 => instr::call(self, hw, !self.regs.f.contains(Flag::Z)),
            0xC5 => instr::push(self, hw, R16::BC),
            0xC6 => instr::add(self, hw, MathReg::Imm),
            0xC7 => instr::rst(self, hw, 0x00),
            0xC8 => instr::retc(self, hw, self.regs.f.contains(Flag::Z)),
            0xC9 => instr::ret(self, hw, false),
            0xCA => instr::jp(self, hw, self.regs.f.contains(Flag::Z)),
            0xCB => self.run_extended(hw),
            0xCC => instr::call(self, hw, self.regs.f.contains(Flag::Z)),
            0xCD => instr::call(self, hw, true),
            0xCE => instr::adc(self, hw, MathReg::Imm),
            0xCF => instr::rst(self, hw, 0x08),
            0xD0 => instr::retc(self, hw, !self.regs.f.contains(Flag::C)),
            0xD1 => instr::pop(self, hw, R16::DE),
            0xD2 => instr::jp(self, hw, !self.regs.f.contains(Flag::C)),
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xF4 | 0xEB..=0xED | 0xFC | 0xFD => {
                instr::invalid(self);
            }
            0xD4 => instr::call(self, hw, !self.regs.f.contains(Flag::C)),
            0xD5 => instr::push(self, hw, R16::DE),
            0xD6 => instr::sub(self, hw, MathReg::Imm),
            0xD7 => instr::rst(self, hw, 0x10),
            0xD8 => instr::retc(self, hw, self.regs.f.contains(Flag::C)),
            0xD9 => instr::ret(self, hw, true),
            0xDA => instr::jp(self, hw, self.regs.f.contains(Flag::C)),
            0xDC => instr::call(self, hw, self.regs.f.contains(Flag::C)),
            0xDE => instr::sbc(self, hw, MathReg::Imm),
            0xDF => instr::rst(self, hw, 0x18),

            0xE0 => instr::ldh_a8_a(self, hw),
            0xE1 => instr::pop(self, hw, R16::HL),
            0xE2 => instr::ldh_c_a(self, hw),
            0xE5 => instr::push(self, hw, R16::HL),
            0xE6 => instr::and(self, hw, MathReg::Imm),
            0xE7 => instr::rst(self, hw, 0x20),
            0xE8 => instr::add_sp_r8(self, hw),
            0xE9 => instr::jp_hl(self),
            0xEA => instr::ld_a16_a(self, hw),
            0xEE => instr::xor(self, hw, MathReg::Imm),
            0xEF => instr::rst(self, hw, 0x28),
            0xF0 => instr::ldh_a_a8(self, hw),
            0xF1 => instr::pop(self, hw, R16::SP),
            0xF2 => instr::ldh_a_c(self, hw),
            0xF3 => instr::di(self),
            0xF5 => instr::push(self, hw, R16::SP),
            0xF6 => instr::or(self, hw, MathReg::Imm),
            0xF7 => instr::rst(self, hw, 0x30),
            0xF8 => instr::ld_hl_sp_r8(self, hw),
            0xF9 => instr::ld_sp_hl(self, hw),
            0xFA => instr::ld_a_a16(self, hw),
            0xFB => instr::ei(self),
            0xFE => instr::cp(self, hw, MathReg::Imm),
            0xFF => instr::rst(self, hw, 0x38),
        }
    }

    fn run_extended(&mut self, hw: &mut Hardware) {
        let op = self.read_ipc_cycle(hw);
        let reg = RegisterArg::from_num(op);

        match op >> 6 {
            0b00 => match op >> 3 {
                0b000 => instr::rlc(self, hw, reg),
                0b001 => instr::rrc(self, hw, reg),
                0b010 => instr::rl(self, hw, reg),
                0b011 => instr::rr(self, hw, reg),
                0b100 => instr::sla(self, hw, reg),
                0b101 => instr::sra(self, hw, reg),
                0b110 => instr::swap(self, hw, reg),
                0b111 => instr::srl(self, hw, reg),
                _ => unreachable!(),
            },

            0b01 => instr::bit(self, hw, reg, 1 << ((op >> 3) & 0b111)),
            0b10 => instr::res(self, hw, reg, 1 << ((op >> 3) & 0b111)),
            0b11 => instr::set(self, hw, reg, 1 << ((op >> 3) & 0b111)),
            _ => unreachable!(),
        }
    }

    fn handle_interrupts(&mut self, hw: &mut Hardware) {
        if !self.ime || (hw.r_ier & hw.r_if & 0x1F) == 0 {
            self.ime |= self.ei;
        } else {
            self.ime = false;
            hw.stall(MCycle(2));
            let old_pc = self.regs.pc;
            self.write_push_cycle(hw, (old_pc >> 8) as u8);
            let b = hw.r_ier & hw.r_if & 0x1F;
            self.regs.pc = 0;
            for i in 0..5 {
                if (b >> i) & 1 == 1 {
                    self.regs.pc = (i * 8 + 0x40) as u16;
                    hw.r_if &= !(1 << i) as u8;
                    break;
                }
            }

            self.write_push_cycle(hw, old_pc as u8);
        }

        self.ei = false;
    }

    fn handle_okay(&mut self, hw: &mut Hardware) {
        hw.interrupt_check(|hw| self.handle_interrupts(hw));
        self.run_instruction(hw);
    }

    fn handle_halt(&mut self, hw: &mut Hardware) {
        hw.stall_one();
        if hw.r_if & hw.r_ier & 0x1F > 0 {
            self.status = State::Okay;
        }
    }

    fn handle_stop(&mut self) {
        // TODO: wait for controller input, there is no controller right now.
    }

    #[must_use]
    pub fn new() -> Self {
        Cpu {
            regs: Registers::default(),
            status: State::Okay,
            ime: false,
            ei: false,
            halt_bugged: false,
            break_point_addresses: Vec::new(),
        }
    }

    pub fn run(&mut self, hw: &mut Hardware) -> Option<Mode> {
        match self.status {
            State::Okay | State::HaltBug => self.handle_okay(hw),
            State::Stop => self.handle_stop(),
            State::Halt => self.handle_halt(hw),
            State::Hang => hw.stall_one(),
        };

        self.break_point_addresses
            .contains(&self.regs.pc)
            .then(|| Mode::Step)
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}
