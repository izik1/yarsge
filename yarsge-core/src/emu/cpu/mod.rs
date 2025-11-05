mod instr;
use super::registers::Registers;
use super::registers::{self, R16, Reg};

use crate::emu::registers::RegisterArg;
use crate::emu::{Hardware, InterruptFlags, Mode};

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Status {
    Running,
    Halt,
    HaltNop,
    Stop,
    InterruptDispatch,
}

pub struct Cpu {
    pub regs: registers::Registers,
    pub status: Status,
    ime: bool,
    break_point_addresses: Vec<u16>,
}

impl Cpu {
    pub fn register_breakpoint(&mut self, address: u16) {
        self.break_point_addresses.push(address);
    }

    fn generic_fetch(&mut self, hw: &mut Hardware) -> Status {
        let (val, st) = self.fetch_cycle(hw);
        self.regs.pc += 1;
        self.regs.ir = val;

        st
    }

    fn fetch_cycle(&self, hw: &mut Hardware) -> (u8, Status) {
        let (val, interrupts) = hw.read_cycle_intr(self.regs.pc);
        if self.ime && !interrupts.is_empty() {
            return (val, Status::InterruptDispatch);
        }

        (val, Status::Running)
    }

    fn fetch_imm8(&mut self, hw: &mut Hardware) -> u8 {
        let val = hw.read_cycle(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        val
    }

    fn fetch_imm16(&mut self, hw: &mut Hardware) -> u16 {
        u16::from_le_bytes([self.fetch_imm8(hw), self.fetch_imm8(hw)])
    }

    fn pop8(&mut self, hw: &mut Hardware) -> u8 {
        let val = hw.read_cycle(self.regs.sp);
        self.regs.sp = self.regs.sp.wrapping_add(1);
        val
    }

    fn pop16(&mut self, hw: &mut Hardware) -> u16 {
        u16::from_le_bytes([self.pop8(hw), self.pop8(hw)])
    }

    fn push16(&mut self, hw: &mut Hardware, val: u16) {
        hw.idle_cycle();
        // no Predecrement in IDU, so
        self.regs.sp -= 1;

        let [hi, lo] = val.to_be_bytes();
        hw.write_cycle(self.regs.sp, hi);
        self.regs.sp -= 1;

        hw.write_cycle(self.regs.sp, lo);
    }

    // there isn't much way to reduce the line count here,
    // we literally need to pick 1 out of 256 possibilites. (512 even)
    #[allow(clippy::too_many_lines)]
    fn decode_execute(&mut self, hw: &mut Hardware) -> Status {
        use self::instr::MathReg;

        match self.regs.ir {
            0x00 => instr::nop(self, hw),
            0x01 => instr::ld_r16_imm16(self, hw, R16::BC),
            0x02 => instr::ld_r16_a(self, hw, R16::BC),
            0x03 => instr::inc_16(self, hw, R16::BC),
            0x04 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::B)),
            0x05 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::B)),
            0x06 => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::B)),
            0x07 => instr::rlca(self, hw),
            0x08 => instr::ld_a16_sp(self, hw),
            0x09 => instr::add_hl_reg16(self, hw, R16::BC),
            0x0A => instr::ld_a_r16(self, hw, R16::BC),
            0x0B => instr::dec_16(self, hw, R16::BC),
            0x0C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0E => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0F => instr::rrca(self, hw),
            0x10 => instr::stop(self, hw),
            0x11 => instr::ld_r16_imm16(self, hw, R16::DE),
            0x12 => instr::ld_r16_a(self, hw, R16::DE),
            0x13 => instr::inc_16(self, hw, R16::DE),
            0x14 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::D)),
            0x15 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::D)),
            0x16 => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::D)),
            0x17 => instr::rla(self, hw),
            0x18 => instr::jr_imm8(self, hw),
            0x19 => instr::add_hl_reg16(self, hw, R16::DE),
            0x1A => instr::ld_a_r16(self, hw, R16::DE),
            0x1B => instr::dec_16(self, hw, R16::DE),
            0x1C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1E => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1F => instr::rra(self, hw),

            0x20 | 0x28 | 0x30 | 0x38 => instr::jr_cc_imm8(self, hw),
            0x21 => instr::ld_r16_imm16(self, hw, R16::HL),
            0x22 => instr::ld_r16_a(self, hw, R16::HL),
            0x23 => instr::inc_16(self, hw, R16::HL),
            0x24 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::H)),
            0x25 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::H)),
            0x26 => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::H)),
            0x27 => instr::daa(self, hw),
            0x29 => instr::add_hl_reg16(self, hw, R16::HL),
            0x2A => instr::ld_a_r16(self, hw, R16::HL),
            0x2B => instr::dec_16(self, hw, R16::HL),
            0x2C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2E => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2F => instr::cpl(self, hw),
            0x31 => instr::ld_r16_imm16(self, hw, R16::SP),
            0x32 => instr::ld_r16_a(self, hw, R16::SP),
            0x33 => instr::inc_16(self, hw, R16::SP),
            0x34 => instr::inc_8(self, hw, RegisterArg::Indirect),
            0x35 => instr::dec_8(self, hw, RegisterArg::Indirect),
            0x36 => instr::ld_r8_imm8(self, hw, RegisterArg::Indirect),
            0x37 => instr::scf(self, hw),
            0x39 => instr::add_hl_reg16(self, hw, R16::SP),
            0x3A => instr::ld_a_r16(self, hw, R16::SP),
            0x3B => instr::dec_16(self, hw, R16::SP),
            0x3C => instr::inc_8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3D => instr::dec_8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3E => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3F => instr::ccf(self, hw),

            0x76 => instr::halt(self, hw),
            0x40..=0x7F => instr::ld(self, hw),

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

            0xC0 | 0xC8 | 0xD0 | 0xD8 => instr::ret_cc(self, hw),
            0xC1 => instr::pop(self, hw, R16::BC),
            0xC2 | 0xCA | 0xD2 | 0xDA => instr::jp_cc_imm16(self, hw),
            0xC3 => instr::jp_imm16(self, hw),
            0xC4 | 0xCC | 0xD4 | 0xDC => instr::call_cc(self, hw),
            0xC5 => instr::push(self, hw, R16::BC),
            0xC6 => instr::add(self, hw, MathReg::Imm),
            0xC7 => instr::rst(self, hw, 0x00),
            0xC9 => instr::ret::<false>(self, hw),
            0xCB => self.execute_cb(hw),
            0xCD => instr::call(self, hw),
            0xCE => instr::adc(self, hw, MathReg::Imm),
            0xCF => instr::rst(self, hw, 0x08),
            0xD1 => instr::pop(self, hw, R16::DE),
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xF4 | 0xEB..=0xED | 0xFC | 0xFD => {
                instr::invalid(self, hw);
            }
            0xD5 => instr::push(self, hw, R16::DE),
            0xD6 => instr::sub(self, hw, MathReg::Imm),
            0xD7 => instr::rst(self, hw, 0x10),
            0xD9 => instr::ret::<true>(self, hw),
            0xDE => instr::sbc(self, hw, MathReg::Imm),
            0xDF => instr::rst(self, hw, 0x18),

            0xE0 => instr::ldh_a8_a(self, hw),
            0xE1 => instr::pop(self, hw, R16::HL),
            0xE2 => instr::ldh_c_a(self, hw),
            0xE5 => instr::push(self, hw, R16::HL),
            0xE6 => instr::and(self, hw, MathReg::Imm),
            0xE7 => instr::rst(self, hw, 0x20),
            0xE8 => instr::add_sp_r8(self, hw),
            0xE9 => instr::jp_hl(self, hw),
            0xEA => instr::ld_a16_a(self, hw),
            0xEE => instr::xor(self, hw, MathReg::Imm),
            0xEF => instr::rst(self, hw, 0x28),
            0xF0 => instr::ldh_a_a8(self, hw),
            0xF1 => instr::pop(self, hw, R16::SP),
            0xF2 => instr::ldh_a_c(self, hw),
            0xF3 => instr::di(self, hw),
            0xF5 => instr::push(self, hw, R16::SP),
            0xF6 => instr::or(self, hw, MathReg::Imm),
            0xF7 => instr::rst(self, hw, 0x30),
            0xF8 => instr::ld_hl_sp_r8(self, hw),
            0xF9 => instr::ld_sp_hl(self, hw),
            0xFA => instr::ld_a_a16(self, hw),
            0xFB => instr::ei(self, hw),
            0xFE => instr::cp(self, hw, MathReg::Imm),
            0xFF => instr::rst(self, hw, 0x38),
        }
    }

    fn execute_cb(&mut self, hw: &mut Hardware) -> Status {
        self.regs.ir = self.fetch_imm8(hw);
        let op = self.regs.ir;
        let reg = RegisterArg::from_num(self.regs.ir);

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

    fn handle_interrupts(&mut self, hw: &mut Hardware) -> Status {
        self.ime = false;
        // Cycle : M1
        // IDU : Dec PC
        self.regs.pc = self.regs.pc.wrapping_sub(1);
        hw.idle_cycle();
        // Cycle : M2
        // IDU : Dec SP
        self.regs.sp = self.regs.sp.wrapping_sub(1);
        hw.idle_cycle();
        // Cycle : M3
        // IDU : Dec SP
        // Addr Bus : SP
        // Data Bus : write PC[15:8]
        hw.write_cycle(self.regs.sp, (self.regs.pc >> 8) as u8);
        self.regs.sp = self.regs.sp.wrapping_sub(1);

        // Cycle : M4
        // Addr Bus : SP
        // Data Bus : write PC[7:0]
        let flags = hw.write_cycle_intr(self.regs.sp, (self.regs.pc) as u8);

        let bits = flags.bits().trailing_zeros() as u16;

        if bits < 5 {
            self.regs.pc = 0x40 + bits * 8;
            hw.reg_if &= !(InterruptFlags::from_bits_retain(1 << bits));
        } else {
            // this is somewhat rare,
            self.regs.pc = 0;
        }

        self.regs.ir = self.fetch_imm8(hw);

        Status::Running
    }

    #[must_use]
    pub fn new() -> Self {
        Cpu {
            regs: Registers::default(),
            status: Status::Running,
            ime: false,
            break_point_addresses: Vec::new(),
        }
    }

    pub fn run(&mut self, hw: &mut Hardware) -> Option<Mode> {
        self.status = match self.status {
            Status::Running => self.decode_execute(hw),
            Status::Halt => {
                hw.idle_cycle();
                if (hw.reg_ie & hw.reg_if).is_empty() {
                    Status::Halt
                } else {
                    Status::HaltNop
                }
            }
            Status::HaltNop => instr::nop(self, hw),
            Status::Stop => {
                hw.idle_cycle();
                self.status
            }
            Status::InterruptDispatch => self.handle_interrupts(hw),
        };

        self.break_point_addresses
            .contains(&self.regs.pc)
            .then_some(Mode::Step)
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}
