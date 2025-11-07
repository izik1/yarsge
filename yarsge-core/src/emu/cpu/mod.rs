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
            0x0a => instr::ld_a_r16(self, hw, R16::BC),
            0x0b => instr::dec_16(self, hw, R16::BC),
            0x0c => instr::inc_8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0d => instr::dec_8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0e => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::C)),
            0x0f => instr::rrca(self, hw),
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
            0x1a => instr::ld_a_r16(self, hw, R16::DE),
            0x1b => instr::dec_16(self, hw, R16::DE),
            0x1c => instr::inc_8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1d => instr::dec_8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1e => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::E)),
            0x1f => instr::rra(self, hw),

            0x20 | 0x28 | 0x30 | 0x38 => instr::jr_cc_imm8(self, hw),
            0x21 => instr::ld_r16_imm16(self, hw, R16::HL),
            0x22 => instr::ld_r16_a(self, hw, R16::HL),
            0x23 => instr::inc_16(self, hw, R16::HL),
            0x24 => instr::inc_8(self, hw, RegisterArg::Reg(Reg::H)),
            0x25 => instr::dec_8(self, hw, RegisterArg::Reg(Reg::H)),
            0x26 => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::H)),
            0x27 => instr::daa(self, hw),
            0x29 => instr::add_hl_reg16(self, hw, R16::HL),
            0x2a => instr::ld_a_r16(self, hw, R16::HL),
            0x2b => instr::dec_16(self, hw, R16::HL),
            0x2c => instr::inc_8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2d => instr::dec_8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2e => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::L)),
            0x2f => instr::cpl(self, hw),
            0x31 => instr::ld_r16_imm16(self, hw, R16::SP),
            0x32 => instr::ld_r16_a(self, hw, R16::SP),
            0x33 => instr::inc_16(self, hw, R16::SP),
            0x34 => instr::inc_8(self, hw, RegisterArg::Indirect),
            0x35 => instr::dec_8(self, hw, RegisterArg::Indirect),
            0x36 => instr::ld_r8_imm8(self, hw, RegisterArg::Indirect),
            0x37 => instr::scf(self, hw),
            0x39 => instr::add_hl_reg16(self, hw, R16::SP),
            0x3a => instr::ld_a_r16(self, hw, R16::SP),
            0x3b => instr::dec_16(self, hw, R16::SP),
            0x3c => instr::inc_8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3d => instr::dec_8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3e => instr::ld_r8_imm8(self, hw, RegisterArg::Reg(Reg::A)),
            0x3f => instr::ccf(self, hw),

            0x76 => instr::halt(self, hw),
            0x40..0x80 => instr::ld(self, hw),

            op @ 0x80..0xc0 => {
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

            0xc0 | 0xc8 | 0xd0 | 0xd8 => instr::ret_cc(self, hw),
            0xc1 => instr::pop(self, hw, R16::BC),
            0xc2 | 0xca | 0xd2 | 0xda => instr::jp_cc_imm16(self, hw),
            0xc3 => instr::jp_imm16(self, hw),
            0xc4 | 0xcc | 0xd4 | 0xdc => instr::call_cc(self, hw),
            0xc5 => instr::push(self, hw, R16::BC),
            0xc6 => instr::add(self, hw, MathReg::Imm),
            0xc7 => instr::rst(self, hw, 0x00),
            0xc9 => instr::ret::<false>(self, hw),
            0xcb => self.execute_cb(hw),
            0xcd => instr::call(self, hw),
            0xce => instr::adc(self, hw, MathReg::Imm),
            0xcf => instr::rst(self, hw, 0x08),
            0xd1 => instr::pop(self, hw, R16::DE),
            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xf4 | 0xeb..0xee | 0xfc | 0xfd => {
                instr::invalid(self, hw);
            }
            0xd5 => instr::push(self, hw, R16::DE),
            0xd6 => instr::sub(self, hw, MathReg::Imm),
            0xd7 => instr::rst(self, hw, 0x10),
            0xd9 => instr::ret::<true>(self, hw),
            0xde => instr::sbc(self, hw, MathReg::Imm),
            0xdf => instr::rst(self, hw, 0x18),

            0xe0 => instr::ldh_a8_a(self, hw),
            0xe1 => instr::pop(self, hw, R16::HL),
            0xe2 => instr::ldh_c_a(self, hw),
            0xe5 => instr::push(self, hw, R16::HL),
            0xe6 => instr::and(self, hw, MathReg::Imm),
            0xe7 => instr::rst(self, hw, 0x20),
            0xe8 => instr::add_sp_r8(self, hw),
            0xe9 => instr::jp_hl(self, hw),
            0xea => instr::ld_a16_a(self, hw),
            0xee => instr::xor(self, hw, MathReg::Imm),
            0xef => instr::rst(self, hw, 0x28),
            0xf0 => instr::ldh_a_a8(self, hw),
            0xf1 => instr::pop(self, hw, R16::SP),
            0xf2 => instr::ldh_a_c(self, hw),
            0xf3 => instr::di(self, hw),
            0xf5 => instr::push(self, hw, R16::SP),
            0xf6 => instr::or(self, hw, MathReg::Imm),
            0xf7 => instr::rst(self, hw, 0x30),
            0xf8 => instr::ld_hl_sp_r8(self, hw),
            0xf9 => instr::ld_sp_hl(self, hw),
            0xfa => instr::ld_a_a16(self, hw),
            0xfb => instr::ei(self, hw),
            0xfe => instr::cp(self, hw, MathReg::Imm),
            0xff => instr::rst(self, hw, 0x38),
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
