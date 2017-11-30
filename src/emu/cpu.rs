// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use std::vec::*;
use super::registers;
use super::registers::*;
use super::flags::*;
use super::timer::Timer;

#[derive(Clone, Copy)]
pub enum State {
    Okay,
    Halt,
    Stop,
    Hang,
}

#[derive(Clone, Copy)]
enum MathReg {
    R(Reg),
    Imm,
}

pub struct Cpu {
    cycle_counter: i64,
    wram: [u8; 0x2000],
    pub regs: registers::Registers,
    pub status: State,
    ime: bool,
    ie: bool,
    halt_bugged: bool,
    r_if: u8,
    r_ier: u8,
    tim: Timer,
    game_rom: Vec<u8>,
    boot_rom: Vec<u8>,
}

impl Cpu {
    fn update(&mut self, _cycles: i64) {
        for _ in 0.._cycles {
            self.tim.update(&mut self.r_if);
        }
    }

    fn read_rom_low(&self, _addr: u16) -> u8 {
        0xFF
    }

    fn read_rom_high(&self, _addr: u16) -> u8 {
        0xFF
    }

    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => self.read_rom_low(addr),
            0x4000...0x7FFF => self.read_rom_high(addr),
            0xC000...0xCFFF => self.wram[(addr - 0xC000) as usize],
            _ => unimplemented!("Not yet implemented range!"),
        }
    }

    fn read_cycle(&mut self, addr: u16) -> u8 {
        self.update(4);
        self.read_byte(addr)
    }

    fn read_pc(&self) -> u8 {
        self.read_byte(self.regs.pc)
    }
    
    fn read_ipc(&mut self) -> u8{
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

    fn write_cycle(&mut self, _addr: u16, _val: u8) {
        // TODO: Stub.
    }

    fn write_hl_cycle(&mut self, val: u8) {
        let hl = self.regs.hl;
        self.write_cycle(hl, val);
        // TODO: STUB
    }
    
    fn read_hl_cycle(&mut self) -> u8 {
        0xFF // TODO: STUB
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
    
    fn get_reg(&mut self, reg: Reg) -> (i64, u8) {
        match reg {
            Reg::HL => (4, self.read_hl_cycle()),
            r => (0, self.regs.get_reg(&r)),
        }
    }
    
    fn get_math_reg(&mut self, reg: MathReg) -> (i64, u8) {
        match reg {
            MathReg::Imm => (4, self.read_ipc_cycle()),
            MathReg::R(r2) => self.get_reg(r2),
        }
    }

    // Mnemonic: JR
    // Full Name: Jump Relative
    // Description: Jumps to pc + r8 if "jump" is true, otherwise it does nothing.
    // Affected Flags: ----
    // Remarks: This instruction stops 4 cycles short if it doesn't jump.
    // Timing: read, <internal delay>
    fn instr_jr(&mut self, jump: bool) -> i64 {
        let val = self.read_ipc_cycle();
        if !jump {
            4
        } else {
            self.update(4);
            self.regs.pc = self.regs.pc.wrapping_add(val as u16);
            8
        }
    }
    
    // Mnemonic: JP
    // Full Name: Jump
    // Description: Jumps to a16 if "jump" is true, otherwise it does nothing.
    // Affected Flags: ----
    // Remarks: This instruction stops 4 cycles short if it doesn't jump.
    // Timing: read, read, <internal delay>
    fn instr_jp(&mut self, jump: bool) -> i64 {
        let addr = self.read_u16_cycle();
        if !jump {
            8 
        } else {
            self.update(4);
            self.regs.pc = addr;
            12
        }
    }
    
    // Mnemonic: LD
    // Full Name: Load
    // Description: Loads dest into src, either one of which can be HL but not both.
    // Affected Flags: ----
    // Remarks: I really like how this function came out. I think it looks nice.
    // Timing: either "write", "read" or instant.
    fn instr_ld(&mut self, dest: Reg, src: Reg) -> i64 {
        match (dest, src) {
            (Reg::HL, Reg::HL) => unreachable!("This while theoretically reachable, should *never* be reached, since this instruction is instead HALT"),
            (Reg::HL, src) =>     {let val = self.regs.get_reg(&src); self.write_hl_cycle(val); 4}
            (dest, Reg::HL) =>    {let val = self.read_hl_cycle(); self.regs.set_reg(dest, val); 4}
            (dest, src) =>        {let val = self.regs.get_reg(&src); self.regs.set_reg(dest, val); 0}
        }
    }

    // Mnemonic: HALT
    // Full Name: Halt
    // Description: Halts the cpu.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: instant.
    fn instr_halt(&mut self) -> i64 {
        if self.ime || (self.r_if & self.r_ier & 0x1F) == 0
            {
                self.status = State::Halt;
            }
            else
            {
                self.halt_bugged = true;
            }

        0
    }

    // Mnemonic: STOP
    // Full Name: Stop
    // Description: Stops the cpu.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: NA.
    fn instr_stop(&mut self) -> i64 {
        self.status = State::Stop;
        0
    }

    fn run_instruction(&mut self) -> i64 {
        self.update(1);
        let op = self.read_ipc();
        self.update(1);

        if self.halt_bugged {
            self.regs.pc = self.regs.pc.wrapping_sub(1);
        }

        2 + match op {
            0x00 => 0,
            0x01 => self.instr_ld_r16_d16(R16::BC),
            0x02 => self.instr_ld_r16_a(R16::BC),
            0x03 => self.instr_inc_16(R16::BC),
            0x04 => self.instr_inc_8(Reg::B),
            0x05 => self.instr_dec_8(Reg::B),
            0x06 => self.instr_ld_r8_d8(Reg::B),
            0x07 => self.instr_rlca(),
            0x08 => self.instr_ld_a16_sp(),
            0x09 => self.instr_add_hl_reg16(R16::BC),
            0x0A => self.instr_ld_a_r16(R16::BC),
            0x0B => self.instr_dec_16(R16::BC),
            0x0C => self.instr_inc_8(Reg::C),
            0x0D => self.instr_dec_8(Reg::C),
            0x0E => self.instr_ld_r8_d8(Reg::C),
            0x0F => self.instr_rrca(),
            0x10 => self.instr_stop(),
            0x11 => self.instr_ld_r16_d16(R16::DE),
            0x12 => self.instr_ld_r16_a(R16::DE),
            0x13 => self.instr_inc_16(R16::DE),
            0x14 => self.instr_inc_8(Reg::D),
            0x15 => self.instr_dec_8(Reg::D),
            0x16 => self.instr_ld_r8_d8(Reg::D),
            0x17 => self.instr_rla(),
            0x18 => self.instr_jr(true),
            0x19 => self.instr_add_hl_reg16(R16::DE),
            0x1A => self.instr_ld_a_r16(R16::DE),
            0x1B => self.instr_dec_16(R16::DE),
            0x1C => self.instr_inc_8(Reg::E),
            0x1D => self.instr_dec_8(Reg::E),
            0x1E => self.instr_ld_r8_d8(Reg::E),
            0x1F => self.instr_rra(),
            
            0x20 => {let j = !self.regs.get_flag(Flag::Z); self.instr_jr(j)}
            0x21 => self.instr_ld_r16_d16(R16::HL),
            0x22 => self.instr_ld_r16_a(R16::HL),
            0x23 => self.instr_inc_16(R16::HL),
            0x24 => self.instr_inc_8(Reg::H),
            0x25 => self.instr_dec_8(Reg::H),
            0x26 => self.instr_ld_r8_d8(Reg::H),
            0x27 => self.instr_daa(),
            0x28 => {let j =  self.regs.get_flag(Flag::Z); self.instr_jr(j)}
            0x29 => self.instr_add_hl_reg16(R16::HL),
            0x2A => self.instr_ld_a_r16(R16::HL),
            0x2B => self.instr_dec_16(R16::HL),
            0x2C => self.instr_inc_8(Reg::L),
            0x2D => self.instr_dec_8(Reg::L),
            0x2E => self.instr_ld_r8_d8(Reg::L),
            0x2F => self.instr_cpl(),
            0x30 => {let j = !self.regs.get_flag(Flag::C); self.instr_jr(j)}
            0x31 => self.instr_ld_r16_d16(R16::SP),
            0x32 => self.instr_ld_r16_a(R16::SP),
            0x33 => self.instr_inc_16(R16::SP),
            0x34 => self.instr_inc_8(Reg::HL),
            0x35 => self.instr_dec_8(Reg::HL),
            0x36 => self.instr_ld_r8_d8(Reg::HL),
            0x37 => self.instr_scf(),
            0x38 => {let j =  self.regs.get_flag(Flag::C); self.instr_jr(j)}
            0x39 => self.instr_add_hl_reg16(R16::SP),
            0x3A => self.instr_ld_a_r16(R16::SP),
            0x3B => self.instr_dec_16(R16::SP),
            0x3C => self.instr_inc_8(Reg::A),
            0x3D => self.instr_dec_8(Reg::A),
            0x3E => self.instr_ld_r8_d8(Reg::A),
            0x3F => self.instr_ccf(),

            0x40 => self.instr_ld(Reg::B , Reg::B), 0x41 => self.instr_ld(Reg::B , Reg::C), 0x42 => self.instr_ld(Reg::B , Reg::D) , 0x43 => self.instr_ld(Reg::B , Reg::E),
            0x44 => self.instr_ld(Reg::B , Reg::H), 0x45 => self.instr_ld(Reg::B , Reg::L), 0x46 => self.instr_ld(Reg::B , Reg::HL), 0x47 => self.instr_ld(Reg::B , Reg::A),
            0x48 => self.instr_ld(Reg::C , Reg::B), 0x49 => self.instr_ld(Reg::C , Reg::C), 0x4A => self.instr_ld(Reg::C , Reg::D) , 0x4B => self.instr_ld(Reg::C , Reg::E),
            0x4C => self.instr_ld(Reg::C , Reg::H), 0x4D => self.instr_ld(Reg::C , Reg::L), 0x4E => self.instr_ld(Reg::C , Reg::HL), 0x4F => self.instr_ld(Reg::C , Reg::A),
            0x50 => self.instr_ld(Reg::D , Reg::B), 0x51 => self.instr_ld(Reg::D , Reg::C), 0x52 => self.instr_ld(Reg::D , Reg::D) , 0x53 => self.instr_ld(Reg::D , Reg::E),
            0x54 => self.instr_ld(Reg::D , Reg::H), 0x55 => self.instr_ld(Reg::D , Reg::L), 0x56 => self.instr_ld(Reg::D , Reg::HL), 0x57 => self.instr_ld(Reg::D , Reg::A),
            0x58 => self.instr_ld(Reg::E , Reg::B), 0x59 => self.instr_ld(Reg::E , Reg::C), 0x5A => self.instr_ld(Reg::E , Reg::D) , 0x5B => self.instr_ld(Reg::E , Reg::E),
            0x5C => self.instr_ld(Reg::E , Reg::B), 0x5D => self.instr_ld(Reg::E , Reg::C), 0x5E => self.instr_ld(Reg::E , Reg::D) , 0x5F => self.instr_ld(Reg::E , Reg::E),
            0x60 => self.instr_ld(Reg::H , Reg::B), 0x61 => self.instr_ld(Reg::H , Reg::C), 0x62 => self.instr_ld(Reg::H , Reg::D) , 0x63 => self.instr_ld(Reg::H , Reg::E),
            0x64 => self.instr_ld(Reg::H , Reg::B), 0x65 => self.instr_ld(Reg::H , Reg::L), 0x66 => self.instr_ld(Reg::H , Reg::HL), 0x67 => self.instr_ld(Reg::H , Reg::A),
            0x68 => self.instr_ld(Reg::L , Reg::B), 0x69 => self.instr_ld(Reg::L , Reg::C), 0x6A => self.instr_ld(Reg::L , Reg::D) , 0x6B => self.instr_ld(Reg::L , Reg::E),
            0x6C => self.instr_ld(Reg::L , Reg::B), 0x6D => self.instr_ld(Reg::L , Reg::L), 0x6E => self.instr_ld(Reg::L , Reg::HL), 0x6F => self.instr_ld(Reg::L , Reg::A),
            0x70 => self.instr_ld(Reg::HL, Reg::B), 0x71 => self.instr_ld(Reg::HL, Reg::C), 0x72 => self.instr_ld(Reg::HL, Reg::D) , 0x73 => self.instr_ld(Reg::HL, Reg::E),
            0x74 => self.instr_ld(Reg::HL, Reg::H), 0x75 => self.instr_ld(Reg::HL, Reg::L), 0x76 => self.instr_halt()              , 0x77 => self.instr_ld(Reg::HL, Reg::A),
            0x78 => self.instr_ld(Reg::A , Reg::B), 0x79 => self.instr_ld(Reg::A , Reg::C), 0x7A => self.instr_ld(Reg::A , Reg::D) , 0x7B => self.instr_ld(Reg::A , Reg::E),
            0x7C => self.instr_ld(Reg::A , Reg::H), 0x7D => self.instr_ld(Reg::A , Reg::L), 0x7E => self.instr_ld(Reg::A , Reg::HL), 0x7F => self.instr_ld(Reg::A , Reg::A),
            
            0x80 => self.instr_add(MathReg::R(Reg::B )), 0x81 => self.instr_add(MathReg::R(Reg::C)),
            0x82 => self.instr_add(MathReg::R(Reg::D )), 0x83 => self.instr_add(MathReg::R(Reg::E)),
            0x84 => self.instr_add(MathReg::R(Reg::H )), 0x85 => self.instr_add(MathReg::R(Reg::L)),
            0x86 => self.instr_add(MathReg::R(Reg::HL)), 0x87 => self.instr_add(MathReg::R(Reg::A)),

            0x88 => self.instr_adc(MathReg::R(Reg::B )), 0x89 => self.instr_adc(MathReg::R(Reg::C)),
            0x8A => self.instr_adc(MathReg::R(Reg::D )), 0x8B => self.instr_adc(MathReg::R(Reg::E)),
            0x8C => self.instr_adc(MathReg::R(Reg::H )), 0x8D => self.instr_adc(MathReg::R(Reg::L)),
            0x8E => self.instr_adc(MathReg::R(Reg::HL)), 0x8F => self.instr_adc(MathReg::R(Reg::A)),

            0x90 => self.instr_sub(MathReg::R(Reg::B )), 0x91 => self.instr_sub(MathReg::R(Reg::C)),
            0x92 => self.instr_sub(MathReg::R(Reg::D )), 0x93 => self.instr_sub(MathReg::R(Reg::E)),
            0x94 => self.instr_sub(MathReg::R(Reg::H )), 0x95 => self.instr_sub(MathReg::R(Reg::L)),
            0x96 => self.instr_sub(MathReg::R(Reg::HL)), 0x97 => self.instr_sub(MathReg::R(Reg::A)),

            0x98 => self.instr_sbc(MathReg::R(Reg::B )), 0x99 => self.instr_sbc(MathReg::R(Reg::C)),
            0x9A => self.instr_sbc(MathReg::R(Reg::D )), 0x9B => self.instr_sbc(MathReg::R(Reg::E)),
            0x9C => self.instr_sbc(MathReg::R(Reg::H )), 0x9D => self.instr_sbc(MathReg::R(Reg::L)),
            0x9E => self.instr_sbc(MathReg::R(Reg::HL)), 0x9F => self.instr_sbc(MathReg::R(Reg::A)),

            0xA0 => self.instr_and(MathReg::R(Reg::B )), 0xA1 => self.instr_and(MathReg::R(Reg::C)),
            0xA2 => self.instr_and(MathReg::R(Reg::D )), 0xA3 => self.instr_and(MathReg::R(Reg::E)),
            0xA4 => self.instr_and(MathReg::R(Reg::H )), 0xA5 => self.instr_and(MathReg::R(Reg::L)),
            0xA6 => self.instr_and(MathReg::R(Reg::HL)), 0xA7 => self.instr_and(MathReg::R(Reg::A)),

            0xA8 => self.instr_xor(MathReg::R(Reg::B )), 0xA9 => self.instr_xor(MathReg::R(Reg::C)),
            0xAA => self.instr_xor(MathReg::R(Reg::D )), 0xAB => self.instr_xor(MathReg::R(Reg::E)),
            0xAC => self.instr_xor(MathReg::R(Reg::H )), 0xAD => self.instr_xor(MathReg::R(Reg::L)),
            0xAE => self.instr_xor(MathReg::R(Reg::HL)), 0xAF => self.instr_xor(MathReg::R(Reg::A)),

            0xB0 => self.instr_or (MathReg::R(Reg::B )), 0xB1 => self.instr_or (MathReg::R(Reg::C)),
            0xB2 => self.instr_or (MathReg::R(Reg::D )), 0xB3 => self.instr_or (MathReg::R(Reg::E)),
            0xB4 => self.instr_or (MathReg::R(Reg::H )), 0xB5 => self.instr_or (MathReg::R(Reg::L)),
            0xB6 => self.instr_or (MathReg::R(Reg::HL)), 0xB7 => self.instr_or (MathReg::R(Reg::A)),

            0xB8 => self.instr_cp (MathReg::R(Reg::B )), 0xB9 => self.instr_cp (MathReg::R(Reg::C)),
            0xBA => self.instr_cp (MathReg::R(Reg::D )), 0xBB => self.instr_cp (MathReg::R(Reg::E)),
            0xBC => self.instr_cp (MathReg::R(Reg::H )), 0xBD => self.instr_cp (MathReg::R(Reg::L)),
            0xBE => self.instr_cp (MathReg::R(Reg::HL)), 0xBF => self.instr_cp (MathReg::R(Reg::A)),
            
            0xC0 => {let j = !self.regs.get_flag(Flag::Z); self.instr_retc(j)}
            0xC1 => self.instr_pop(R16::BC),
            0xC2 => {let j = !self.regs.get_flag(Flag::Z); self.instr_jp  (j)}
            0xC3 => self.instr_jp (true ),
            0xC4 => {let j = !self.regs.get_flag(Flag::Z); self.instr_call(j)}
            0xC5 => self.instr_push(R16::BC),
            0xC6 => self.instr_add(MathReg::Imm),
            0xC7 => self.instr_rst(0x00),
            0xC8 => {let j =  self.regs.get_flag(Flag::Z); self.instr_retc(j)} 
            0xC9 => self.instr_ret(false),
            0xCA => {let j =  self.regs.get_flag(Flag::Z); self.instr_jp  (j)}
            0xCB => self.run_extended(),
            0xCC => {let j = self.regs.get_flag(Flag::Z); self.instr_call(j)}
            0xCD => self.instr_call(true),
            0xCE => self.instr_adc(MathReg::Imm),
            0xCF => self.instr_rst(0x08),
            0xD0 => {let j = !self.regs.get_flag(Flag::C); self.instr_retc(j)}
            0xD1 => self.instr_pop(R16::DE),
            0xD2 => {let j = !self.regs.get_flag(Flag::C); self.instr_jp  (j)}
            0xD3 => self.instr_invalid(),
            0xD4 => {let j =  self.regs.get_flag(Flag::C); self.instr_call(j)}
            0xD5 => self.instr_push(R16::DE),
            0xD6 => self.instr_sub(MathReg::Imm),
            0xD7 => self.instr_rst(0x10),
            0xD8 => {let j =  self.regs.get_flag(Flag::C); self.instr_retc(j)}
            0xD9 => self.instr_ret(true ),
            0xDA => {let j =  self.regs.get_flag(Flag::C); self.instr_jp  (j)}
            0xDB => self.instr_invalid(),
            0xDC => {let j =  self.regs.get_flag(Flag::C); self.instr_call(j)}
            0xDD => self.instr_invalid(),
            0xDE => self.instr_sbc(MathReg::Imm),
            0xDF => self.instr_rst(0x18),
            
            0xE0 => self.instr_ldh_a8_a(),
            0xE1 => self.instr_pop(R16::HL),
            0xE2 => self.instr_ldh_c_a(),
            0xE3 => self.instr_invalid(),
            0xE4 => self.instr_invalid(),
            0xE5 => self.instr_push(R16::HL),
            0xE6 => self.instr_and(MathReg::Imm),
            0xE7 => self.instr_rst(0x20),
            0xE8 => self.instr_add_sp_r8(),
            0xE9 => self.instr_jp_hl(),
            0xEA => self.instr_ld_a16_a(),
            0xEB => self.instr_invalid(),
            0xEC => self.instr_invalid(),
            0xED => self.instr_invalid(),
            0xEE => self.instr_xor(MathReg::Imm),
            0xEF => self.instr_rst(0x28),
            0xF0 => self.instr_ldh_a_a8(),
            0xF1 => self.instr_pop(R16::SP),
            0xF2 => self.instr_ldh_a_c(),
            0xF3 => self.instr_di(),
            0xF4 => self.instr_invalid(),
            0xF5 => self.instr_push(R16::SP),
            0xF6 => self.instr_or(MathReg::Imm),
            0xF7 => self.instr_rst(0x30),
            0xF8 => self.instr_ld_hl_sp_r8(),
            0xF9 => self.instr_ld_sp_hl(),
            0xFA => self.instr_ld_a_a16(),
            0xFB => self.instr_ie(),
            0xFC => self.instr_invalid(),
            0xFD => self.instr_invalid(),
            0xFE => self.instr_cp(MathReg::Imm),
            0xFF => self.instr_rst(0x38),
            _ => unreachable!("Unimplemented instruction?"),
        }
    }
    
    fn instr_invalid(&mut self) -> i64 {
        self.status = State::Hang;0
    }
    
    // Mnemonic: LD r16,d16
    // Full Name: Load <r16>, d16
    // Description: Sets the given 16 bit register to a 2 byte immediate value.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read 
    fn instr_ld_r16_d16(&mut self, reg: R16) -> i64 {
        let val = self.read_u16_cycle();
        self.regs.set_reg_16(reg, val);
        8
    }

    // Mnemonic: LD (r16),A
    // Full Name: Load (<r16>), A
    // Description: Sets the address referenced by the 16 bit register r16 to A.
    // Affected Flags: ----
    // Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
    // Timing: Write     
    fn instr_ld_r16_a(&mut self, reg: R16) -> i64 {
        let a = self.regs.a;
        let reg = match reg {
            R16::BC => self.regs.bc,
            R16::DE => self.regs.de,
            R16::HL => {let v = self.regs.hl;self.regs.hl += 1;v}
            R16::SP => {let v = self.regs.hl;self.regs.hl -= 1;v}
        };
        
        self.write_cycle(reg, a);
        4
    }
    
    // Mnemonic: INC r16
    // Full Name: Increment r16
    // Description: Increments the given 16-bit register.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Internal Delay. 
    fn instr_inc_16(&mut self, reg: R16) -> i64 {
        self.update(4);
        let v = self.regs.get_reg_16(&reg).wrapping_add(1);
        self.regs.set_reg_16(reg, v);
        4
    }
    
    
    // Mnemonic: INC reg8
    // Full Name: Increment reg8
    // Description: Increments the given 8-bit register (or hl) reg8.
    // Affected Flags: Z (set|res), N (res), H (set|res)
    // Remarks: Zero is set if reg8 overflows, Half carry is set if there is a half carry between reg8 and 1.
    // If a flags conditions aren't met, it is instead reset. 
    // Timing: "Instant" or "Read, Write" 
    fn instr_inc_8(&mut self, reg: Reg) -> i64 {
        self.regs.f &= Flag::C.to_mask();
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle(val.wrapping_add(1));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, val.wrapping_add(1));0} 
        };
        
        if val == 0xFF {self.regs.set_flag(Flag::Z)};
        if get_hca(val, 1) {self.regs.set_flag(Flag::H)};
        cycles
    }
    
    // Mnemonic: DEC reg8
    // Full Name: Decrement reg8
    // Description: Decrements the given 8-bit register (or hl) reg8.
    // Affected Flags: Z (set|res), N (set), H (set|res)
    // Remarks: Zero is set if reg8 is 1, Half carry is set if reg8 & 0xf == 0. 
    // If a flags conditions aren't met, it is instead reset. 
    // Timing: "Instant" or "Read, Write" 
    fn instr_dec_8(&mut self, reg: Reg) -> i64 {
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle(val.wrapping_sub(1));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, val.wrapping_sub(1));0} 
        };
        
        self.regs.f = self.regs.f & Flag::C.to_mask();
        if val == 1 {self.regs.set_flag(Flag::Z)};
        self.regs.set_flag(Flag::N);
        if (val & 0xF) == 0 {self.regs.set_flag(Flag::H)};
        cycles
    }
    
    // Mnemonic: LD reg8,d8
    // Full Name: Load reg8,d8
    // Description: Loads 8-bit unsigned data d8 into the register (or hl) reg8.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: "Read" or "Read Write"
    fn instr_ld_r8_d8(&mut self, reg: Reg) -> i64 {
        let val = self.read_ipc_cycle();
        4 + match reg {
            Reg::HL => {self.write_hl_cycle(val);4}
            r => {self.regs.set_reg(r, val);0}
        }
    }
    
    // Mnemonic: RLCA
    // Full Name: Rotate Left Circular A
    // Description: Sets A to, (A << 1) | (A >> 7)
    // Affected Flags: Z (res), N (res), H (res), C (set|res)
    // Remarks: Carry is set if bit 7 is set, otherwise it is reset.
    // Timing: Instant.
    fn instr_rlca(&mut self) -> i64 {
        self.regs.a = (self.regs.a << 1) | (self.regs.a >> 7);
        self.regs.res_all_flags();

        if (self.regs.a & 0x80) > 0 {
            self.regs.set_flag(Flag::C);
        }
        0
    }
    
    // Mnemonic: LD (a16),SP
    // Full Name: Load (a16),sp
    // Description: Loads sp into the address pointed to by 16-bit unsigned data a16.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read, Write, Write
    fn instr_ld_a16_sp(&mut self) -> i64 {
        let address = self.read_u16_cycle();
        let sp = self.regs.sp;
        self.write_u16_cycle(address, sp);
        16
    }
    
    // Mnemonic: ADD HL,R16
    // Full Name: Add HL,R16
    // Description: Adds 16-bit register R16 to HL storing the result in HL
    // Affected Flags: N (res), H (set|res), C (set|res)
    // Remarks: Half Carry is set if there is a carry between bits 11 and 12. Carry is set if there is a carry out. Otherwise reset Half Carry or Carry respectively
    // Timing: Internal Delay
    fn instr_add_hl_reg16(&mut self, reg: R16) -> i64 {
        let val = self.regs.get_reg_16(&reg);
        let res = self.regs.hl.wrapping_add(val);
        self.regs.f &= Flag::Z.to_mask();
        if (((self.regs.hl & 0xFFF) + (val & 0xFFF)) & 0x1000) == 0x1000 {
            self.regs.set_flag(Flag::H);
        }

        if res < self.regs.hl {
            self.regs.set_flag(Flag::C);
        }
        self.update(4);
        self.regs.hl = res;
        4
    }
    
    // Mnemonic: LD A,(r16)
    // Full Name: Load A, (<r16>)
    // Description: Sets A to the address referenced by the 16 bit register r16.
    // Affected Flags: ----
    // Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
    // Timing: Write     
    fn instr_ld_a_r16(&mut self, reg: R16) -> i64 {
        let reg = match reg {
            R16::BC => self.regs.bc,
            R16::DE => self.regs.de,
            R16::HL => {let v = self.regs.hl;self.regs.hl += 1;v}
            R16::SP => {let v = self.regs.hl;self.regs.hl -= 1;v}
        };
        
        let val = self.read_cycle(reg);
        self.regs.set_reg(Reg::A, val);
        4
    }
    
    // Mnemonic: DEC r16
    // Full Name: Decrement r16
    // Description: Decrements the given 16-bit register.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Internal Delay. 
    fn instr_dec_16(&mut self, reg: R16) -> i64 {
        self.update(4);
        let v = self.regs.get_reg_16(&reg).wrapping_sub(1);
        self.regs.set_reg_16(reg, v);
        4
    }

    
    // Mnemonic: RRCA
    // Full Name: Rotate Right Circular A
    // Description: Sets A to, (A >> 1) | (A << 7)
    // Affected Flags: Z (res), N (res), H (res), C (set|res)
    // Remarks: Carry is set if bit 0 is set, otherwise it is reset.
    // Timing: Instant.
    fn instr_rrca(&mut self) -> i64 {
        self.regs.a = (self.regs.a >> 1) | (self.regs.a << 7);
        self.regs.res_all_flags();
        if (self.regs.a & 0x01) == 0x01 {
            self.regs.set_flag(Flag::C);
        }

        0
    }
    
    // Mnemonic: RLA
    // Full Name: Rotate Left A
    // Description: Sets A to, (A << 1) | (c_in)
    // Affected Flags: Z (res), N (res), H (res), C (set|res)
    // Remarks: Carry is set if bit 7 is set, otherwise it is reset.
    // Timing: Instant.
    fn instr_rla(&mut self) -> i64 {
        let a = self.regs.a;
        self.regs.a = (a << 1) | if self.regs.get_flag(Flag::C) {1} else {0};
        self.regs.res_all_flags();
        if a & 0x80 == 0x80 {
            self.regs.set_flag(Flag::C);
        }

        0
    } 

    // Mnemonic: RRA
    // Full Name: Rotate Right A
    // Description: Sets A to, (A >> 1) | (c_in)
    // Affected Flags: Z (res), N (res), H (res), C (set|res)
    // Remarks: Carry is set if bit 0 is set, otherwise it is reset.
    // Timing: Instant.
    fn instr_rra(&mut self) -> i64 {
        let a = self.regs.a;
        self.regs.a=  (a >> 1) | if self.regs.get_flag(Flag::C) {0x80} else {0};
        self.regs.res_all_flags();
        if a & 0x01 == 0x01 {
            self.regs.set_flag(Flag::C);
        }

        0
    } 
    
    // Mnemonic: DAA
    // Full Name: ???
    // Description: ???
    // Affected Flags: Z (set|res), H (res), C (-|set)
    // Remarks: Confusing
    // Timing: Instant.
    fn instr_daa(&mut self) -> i64 {
        let mut res = self.regs.a as i32;
        if self.regs.get_flag(Flag::N) {
            if self.regs.get_flag(Flag::H) {
                res = (res - 6) & 0xFF;
            }
            
            if self.regs.get_flag(Flag::C) {
                res -= 0x60;
            }
        } else {
            if self.regs.get_flag(Flag::H) || (res & 0xF) > 9 {
                res += 0x06;
            }
            
            if self.regs.get_flag(Flag::C) || res > 0x9F {
                res += 0x60;
            }
        };
        
        self.regs.f &= Flag::Z.to_mask() | Flag::C.to_mask();
        if (res & 0x100) == 0x100 {
            self.regs.set_flag(Flag::C);
        }

        self.regs.a = res as u8;
        
        if self.regs.a == 0 {
            self.regs.set_flag(Flag::Z);
        }

        0
    }
    
    // Mnemonic: CPL
    // Full Name: Complement
    // Description: Bitwise complements A
    // Affected Flags: N (set), H (set)
    // Remarks: ----
    // Timing: Instant.
    fn instr_cpl(&mut self) -> i64 {
        self.regs.a = !self.regs.a;
        self.regs.set_flag(Flag::N);
        self.regs.set_flag(Flag::H);
        0
    }
    
    // Mnemonic: SCF
    // Full Name: Set Carry Flag
    // Description: Sets the carry flag
    // Affected Flags: N (res), H (res), C (set)
    // Remarks: ----
    // Timing: Instant.
    fn instr_scf(&mut self) -> i64 {
        self.regs.f = Flag::Z.to_mask();
        self.regs.set_flag(Flag::C);
        0
    }
    
    // Mnemonic: CCF
    // Full Name: Complement Carry Flag
    // Description: Complements the carry flag
    // Affected Flags: N (res), H (res), C (^C)
    // Remarks: ----
    // Timing: Instant.
    fn instr_ccf(&mut self) -> i64 {
        self.regs.f &= Flag::Z.to_mask() | Flag::C.to_mask();
        self.regs.f ^= Flag::C.to_mask();
        0
    }
        
    // Mnemonic: ADD
    // Full Name: Add 
    // Description: Adds the given reg (or hl, or imm) r to A and stores the result into A
    // Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_add(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.a;
        let (cycles, val) = self.get_math_reg(reg);

        self.regs.a = a.wrapping_add(val);
        self.regs.res_all_flags();
        if self.regs.a == 0 {
            self.regs.set_flag(Flag::Z);
        }

        if get_hca(a, self.regs.a) {
            self.regs.set_flag(Flag::H);
        }

        if self.regs.a < a {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }
    
    
    // Mnemonic: ADC
    // Full Name: Add with carry 
    // Description: Adds the given reg (or hl, or imm) r and carry to A and stores the result into A
    // Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_adc(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.a;
        let c_in = self.regs.get_flag(Flag::C);
        let (cycles, val) = self.get_math_reg(reg);
        
        self.regs.a = a.wrapping_add(val).wrapping_add(c_in as u8);

        self.regs.res_all_flags();
        if self.regs.a == 0 {
            self.regs.set_flag(Flag::Z);
        }
        if (a & 0xF) + (val & 0xF) + c_in as u8 > 0xF {
            self.regs.set_flag(Flag::H);
        }

        if (a.wrapping_add(val) as u16).wrapping_add(c_in as u16) > 0xFF {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }

    
    // Mnemonic: SUB
    // Full Name: Sub 
    // Description: Subtracts the given reg (or hl, or imm) r from A and stores the result into A
    // Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_sub(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.get_reg(&Reg::A);
        let (cycles, val) = self.get_math_reg(reg);
        
        let res = a.wrapping_sub(val);
        self.regs.set_reg(Reg::A, res);
        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }
        
        self.regs.set_flag(Flag::N);

        if (a & 0xF) < (val & 0xF) {
            self.regs.set_flag(Flag::H);
        }
        
        if res > a {
            self.regs.set_flag(Flag::C);
        }
        
        cycles
    }
    
    // Mnemonic: SBC
    // Full Name: Sub with carry 
    // Description: Subtracts the given reg (or hl, or imm) r and carry from A and stores the result into A
    // Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_sbc(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.get_reg(&Reg::A);
        let c_in = self.regs.get_flag(Flag::C);
        let (cycles, val) = self.get_math_reg(reg);
        
        let res = (a.wrapping_sub(val) as u16).wrapping_sub(c_in as u16);
        self.regs.set_reg(Reg::A, res as u8);
        self.regs.res_all_flags();
        if (res & 0xFF) == 0 {
            self.regs.set_flag(Flag::Z);
        }

        self.regs.set_flag(Flag::N);
        
        if (a & 0xF) < ((val & 0xF) + c_in as u8) {
            self.regs.set_flag(Flag::H);
        }

        if res > 0xFF {
            self.regs.set_flag(Flag::C);
        }
        
        cycles
    }
    
    // Mnemonic: AND
    // Full Name: Bitwise And
    // Description: Preforms bitwise AND on the given reg (or hl, or imm) r and A, storing the result into A
    // Affected Flags: Z (set|res), N (res), H (set), C (res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_and(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.get_reg(&Reg::A);
        let (cycles, val) = self.get_math_reg(reg);
        
        let res = a & val;
        self.regs.set_reg(Reg::A, res);
        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }
        
        self.regs.set_flag(Flag::H);
        cycles
    }

    // Mnemonic: XOR
    // Full Name: Bitwise Xor
    // Description: Preforms bitwise XOR on the given reg (or hl, or imm) r and A, storing the result into A
    // Affected Flags: Z (set|res), N (res), H (res), C (res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_xor(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.get_reg(&Reg::A);
        let (cycles, val) = self.get_math_reg(reg);
        
        let res = a ^ val;
        self.regs.set_reg(Reg::A, res);
        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }
        
        cycles
    }
    
    // Mnemonic: OR
    // Full Name: Bitwise Or
    // Description: Preforms bitwise Or on the given reg (or hl, or imm) r and A, storing the result into A
    // Affected Flags: Z (set|res), N (res), H (res), C (res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_or(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.get_reg(&Reg::A);
        let (cycles, val) = self.get_math_reg(reg);
        
        let res = a | val;
        self.regs.set_reg(Reg::A, res);
        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }
        
        cycles
    }
    
    // Mnemonic: CP
    // Full Name: Compare 
    // Description: Subtracts the given reg (or hl, or imm) r from A discarding the result.
    // Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
    // Remarks: ----
    // Timing: Read or Instant
    fn instr_cp(&mut self, reg: MathReg) -> i64 {
        let a = self.regs.get_reg(&Reg::A);
        let (cycles, val) = self.get_math_reg(reg);
        
        let res = a.wrapping_sub(val);
        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }
        
        self.regs.set_flag(Flag::N);

        if (a & 0xF) < (val & 0xF) {
            self.regs.set_flag(Flag::H);
        }
        
        if res > a {
            self.regs.set_flag(Flag::C);
        }
        
        cycles

    }

    // Mnemonic: RET <COND>
    // Full Name: Return <COND>
    // Description: Returns Conditionally. (NZ/Z/NC/C)
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read, Internal Delay
    fn instr_retc(&mut self, jump: bool) -> i64 {
        self.update(4);
        if !jump {
            4
        } else {
            let addr = self.read_pop_16_cycle();
            self.regs.pc = addr;
            self.update(4);
            16
        }
    }
    
    // Mnemonic: POP <Reg-16>
    // Full Name: Pop <Reg-16>
    // Description: Pops the 16-bit register Reg-16 off of the stack.
    // Affected Flags: ---- or Z (set|res), N (set|res), H (set|res), C (set|res)
    // Remarks: ----
    // Timing: Read, Read
    fn instr_pop(&mut self, reg: R16) -> i64 {
        let val = self.read_pop_16_cycle();
        match reg {
            R16::SP => self.regs.s_af(val),
            r => self.regs.set_reg_16(r, val),
        };
        
        8
    }
    
    // Mnemonic: CALL <COND> | CALL
    // Full Name: Call <COND> | Call
    // Description: Calls (possibly conditionally (NZ/Z/NC/C))
    // Affected Flags: ----
    // Remarks: ----
    // Timing: "Read, Read" | "Read, Read, Write, Write"
    fn instr_call(&mut self, jump: bool) -> i64 {
        let addr = self.read_u16_cycle();
        if jump {
            let pc = self.regs.pc;
            self.write_push_16_cycle(pc);
            self.regs.pc = addr;
            16
        } else {
            8
        }
    }
    
    // Mnemonic: PUSH <Reg-16>
    // Full Name: Push <Reg-16>
    // Description: Pushes the 16-bit register Reg-16 onto the stack.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Delay, Write, Write
    fn instr_push(&mut self, reg: R16) -> i64 {
        let val = match reg {
            R16::SP => self.regs.g_af(),
            r => self.regs.get_reg_16(&r),
        };
        self.update(4);
        self.write_push_16_cycle(val);
        12        
    }
    
    // Mnemonic: RST <addr>
    // Full Name: Reset <addr>
    // Description: calls <addr>.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Delay, Write, Write
    fn instr_rst(&mut self, addr: u16) -> i64 {
        self.update(4);
        let pc = self.regs.pc;
        self.write_push_16_cycle(pc);
        self.regs.pc = addr;
        12
    }
    
    // Mnemonic: RET/RETI
    // Full Name: Return / Return enable Interrupts
    // Description: Returns unconditionally, if it's a reti instruction it will also enable IME.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read, Internal Delay
    fn instr_ret(&mut self, reti: bool) -> i64 {
        let addr = self.read_pop_16_cycle();
        self.regs.pc = addr;
        if reti {
            self.ime = true;
        }
        self.update(4);
        12
    }

    // Mnemonic: ldh (a8),A
    // Full Name: Load High (a8),A
    // Description: loads A into (0xFF00 | a8).
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Write
    fn instr_ldh_a8_a(&mut self) -> i64 {
        let addr = 0xFF00 | self.read_ipc_cycle() as u16;
        let a = self.regs.get_reg(&Reg::A);
        self.write_cycle(addr, a);
        8
    }

    // Mnemonic: ldh (c),A
    // Full Name: Load High (c),A
    // Description: loads A into (0xFF00 | c).
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Write
    fn instr_ldh_c_a(&mut self) -> i64 {
        let addr = 0xFF00 | self.regs.get_reg(&Reg::C) as u16;
        let a = self.regs.get_reg(&Reg::A);
        self.write_cycle(addr, a);
        4
    }

    // Mnemonic: ldh (a8),A
    // Full Name: Load High (a8),A
    // Description: loads A into (0xFF00 | a8).
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read
    fn instr_ldh_a_a8(&mut self) -> i64 {
        let addr = 0xFF00 | self.read_ipc_cycle() as u16;
        let val = self.read_cycle(addr);
        self.regs.set_reg(Reg::A, val);
        8
    }

    // Mnemonic: ldh (c),A
    // Full Name: Load High (c),A
    // Description: loads A into (0xFF00 | c).
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read
    fn instr_ldh_a_c(&mut self) -> i64 {
        let addr = 0xFF00 | self.regs.get_reg(&Reg::C) as u16;
        let val = self.read_cycle(addr);
        self.regs.set_reg(Reg::A, val);
        4
    }

    // Mnemonic: DI
    // Full Name: Disable Interrupts
    // Description: Disables interrupts.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Instant
    fn instr_di(&mut self) -> i64 {
        self.ime = false;
        0
    }

    // Mnemonic: EI
    // Full Name: Enable Interrupts
    // Description: Enables interrupts.
    // Affected Flags: ----
    // Remarks: Interrupt enabling is delayed by 4-TCycles.
    // Timing: Instant (delayed affect)
    fn instr_ie(&mut self) -> i64 {
        self.ie = true;
        0
    }

    // Mnemonic: JP (HL)
    // Full Name: Jump (HL)
    // Description: Jumps to HL.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Instant
    fn instr_jp_hl(&mut self) -> i64 {
        self.regs.pc = self.regs.hl;
        0
    }

    // Mnemonic: LD (HL),SP+r8
    // Full Name: Load (HL), SP+r8
    // Description: Loads SP+signed 8-bit value r8 into HL
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Internal Delay
    fn instr_ld_hl_sp_r8(&mut self) -> i64 {
        let r8 = self.read_ipc_cycle() as i8;
        self.update(4);
        self.regs.res_all_flags();

        if ((self.regs.sp & 0x0F) + (r8 as u16 & 0x0F)) > 0x0F {
            self.regs.set_flag(Flag::H);
        }

        if (((self.regs.sp) & 0xFF) + (r8 as u16 & 0xFF)) > 0xFF {
            self.regs.set_flag(Flag::C);
        }

        self.regs.sp = r8 as u16;
        8
    }

    // Mnemonic: LD A,(a16)
    // Full Name: Load A,(a16)
    // Description: Load the value pointed at by 16-bit unsigned data a16 into A
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read, Read
    fn instr_ld_a_a16(&mut self) -> i64 {
        let addr = self.read_u16_cycle();
        let val = self.read_cycle(addr);
        self.regs.set_reg(Reg::A, val);
        12
    }

    // Mnemonic: LD (a16),A
    // Full Name: Load (a16),A
    // Description: Load A into the value pointed at by 16-bit unsigned data a16.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Read, Write
    fn instr_ld_a16_a(&mut self) -> i64 {
        let addr = self.read_u16_cycle();
        let a = self.regs.get_reg(&Reg::A);
        self.write_cycle(addr, a);
        12
    }

    // Mnemonic: LD SP,HL
    // Full Name: Load sp,hl
    // Description: Load hl into sp.
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Internal delay
    fn instr_ld_sp_hl(&mut self) -> i64 {
        let hl = self.regs.hl;
        self.update(4);
        self.regs.sp = hl;
        4
    }

    // Mnemonic: ADD SP,r8
    // Full Name: Add sp, r8
    // Description: Add 8-bit signed data r8 into sp
    // Affected Flags: ----
    // Remarks: ----
    // Timing: Read, Internal Delay, Internal Delay
    fn instr_add_sp_r8(&mut self) -> i64 {
        let r8 = self.read_ipc_cycle() as i8;
        self.update(8);
        self.regs.res_all_flags();
        if ((self.regs.sp & 0x0F) + (r8 as u16 & 0x0F)) > 0x0F {
            self.regs.set_flag(Flag::H);
        }
        if ((self.regs.sp & 0xFF) + (r8 as u16 & 0xFF)) > 0xFF {
            self.regs.set_flag(Flag::C);
        }
        self.regs.sp += r8 as u16;

        12
    }

    fn run_extended(&mut self) -> i64 {
        let op = self.read_ipc_cycle();
        4 + match op {
            0x00 => self.instr_rlc (Reg::B), 0x01 => self.instr_rlc (Reg::C), 0x02 => self.instr_rlc (Reg::D) , 0x03 => self.instr_rlc (Reg::E),
            0x04 => self.instr_rlc (Reg::H), 0x05 => self.instr_rlc (Reg::L), 0x06 => self.instr_rlc (Reg::HL), 0x07 => self.instr_rlc (Reg::A),
            0x08 => self.instr_rrc (Reg::B), 0x09 => self.instr_rrc (Reg::C), 0x0A => self.instr_rrc (Reg::D) , 0x0B => self.instr_rrc (Reg::E),
            0x0C => self.instr_rrc (Reg::H), 0x0D => self.instr_rrc (Reg::L), 0x0E => self.instr_rrc (Reg::HL), 0x0F => self.instr_rrc (Reg::A),
            
            0x10 => self.instr_rl  (Reg::B), 0x11 => self.instr_rl  (Reg::C), 0x12 => self.instr_rl  (Reg::D) , 0x13 => self.instr_rl  (Reg::E),
            0x14 => self.instr_rl  (Reg::H), 0x15 => self.instr_rl  (Reg::L), 0x16 => self.instr_rl  (Reg::HL), 0x17 => self.instr_rl  (Reg::A),
            0x18 => self.instr_rr  (Reg::B), 0x19 => self.instr_rr  (Reg::C), 0x1A => self.instr_rr  (Reg::D) , 0x1B => self.instr_rr  (Reg::E),
            0x1C => self.instr_rr  (Reg::H), 0x1D => self.instr_rr  (Reg::L), 0x1E => self.instr_rr  (Reg::HL), 0x1F => self.instr_rr  (Reg::A),

            0x20 => self.instr_sla (Reg::B), 0x21 => self.instr_sla (Reg::C), 0x22 => self.instr_sla (Reg::D) , 0x23 => self.instr_sla (Reg::E),
            0x24 => self.instr_sla (Reg::H), 0x25 => self.instr_sla (Reg::L), 0x26 => self.instr_sla (Reg::HL), 0x27 => self.instr_sla (Reg::A),
            0x28 => self.instr_sra (Reg::B), 0x29 => self.instr_sra (Reg::C), 0x2A => self.instr_sra (Reg::D) , 0x2B => self.instr_sra (Reg::E),
            0x2C => self.instr_sra (Reg::H), 0x2D => self.instr_sra (Reg::L), 0x2E => self.instr_sra (Reg::HL), 0x2F => self.instr_sra (Reg::A),
            
            0x30 => self.instr_swap(Reg::B), 0x31 => self.instr_swap(Reg::C), 0x32 => self.instr_swap(Reg::D) , 0x33 => self.instr_swap(Reg::E),
            0x34 => self.instr_swap(Reg::H), 0x35 => self.instr_swap(Reg::L), 0x36 => self.instr_swap(Reg::HL), 0x37 => self.instr_swap(Reg::A),
            0x38 => self.instr_srl (Reg::B), 0x39 => self.instr_srl (Reg::C), 0x3A => self.instr_srl (Reg::D) , 0x3B => self.instr_srl (Reg::E),
            0x3C => self.instr_srl (Reg::H), 0x3D => self.instr_srl (Reg::L), 0x3E => self.instr_srl (Reg::HL), 0x3F => self.instr_srl (Reg::A),
            
            0x40 => self.instr_bit(Reg::B , 0x01), 0x41 => self.instr_bit(Reg::B , 0x02), 0x42 => self.instr_bit(Reg::B , 0x04), 0x43 => self.instr_bit(Reg::B , 0x08),
            0x44 => self.instr_bit(Reg::B , 0x10), 0x45 => self.instr_bit(Reg::B , 0x20), 0x46 => self.instr_bit(Reg::B , 0x40), 0x47 => self.instr_bit(Reg::B , 0x80),
            0x48 => self.instr_bit(Reg::C , 0x01), 0x49 => self.instr_bit(Reg::C , 0x02), 0x4A => self.instr_bit(Reg::C , 0x04), 0x4B => self.instr_bit(Reg::C , 0x08),
            0x4C => self.instr_bit(Reg::C , 0x10), 0x4D => self.instr_bit(Reg::C , 0x20), 0x4E => self.instr_bit(Reg::C , 0x40), 0x4F => self.instr_bit(Reg::C , 0x80),
            0x50 => self.instr_bit(Reg::D , 0x01), 0x51 => self.instr_bit(Reg::D , 0x02), 0x52 => self.instr_bit(Reg::D , 0x04), 0x53 => self.instr_bit(Reg::D , 0x08),
            0x54 => self.instr_bit(Reg::D , 0x10), 0x55 => self.instr_bit(Reg::D , 0x20), 0x56 => self.instr_bit(Reg::D , 0x40), 0x57 => self.instr_bit(Reg::D , 0x80),
            0x58 => self.instr_bit(Reg::E , 0x01), 0x59 => self.instr_bit(Reg::E , 0x02), 0x5A => self.instr_bit(Reg::E , 0x04), 0x5B => self.instr_bit(Reg::E , 0x08),
            0x5C => self.instr_bit(Reg::E , 0x10), 0x5D => self.instr_bit(Reg::E , 0x20), 0x5E => self.instr_bit(Reg::E , 0x40), 0x5F => self.instr_bit(Reg::E , 0x80),
            0x60 => self.instr_bit(Reg::H , 0x01), 0x61 => self.instr_bit(Reg::H , 0x02), 0x62 => self.instr_bit(Reg::H , 0x04), 0x63 => self.instr_bit(Reg::H , 0x08),
            0x64 => self.instr_bit(Reg::H , 0x10), 0x65 => self.instr_bit(Reg::H , 0x20), 0x66 => self.instr_bit(Reg::H , 0x40), 0x67 => self.instr_bit(Reg::H , 0x80),
            0x68 => self.instr_bit(Reg::L , 0x01), 0x69 => self.instr_bit(Reg::L , 0x02), 0x6A => self.instr_bit(Reg::L , 0x04), 0x6B => self.instr_bit(Reg::L , 0x08),
            0x6C => self.instr_bit(Reg::L , 0x10), 0x6D => self.instr_bit(Reg::L , 0x20), 0x6E => self.instr_bit(Reg::L , 0x40), 0x6F => self.instr_bit(Reg::L , 0x80),
            0x70 => self.instr_bit(Reg::HL, 0x01), 0x71 => self.instr_bit(Reg::HL, 0x02), 0x72 => self.instr_bit(Reg::HL, 0x04), 0x73 => self.instr_bit(Reg::HL, 0x08),
            0x74 => self.instr_bit(Reg::HL, 0x10), 0x75 => self.instr_bit(Reg::HL, 0x20), 0x76 => self.instr_bit(Reg::HL, 0x40), 0x77 => self.instr_bit(Reg::HL, 0x80),
            0x78 => self.instr_bit(Reg::A , 0x01), 0x79 => self.instr_bit(Reg::A , 0x02), 0x7A => self.instr_bit(Reg::A , 0x04), 0x7B => self.instr_bit(Reg::A , 0x08),
            0x7C => self.instr_bit(Reg::A , 0x10), 0x7D => self.instr_bit(Reg::A , 0x20), 0x7E => self.instr_bit(Reg::A , 0x40), 0x7F => self.instr_bit(Reg::A , 0x80),
            
            0x80 => self.instr_res(Reg::B , 0x01), 0x81 => self.instr_res(Reg::B , 0x02), 0x82 => self.instr_res(Reg::B , 0x04), 0x83 => self.instr_res(Reg::B , 0x08),
            0x84 => self.instr_res(Reg::B , 0x10), 0x85 => self.instr_res(Reg::B , 0x20), 0x86 => self.instr_res(Reg::B , 0x40), 0x87 => self.instr_res(Reg::B , 0x80),
            0x88 => self.instr_res(Reg::C , 0x01), 0x89 => self.instr_res(Reg::C , 0x02), 0x8A => self.instr_res(Reg::C , 0x04), 0x8B => self.instr_res(Reg::C , 0x08),
            0x8C => self.instr_res(Reg::C , 0x10), 0x8D => self.instr_res(Reg::C , 0x20), 0x8E => self.instr_res(Reg::C , 0x40), 0x8F => self.instr_res(Reg::C , 0x80),
            0x90 => self.instr_res(Reg::D , 0x01), 0x91 => self.instr_res(Reg::D , 0x02), 0x92 => self.instr_res(Reg::D , 0x04), 0x93 => self.instr_res(Reg::D , 0x08),
            0x94 => self.instr_res(Reg::D , 0x10), 0x95 => self.instr_res(Reg::D , 0x20), 0x96 => self.instr_res(Reg::D , 0x40), 0x97 => self.instr_res(Reg::D , 0x80),
            0x98 => self.instr_res(Reg::E , 0x01), 0x99 => self.instr_res(Reg::E , 0x02), 0x9A => self.instr_res(Reg::E , 0x04), 0x9B => self.instr_res(Reg::E , 0x08),
            0x9C => self.instr_res(Reg::E , 0x10), 0x9D => self.instr_res(Reg::E , 0x20), 0x9E => self.instr_res(Reg::E , 0x40), 0x9F => self.instr_res(Reg::E , 0x80),
            0xA0 => self.instr_res(Reg::H , 0x01), 0xA1 => self.instr_res(Reg::H , 0x02), 0xA2 => self.instr_res(Reg::H , 0x04), 0xA3 => self.instr_res(Reg::H , 0x08),
            0xA4 => self.instr_res(Reg::H , 0x10), 0xA5 => self.instr_res(Reg::H , 0x20), 0xA6 => self.instr_res(Reg::H , 0x40), 0xA7 => self.instr_res(Reg::H , 0x80),
            0xA8 => self.instr_res(Reg::L , 0x01), 0xA9 => self.instr_res(Reg::L , 0x02), 0xAA => self.instr_res(Reg::L , 0x04), 0xAB => self.instr_res(Reg::L , 0x08),
            0xAC => self.instr_res(Reg::L , 0x10), 0xAD => self.instr_res(Reg::L , 0x20), 0xAE => self.instr_res(Reg::L , 0x40), 0xAF => self.instr_res(Reg::L , 0x80),
            0xB0 => self.instr_res(Reg::HL, 0x01), 0xB1 => self.instr_res(Reg::HL, 0x02), 0xB2 => self.instr_res(Reg::HL, 0x04), 0xB3 => self.instr_res(Reg::HL, 0x08),
            0xB4 => self.instr_res(Reg::HL, 0x10), 0xB5 => self.instr_res(Reg::HL, 0x20), 0xB6 => self.instr_res(Reg::HL, 0x40), 0xB7 => self.instr_res(Reg::HL, 0x80),
            0xB8 => self.instr_res(Reg::A , 0x01), 0xB9 => self.instr_res(Reg::A , 0x02), 0xBA => self.instr_res(Reg::A , 0x04), 0xBB => self.instr_res(Reg::A , 0x08),
            0xBC => self.instr_res(Reg::A , 0x10), 0xBD => self.instr_res(Reg::A , 0x20), 0xBE => self.instr_res(Reg::A , 0x40), 0xBF => self.instr_res(Reg::A , 0x80),
            
            0xC0 => self.instr_set(Reg::B , 0x01), 0xC1 => self.instr_set(Reg::B , 0x02), 0xC2 => self.instr_set(Reg::B , 0x04), 0xC3 => self.instr_set(Reg::B , 0x08),
            0xC4 => self.instr_set(Reg::B , 0x10), 0xC5 => self.instr_set(Reg::B , 0x20), 0xC6 => self.instr_set(Reg::B , 0x40), 0xC7 => self.instr_set(Reg::B , 0x80),
            0xC8 => self.instr_set(Reg::C , 0x01), 0xC9 => self.instr_set(Reg::C , 0x02), 0xCA => self.instr_set(Reg::C , 0x04), 0xCB => self.instr_set(Reg::C , 0x08),
            0xCC => self.instr_set(Reg::C , 0x10), 0xCD => self.instr_set(Reg::C , 0x20), 0xCE => self.instr_set(Reg::C , 0x40), 0xCF => self.instr_set(Reg::C , 0x80),
            0xD0 => self.instr_set(Reg::D , 0x01), 0xD1 => self.instr_set(Reg::D , 0x02), 0xD2 => self.instr_set(Reg::D , 0x04), 0xD3 => self.instr_set(Reg::D , 0x08),
            0xD4 => self.instr_set(Reg::D , 0x10), 0xD5 => self.instr_set(Reg::D , 0x20), 0xD6 => self.instr_set(Reg::D , 0x40), 0xD7 => self.instr_set(Reg::D , 0x80),
            0xD8 => self.instr_set(Reg::E , 0x01), 0xD9 => self.instr_set(Reg::E , 0x02), 0xDA => self.instr_set(Reg::E , 0x04), 0xDB => self.instr_set(Reg::E , 0x08),
            0xDC => self.instr_set(Reg::E , 0x10), 0xDD => self.instr_set(Reg::E , 0x20), 0xDE => self.instr_set(Reg::E , 0x40), 0xDF => self.instr_set(Reg::E , 0x80),
            0xE0 => self.instr_set(Reg::H , 0x01), 0xE1 => self.instr_set(Reg::H , 0x02), 0xE2 => self.instr_set(Reg::H , 0x04), 0xE3 => self.instr_set(Reg::H , 0x08),
            0xE4 => self.instr_set(Reg::H , 0x10), 0xE5 => self.instr_set(Reg::H , 0x20), 0xE6 => self.instr_set(Reg::H , 0x40), 0xE7 => self.instr_set(Reg::H , 0x80),
            0xE8 => self.instr_set(Reg::L , 0x01), 0xE9 => self.instr_set(Reg::L , 0x02), 0xEA => self.instr_set(Reg::L , 0x04), 0xEB => self.instr_set(Reg::L , 0x08),
            0xEC => self.instr_set(Reg::L , 0x10), 0xED => self.instr_set(Reg::L , 0x20), 0xEE => self.instr_set(Reg::L , 0x40), 0xEF => self.instr_set(Reg::L , 0x80),
            0xF0 => self.instr_set(Reg::HL, 0x01), 0xF1 => self.instr_set(Reg::HL, 0x02), 0xF2 => self.instr_set(Reg::HL, 0x04), 0xF3 => self.instr_set(Reg::HL, 0x08),
            0xF4 => self.instr_set(Reg::HL, 0x10), 0xF5 => self.instr_set(Reg::HL, 0x20), 0xF6 => self.instr_set(Reg::HL, 0x40), 0xF7 => self.instr_set(Reg::HL, 0x80),
            0xF8 => self.instr_set(Reg::A , 0x01), 0xF9 => self.instr_set(Reg::A , 0x02), 0xFA => self.instr_set(Reg::A , 0x04), 0xFB => self.instr_set(Reg::A , 0x08),
            0xFC => self.instr_set(Reg::A , 0x10), 0xFD => self.instr_set(Reg::A , 0x20), 0xFE => self.instr_set(Reg::A , 0x40), 0xFF => self.instr_set(Reg::A , 0x80),
            _ => unreachable!("Unimplemented CB prefixed instruction?"),
        }
        
    }
    
    // Mnemonic: RLC
    // Full Name: Rotate Left Circular
    // Description: Sets the given reg (or hl) r to, (r << 1) | (r >> 7)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set.  If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.
    fn instr_rlc(&mut self, reg: Reg) -> i64 {
        self.regs.res_all_flags();
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle((val << 1) | (val >> 7));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, (val << 1) | (val >> 7));0}
        };
        
        if val == 0 {
            self.regs.set_flag(Flag::Z);
        } else if (val & 0x80) > 0 {
            self.regs.set_flag(Flag::C);
        } 
        
        cycles
    }

    // Mnemonic: RRC
    // Full Name: Rotate Right Circular
    // Description: Sets the given reg (or hl) r to, (r >> 1) | (r << 7)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.    
    fn instr_rrc(&mut self, reg: Reg) -> i64 {
        self.regs.res_all_flags();
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle((val >> 1) | (val << 7));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, (val >> 1) | (val << 7));0}
        };
        
        if val == 0 {
            self.regs.set_flag(Flag::Z);
        } else if (val & 0x01) == 1 {
            self.regs.set_flag(Flag::C);
        }
        
        cycles
    }
    
    // Mnemonic: RL
    // Full Name: Rotate Left
    // Description, Sets the given reg (or hl) r to, (r << 1) | (carry_in)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.    
    fn instr_rl(&mut self, reg: Reg) -> i64 {
        let val;
        let carry_in = if self.regs.get_flag(Flag::C) {1} else {0};
        let res;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle();res = (val << 1) | (carry_in); self.write_hl_cycle(res);8}
            r => {val = self.regs.get_reg(&r); res = (val << 1) | (carry_in); self.regs.set_reg(r, res);0}
        };

        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }

        if (val & 0x80) == 0x80 {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }
    
    // Mnemonic: RR
    // Full Name: Rotate Right
    // Description, Sets the given reg (or hl) r to, (r >> 1) | (carry_in)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.    
    fn instr_rr(&mut self, reg: Reg) -> i64 {
        let val;
        let carry_in = if self.regs.get_flag(Flag::C) {0x80} else {0x00};
        let res;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle();res = (val >> 1) | (carry_in); self.write_hl_cycle(res);8}
            r => {val = self.regs.get_reg(&r); res = (val >> 1) | (carry_in);self.regs.set_reg(r, res);0}
        };

        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }

        if (val & 0x01) == 0x01 {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }
    
    // Mnemonic: SlA
    // Full Name: Shift Left Arithmetic
    // Description, Sets the given reg (or hl) r to (r << 1)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.    
    fn instr_sla(&mut self, reg: Reg) -> i64 {
        let val;
        let res;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); res = val << 1; self.write_hl_cycle(res);8}
            r => {val = self.regs.get_reg(&r); res=val << 1; self.regs.set_reg(r, res);0}
        };

        self.regs.res_all_flags();
        if res == 0 {
            self.regs.set_flag(Flag::Z);
        }

        if (val & 0x80) == 0x80 {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }
    
    // Mnemonic: SRA
    // Full Name: Shift Right Arithmetic
    // Description, Sets the given reg (or hl) r to (r >> 1) | (r & 0x80)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.    
    fn instr_sra(&mut self, reg: Reg) -> i64 {
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle((val >> 1) | (val & 0x80));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, (val >> 1) | (val & 0x80));0}
        };

        self.regs.res_all_flags();
        if val == 0 {
            self.regs.set_flag(Flag::Z);
        }

        if (val & 0x01) == 0x01 {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }
    
    // Mnemonic: SWAP
    // Full Name: Swap
    // Description: Swaps the upper and lower nibbles of the given reg (or hl) r. r=((r << 4) | (r >> 4))
    // Affected Flags: Z (set|res), N (res), H (res), C (res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set otherwise, zero is reset
    // Timing: "read, write" or instant.
    fn instr_swap(&mut self, reg: Reg) -> i64 {
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle((val << 4) | (val >> 4));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, (val << 4) | (val >> 4));0}
        };

        self.regs.res_all_flags();
        if val == 0 {
            self.regs.set_flag(Flag::Z);
        }

        cycles
    }

    // Mnemonic: SRL
    // Full Name: Shift Right Logical
    // Description, Sets the given reg (or hl) r to (r >> 1)
    // Affected Flags: Z (set|res), N (res), H (res), C (set|res)
    // Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
    // Timing: "read, write" or instant.    
    fn instr_srl(&mut self, reg: Reg) -> i64 {
        let val;
        let cycles = match reg {
            Reg::HL => {val = self.read_hl_cycle(); self.write_hl_cycle((val >> 1));8}
            r => {val = self.regs.get_reg(&r); self.regs.set_reg(r, (val >> 1));0}
        };

        self.regs.res_all_flags();
        if val == 0 {
            self.regs.set_flag(Flag::Z);
        }

        if (val & 0x01) == 0x01 {
            self.regs.set_flag(Flag::C);
        }

        cycles
    }

    
    // Mnemonic: BIT
    // Full Name: Bit Test
    // Description: Tests the given bit (in the form of a mask --that is implementation specific,
    // other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
    // Affected Flags: Z (set|res), N (res), H (set)
    // Remarks: Zero is set if the bit is unset, and gets reset otherwise.
    // Timing: "read" or instant.
    fn instr_bit(&mut self, reg: Reg, mask: u8) -> i64 {

        self.regs.f &= Flag::C.to_mask();
        self.regs.set_flag(Flag::H);
        match reg {
            Reg::HL => {if (self.read_hl_cycle() & mask) == 0 {self.regs.set_flag(Flag::Z)}; 4}
            r => {if (self.regs.get_reg(&r) & mask) == 0 {self.regs.set_flag(Flag::Z)}; 0}
        }
    }
    
    // Mnemonic: RES
    // Full Name: Reset Bit
    // Description: Resets the given bit (in the form of a mask --that is implementation specific,
    // other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
    // Affected Flags: ----
    // Remarks: ----
    // Timing: "read, write" or instant.
    fn instr_res(&mut self, reg: Reg, mask: u8) -> i64 {
        match reg {
            Reg::HL => {let val = self.read_hl_cycle(); self.write_hl_cycle(val & mask); 8}
            r => {let val = self.regs.get_reg(&r); self.regs.set_reg(r, val & mask) ; 0}
        }
    }
    
    // Mnemonic: SET
    // Full Name: Set
    // Description: Sets the given bit (in the form of a mask --that is implementation specific,
    // other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
    // Affected Flags: ----
    // Remarks: ----
    // Timing: "read, write" or instant.
    fn instr_set(&mut self, reg: Reg, mask: u8) -> i64 {
        match reg {
            Reg::HL => {let val = self.read_hl_cycle(); self.write_hl_cycle(val | mask); 8}
            r => {let val = self.regs.get_reg(&r); self.regs.set_reg(r, val | mask) ; 0}
        }
    }
    
    fn handle_interrupts(&mut self) -> i64 {
        0 // STUB
    }
    
    
    fn handle_okay(&mut self) -> i64 {
        self.update(2);
        2 + self.handle_interrupts() + self.run_instruction()
    }
    
    pub fn new(boot_rom: Vec<u8>, game_rom: Vec<u8>) -> Option<Cpu> {
        match boot_rom.len() {
            0x100 => Some(Cpu {
                cycle_counter: 0,
                wram: [0; 0x2000],
                regs: registers::Registers::new(),
                status: State::Okay,
                ime: false,
                ie: false,
                halt_bugged: false,
                r_ier: 0,
                r_if: 0,
                tim: Timer::new(),
                game_rom,
                boot_rom,
            }),
            _ => None
        }
    }
    
    pub fn run(&mut self, ticks: i64) {
        self.cycle_counter += ticks;
        while self.cycle_counter > 0 {
            self.cycle_counter -= match self.status {
                State::Okay => self.handle_okay(),
                State::Stop => unimplemented!("Implement CPU stop behavior!"),
                State::Halt => unimplemented!("Implement CPU halt behavior!"),
                State::Hang => unimplemented!("Implement CPU hung behavior!"),
            }
        }
    }
}
