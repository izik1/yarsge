// Copyright Zachery Gyurkovitz 2017-2018 MIT License, see licence.md for more details.

use crate::emu::{
    cpu::{Cpu, State},
    flags::Flag,
    hardware::Hardware,
    registers::{Reg, R16},
    MCycle,
};

#[derive(Clone, Copy)]
pub enum MathReg {
    R(Reg),
    Imm,
}

fn get_reg(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) -> u8 {
    match reg {
        Reg::HL => hw.read_cycle(cpu.regs.hl),
        r => cpu.regs.get_reg(r),
    }
}

fn set_reg(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg, value: u8) {
    match reg {
        Reg::HL => hw.write_cycle(cpu.regs.hl, value),
        r => cpu.regs.set_reg(r, value),
    }
}

fn get_math_reg(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) -> u8 {
    match reg {
        MathReg::Imm => cpu.read_ipc_cycle(hw),
        MathReg::R(r2) => get_reg(cpu, hw, r2),
    }
}

fn get_cin_lsb(flags: Flag) -> u8 {
    if flags.contains(Flag::C) {
        0x01
    } else {
        0x00
    }
}

fn get_cin_msb(flags: Flag) -> u8 {
    get_cin_lsb(flags) * 0x80
}

pub fn invalid(cpu: &mut Cpu) {
    cpu.status = State::Hang;
}

// Mnemonic: JR
// Full Name: Jump Relative
// Description: Jumps to pc + r8 if "jump" is true, otherwise it does nothing.
// Affected Flags: ----
// Remarks: This instruction stops 4 cycles short if it doesn't jump.
// Timing: read, <internal delay>
pub fn jr(cpu: &mut Cpu, hw: &mut Hardware, jump: bool) {
    let val = cpu.read_ipc_cycle(hw) as i8;
    if jump {
        hw.stall_one();
        cpu.regs.pc = cpu.regs.pc.wrapping_add(val as u16);
    }
}

// Mnemonic: JP
// Full Name: Jump
// Description: Jumps to a16 if "jump" is true, otherwise it does nothing.
// Affected Flags: ----
// Remarks: This instruction stops 4 cycles short if it doesn't jump.
// Timing: read, read, <internal delay>
pub fn jp(cpu: &mut Cpu, hw: &mut Hardware, jump: bool) {
    let addr = cpu.read_u16_cycle(hw);
    if jump {
        hw.stall_one();
        cpu.regs.pc = addr;
    }
}

// Mnemonic: LD
// Full Name: Load
// Description: Loads dest into src, either one of which can be HL but not both.
// Affected Flags: ----
// Remarks: I really like how this function came out. I think it looks nice.
// Timing: either "write", "read" or instant.
pub fn ld(cpu: &mut Cpu, hw: &mut Hardware, dest: Reg, src: Reg) {
    match (dest, src) {
        (Reg::HL, Reg::HL) => halt(cpu, hw),
        (Reg::HL, src) => hw.write_cycle(cpu.regs.hl, cpu.regs.get_reg(src)),
        (dest, Reg::HL) => cpu.regs.set_reg(dest, hw.read_cycle(cpu.regs.hl)),
        (dest, src) => {
            let val = cpu.regs.get_reg(src);
            cpu.regs.set_reg(dest, val)
        }
    }
}

// Mnemonic: HALT
// Full Name: Halt
// Description: Halts the cpu.
// Affected Flags: ----
// Remarks: ----
// Timing: instant.
pub fn halt(cpu: &mut Cpu, hw: &mut Hardware) {
    if cpu.ime || (hw.r_if & hw.r_ier & 0x1F) == 0 {
        cpu.status = State::Halt;
    } else {
        cpu.halt_bugged = true;
    }
}

// Mnemonic: STOP
// Full Name: Stop
// Description: Stops the cpu.
// Affected Flags: ----
// Remarks: ----
// Timing: NA.
pub fn stop(cpu: &mut Cpu) {
    cpu.status = State::Stop;
}

// Mnemonic: LD r16,d16
// Full Name: Load <r16>, d16
// Description: Sets the given 16 bit register to a 2 byte immediate value.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read
pub fn ld_r16_d16(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    let val = cpu.read_u16_cycle(hw);
    cpu.regs.set_reg_16(reg, val);
}

// Mnemonic: LD (r16),A
// Full Name: Load (<r16>), A
// Description: Sets the address referenced by the 16 bit register r16 to A.
// Affected Flags: ----
// Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
// Timing: Write
pub fn ld_r16_a(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    let (addr, hl_mod) = match reg {
        R16::BC => (cpu.regs.bc, 0x0000),
        R16::DE => (cpu.regs.de, 0x0000),
        R16::HL => (cpu.regs.hl, 0x0001),
        R16::SP => (cpu.regs.hl, 0xFFFF),
    };

    cpu.regs.hl = cpu.regs.hl.wrapping_add(hl_mod);
    hw.write_cycle(addr, cpu.regs.a);
}

// Mnemonic: INC r16
// Full Name: Increment r16
// Description: Increments the given 16-bit register.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal Delay.
pub fn inc_16(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    hw.stall_one();
    let v = cpu.regs.get_reg_16(reg).wrapping_add(1);
    cpu.regs.set_reg_16(reg, v);
}

// Mnemonic: INC reg8
// Full Name: Increment reg8
// Description: Increments the given 8-bit register (or hl) reg8.
// Affected Flags: Z (set|res), N (res), H (set|res)
// Remarks: Zero is set if reg8 overflows, Half carry is set if there is a half carry between reg8 and 1.
// If a flags conditions aren't met, it is instead reset.
// Timing: "Instant" or "Read, Write"
pub fn inc_8(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    set_reg(cpu, hw, reg, val.wrapping_add(1));

    cpu.regs.f.remove(Flag::N);
    cpu.regs.f.set(Flag::Z, val == 0xFF);
    cpu.regs.f.set(Flag::H, (((val & 0xF) + 1) & 0x10) == 0x10);
}

// Mnemonic: DEC reg8
// Full Name: Decrement reg8
// Description: Decrements the given 8-bit register (or hl) reg8.
// Affected Flags: Z (set|res), N (set), H (set|res)
// Remarks: Zero is set if reg8 is 1, Half carry is set if reg8 & 0xf == 0.
// If a flags conditions aren't met, it is instead reset.
// Timing: "Instant" or "Read, Write"
pub fn dec_8(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    set_reg(cpu, hw, reg, val.wrapping_sub(1));

    cpu.regs.f.set(Flag::Z, val == 1);
    cpu.regs.f.insert(Flag::N);
    cpu.regs.f.set(Flag::H, (val & 0xF) == 0);
}

// Mnemonic: LD reg8,d8
// Full Name: Load reg8,d8
// Description: Loads 8-bit unsigned data d8 into the register (or hl) reg8.
// Affected Flags: ----
// Remarks: ----
// Timing: "Read" or "Read, Write"
pub fn ld_r8_d8(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = cpu.read_ipc_cycle(hw);
    set_reg(cpu, hw, reg, val);
}

// Mnemonic: RLCA
// Full Name: Rotate Left Circular A
// Description: Sets A to, (A << 1) | (A >> 7)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 7 is set, otherwise it is reset.
// Timing: Instant.
pub fn rlca(cpu: &mut Cpu) {
    cpu.regs.f = Flag::empty();
    cpu.regs.f.set(Flag::C, cpu.regs.a >= 0x80);

    cpu.regs.a = (cpu.regs.a << 1) | (cpu.regs.a >> 7);
}

// Mnemonic: LD (a16),SP
// Full Name: Load (a16),sp
// Description: Loads sp into the address pointed to by 16-bit unsigned data a16.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Write, Write
pub fn ld_a16_sp(cpu: &mut Cpu, hw: &mut Hardware) {
    let address = cpu.read_u16_cycle(hw);
    let sp = cpu.regs.sp;
    hw.write_u16_cycle(address, sp);
}

// Mnemonic: ADD HL,R16
// Full Name: Add HL,R16
// Description: Adds 16-bit register R16 to HL storing the result in HL
// Affected Flags: N (res), H (set|res), C (set|res)
// Remarks: Half Carry is set if there is a carry between bits 11 and 12. Carry is set if there is a carry out. Otherwise reset Half Carry or Carry respectively
// Timing: Internal Delay
pub fn add_hl_reg16(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    let val = cpu.regs.get_reg_16(reg);
    let res = cpu.regs.hl.wrapping_add(val);

    cpu.regs.f.remove(Flag::N);
    cpu.regs.f.set(
        Flag::H,
        (((cpu.regs.hl & 0xFFF) + (val & 0xFFF)) & 0x1000) == 0x1000,
    );
    cpu.regs.f.set(Flag::C, res < cpu.regs.hl);

    hw.stall_one();
    cpu.regs.hl = res;
}

// Mnemonic: LD A,(r16)
// Full Name: Load A, (<r16>)
// Description: Sets A to the address referenced by the 16 bit register r16.
// Affected Flags: ----
// Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
// Timing: Read
pub fn ld_a_r16(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    let (addr, hl_mod) = match reg {
        R16::BC => (cpu.regs.bc, 0x0000),
        R16::DE => (cpu.regs.de, 0x0000),
        R16::HL => (cpu.regs.hl, 0x0001),
        R16::SP => (cpu.regs.hl, 0xFFFF),
    };

    cpu.regs.hl = cpu.regs.hl.wrapping_add(hl_mod);
    let val = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: DEC r16
// Full Name: Decrement r16
// Description: Decrements the given 16-bit register.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal Delay.
pub fn dec_16(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    hw.stall_one();
    cpu.regs
        .set_reg_16(reg, cpu.regs.get_reg_16(reg).wrapping_sub(1));
}

// Mnemonic: RRCA
// Full Name: Rotate Right Circular A
// Description: Sets A to, (A >> 1) | (A << 7)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 0 is set, otherwise it is reset.
// Timing: Instant.
pub fn rrca(cpu: &mut Cpu) {
    cpu.regs.f = Flag::empty();
    cpu.regs.f.set(Flag::C, (cpu.regs.a & 0x01) == 0x01);
    cpu.regs.a = (cpu.regs.a >> 1) | (cpu.regs.a << 7);
}

// Mnemonic: RLA
// Full Name: Rotate Left A
// Description: Sets A to, (A << 1) | (c_in)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 7 is set, otherwise it is reset.
// Timing: Instant.
pub fn rla(cpu: &mut Cpu) {
    let a = cpu.regs.a;
    cpu.regs.a = (a << 1) | get_cin_lsb(cpu.regs.f);
    cpu.regs.f = Flag::empty();
    cpu.regs.f.set(Flag::C, a & 0x80 == 0x80);
}

// Mnemonic: RRA
// Full Name: Rotate Right A
// Description: Sets A to, (A >> 1) | (c_in)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 0 is set, otherwise it is reset.
// Timing: Instant.
pub fn rra(cpu: &mut Cpu) {
    let a = cpu.regs.a;
    cpu.regs.a = (a >> 1) | get_cin_msb(cpu.regs.f);
    cpu.regs.f = Flag::empty();
    cpu.regs.f.set(Flag::C, a & 0x01 == 0x01);
}

// Mnemonic: DAA
// Full Name: Decimal Adjust AL
// Description: ???
// Affected Flags: Z (set|res), H (res), C (-|set)
// Remarks: Confusing
// Timing: Instant.
pub fn daa(cpu: &mut Cpu) {
    let mut res = i32::from(cpu.regs.a); // todo: check if this can be i16.
    if cpu.regs.f.contains(Flag::N) {
        if cpu.regs.f.contains(Flag::H) {
            res = (res - 6) & 0xFF;
        }

        if cpu.regs.f.contains(Flag::C) {
            res -= 0x60;
        }
    } else {
        if cpu.regs.f.contains(Flag::H) || (res & 0xF) > 9 {
            res += 0x06;
        }

        if cpu.regs.f.contains(Flag::C) || res > 0x9F {
            res += 0x60;
        }
    };

    cpu.regs.f.remove(Flag::H);

    if (res & 0x100) == 0x100 {
        cpu.regs.f |= Flag::C;
    }

    cpu.regs.a = res as u8;

    cpu.regs.f.set(Flag::Z, cpu.regs.a == 0);
}

// Mnemonic: CPL
// Full Name: Complement
// Description: Bitwise complements A
// Affected Flags: N (set), H (set)
// Remarks: ----
// Timing: Instant.
pub fn cpl(cpu: &mut Cpu) {
    cpu.regs.a = !cpu.regs.a;
    cpu.regs.f |= Flag::N | Flag::H;
}

// Mnemonic: SCF
// Full Name: Set Carry Flag
// Description: Sets the carry flag
// Affected Flags: N (res), H (res), C (set)
// Remarks: ----
// Timing: Instant.
pub fn scf(cpu: &mut Cpu) {
    cpu.regs.f &= Flag::Z;
    cpu.regs.f |= Flag::C;
}

// Mnemonic: CCF
// Full Name: Complement Carry Flag
// Description: Complements the carry flag
// Affected Flags: N (res), H (res), C (^C)
// Remarks: ----
// Timing: Instant.
pub fn ccf(cpu: &mut Cpu) {
    cpu.regs.f &= Flag::Z | Flag::C;
    cpu.regs.f ^= Flag::C;
}

// Mnemonic: ADD
// Full Name: Add
// Description: Adds the given reg (or hl, or imm) r to A and stores the result into A
// Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn add(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.a;
    let val = get_math_reg(cpu, hw, reg);

    cpu.regs.a = a.wrapping_add(val);

    cpu.regs.f.set(Flag::Z, cpu.regs.a == 0);
    cpu.regs.f.remove(Flag::N);
    cpu.regs
        .f
        .set(Flag::H, (((a & 0xF) + (val & 0xF)) & 0x10) == 0x10);
    cpu.regs.f.set(Flag::C, cpu.regs.a < a);
}

// Mnemonic: ADC
// Full Name: Add with carry
// Description: Adds the given reg (or hl, or imm) r and carry to A and stores the result into A
// Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn adc(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.a;
    let c_in = get_cin_lsb(cpu.regs.f);
    let val = get_math_reg(cpu, hw, reg);

    cpu.regs.a = a.wrapping_add(val).wrapping_add(c_in);

    cpu.regs.f.set(Flag::Z, cpu.regs.a == 0);
    cpu.regs.f.remove(Flag::N);
    cpu.regs
        .f
        .set(Flag::H, (a & 0xF) + (val & 0xF) + c_in > 0xF);
    cpu.regs.f.set(
        Flag::C,
        u16::from(a) + u16::from(val) + u16::from(c_in) > 0xFF,
    );
}

// Mnemonic: SUB
// Full Name: Sub
// Description: Subtracts the given reg (or hl, or imm) r from A and stores the result into A
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn sub(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.get_reg(Reg::A);
    let val = get_math_reg(cpu, hw, reg);

    let res = a.wrapping_sub(val);
    cpu.regs.set_reg(Reg::A, res);

    cpu.regs.f.set(Flag::Z, res == 0);
    cpu.regs.f.insert(Flag::N);
    cpu.regs.f.set(Flag::H, (a & 0xF) < (val & 0xF));
    cpu.regs.f.set(Flag::C, val > a);
}

// Mnemonic: SBC
// Full Name: Sub with carry
// Description: Subtracts the given reg (or hl, or imm) r and carry from A and stores the result into A
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn sbc(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.get_reg(Reg::A);
    let c_in = get_cin_lsb(cpu.regs.f);
    let val = get_math_reg(cpu, hw, reg);

    let res = u16::from(a)
        .wrapping_sub(u16::from(val))
        .wrapping_sub(u16::from(c_in));

    cpu.regs.set_reg(Reg::A, res as u8);

    cpu.regs.f.set(Flag::Z, (res & 0xFF) == 0);
    cpu.regs.f.insert(Flag::N);
    cpu.regs.f.set(Flag::H, (a & 0xF) < ((val & 0xF) + c_in));
    cpu.regs.f.set(Flag::C, res > 0xFF);
}

// Mnemonic: AND
// Full Name: Bitwise And
// Description: Preforms bitwise AND on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (set), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn and(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.get_reg(Reg::A);
    let val = get_math_reg(cpu, hw, reg);

    let res = a & val;
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.f = Flag::H | if res == 0 { Flag::Z } else { Flag::empty() }
}

// Mnemonic: XOR
// Full Name: Bitwise Xor
// Description: Preforms bitwise XOR on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn xor(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.get_reg(Reg::A);
    let val = get_math_reg(cpu, hw, reg);

    let res = a ^ val;
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.f = if res == 0 { Flag::Z } else { Flag::empty() }
}

// Mnemonic: OR
// Full Name: Bitwise Or
// Description: Preforms bitwise Or on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn or(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.get_reg(Reg::A);
    let val = get_math_reg(cpu, hw, reg);

    let res = a | val;
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.f = if res == 0 { Flag::Z } else { Flag::empty() }
}

// Mnemonic: CP
// Full Name: Compare
// Description: Subtracts the given reg (or hl, or imm) r from A discarding the result.
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn cp(cpu: &mut Cpu, hw: &mut Hardware, reg: MathReg) {
    let a = cpu.regs.get_reg(Reg::A);
    let val = get_math_reg(cpu, hw, reg);

    cpu.regs.f.set(Flag::Z, a == val);
    cpu.regs.f.insert(Flag::N);
    cpu.regs.f.set(Flag::H, (a & 0xF) < (val & 0xF));
    cpu.regs.f.set(Flag::C, a < val);
}

// Mnemonic: RET <COND>
// Full Name: Return <COND>
// Description: Returns Conditionally. (NZ/Z/NC/C)
// Affected Flags: ----
// Remarks: ----
// Timing: "Internal Delay" or "Read, Read, Internal Delay"
pub fn retc(cpu: &mut Cpu, hw: &mut Hardware, jump: bool) {
    hw.stall_one();
    if jump {
        let addr = cpu.read_pop_16_cycle(hw);
        cpu.regs.pc = addr;
        hw.stall_one();
    }
}

// Mnemonic: POP <Reg-16>
// Full Name: Pop <Reg-16>
// Description: Pops the 16-bit register Reg-16 off of the stack.
// Affected Flags: ---- or Z (set|res), N (set|res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Read
pub fn pop(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    let val = cpu.read_pop_16_cycle(hw);
    match reg {
        R16::SP => cpu.regs.s_af(val),
        r => cpu.regs.set_reg_16(r, val),
    };
}

// Mnemonic: CALL <COND> | CALL
// Full Name: Call <COND> | Call
// Description: Calls (possibly conditionally (NZ/Z/NC/C))
// Affected Flags: ----
// Remarks: ----
// Timing: "Read, Read" | "Read, Read, Delay, Write, Write"
pub fn call(cpu: &mut Cpu, hw: &mut Hardware, jump: bool) {
    let addr = cpu.read_u16_cycle(hw);
    if jump {
        hw.stall_one();
        let pc = cpu.regs.pc;
        cpu.write_push_16_cycle(hw, pc);
        cpu.regs.pc = addr;
    }
}

// Mnemonic: PUSH <Reg-16>
// Full Name: Push <Reg-16>
// Description: Pushes the 16-bit register Reg-16 onto the stack.
// Affected Flags: ----
// Remarks: ----
// Timing: Delay, Write, Write
pub fn push(cpu: &mut Cpu, hw: &mut Hardware, reg: R16) {
    let val = match reg {
        R16::SP => cpu.regs.g_af(),
        r => cpu.regs.get_reg_16(r),
    };
    hw.stall_one();
    cpu.write_push_16_cycle(hw, val);
}

// Mnemonic: RST <addr>
// Full Name: Reset <addr>
// Description: calls <addr>.
// Affected Flags: ----
// Remarks: ----
// Timing: Delay, Write, Write
pub fn rst(cpu: &mut Cpu, hw: &mut Hardware, addr: u16) {
    hw.stall_one();
    let pc = cpu.regs.pc;
    cpu.write_push_16_cycle(hw, pc);
    cpu.regs.pc = addr;
}

// Mnemonic: RET/RETI
// Full Name: Return / Return enable Interrupts
// Description: Returns unconditionally, if it's a reti instruction it will also enable IME.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Internal Delay
pub fn ret(cpu: &mut Cpu, hw: &mut Hardware, reti: bool) {
    let addr = cpu.read_pop_16_cycle(hw);
    cpu.regs.pc = addr;
    cpu.ime |= reti;
    hw.stall_one();
}

// Mnemonic: LDH (a8),A
// Full Name: Load High (a8),A
// Description: loads A into (0xFF00 | a8).
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Write
pub fn ldh_a8_a(cpu: &mut Cpu, hw: &mut Hardware) {
    let addr = 0xFF00 | u16::from(cpu.read_ipc_cycle(hw));
    let a = cpu.regs.get_reg(Reg::A);
    hw.write_cycle(addr, a);
}

// Mnemonic: LDH (c),A
// Full Name: Load High (c),A
// Description: loads A into (0xFF00 | c).
// Affected Flags: ----
// Remarks: ----
// Timing: Write
pub fn ldh_c_a(cpu: &mut Cpu, hw: &mut Hardware) {
    let addr = 0xFF00 | u16::from(cpu.regs.get_reg(Reg::C));
    let a = cpu.regs.get_reg(Reg::A);
    hw.write_cycle(addr, a);
}

// Mnemonic: LDH (a8),A
// Full Name: Load High (a8),A
// Description: loads A into (0xFF00 | a8).
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read
pub fn ldh_a_a8(cpu: &mut Cpu, hw: &mut Hardware) {
    let addr = 0xFF00 | u16::from(cpu.read_ipc_cycle(hw));
    let val = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: LDH (c),A
// Full Name: Load High (c),A
// Description: loads A into (0xFF00 | c).
// Affected Flags: ----
// Remarks: ----
// Timing: Read
pub fn ldh_a_c(cpu: &mut Cpu, hw: &mut Hardware) {
    let addr = 0xFF00 | u16::from(cpu.regs.get_reg(Reg::C));
    let val = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: DI
// Full Name: Disable Interrupts
// Description: Disables interrupts.
// Affected Flags: ----
// Remarks: ----
// Timing: Instant
pub fn di(cpu: &mut Cpu) {
    cpu.ime = false;
}

// Mnemonic: EI
// Full Name: Enable Interrupts
// Description: Enables interrupts.
// Affected Flags: ----
// Remarks: Interrupt enabling is delayed by 4-TCycles.
// Timing: Instant (delayed affect)
pub fn ei(cpu: &mut Cpu) {
    cpu.ei = true;
}

// Mnemonic: JP (HL)
// Full Name: Jump (HL)
// Description: Jumps to HL.
// Affected Flags: ----
// Remarks: ----
// Timing: Instant
pub fn jp_hl(cpu: &mut Cpu) {
    cpu.regs.pc = cpu.regs.hl;
}

// Mnemonic: LD (HL),SP+r8
// Full Name: Load (HL), SP+r8
// Description: Loads SP+signed 8-bit value r8 into HL
// Affected Flags: Z (res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Internal Delay
pub fn ld_hl_sp_r8(cpu: &mut Cpu, hw: &mut Hardware) {
    let r8 = (cpu.read_ipc_cycle(hw) as i8) as u16;
    hw.stall_one();

    cpu.regs.f = Flag::empty();
    cpu.regs
        .f
        .set(Flag::H, ((cpu.regs.sp & 0x0F) + (r8 & 0x0F)) > 0x0F);
    cpu.regs
        .f
        .set(Flag::C, (((cpu.regs.sp) & 0xFF) + (r8 & 0xFF)) > 0xFF);

    cpu.regs.hl = cpu.regs.sp.wrapping_add(r8);
}

// Mnemonic: LD A,(a16)
// Full Name: Load A,(a16)
// Description: Load the value pointed at by 16-bit unsigned data a16 into A
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Read
pub fn ld_a_a16(cpu: &mut Cpu, hw: &mut Hardware) {
    let addr = cpu.read_u16_cycle(hw);
    let val = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: LD (a16),A
// Full Name: Load (a16),A
// Description: Load A into the value pointed at by 16-bit unsigned data a16.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Write
pub fn ld_a16_a(cpu: &mut Cpu, hw: &mut Hardware) {
    let addr = cpu.read_u16_cycle(hw);
    let a = cpu.regs.get_reg(Reg::A);
    hw.write_cycle(addr, a);
}

// Mnemonic: LD SP,HL
// Full Name: Load sp,hl
// Description: Load hl into sp.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal delay
pub fn ld_sp_hl(cpu: &mut Cpu, hw: &mut Hardware) {
    let hl = cpu.regs.hl;
    hw.stall_one();
    cpu.regs.sp = hl;
}

// Mnemonic: ADD SP,r8
// Full Name: Add sp, r8
// Description: Add 8-bit signed data r8 into sp
// Affected Flags: Z (res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Internal Delay, Internal Delay
pub fn add_sp_r8(cpu: &mut Cpu, hw: &mut Hardware) {
    let r8 = cpu.read_ipc_cycle(hw) as i8;
    hw.stall(MCycle(2));

    cpu.regs.f = Flag::empty();
    cpu.regs
        .f
        .set(Flag::H, ((cpu.regs.sp & 0x0F) + (r8 as u16 & 0x0F)) > 0x0F);
    cpu.regs
        .f
        .set(Flag::C, ((cpu.regs.sp & 0xFF) + (r8 as u16 & 0xFF)) > 0xFF);

    cpu.regs.sp = cpu.regs.sp.wrapping_add(r8 as u16);
}

// ----------
// CB Prefix:
// ----------

// Mnemonic: RLC
// Full Name: Rotate Left Circular
// Description: Sets the given reg (or hl) r to, (r << 1) | (r >> 7)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set.  If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rlc(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    set_reg(cpu, hw, reg, (val << 1) | (val >> 7));

    cpu.regs.f.set(Flag::Z, val == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x80) == 0x80);
}

// Mnemonic: RRC
// Full Name: Rotate Right Circular
// Description: Sets the given reg (or hl) r to, (r >> 1) | (r << 7)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rrc(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    set_reg(cpu, hw, reg, (val >> 1) | (val << 7));

    cpu.regs.f.set(Flag::Z, val == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x01) == 0x01);
}

// Mnemonic: RL
// Full Name: Rotate Left
// Description, Sets the given reg (or hl) r to, (r << 1) | (carry_in)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rl(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    let res = (val << 1) | get_cin_lsb(cpu.regs.f);
    set_reg(cpu, hw, reg, res);

    cpu.regs.f.set(Flag::Z, res == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x80) == 0x80);
}

// Mnemonic: RR
// Full Name: Rotate Right
// Description, Sets the given reg (or hl) r to, (r >> 1) | (carry_in)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rr(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    let res = (val >> 1) | get_cin_msb(cpu.regs.f);

    set_reg(cpu, hw, reg, res);

    cpu.regs.f.set(Flag::Z, res == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x01) == 0x01);
}

// Mnemonic: SLA
// Full Name: Shift Left Arithmetic
// Description, Sets the given reg (or hl) r to (r << 1)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn sla(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    let res = val << 1;
    set_reg(cpu, hw, reg, res);

    cpu.regs.f.set(Flag::Z, res == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x80) == 0x80);
}

// Mnemonic: SRA
// Full Name: Shift Right Arithmetic
// Description, Sets the given reg (or hl) r to (r >> 1) | (r & 0x80)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn sra(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    let res = (val >> 1) | (val & 0x80);
    set_reg(cpu, hw, reg, res);

    cpu.regs.f.set(Flag::Z, res == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x01) == 0x01);
}

// Mnemonic: SWAP
// Full Name: Swap
// Description: Swaps the upper and lower nibbles of the given reg (or hl) r. r=((r << 4) | (r >> 4))
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: Zero is set if the input was 0, otherwise it is reset
// Timing: "read, write" or instant.
pub fn swap(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    set_reg(cpu, hw, reg, (val << 4) | (val >> 4));

    cpu.regs.f = Flag::empty();
    cpu.regs.f.set(Flag::Z, val == 0);
}

// Mnemonic: SRL
// Full Name: Shift Right Logical
// Description, Sets the given reg (or hl) r to (r >> 1)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn srl(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg) {
    let val = get_reg(cpu, hw, reg);
    let res = val >> 1;
    set_reg(cpu, hw, reg, res);

    cpu.regs.f.set(Flag::Z, res == 0);
    cpu.regs.f.remove(Flag::N | Flag::H);
    cpu.regs.f.set(Flag::C, (val & 0x01) == 0x01);
}

// Mnemonic: BIT
// Full Name: Bit Test
// Description: Tests the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: Z (set|res), N (res), H (set)
// Remarks: Zero is set if the bit is unset, and gets reset otherwise.
// Timing: "read" or instant.
pub fn bit(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg, mask: u8) {
    cpu.regs.f &= Flag::C;
    cpu.regs.f |= Flag::H;
    let b = (get_reg(cpu, hw, reg) & mask) == 0;
    cpu.regs.f.set(Flag::Z, b);

    // TODO: Check for proper NLL tracking issue for this as it *should* compile, but as of nightly 2018/01/27 complains about borrowing cpu while it's already borrowed.
    // If the prior error was fixed, this would be the preferred method of writing this function.
    // cpu.regs.f.set(Flag::Z, (get_reg(cpu, reg) & mask) == 0);
}

// Mnemonic: RES
// Full Name: Reset Bit
// Description: Resets the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: ----
// Remarks: ----
// Timing: "read, write" or instant.
pub fn res(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg, mask: u8) {
    let val = get_reg(cpu, hw, reg) & !mask;
    set_reg(cpu, hw, reg, val);
}

// Mnemonic: SET
// Full Name: Set
// Description: Sets the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: ----
// Remarks: ----
// Timing: "read, write" or instant.
pub fn set(cpu: &mut Cpu, hw: &mut Hardware, reg: Reg, mask: u8) {
    let val = get_reg(cpu, hw, reg) | mask;
    set_reg(cpu, hw, reg, val);
}
