// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use super::*;

#[derive(Clone, Copy)]
pub enum MathReg {
    R(Reg),
    Imm,
}

fn get_reg(cpu: &mut Cpu, reg: Reg) -> u8 {
    match reg {
        Reg::HL => cpu.read_hl_cycle(),
        r => cpu.regs.get_reg(&r),
    }
}

fn get_math_reg(cpu: &mut Cpu, reg: MathReg) -> u8 {
    match reg {
        MathReg::Imm => cpu.read_ipc_cycle(),
        MathReg::R(r2) => get_reg(cpu, r2),
    }
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
pub fn jr(cpu: &mut Cpu, jump: bool) {
    let val = cpu.read_ipc_cycle() as i8;
    if jump {
        cpu.update(4);
        cpu.regs.pc = cpu.regs.pc.wrapping_add(val as u16);
    }
}

// Mnemonic: JP
// Full Name: Jump
// Description: Jumps to a16 if "jump" is true, otherwise it does nothing.
// Affected Flags: ----
// Remarks: This instruction stops 4 cycles short if it doesn't jump.
// Timing: read, read, <internal delay>
pub fn jp(cpu: &mut Cpu, jump: bool) {
    let addr = cpu.read_u16_cycle();
    if jump {
        cpu.update(4);
        cpu.regs.pc = addr;
    }
}

// Mnemonic: LD
// Full Name: Load
// Description: Loads dest into src, either one of which can be HL but not both.
// Affected Flags: ----
// Remarks: I really like how this function came out. I think it looks nice.
// Timing: either "write", "read" or instant.
pub fn ld(cpu: &mut Cpu, dest: Reg, src: Reg) {
    match (dest, src) {
        (Reg::HL, Reg::HL) => unreachable!("This while theoretically reachable, should *never* be reached, since this instruction is instead HALT"),
        (Reg::HL, src) =>     {let val = cpu.regs.get_reg(&src); cpu.write_hl_cycle(val)}
        (dest, Reg::HL) =>    {let val = cpu.read_hl_cycle(); cpu.regs.set_reg(dest, val)}
        (dest, src) =>        {let val = cpu.regs.get_reg(&src); cpu.regs.set_reg(dest, val)}
    }
}

// Mnemonic: HALT
// Full Name: Halt
// Description: Halts the cpu.
// Affected Flags: ----
// Remarks: ----
// Timing: instant.
pub fn halt(cpu: &mut Cpu) {
    if cpu.ime || (cpu.r_if & cpu.r_ier & 0x1F) == 0 {
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
pub fn ld_r16_d16(cpu: &mut Cpu, reg: R16) {
    let val = cpu.read_u16_cycle();
    cpu.regs.set_reg_16(reg, val);
}

// Mnemonic: LD (r16),A
// Full Name: Load (<r16>), A
// Description: Sets the address referenced by the 16 bit register r16 to A.
// Affected Flags: ----
// Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
// Timing: Write
pub fn ld_r16_a(cpu: &mut Cpu, reg: R16) {
    let a = cpu.regs.a;
    let reg = match reg {
        R16::BC => cpu.regs.bc,
        R16::DE => cpu.regs.de,
        R16::HL => {
            let v = cpu.regs.hl;
            cpu.regs.hl += 1;
            v
        }
        R16::SP => {
            let v = cpu.regs.hl;
            cpu.regs.hl -= 1;
            v
        }
    };

    cpu.write_cycle(reg, a);
}

// Mnemonic: INC r16
// Full Name: Increment r16
// Description: Increments the given 16-bit register.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal Delay.
pub fn inc_16(cpu: &mut Cpu, reg: R16) {
    cpu.update(4);
    let v = cpu.regs.get_reg_16(&reg).wrapping_add(1);
    cpu.regs.set_reg_16(reg, v);
}

// Mnemonic: INC reg8
// Full Name: Increment reg8
// Description: Increments the given 8-bit register (or hl) reg8.
// Affected Flags: Z (set|res), N (res), H (set|res)
// Remarks: Zero is set if reg8 overflows, Half carry is set if there is a half carry between reg8 and 1.
// If a flags conditions aren't met, it is instead reset.
// Timing: "Instant" or "Read, Write"
pub fn inc_8(cpu: &mut Cpu, reg: Reg) {
    cpu.regs.f &= Flag::C.to_mask();
    let val;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            cpu.write_hl_cycle(val.wrapping_add(1))
        }
        r => {
            val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, val.wrapping_add(1))
        }
    };

    if val == 0xFF {
        cpu.regs.set_flag(Flag::Z)
    };
    if get_hca(val, 1) {
        cpu.regs.set_flag(Flag::H)
    };
}

// Mnemonic: DEC reg8
// Full Name: Decrement reg8
// Description: Decrements the given 8-bit register (or hl) reg8.
// Affected Flags: Z (set|res), N (set), H (set|res)
// Remarks: Zero is set if reg8 is 1, Half carry is set if reg8 & 0xf == 0.
// If a flags conditions aren't met, it is instead reset.
// Timing: "Instant" or "Read, Write"
pub fn dec_8(cpu: &mut Cpu, reg: Reg) {
    let val;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            cpu.write_hl_cycle(val.wrapping_sub(1))
        }
        r => {
            val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, val.wrapping_sub(1))
        }
    };

    cpu.regs.f &= Flag::C.to_mask();
    if val == 1 {
        cpu.regs.set_flag(Flag::Z)
    };
    cpu.regs.set_flag(Flag::N);
    if (val & 0xF) == 0 {
        cpu.regs.set_flag(Flag::H)
    };
}

// Mnemonic: LD reg8,d8
// Full Name: Load reg8,d8
// Description: Loads 8-bit unsigned data d8 into the register (or hl) reg8.
// Affected Flags: ----
// Remarks: ----
// Timing: "Read" or "Read, Write"
pub fn ld_r8_d8(cpu: &mut Cpu, reg: Reg) {
    let val = cpu.read_ipc_cycle();
    match reg {
        Reg::HL => cpu.write_hl_cycle(val),
        r => cpu.regs.set_reg(r, val),
    }
}

// Mnemonic: RLCA
// Full Name: Rotate Left Circular A
// Description: Sets A to, (A << 1) | (A >> 7)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 7 is set, otherwise it is reset.
// Timing: Instant.
pub fn rlca(cpu: &mut Cpu) {
    cpu.regs.res_all_flags();
    if (cpu.regs.a & 0x80) > 0 {
        cpu.regs.set_flag(Flag::C);
    }

    cpu.regs.a = (cpu.regs.a << 1) | (cpu.regs.a >> 7);
}

// Mnemonic: LD (a16),SP
// Full Name: Load (a16),sp
// Description: Loads sp into the address pointed to by 16-bit unsigned data a16.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Write, Write
pub fn ld_a16_sp(cpu: &mut Cpu) {
    let address = cpu.read_u16_cycle();
    let sp = cpu.regs.sp;
    cpu.write_u16_cycle(address, sp);
}

// Mnemonic: ADD HL,R16
// Full Name: Add HL,R16
// Description: Adds 16-bit register R16 to HL storing the result in HL
// Affected Flags: N (res), H (set|res), C (set|res)
// Remarks: Half Carry is set if there is a carry between bits 11 and 12. Carry is set if there is a carry out. Otherwise reset Half Carry or Carry respectively
// Timing: Internal Delay
pub fn add_hl_reg16(cpu: &mut Cpu, reg: R16) {
    let val = cpu.regs.get_reg_16(&reg);
    let res = cpu.regs.hl.wrapping_add(val);
    cpu.regs.f &= Flag::Z.to_mask();
    if (((cpu.regs.hl & 0xFFF) + (val & 0xFFF)) & 0x1000) == 0x1000 {
        cpu.regs.set_flag(Flag::H);
    }

    if res < cpu.regs.hl {
        cpu.regs.set_flag(Flag::C);
    }
    cpu.update(4);
    cpu.regs.hl = res;
}

// Mnemonic: LD A,(r16)
// Full Name: Load A, (<r16>)
// Description: Sets A to the address referenced by the 16 bit register r16.
// Affected Flags: ----
// Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
// Timing: Read
pub fn ld_a_r16(cpu: &mut Cpu, reg: R16) {
    let reg = match reg {
        R16::BC => cpu.regs.bc,
        R16::DE => cpu.regs.de,
        R16::HL => {
            let v = cpu.regs.hl;
            cpu.regs.hl += 1;
            v
        }
        R16::SP => {
            let v = cpu.regs.hl;
            cpu.regs.hl -= 1;
            v
        }
    };

    let val = cpu.read_cycle(reg);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: DEC r16
// Full Name: Decrement r16
// Description: Decrements the given 16-bit register.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal Delay.
pub fn dec_16(cpu: &mut Cpu, reg: R16) {
    cpu.update(4);
    let v = cpu.regs.get_reg_16(&reg).wrapping_sub(1);
    cpu.regs.set_reg_16(reg, v);
}

// Mnemonic: RRCA
// Full Name: Rotate Right Circular A
// Description: Sets A to, (A >> 1) | (A << 7)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 0 is set, otherwise it is reset.
// Timing: Instant.
pub fn rrca(cpu: &mut Cpu) {
    cpu.regs.res_all_flags();
    if (cpu.regs.a & 0x01) == 0x01 {
        cpu.regs.set_flag(Flag::C);
    }

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
    cpu.regs.a = (a << 1) | if cpu.regs.get_flag(Flag::C) { 1 } else { 0 };
    cpu.regs.res_all_flags();
    if a & 0x80 == 0x80 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: RRA
// Full Name: Rotate Right A
// Description: Sets A to, (A >> 1) | (c_in)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 0 is set, otherwise it is reset.
// Timing: Instant.
pub fn rra(cpu: &mut Cpu) {
    let a = cpu.regs.a;
    cpu.regs.a = (a >> 1) | if cpu.regs.get_flag(Flag::C) { 0x80 } else { 0 };
    cpu.regs.res_all_flags();
    if a & 0x01 == 0x01 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: DAA
// Full Name: Decimal Adjust AL
// Description: ???
// Affected Flags: Z (set|res), H (res), C (-|set)
// Remarks: Confusing
// Timing: Instant.
pub fn daa(cpu: &mut Cpu) {
    let mut res = cpu.regs.a as i32;
    if cpu.regs.get_flag(Flag::N) {
        if cpu.regs.get_flag(Flag::H) {
            res = (res - 6) & 0xFF;
        }

        if cpu.regs.get_flag(Flag::C) {
            res -= 0x60;
        }
    } else {
        if cpu.regs.get_flag(Flag::H) || (res & 0xF) > 9 {
            res += 0x06;
        }

        if cpu.regs.get_flag(Flag::C) || res > 0x9F {
            res += 0x60;
        }
    };

    cpu.regs.f &= Flag::N.to_mask() | Flag::C.to_mask();
    if (res & 0x100) == 0x100 {
        cpu.regs.set_flag(Flag::C);
    }

    cpu.regs.a = res as u8;

    if cpu.regs.a == 0 {
        cpu.regs.set_flag(Flag::Z);
    }
}

// Mnemonic: CPL
// Full Name: Complement
// Description: Bitwise complements A
// Affected Flags: N (set), H (set)
// Remarks: ----
// Timing: Instant.
pub fn cpl(cpu: &mut Cpu) {
    cpu.regs.a = !cpu.regs.a;
    cpu.regs.set_flag(Flag::N);
    cpu.regs.set_flag(Flag::H);
}

// Mnemonic: SCF
// Full Name: Set Carry Flag
// Description: Sets the carry flag
// Affected Flags: N (res), H (res), C (set)
// Remarks: ----
// Timing: Instant.
pub fn scf(cpu: &mut Cpu) {
    cpu.regs.f &= Flag::Z.to_mask();
    cpu.regs.set_flag(Flag::C);
}

// Mnemonic: CCF
// Full Name: Complement Carry Flag
// Description: Complements the carry flag
// Affected Flags: N (res), H (res), C (^C)
// Remarks: ----
// Timing: Instant.
pub fn ccf(cpu: &mut Cpu) {
    cpu.regs.f &= Flag::Z.to_mask() | Flag::C.to_mask();
    cpu.regs.f ^= Flag::C.to_mask();
}

// Mnemonic: ADD
// Full Name: Add
// Description: Adds the given reg (or hl, or imm) r to A and stores the result into A
// Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn add(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.a;
    let val = get_math_reg(cpu, reg);

    cpu.regs.a = a.wrapping_add(val);
    cpu.regs.res_all_flags();
    if cpu.regs.a == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if get_hca(a, val) {
        cpu.regs.set_flag(Flag::H);
    }

    if cpu.regs.a < a {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: ADC
// Full Name: Add with carry
// Description: Adds the given reg (or hl, or imm) r and carry to A and stores the result into A
// Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn adc(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.a;
    let c_in = cpu.regs.get_flag(Flag::C);
    let val = get_math_reg(cpu, reg);

    cpu.regs.a = a.wrapping_add(val).wrapping_add(c_in as u8);

    cpu.regs.res_all_flags();
    if cpu.regs.a == 0 {
        cpu.regs.set_flag(Flag::Z);
    }
    if (a & 0xF) + (val & 0xF) + c_in as u8 > 0xF {
        cpu.regs.set_flag(Flag::H);
    }

    if (a as u16)
        .wrapping_add(val as u16)
        .wrapping_add(c_in as u16) > 0xFF
    {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: SUB
// Full Name: Sub
// Description: Subtracts the given reg (or hl, or imm) r from A and stores the result into A
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn sub(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.get_reg(&Reg::A);
    let val = get_math_reg(cpu, reg);

    let res = a.wrapping_sub(val);
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    cpu.regs.set_flag(Flag::N);

    if (a & 0xF) < (val & 0xF) {
        cpu.regs.set_flag(Flag::H);
    }

    if val > a {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: SBC
// Full Name: Sub with carry
// Description: Subtracts the given reg (or hl, or imm) r and carry from A and stores the result into A
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn sbc(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.get_reg(&Reg::A);
    let c_in = cpu.regs.get_flag(Flag::C);
    let val = get_math_reg(cpu, reg);

    let res = (a as u16)
        .wrapping_sub(val as u16)
        .wrapping_sub(c_in as u16);
    cpu.regs.set_reg(Reg::A, res as u8);
    cpu.regs.res_all_flags();
    if (res & 0xFF) == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    cpu.regs.set_flag(Flag::N);

    if (a & 0xF) < ((val & 0xF) + c_in as u8) {
        cpu.regs.set_flag(Flag::H);
    }

    if res > 0xFF {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: AND
// Full Name: Bitwise And
// Description: Preforms bitwise AND on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (set), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn and(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.get_reg(&Reg::A);
    let val = get_math_reg(cpu, reg);

    let res = a & val;
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    cpu.regs.set_flag(Flag::H);
}

// Mnemonic: XOR
// Full Name: Bitwise Xor
// Description: Preforms bitwise XOR on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn xor(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.get_reg(&Reg::A);
    let val = get_math_reg(cpu, reg);

    let res = a ^ val;
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }
}

// Mnemonic: OR
// Full Name: Bitwise Or
// Description: Preforms bitwise Or on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn or(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.get_reg(&Reg::A);
    let val = get_math_reg(cpu, reg);

    let res = a | val;
    cpu.regs.set_reg(Reg::A, res);
    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }
}

// Mnemonic: CP
// Full Name: Compare
// Description: Subtracts the given reg (or hl, or imm) r from A discarding the result.
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn cp(cpu: &mut Cpu, reg: MathReg) {
    let a = cpu.regs.get_reg(&Reg::A);
    let val = get_math_reg(cpu, reg);

    cpu.regs.res_all_flags();
    if a == val {
        cpu.regs.set_flag(Flag::Z);
    }

    cpu.regs.set_flag(Flag::N);

    if (a & 0xF) < (val & 0xF) {
        cpu.regs.set_flag(Flag::H);
    }

    if a < val {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: RET <COND>
// Full Name: Return <COND>
// Description: Returns Conditionally. (NZ/Z/NC/C)
// Affected Flags: ----
// Remarks: ----
// Timing: "Internal Delay" or "Read, Read, Internal Delay"
pub fn retc(cpu: &mut Cpu, jump: bool) {
    cpu.update(4);
    if jump {
        let addr = cpu.read_pop_16_cycle();
        cpu.regs.pc = addr;
        cpu.update(4);
    }
}

// Mnemonic: POP <Reg-16>
// Full Name: Pop <Reg-16>
// Description: Pops the 16-bit register Reg-16 off of the stack.
// Affected Flags: ---- or Z (set|res), N (set|res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Read
pub fn pop(cpu: &mut Cpu, reg: R16) {
    let val = cpu.read_pop_16_cycle();
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
// Timing: "Read, Read" | "Read, Read, Write, Write"
pub fn call(cpu: &mut Cpu, jump: bool) {
    let addr = cpu.read_u16_cycle();
    if jump {
        let pc = cpu.regs.pc;
        cpu.write_push_16_cycle(pc);
        cpu.regs.pc = addr;
    }
}

// Mnemonic: PUSH <Reg-16>
// Full Name: Push <Reg-16>
// Description: Pushes the 16-bit register Reg-16 onto the stack.
// Affected Flags: ----
// Remarks: ----
// Timing: Delay, Write, Write
pub fn push(cpu: &mut Cpu, reg: R16) {
    let val = match reg {
        R16::SP => cpu.regs.g_af(),
        r => cpu.regs.get_reg_16(&r),
    };
    cpu.update(4);
    cpu.write_push_16_cycle(val);
}

// Mnemonic: RST <addr>
// Full Name: Reset <addr>
// Description: calls <addr>.
// Affected Flags: ----
// Remarks: ----
// Timing: Delay, Write, Write
pub fn rst(cpu: &mut Cpu, addr: u16) {
    cpu.update(4);
    let pc = cpu.regs.pc;
    cpu.write_push_16_cycle(pc);
    cpu.regs.pc = addr;
}

// Mnemonic: RET/RETI
// Full Name: Return / Return enable Interrupts
// Description: Returns unconditionally, if it's a reti instruction it will also enable IME.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Internal Delay
pub fn ret(cpu: &mut Cpu, reti: bool) {
    let addr = cpu.read_pop_16_cycle();
    cpu.regs.pc = addr;
    cpu.ime |= reti;
    cpu.update(4);
}

// Mnemonic: ldh (a8),A
// Full Name: Load High (a8),A
// Description: loads A into (0xFF00 | a8).
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Write
pub fn ldh_a8_a(cpu: &mut Cpu) {
    let addr = 0xFF00 | cpu.read_ipc_cycle() as u16;
    let a = cpu.regs.get_reg(&Reg::A);
    cpu.write_cycle(addr, a);
}

// Mnemonic: ldh (c),A
// Full Name: Load High (c),A
// Description: loads A into (0xFF00 | c).
// Affected Flags: ----
// Remarks: ----
// Timing: Write
pub fn ldh_c_a(cpu: &mut Cpu) {
    let addr = 0xFF00 | cpu.regs.get_reg(&Reg::C) as u16;
    let a = cpu.regs.get_reg(&Reg::A);
    cpu.write_cycle(addr, a);
}

// Mnemonic: ldh (a8),A
// Full Name: Load High (a8),A
// Description: loads A into (0xFF00 | a8).
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read
pub fn ldh_a_a8(cpu: &mut Cpu) {
    let addr = 0xFF00 | cpu.read_ipc_cycle() as u16;
    let val = cpu.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: ldh (c),A
// Full Name: Load High (c),A
// Description: loads A into (0xFF00 | c).
// Affected Flags: ----
// Remarks: ----
// Timing: Read
pub fn ldh_a_c(cpu: &mut Cpu) {
    let addr = 0xFF00 | cpu.regs.get_reg(&Reg::C) as u16;
    let val = cpu.read_cycle(addr);
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
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Internal Delay
pub fn ld_hl_sp_r8(cpu: &mut Cpu) {
    let r8 = (cpu.read_ipc_cycle() as i8) as u16;
    cpu.update(4);
    cpu.regs.res_all_flags();

    if ((cpu.regs.sp & 0x0F) + (r8 & 0x0F)) > 0x0F {
        cpu.regs.set_flag(Flag::H);
    }

    if (((cpu.regs.sp) & 0xFF) + (r8 & 0xFF)) > 0xFF {
        cpu.regs.set_flag(Flag::C);
    }

    cpu.regs.hl = cpu.regs.sp.wrapping_add(r8);
}

// Mnemonic: LD A,(a16)
// Full Name: Load A,(a16)
// Description: Load the value pointed at by 16-bit unsigned data a16 into A
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Read
pub fn ld_a_a16(cpu: &mut Cpu) {
    let addr = cpu.read_u16_cycle();
    let val = cpu.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, val);
}

// Mnemonic: LD (a16),A
// Full Name: Load (a16),A
// Description: Load A into the value pointed at by 16-bit unsigned data a16.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Write
pub fn ld_a16_a(cpu: &mut Cpu) {
    let addr = cpu.read_u16_cycle();
    let a = cpu.regs.get_reg(&Reg::A);
    cpu.write_cycle(addr, a);
}

// Mnemonic: LD SP,HL
// Full Name: Load sp,hl
// Description: Load hl into sp.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal delay
pub fn ld_sp_hl(cpu: &mut Cpu) {
    let hl = cpu.regs.hl;
    cpu.update(4);
    cpu.regs.sp = hl;
}

// Mnemonic: ADD SP,r8
// Full Name: Add sp, r8
// Description: Add 8-bit signed data r8 into sp
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Internal Delay, Internal Delay
pub fn add_sp_r8(cpu: &mut Cpu) {
    let r8 = cpu.read_ipc_cycle() as i8;
    cpu.update(8);
    cpu.regs.res_all_flags();
    if ((cpu.regs.sp & 0x0F) + (r8 as u16 & 0x0F)) > 0x0F {
        cpu.regs.set_flag(Flag::H);
    }

    if ((cpu.regs.sp & 0xFF) + (r8 as u16 & 0xFF)) > 0xFF {
        cpu.regs.set_flag(Flag::C);
    }

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
pub fn rlc(cpu: &mut Cpu, reg: Reg) {
    cpu.regs.res_all_flags();
    let val;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            cpu.write_hl_cycle((val << 1) | (val >> 7))
        }
        r => {
            val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, (val << 1) | (val >> 7))
        }
    };

    if val == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x80) > 0 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: RRC
// Full Name: Rotate Right Circular
// Description: Sets the given reg (or hl) r to, (r >> 1) | (r << 7)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rrc(cpu: &mut Cpu, reg: Reg) {
    cpu.regs.res_all_flags();
    let val;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            cpu.write_hl_cycle((val >> 1) | (val << 7))
        }
        r => {
            val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, (val >> 1) | (val << 7))
        }
    };

    if val == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x01) == 1 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: RL
// Full Name: Rotate Left
// Description, Sets the given reg (or hl) r to, (r << 1) | (carry_in)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rl(cpu: &mut Cpu, reg: Reg) {
    let val;
    let carry_in = if cpu.regs.get_flag(Flag::C) { 1 } else { 0 };
    let res;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            res = (val << 1) | (carry_in);
            cpu.write_hl_cycle(res)
        }
        r => {
            val = cpu.regs.get_reg(&r);
            res = (val << 1) | (carry_in);
            cpu.regs.set_reg(r, res)
        }
    };

    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x80) == 0x80 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: RR
// Full Name: Rotate Right
// Description, Sets the given reg (or hl) r to, (r >> 1) | (carry_in)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rr(cpu: &mut Cpu, reg: Reg) {
    let val;
    let carry_in = if cpu.regs.get_flag(Flag::C) {
        0x80
    } else {
        0x00
    };
    let res;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            res = (val >> 1) | (carry_in);
            cpu.write_hl_cycle(res)
        }
        r => {
            val = cpu.regs.get_reg(&r);
            res = (val >> 1) | (carry_in);
            cpu.regs.set_reg(r, res)
        }
    };

    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x01) == 0x01 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: SLA
// Full Name: Shift Left Arithmetic
// Description, Sets the given reg (or hl) r to (r << 1)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn sla(cpu: &mut Cpu, reg: Reg) {
    let val;
    let res;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            res = val << 1;
            cpu.write_hl_cycle(res)
        }
        r => {
            val = cpu.regs.get_reg(&r);
            res = val << 1;
            cpu.regs.set_reg(r, res)
        }
    };

    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x80) == 0x80 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: SRA
// Full Name: Shift Right Arithmetic
// Description, Sets the given reg (or hl) r to (r >> 1) | (r & 0x80)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn sra(cpu: &mut Cpu, reg: Reg) {
    let val;
    let res;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            res = (val >> 1) | (val & 0x80);
            cpu.write_hl_cycle(res)
        }
        r => {
            val = cpu.regs.get_reg(&r);
            res = (val >> 1) | (val & 0x80);
            cpu.regs.set_reg(r, res)
        }
    };

    cpu.regs.res_all_flags();
    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x01) == 0x01 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: SWAP
// Full Name: Swap
// Description: Swaps the upper and lower nibbles of the given reg (or hl) r. r=((r << 4) | (r >> 4))
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: Zero is set if the input was 0, otherwise it is reset
// Timing: "read, write" or instant.
pub fn swap(cpu: &mut Cpu, reg: Reg) {
    let val;
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            cpu.write_hl_cycle((val << 4) | (val >> 4))
        }
        r => {
            val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, (val << 4) | (val >> 4))
        }
    };

    cpu.regs.res_all_flags();
    if val == 0 {
        cpu.regs.set_flag(Flag::Z);
    }
}

// Mnemonic: SRL
// Full Name: Shift Right Logical
// Description, Sets the given reg (or hl) r to (r >> 1)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn srl(cpu: &mut Cpu, reg: Reg) {
    let val;
    let res;
    cpu.regs.res_all_flags();
    match reg {
        Reg::HL => {
            val = cpu.read_hl_cycle();
            res = val >> 1;
            cpu.write_hl_cycle(res);
        }
        r => {
            val = cpu.regs.get_reg(&r);
            res = val >> 1;
            cpu.regs.set_reg(r, res);
        }
    };

    if res == 0 {
        cpu.regs.set_flag(Flag::Z);
    }

    if (val & 0x01) == 0x01 {
        cpu.regs.set_flag(Flag::C);
    }
}

// Mnemonic: BIT
// Full Name: Bit Test
// Description: Tests the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: Z (set|res), N (res), H (set)
// Remarks: Zero is set if the bit is unset, and gets reset otherwise.
// Timing: "read" or instant.
pub fn bit(cpu: &mut Cpu, reg: Reg, mask: u8) {
    cpu.regs.f &= Flag::C.to_mask();
    cpu.regs.set_flag(Flag::H);
    match reg {
        Reg::HL => {
            if (cpu.read_hl_cycle() & mask) == 0 {
                cpu.regs.set_flag(Flag::Z)
            }
        }
        r => {
            if (cpu.regs.get_reg(&r) & mask) == 0 {
                cpu.regs.set_flag(Flag::Z)
            }
        }
    }
}

// Mnemonic: RES
// Full Name: Reset Bit
// Description: Resets the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: ----
// Remarks: ----
// Timing: "read, write" or instant.
pub fn res(cpu: &mut Cpu, reg: Reg, mask: u8) {
    match reg {
        Reg::HL => {
            let val = cpu.read_hl_cycle();
            cpu.write_hl_cycle(val & !mask)
        }
        r => {
            let val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, val & !mask)
        }
    }
}

// Mnemonic: SET
// Full Name: Set
// Description: Sets the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: ----
// Remarks: ----
// Timing: "read, write" or instant.
pub fn set(cpu: &mut Cpu, reg: Reg, mask: u8) {
    match reg {
        Reg::HL => {
            let val = cpu.read_hl_cycle();
            cpu.write_hl_cycle(val | mask)
        }
        r => {
            let val = cpu.regs.get_reg(&r);
            cpu.regs.set_reg(r, val | mask)
        }
    }
}
