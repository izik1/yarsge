use crate::emu::registers::RegisterArg;
use crate::emu::{
    MCycle,
    cpu::{Cpu, Status},
    flags::CpuFlags,
    hardware::Hardware,
    registers::{R16, Reg},
};

#[derive(Clone, Copy)]
pub enum MathReg {
    R(RegisterArg),
    Imm,
}

fn get_register_arg(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> u8 {
    match register {
        RegisterArg::Indirect => hw.read_cycle(cpu.regs.hl),
        RegisterArg::Reg(r) => cpu.regs.reg(r),
    }
}

fn set_register_arg(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg, value: u8) {
    match register {
        RegisterArg::Indirect => hw.write_cycle(cpu.regs.hl, value),
        RegisterArg::Reg(r) => cpu.regs.set_reg(r, value),
    }
}

fn get_math_reg(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> u8 {
    match register {
        MathReg::Imm => cpu.fetch_imm8(hw),
        MathReg::R(r2) => get_register_arg(cpu, hw, r2),
    }
}

fn get_cin_lsb(flags: CpuFlags) -> u8 {
    u8::from(flags.contains(CpuFlags::C))
}

fn get_cin_msb(flags: CpuFlags) -> u8 {
    get_cin_lsb(flags) * 0x80
}

const fn eval_cc(ir: u8, flags: CpuFlags) -> bool {
    // I could do a bunch of really fun bitmath to do this, or, I could do it nicely, let's
    // do it nicely, I doubt it'll ever be a performance bottleneck. This always uses the
    // same bits of the instruction (mask: 0b0001_1000)

    // b1: Z vs C
    // b0 : not vs identity
    // isolate bits 1:0
    let cc = (ir >> 3) & 0b11;
    let flag = if (cc & 0b10) == 0b10 {
        CpuFlags::C
    } else {
        CpuFlags::Z
    };

    // isolate the specific flag, check if there are no flags (Nc), if `b0` is 1 (c), invert
    // result.
    flags.intersection(flag).is_empty() ^ ((cc & 0b01) == 1)
}

#[inline(never)]
#[cold]
pub fn invalid(cpu: &mut Cpu, _hw: &mut Hardware) -> ! {
    panic!("Invalid instruction executed: ir={:02x}", cpu.regs.ir);
}

// Mnemonic: NOP
// Full Name : No Operation
pub fn nop(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    // Cycle: M2/M1
    // Addr Bus : PC
    // Data bus : read IR
    // IDU : Inc PC
    cpu.generic_fetch(hw)
}

fn jr_jmp(cpu: &mut Cpu, hw: &mut Hardware, imm: i8) {
    // this should be putting some specific junk on the bus (but not asserting read or write)
    hw.idle_cycle();
    cpu.regs.pc = cpu.regs.pc.wrapping_add(i16::from(imm).cast_unsigned());
}

fn jp_jmp(cpu: &mut Cpu, hw: &mut Hardware, addr: u16) {
    // this should be putting some specific junk on the bus (but not asserting read or write)
    hw.idle_cycle();
    cpu.regs.pc = addr;
}

fn call_jmp(cpu: &mut Cpu, hw: &mut Hardware, addr: u16) {
    cpu.push16(hw, cpu.regs.pc);

    cpu.regs.pc = addr;
}

fn ret_jmp(cpu: &mut Cpu, hw: &mut Hardware, ei: bool) {
    let val = cpu.pop16(hw);

    // this should be putting some specific junk on the bus (but not asserting read or write)
    // in the specific case of RETI this also enables interrupts
    hw.idle_cycle();
    if ei {
        cpu.ime = true;
    }

    cpu.regs.pc = val;
}

// Mnemonic: JR
// Full Name: Jump Relative
// Description: Jumps to pc + r8.
// Affected Flags: ----
// Timing: read, addr-no-rw, <prefetch next>
pub fn jr_imm8(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    //   Cycle : M2
    //   Addr Bus : PC
    //   Data bus : read X
    //   IDU : Inc PC
    //   ---------
    //   Cycle : M3
    //   Addr Bus : PCH
    //   Data bus : ALU
    //   IDU : Y = adj PCh
    //   ALU : Z = PCL + Z
    //   ---------
    //   Cycle : M4/M1
    //   Addr Bus : YZ
    //   Data bus : read IR
    //   IDU : PC = Inc YZ

    let imm = cpu.fetch_imm8(hw).cast_signed();

    jr_jmp(cpu, hw, imm);

    cpu.generic_fetch(hw)
}

// Mnemonic: JP
// Full Name: Jump
// Description: Jumps to a16.
// Affected Flags: ----
// Timing: read, read, addr-no-rw, <prefetch next>
pub fn jp_imm16(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = cpu.fetch_imm16(hw);
    jp_jmp(cpu, hw, addr);
    cpu.generic_fetch(hw)
}

// Mnemonic: JR cc
// Full Name: Jump Relative cc
// Description: Jumps to pc + r8 if "cc" is true, otherwise it does nothing.
// Affected Flags: ----
// Remarks: This instruction stops 4 cycles short if it doesn't jump.
// Timing: read, <prefetch next>
pub fn jr_cc_imm8(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let imm = cpu.fetch_imm8(hw).cast_signed();
    if eval_cc(cpu.regs.ir, cpu.regs.f) {
        jr_jmp(cpu, hw, imm);
    }

    cpu.generic_fetch(hw)
}

// Mnemonic: JP cc
// Full Name: Jump
// Description: Jumps to a16 if "cc" is true, otherwise it does nothing.
// Affected Flags: ----
// Remarks: This instruction stops 4 cycles short if it doesn't jump.
// Timing: read, read, <prefetch next>
pub fn jp_cc_imm16(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = cpu.fetch_imm16(hw);
    if eval_cc(cpu.regs.ir, cpu.regs.f) {
        jp_jmp(cpu, hw, addr);
    }

    cpu.generic_fetch(hw)
}

// Mnemonic: LD
// Full Name: Load
// Description: Loads dest into src, either one of which can be HL but not both.
// Affected Flags: ----
// Remarks: I really like how this function came out. I think it looks nice.
// Timing: either "write", "read" or instant.
pub fn ld(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let dest = RegisterArg::from_num(cpu.regs.ir >> 3);
    let src = RegisterArg::from_num(cpu.regs.ir);

    match (dest, src) {
        // special case: can't double indirect. Game Boy CPU chose to use this for `HALT`.
        (RegisterArg::Indirect, RegisterArg::Indirect) => unreachable!(),
        (RegisterArg::Indirect, RegisterArg::Reg(src)) => {
            hw.write_cycle(cpu.regs.hl, cpu.regs.reg(src));
        }

        (RegisterArg::Reg(dest), RegisterArg::Indirect) => {
            cpu.regs.set_reg(dest, hw.read_cycle(cpu.regs.hl));
        }

        (RegisterArg::Reg(dest), RegisterArg::Reg(src)) => {
            let value = cpu.regs.reg(src);
            cpu.regs.set_reg(dest, value);
        }
    }

    cpu.generic_fetch(hw)
}

// Mnemonic: HALT
// Full Name: Halt
// Description: Halts the cpu.
// Affected Flags: ----
// Remarks: ----
// Timing: instant.
pub fn halt(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let (ir, interrupts) = hw.read_cycle_intr(cpu.regs.pc);
    cpu.regs.ir = ir;

    if !interrupts.is_empty() {
        if cpu.ime {
            return Status::InterruptDispatch;
        }

        return Status::Running;
    }

    Status::Halt
}

// Mnemonic: STOP
// Full Name: Stop
// Description: Stops the cpu.
// Affected Flags: ----
// Remarks: ----
// Timing: NA.
pub fn stop(_cpu: &mut Cpu, _hw: &mut Hardware) -> Status {
    log::warn!("todo: stop bug");
    // cpu.status = Status::Stop;
    Status::Stop
}

// Mnemonic: LD r16,d16
// Full Name: Load <r16>, d16
// Description: Sets the given 16 bit register to a 2 byte immediate value.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read
pub fn ld_r16_imm16(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    let value = cpu.fetch_imm16(hw);
    cpu.regs.set_reg_16(register, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: LD (r16),A
// Full Name: Load (<r16>), A
// Description: Sets the address referenced by the 16 bit register r16 to A.
// Affected Flags: ----
// Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
// Timing: Write
pub fn ld_r16_a(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    let (addr, hl_mod) = match register {
        R16::BC => (cpu.regs.bc, 0x0000),
        R16::DE => (cpu.regs.de, 0x0000),
        R16::HL => (cpu.regs.hl, 0x0001),
        R16::SP => (cpu.regs.hl, 0xffff),
    };

    cpu.regs.hl = cpu.regs.hl.wrapping_add(hl_mod);
    hw.write_cycle(addr, cpu.regs.a);

    cpu.generic_fetch(hw)
}

// Mnemonic: INC r16
// Full Name: Increment r16
// Description: Increments the given 16-bit register.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal Delay.
pub fn inc_16(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    hw.idle_cycle();
    let value = cpu.regs.get_reg_16(register).wrapping_add(1);
    cpu.regs.set_reg_16(register, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: INC reg8
// Full Name: Increment reg8
// Description: Increments the given 8-bit register (or hl) reg8.
// Affected Flags: Z (set|res), N (res), H (set|res)
// Remarks: Zero is set if reg8 overflows, Half carry is set if there is a half carry between reg8 and 1.
// If a flags conditions aren't met, it is instead reset.
// Timing: "Instant" or "Read, Write"
pub fn inc_8(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    set_register_arg(cpu, hw, register, value.wrapping_add(1));

    cpu.regs.f.remove(CpuFlags::N);
    cpu.regs.f.set(CpuFlags::Z, value == 0xff);

    let half_carry = (((value & 0xf) + 1) & 0x10) == 0x10;
    cpu.regs.f.set(CpuFlags::H, half_carry);

    cpu.generic_fetch(hw)
}

// Mnemonic: DEC reg8
// Full Name: Decrement reg8
// Description: Decrements the given 8-bit register (or hl) reg8.
// Affected Flags: Z (set|res), N (set), H (set|res)
// Remarks: Zero is set if reg8 is 1, Half carry is set if reg8 & 0xf == 0.
// If a flags conditions aren't met, it is instead reset.
// Timing: "Instant" or "Read, Write"
pub fn dec_8(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    set_register_arg(cpu, hw, register, value.wrapping_sub(1));

    cpu.regs.f.set(CpuFlags::Z, value == 1);
    cpu.regs.f.insert(CpuFlags::N);
    cpu.regs.f.set(CpuFlags::H, (value & 0xf) == 0);

    cpu.generic_fetch(hw)
}

// Mnemonic: LD reg8,d8
// Full Name: Load reg8,d8
// Description: Loads 8-bit unsigned data d8 into the register (or hl) reg8.
// Affected Flags: ----
// Remarks: ----
// Timing: "Read" or "Read, Write"
pub fn ld_r8_imm8(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = cpu.fetch_imm8(hw);
    set_register_arg(cpu, hw, register, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: RLCA
// Full Name: Rotate Left Circular A
// Description: Sets A to, (A << 1) | (A >> 7)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 7 is set, otherwise it is reset.
// Timing: Instant.
pub fn rlca(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.regs.f = CpuFlags::empty();
    cpu.regs.f.set(CpuFlags::C, cpu.regs.a >= 0x80);

    cpu.regs.a = cpu.regs.a.rotate_left(1);

    cpu.generic_fetch(hw)
}

// Mnemonic: LD (a16),SP
// Full Name: Load (a16),sp
// Description: Loads sp into the address pointed to by 16-bit unsigned data a16.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Write, Write
pub fn ld_a16_sp(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let address = cpu.fetch_imm16(hw);
    let sp = cpu.regs.sp;
    hw.write_u16_cycle(address, sp);

    cpu.generic_fetch(hw)
}

// Mnemonic: ADD HL,R16
// Full Name: Add HL,R16
// Description: Adds 16-bit register R16 to HL storing the result in HL
// Affected Flags: N (res), H (set|res), C (set|res)
// Remarks: Half Carry is set if there is a carry between bits 11 and 12. Carry is set if there is a carry out. Otherwise reset Half Carry or Carry respectively
// Timing: Internal Delay
pub fn add_hl_reg16(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    let value = cpu.regs.get_reg_16(register);
    let result = cpu.regs.hl.wrapping_add(value);

    cpu.regs.f.remove(CpuFlags::N);

    let half_carry = (((cpu.regs.hl & 0xfff) + (value & 0xfff)) & 0x1000) == 0x1000;

    cpu.regs.f.set(CpuFlags::H, half_carry);
    cpu.regs.f.set(CpuFlags::C, result < cpu.regs.hl);

    hw.idle_cycle();
    cpu.regs.hl = result;

    cpu.generic_fetch(hw)
}

// Mnemonic: LD A,(r16)
// Full Name: Load A, (<r16>)
// Description: Sets A to the address referenced by the 16 bit register r16.
// Affected Flags: ----
// Remarks: If r16 is HL, then HL increments after the operation. If r16 is SP it instead uses HL for the operation, and decrements HL after.
// Timing: Read
pub fn ld_a_r16(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    let (addr, hl_mod) = match register {
        R16::BC => (cpu.regs.bc, 0x0000),
        R16::DE => (cpu.regs.de, 0x0000),
        R16::HL => (cpu.regs.hl, 0x0001),
        R16::SP => (cpu.regs.hl, 0xffff),
    };

    cpu.regs.hl = cpu.regs.hl.wrapping_add(hl_mod);

    let value = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: DEC r16
// Full Name: Decrement r16
// Description: Decrements the given 16-bit register.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal Delay.
pub fn dec_16(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    hw.idle_cycle();
    cpu.regs
        .set_reg_16(register, cpu.regs.get_reg_16(register).wrapping_sub(1));

    cpu.generic_fetch(hw)
}

// Mnemonic: RRCA
// Full Name: Rotate Right Circular A
// Description: Sets A to, (A >> 1) | (A << 7)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 0 is set, otherwise it is reset.
// Timing: Instant.
pub fn rrca(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.regs.f = CpuFlags::empty();
    cpu.regs.f.set(CpuFlags::C, (cpu.regs.a & 0x01) == 0x01);
    cpu.regs.a = cpu.regs.a.rotate_right(1);

    cpu.generic_fetch(hw)
}

// Mnemonic: RLA
// Full Name: Rotate Left A
// Description: Sets A to, (A << 1) | (c_in)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 7 is set, otherwise it is reset.
// Timing: Instant.
pub fn rla(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let a = cpu.regs.a;
    cpu.regs.a = (a << 1) | get_cin_lsb(cpu.regs.f);
    cpu.regs.f = CpuFlags::empty();
    cpu.regs.f.set(CpuFlags::C, a & 0x80 == 0x80);

    cpu.generic_fetch(hw)
}

// Mnemonic: RRA
// Full Name: Rotate Right A
// Description: Sets A to, (A >> 1) | (c_in)
// Affected Flags: Z (res), N (res), H (res), C (set|res)
// Remarks: Carry is set if bit 0 is set, otherwise it is reset.
// Timing: Instant.
pub fn rra(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let a = cpu.regs.a;
    cpu.regs.a = (a >> 1) | get_cin_msb(cpu.regs.f);
    cpu.regs.f = CpuFlags::empty();
    cpu.regs.f.set(CpuFlags::C, a & 0x01 == 0x01);

    cpu.generic_fetch(hw)
}

// Mnemonic: DAA
// Full Name: Decimal Adjust AL
// Description: ???
// Affected Flags: Z (set|res), H (res), C (-|set)
// Remarks: Confusing
// Timing: Instant.
pub fn daa(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let mut res = i32::from(cpu.regs.a); // todo: check if this can be i16.
    if cpu.regs.f.contains(CpuFlags::N) {
        if cpu.regs.f.contains(CpuFlags::H) {
            res = (res - 6) & 0xff;
        }

        if cpu.regs.f.contains(CpuFlags::C) {
            res -= 0x60;
        }
    } else {
        if cpu.regs.f.contains(CpuFlags::H) || (res & 0xf) > 9 {
            res += 0x06;
        }

        if cpu.regs.f.contains(CpuFlags::C) || res >= 0xa0 {
            res += 0x60;
        }
    }

    cpu.regs.f.remove(CpuFlags::H);

    if (res & 0x100) == 0x100 {
        cpu.regs.f |= CpuFlags::C;
    }

    cpu.regs.a = res as u8;

    cpu.regs.f.set(CpuFlags::Z, cpu.regs.a == 0);

    cpu.generic_fetch(hw)
}

// Mnemonic: CPL
// Full Name: Complement
// Description: Bitwise complements A
// Affected Flags: N (set), H (set)
// Remarks: ----
// Timing: Instant.
pub fn cpl(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.regs.a = !cpu.regs.a;
    cpu.regs.f |= CpuFlags::N | CpuFlags::H;

    cpu.generic_fetch(hw)
}

// Mnemonic: SCF
// Full Name: Set Carry Flag
// Description: Sets the carry flag
// Affected Flags: N (res), H (res), C (set)
// Remarks: ----
// Timing: Instant.
pub fn scf(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.regs.f &= CpuFlags::Z;
    cpu.regs.f |= CpuFlags::C;

    cpu.generic_fetch(hw)
}

// Mnemonic: CCF
// Full Name: Complement Carry Flag
// Description: Complements the carry flag
// Affected Flags: N (res), H (res), C (^C)
// Remarks: ----
// Timing: Instant.
pub fn ccf(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.regs.f &= CpuFlags::Z | CpuFlags::C;
    cpu.regs.f ^= CpuFlags::C;

    cpu.generic_fetch(hw)
}

// Mnemonic: ADD
// Full Name: Add
// Description: Adds the given reg (or hl, or imm) r to A and stores the result into A
// Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn add(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.a;
    let value = get_math_reg(cpu, hw, register);

    cpu.regs.a = a.wrapping_add(value);

    cpu.regs.f.set(CpuFlags::Z, cpu.regs.a == 0);
    cpu.regs.f.remove(CpuFlags::N);

    let half_carry = (((a & 0xf) + (value & 0xf)) & 0x10) == 0x10;
    cpu.regs.f.set(CpuFlags::H, half_carry);

    cpu.regs.f.set(CpuFlags::C, cpu.regs.a < a);

    cpu.generic_fetch(hw)
}

// Mnemonic: ADC
// Full Name: Add with carry
// Description: Adds the given reg (or hl, or imm) r and carry to A and stores the result into A
// Affected Flags: Z (set|res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn adc(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.a;
    let c_in = get_cin_lsb(cpu.regs.f);
    let value = get_math_reg(cpu, hw, register);

    cpu.regs.a = a.wrapping_add(value).wrapping_add(c_in);

    cpu.regs.f.set(CpuFlags::Z, cpu.regs.a == 0);
    cpu.regs.f.remove(CpuFlags::N);

    let half_carry = (a & 0xf) + (value & 0xf) + c_in > 0xf;
    cpu.regs.f.set(CpuFlags::H, half_carry);

    let carry_flag = u16::from(a) + u16::from(value) + u16::from(c_in) > 0xff;
    cpu.regs.f.set(CpuFlags::C, carry_flag);

    cpu.generic_fetch(hw)
}

// Mnemonic: SUB
// Full Name: Sub
// Description: Subtracts the given reg (or hl, or imm) r from A and stores the result into A
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn sub(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.reg(Reg::A);
    let value = get_math_reg(cpu, hw, register);

    let result = a.wrapping_sub(value);
    cpu.regs.set_reg(Reg::A, result);

    cpu.regs.f.set(CpuFlags::Z, result == 0);
    cpu.regs.f.insert(CpuFlags::N);
    cpu.regs.f.set(CpuFlags::H, (a & 0xf) < (value & 0xf));
    cpu.regs.f.set(CpuFlags::C, value > a);

    cpu.generic_fetch(hw)
}

// Mnemonic: SBC
// Full Name: Sub with carry
// Description: Subtracts the given reg (or hl, or imm) r and carry from A and stores the result into A
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn sbc(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.reg(Reg::A);
    let carry_in = get_cin_lsb(cpu.regs.f);
    let value = get_math_reg(cpu, hw, register);

    let result = u16::from(a)
        .wrapping_sub(u16::from(value))
        .wrapping_sub(u16::from(carry_in));

    cpu.regs.set_reg(Reg::A, result as u8);

    cpu.regs.f.set(CpuFlags::Z, (result & 0xff) == 0);
    cpu.regs.f.insert(CpuFlags::N);

    let half_carry = (a & 0xf) < ((value & 0xf) + carry_in);
    cpu.regs.f.set(CpuFlags::H, half_carry);

    cpu.regs.f.set(CpuFlags::C, result > 0xff);

    cpu.generic_fetch(hw)
}

// Mnemonic: AND
// Full Name: Bitwise And
// Description: Preforms bitwise AND on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (set), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn and(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.reg(Reg::A);
    let value = get_math_reg(cpu, hw, register);

    let result = a & value;
    cpu.regs.set_reg(Reg::A, result);
    cpu.regs.f = CpuFlags::H
        | if result == 0 {
            CpuFlags::Z
        } else {
            CpuFlags::empty()
        };

    cpu.generic_fetch(hw)
}

// Mnemonic: XOR
// Full Name: Bitwise Xor
// Description: Preforms bitwise XOR on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn xor(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.reg(Reg::A);
    let value = get_math_reg(cpu, hw, register);

    let result = a ^ value;
    cpu.regs.set_reg(Reg::A, result);
    cpu.regs.f = if result == 0 {
        CpuFlags::Z
    } else {
        CpuFlags::empty()
    };

    cpu.generic_fetch(hw)
}

// Mnemonic: OR
// Full Name: Bitwise Or
// Description: Preforms bitwise Or on the given reg (or hl, or imm) r and A, storing the result into A
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: ----
// Timing: Read or Instant
pub fn or(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.reg(Reg::A);
    let value = get_math_reg(cpu, hw, register);

    let result = a | value;
    cpu.regs.set_reg(Reg::A, result);
    cpu.regs.f = if result == 0 {
        CpuFlags::Z
    } else {
        CpuFlags::empty()
    };

    cpu.generic_fetch(hw)
}

// Mnemonic: CP
// Full Name: Compare
// Description: Subtracts the given reg (or hl, or imm) r from A discarding the result.
// Affected Flags: Z (set|res), N (set), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read or Instant
pub fn cp(cpu: &mut Cpu, hw: &mut Hardware, register: MathReg) -> Status {
    let a = cpu.regs.reg(Reg::A);
    let value = get_math_reg(cpu, hw, register);

    cpu.regs.f.set(CpuFlags::Z, a == value);
    cpu.regs.f.insert(CpuFlags::N);
    cpu.regs.f.set(CpuFlags::H, (a & 0xf) < (value & 0xf));
    cpu.regs.f.set(CpuFlags::C, a < value);

    cpu.generic_fetch(hw)
}

// Mnemonic: RET <cc>
// Full Name: Return <cc>
// Description: Returns Conditionally. (NZ/Z/NC/C)
// Affected Flags: ----
// Remarks: ----
// Timing: "Internal Delay" or "Read, Read, Internal Delay"
pub fn ret_cc(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    // cc=true
    //   Cycle : M2
    //   Addr Bus : 0000
    //   Misc : cc check
    //   ---------
    //   Cycle : M3
    //   Addr Bus : SP
    //   Data Bus : read Y
    //   IDU : Inc SP
    //   ---------
    //   Cycle : M4
    //   Addr Bus : SP
    //   Data Bus : read X
    //   IDU : Inc SP
    //   ---------
    //   Cycle : M5
    //   Addr Bus : 0000 ; (really? really)
    //   Misc : PC = XY
    //   ---------
    //   Cycle : M6/M1
    //   Addr Bus : PC
    //   Data Bus : read IR
    //   IDU : Inc PC
    // cc=false
    //   Cycle : M2
    //   Addr Bus : 0000
    //   Misc : cc check
    //   ---------
    //   Cycle : M3/M1
    //   Addr Bus : PC
    //   Data Bus : read IR
    //   IDU : Inc PC

    // check cc
    hw.idle_cycle();

    if eval_cc(cpu.regs.ir, cpu.regs.f) {
        ret_jmp(cpu, hw, false);
    }

    // generic fetch that's done either way
    cpu.generic_fetch(hw)
}

// Mnemonic: POP <Reg-16>
// Full Name: Pop <Reg-16>
// Description: Pops the 16-bit register Reg-16 off of the stack.
// Affected Flags: ---- or Z (set|res), N (set|res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Read
pub fn pop(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    let value = cpu.pop16(hw);
    match register {
        R16::SP => cpu.regs.set_af(value),
        r => cpu.regs.set_reg_16(r, value),
    }

    cpu.generic_fetch(hw)
}

// Mnemonic: CALL cc
// Full Name: Call
// Description: Calls
// Affected Flags: ----
// Remarks: ----
// Timing: "Read, Read, Delay, Write, Write"
pub fn call(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = cpu.fetch_imm16(hw);

    call_jmp(cpu, hw, addr);

    cpu.generic_fetch(hw)
}
// Mnemonic: CALL <COND> | CALL
// Full Name: Call <COND> | Call
// Description: Calls conditionally (NZ/Z/NC/C)
// Affected Flags: ----
// Remarks: ----
// Timing: "Read, Read" | "Read, Read, Delay, Write, Write"
pub fn call_cc(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = cpu.fetch_imm16(hw);
    if eval_cc(cpu.regs.ir, cpu.regs.f) {
        call_jmp(cpu, hw, addr);
    }

    cpu.generic_fetch(hw)
}

// Mnemonic: PUSH <Reg-16>
// Full Name: Push <Reg-16>
// Description: Pushes the 16-bit register Reg-16 onto the stack.
// Affected Flags: ----
// Remarks: ----
// Timing: Delay, Write, Write
pub fn push(cpu: &mut Cpu, hw: &mut Hardware, register: R16) -> Status {
    let value = match register {
        R16::SP => cpu.regs.get_af(),
        r => cpu.regs.get_reg_16(r),
    };

    cpu.push16(hw, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: RST <addr>
// Full Name: Reset <addr>
// Description: calls <addr>.
// Affected Flags: ----
// Remarks: ----
// Timing: Delay, Write, Write
pub fn rst(cpu: &mut Cpu, hw: &mut Hardware, addr: u16) -> Status {
    call_jmp(cpu, hw, addr);

    cpu.generic_fetch(hw)
}

// Mnemonic: RET/RETI
// Full Name: Return / Return enable Interrupts
// Description: Returns unconditionally, if it's a reti instruction it will also enable IME.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Internal Delay
pub fn ret<const EI: bool>(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    ret_jmp(cpu, hw, EI);
    cpu.generic_fetch(hw)
}

// Mnemonic: LDH (a8),A
// Full Name: Load High (a8),A
// Description: loads A into (0xff00 | a8).
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Write
pub fn ldh_a8_a(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = 0xff00 | u16::from(cpu.fetch_imm8(hw));
    let a = cpu.regs.reg(Reg::A);
    hw.write_cycle(addr, a);

    cpu.generic_fetch(hw)
}

// Mnemonic: LDH (c),A
// Full Name: Load High (c),A
// Description: loads A into (0xff00 | c).
// Affected Flags: ----
// Remarks: ----
// Timing: Write
pub fn ldh_c_a(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = 0xff00 | u16::from(cpu.regs.reg(Reg::C));
    let a = cpu.regs.reg(Reg::A);
    hw.write_cycle(addr, a);

    cpu.generic_fetch(hw)
}

// Mnemonic: LDH (a8),A
// Full Name: Load High (a8),A
// Description: loads A into (0xff00 | a8).
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read
pub fn ldh_a_a8(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = 0xff00 | u16::from(cpu.fetch_imm8(hw));
    let value = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: LDH (c),A
// Full Name: Load High (c),A
// Description: loads A into (0xff00 | c).
// Affected Flags: ----
// Remarks: ----
// Timing: Read
pub fn ldh_a_c(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = 0xff00 | u16::from(cpu.regs.reg(Reg::C));
    let value = hw.read_cycle(addr);
    cpu.regs.set_reg(Reg::A, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: DI
// Full Name: Disable Interrupts
// Description: Disables interrupts.
// Affected Flags: ----
// Remarks: ----
// Timing: Instant
pub fn di(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.ime = false;
    cpu.generic_fetch(hw)
}

// Mnemonic: EI
// Full Name: Enable Interrupts
// Description: Enables interrupts.
// Affected Flags: ----
// Remarks: Interrupt enabling is delayed by 4-TCycles.
// Timing: Instant (delayed affect)
pub fn ei(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let status = cpu.generic_fetch(hw);
    cpu.ime = true;

    status
}

// Mnemonic: JP (HL)
// Full Name: Jump (HL)
// Description: Jumps to HL.
// Affected Flags: ----
// Remarks: ----
// Timing: Instant
pub fn jp_hl(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    cpu.regs.pc = cpu.regs.hl;

    cpu.generic_fetch(hw)
}

// Mnemonic: LD (HL),SP+r8
// Full Name: Load (HL), SP+r8
// Description: Loads SP+signed 8-bit value r8 into HL
// Affected Flags: Z (res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Internal Delay
pub fn ld_hl_sp_r8(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let r8 = (cpu.fetch_imm8(hw) as i8) as u16;
    hw.idle_cycle();

    cpu.regs.f = CpuFlags::empty();
    cpu.regs
        .f
        .set(CpuFlags::H, ((cpu.regs.sp & 0x0f) + (r8 & 0x0f)) > 0x0f);
    cpu.regs
        .f
        .set(CpuFlags::C, (((cpu.regs.sp) & 0xff) + (r8 & 0xff)) > 0xff);

    cpu.regs.hl = cpu.regs.sp.wrapping_add(r8);

    cpu.generic_fetch(hw)
}

// Mnemonic: LD A,(a16)
// Full Name: Load A,(a16)
// Description: Load the value pointed at by 16-bit unsigned data a16 into A
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Read
pub fn ld_a_a16(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let address = cpu.fetch_imm16(hw);
    let value = hw.read_cycle(address);
    cpu.regs.set_reg(Reg::A, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: LD (a16),A
// Full Name: Load (a16),A
// Description: Load A into the value pointed at by 16-bit unsigned data a16.
// Affected Flags: ----
// Remarks: ----
// Timing: Read, Read, Write
pub fn ld_a16_a(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let addr = cpu.fetch_imm16(hw);
    let a = cpu.regs.reg(Reg::A);
    hw.write_cycle(addr, a);

    cpu.generic_fetch(hw)
}

// Mnemonic: LD SP,HL
// Full Name: Load sp,hl
// Description: Load hl into sp.
// Affected Flags: ----
// Remarks: ----
// Timing: Internal delay
pub fn ld_sp_hl(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let hl = cpu.regs.hl;
    hw.idle_cycle();
    cpu.regs.sp = hl;

    cpu.generic_fetch(hw)
}

// Mnemonic: ADD SP,r8
// Full Name: Add sp, r8
// Description: Add 8-bit signed data r8 into sp
// Affected Flags: Z (res), N (res), H (set|res), C (set|res)
// Remarks: ----
// Timing: Read, Internal Delay, Internal Delay
pub fn add_sp_r8(cpu: &mut Cpu, hw: &mut Hardware) -> Status {
    let r8 = cpu.fetch_imm8(hw) as i8;
    hw.stall(MCycle(2));

    cpu.regs.f = CpuFlags::empty();
    cpu.regs.f.set(
        CpuFlags::H,
        ((cpu.regs.sp & 0x0f) + (r8 as u16 & 0x0f)) > 0x0f,
    );
    cpu.regs.f.set(
        CpuFlags::C,
        ((cpu.regs.sp & 0xff) + (r8 as u16 & 0xff)) > 0xff,
    );

    cpu.regs.sp = cpu.regs.sp.wrapping_add(r8 as u16);

    cpu.generic_fetch(hw)
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
pub fn rlc(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    set_register_arg(cpu, hw, register, value.rotate_left(1));

    cpu.regs.f.set(CpuFlags::Z, value == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x80) == 0x80);

    cpu.generic_fetch(hw)
}

// Mnemonic: RRC
// Full Name: Rotate Right Circular
// Description: Sets the given reg (or hl) r to, (r >> 1) | (r << 7)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rrc(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    set_register_arg(cpu, hw, register, value.rotate_right(1));

    cpu.regs.f.set(CpuFlags::Z, value == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x01) == 0x01);

    cpu.generic_fetch(hw)
}

// Mnemonic: RL
// Full Name: Rotate Left
// Description, Sets the given reg (or hl) r to, (r << 1) | (carry_in)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rl(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    let result = (value << 1) | get_cin_lsb(cpu.regs.f);
    set_register_arg(cpu, hw, register, result);

    cpu.regs.f.set(CpuFlags::Z, result == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x80) == 0x80);

    cpu.generic_fetch(hw)
}

// Mnemonic: RR
// Full Name: Rotate Right
// Description, Sets the given reg (or hl) r to, (r >> 1) | (carry_in)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn rr(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    let result = (value >> 1) | get_cin_msb(cpu.regs.f);

    set_register_arg(cpu, hw, register, result);

    cpu.regs.f.set(CpuFlags::Z, result == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x01) == 0x01);

    cpu.generic_fetch(hw)
}

// Mnemonic: SLA
// Full Name: Shift Left Arithmetic
// Description, Sets the given reg (or hl) r to (r << 1)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 7 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn sla(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    let result = value << 1;
    set_register_arg(cpu, hw, register, result);

    cpu.regs.f.set(CpuFlags::Z, result == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x80) == 0x80);

    cpu.generic_fetch(hw)
}

// Mnemonic: SRA
// Full Name: Shift Right Arithmetic
// Description, Sets the given reg (or hl) r to (r >> 1) | (r & 0x80)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn sra(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    let result = (value >> 1) | (value & 0x80);
    set_register_arg(cpu, hw, register, result);

    cpu.regs.f.set(CpuFlags::Z, result == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x01) == 0x01);

    cpu.generic_fetch(hw)
}

// Mnemonic: SWAP
// Full Name: Swap
// Description: Swaps the upper and lower nibbles of the given reg (or hl) r. r=((r << 4) | (r >> 4))
// Affected Flags: Z (set|res), N (res), H (res), C (res)
// Remarks: Zero is set if the input was 0, otherwise it is reset
// Timing: "read, write" or instant.
pub fn swap(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    set_register_arg(cpu, hw, register, value.rotate_right(4));

    cpu.regs.f = CpuFlags::empty();
    cpu.regs.f.set(CpuFlags::Z, value == 0);

    cpu.generic_fetch(hw)
}

// Mnemonic: SRL
// Full Name: Shift Right Logical
// Description, Sets the given reg (or hl) r to (r >> 1)
// Affected Flags: Z (set|res), N (res), H (res), C (set|res)
// Remarks: Zero is set if the input was 0, Carry is set if bit 0 is set. If their conditions aren't satisfied, they are reset.
// Timing: "read, write" or instant.
pub fn srl(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg) -> Status {
    let value = get_register_arg(cpu, hw, register);
    let result = value >> 1;
    set_register_arg(cpu, hw, register, result);

    cpu.regs.f.set(CpuFlags::Z, result == 0);
    cpu.regs.f.remove(CpuFlags::N | CpuFlags::H);
    cpu.regs.f.set(CpuFlags::C, (value & 0x01) == 0x01);

    cpu.generic_fetch(hw)
}

// Mnemonic: BIT
// Full Name: Bit Test
// Description: Tests the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: Z (set|res), N (res), H (set)
// Remarks: Zero is set if the bit is unset, and gets reset otherwise.
// Timing: "read" or instant.
pub fn bit(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg, mask: u8) -> Status {
    cpu.regs.f &= CpuFlags::C;
    cpu.regs.f |= CpuFlags::H;
    let b = (get_register_arg(cpu, hw, register) & mask) == 0;

    cpu.regs.f.set(CpuFlags::Z, b);

    cpu.generic_fetch(hw)
}

// Mnemonic: RES
// Full Name: Reset Bit
// Description: Resets the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: ----
// Remarks: ----
// Timing: "read, write" or instant.
pub fn res(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg, mask: u8) -> Status {
    let value = get_register_arg(cpu, hw, register) & !mask;
    set_register_arg(cpu, hw, register, value);

    cpu.generic_fetch(hw)
}

// Mnemonic: SET
// Full Name: Set
// Description: Sets the given bit (in the form of a mask --that is implementation specific,
// other ways to do the same thing include 1 << N where N is the bit number), of the given reg (or hl)
// Affected Flags: ----
// Remarks: ----
// Timing: "read, write" or instant.
pub fn set(cpu: &mut Cpu, hw: &mut Hardware, register: RegisterArg, mask: u8) -> Status {
    let value = get_register_arg(cpu, hw, register) | mask;
    set_register_arg(cpu, hw, register, value);

    cpu.generic_fetch(hw)
}
