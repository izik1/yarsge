use std::num::Wrapping;

use super::flags::CpuFlags;

#[derive(Clone, Copy)]
pub enum Reg {
    B,
    C,
    D,
    E,
    H,
    L,
    A,
}

#[derive(Clone, Copy)]
pub enum RegisterArg {
    Reg(Reg),
    Indirect,
}

impl RegisterArg {
    #[must_use]
    #[inline]
    pub fn from_num(num: u8) -> Self {
        match num & 7 {
            0b000 => Self::Reg(Reg::B),
            0b001 => Self::Reg(Reg::C),
            0b010 => Self::Reg(Reg::D),
            0b011 => Self::Reg(Reg::E),
            0b100 => Self::Reg(Reg::H),
            0b101 => Self::Reg(Reg::L),
            0b110 => Self::Indirect,
            0b111 => Self::Reg(Reg::A),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum R16 {
    BC,
    DE,
    HL,
    SP,
}

pub struct Registers {
    pub pc: Wrapping<u16>,
    pub a: u8,
    pub f: CpuFlags,
    pub bc: Wrapping<u16>,
    pub de: Wrapping<u16>,
    pub hl: Wrapping<u16>,
    pub sp: Wrapping<u16>,
    pub ir: u8,
}

impl Registers {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            pc: Wrapping(0),
            a: 0,
            f: CpuFlags::empty(),
            bc: Wrapping(0),
            de: Wrapping(0),
            hl: Wrapping(0),
            sp: Wrapping(0),
            ir: 0,
        }
    }

    #[must_use]
    pub fn reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::B => (self.bc.0 >> 8) as u8,
            Reg::C => self.bc.0 as u8,
            Reg::D => (self.de.0 >> 8) as u8,
            Reg::E => self.de.0 as u8,
            Reg::H => (self.hl.0 >> 8) as u8,
            Reg::L => self.hl.0 as u8,
            Reg::A => self.a,
        }
    }

    #[must_use]
    pub fn get_reg_16(&self, reg: R16) -> u16 {
        match reg {
            R16::BC => self.bc.0,
            R16::DE => self.de.0,
            R16::HL => self.hl.0,
            R16::SP => self.sp.0,
        }
    }

    pub fn set_reg(&mut self, reg: Reg, val: u8) {
        match reg {
            Reg::B => self.bc.0 = (self.bc.0 & 0xff) | (u16::from(val) << 8),
            Reg::C => self.bc.0 = (self.bc.0 & 0xff00) | u16::from(val),
            Reg::D => self.de.0 = (self.de.0 & 0xff) | (u16::from(val) << 8),
            Reg::E => self.de.0 = (self.de.0 & 0xff00) | u16::from(val),
            Reg::H => self.hl.0 = (self.hl.0 & 0xff) | (u16::from(val) << 8),
            Reg::L => self.hl.0 = (self.hl.0 & 0xff00) | u16::from(val),
            Reg::A => self.a = val,
        }
    }

    #[must_use]
    pub fn get_af(&self) -> u16 {
        u16::from_be_bytes([self.a, self.f.bits()])
    }

    pub fn set_af(&mut self, val: u16) {
        let [a, f] = val.to_be_bytes();
        self.f = CpuFlags::from_bits_truncate(f);
        self.a = a;
    }

    pub fn set_reg_16(&mut self, reg: R16, val: u16) {
        match reg {
            R16::BC => self.bc.0 = val,
            R16::DE => self.de.0 = val,
            R16::HL => self.hl.0 = val,
            R16::SP => self.sp.0 = val,
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self::new()
    }
}
