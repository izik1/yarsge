// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use super::flags::Flag;

#[derive(Clone, Copy)]
pub enum Reg {
    B,
    C,
    D,
    E,
    H,
    L,
    HL,
    A,
}

impl Reg {
    pub fn from_num(num: u8) -> Self {
        match num & 7 {
            0b000 => Reg::B,
            0b001 => Reg::C,
            0b010 => Reg::D,
            0b011 => Reg::E,
            0b100 => Reg::H,
            0b101 => Reg::L,
            0b110 => Reg::HL,
            0b111 => Reg::A,
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
    pub pc: u16,
    pub a: u8,
    pub f: u8,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub sp: u16,
}

impl Registers {
    pub fn get_reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::B => (self.bc >> 8) as u8,
            Reg::C => (self.bc >> 0) as u8,
            Reg::D => (self.de >> 8) as u8,
            Reg::E => (self.de >> 0) as u8,
            Reg::H => (self.hl >> 8) as u8,
            Reg::L => (self.hl >> 0) as u8,
            Reg::HL => panic!(),
            Reg::A => self.a,
        }
    }

    pub fn get_reg_16(&self, reg: R16) -> u16 {
        match reg {
            R16::BC => self.bc,
            R16::DE => self.de,
            R16::HL => self.hl,
            R16::SP => self.sp,
        }
    }

    pub fn set_reg(&mut self, reg: Reg, val: u8) {
        match reg {
            Reg::B => self.bc = (self.bc & 0xFF) | ((val as u16) << 8),
            Reg::C => self.bc = (self.bc & 0xFF00) | (val as u16),
            Reg::D => self.de = (self.de & 0xFF) | ((val as u16) << 8),
            Reg::E => self.de = (self.de & 0xFF00) | (val as u16),
            Reg::H => self.hl = (self.hl & 0xFF) | ((val as u16) << 8),
            Reg::L => self.hl = (self.hl & 0xFF00) | (val as u16),
            Reg::HL => panic!(),
            Reg::A => self.a = val,
        }
    }

    pub fn g_af(&self) -> u16 {
        ((self.a as u16) << 8) | self.f as u16
    }

    pub fn s_af(&mut self, val: u16) {
        self.f = (val as u8) & 0b1111_0000;
        self.a = (val >> 8) as u8;
    }

    pub fn set_reg_16(&mut self, reg: R16, val: u16) {
        match reg {
            R16::BC => self.bc = val,
            R16::DE => self.de = val,
            R16::HL => self.hl = val,
            R16::SP => self.sp = val,
        }
    }

    pub fn new() -> Registers {
        Registers {
            pc: 0,
            a: 0,
            f: 0,
            bc: 0,
            de: 0,
            hl: 0,
            sp: 0,
        }
    }

    pub fn res_all_flags(&mut self) {
        self.f = 0b0000_0000;
    }

    pub fn assign_all_flags(&mut self, z: bool, n: bool, h: bool, c: bool) {
        self.res_all_flags();
        if z {
            self.set_flag(Flag::Z);
        }

        if n {
            self.set_flag(Flag::N);
        }

        if h {
            self.set_flag(Flag::H);
        }

        if c {
            self.set_flag(Flag::C);
        }
    }

    pub fn set_flag(&mut self, flag: Flag) {
        self.f |= flag.to_mask();
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        (self.f & flag.to_mask()) > 0
    }
}
