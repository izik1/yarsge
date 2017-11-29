// Copyright Zachery Gyurkovitz 2017 MIT License, see licence.md for more details.

use super::flags::Flag;

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

pub enum R16 {
    BC,
    DE,
    HL,
    SP,
}

pub struct Registers {
    pub pc: u16,
    pub af: u16,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub sp: u16,
}

impl Registers{
    pub fn get_reg(&self, reg: &Reg) -> u8 {
        match reg {
            &Reg::B => (self.bc >> 8) as u8,
            &Reg::C => (self.bc >> 0) as u8,
            &Reg::D => (self.de >> 8) as u8,
            &Reg::E => (self.de >> 0) as u8,
            &Reg::H => (self.hl >> 8) as u8,
            &Reg::L => (self.hl >> 0) as u8,
            &Reg::HL => panic!(),
            &Reg::A => (self.af >> 8) as u8,
        }
    }
    
    pub fn get_reg_16(&self, reg: &R16) -> u16 {
        match reg {
            &R16::BC => self.bc,
            &R16::DE => self.de,
            &R16::HL => self.hl,
            &R16::SP => self.sp,
        }
    }
    
    
    pub fn set_reg(&mut self, reg: Reg, val: u8) {
        match reg {
            Reg::B => self.bc = (self.bc & 0xFF)   | ((val as u16) << 8),
            Reg::C => self.bc = (self.bc & 0xFF00) |  (val as u16),
            Reg::D => self.de = (self.de & 0xFF)   | ((val as u16) << 8),
            Reg::E => self.de = (self.de & 0xFF00) |  (val as u16),
            Reg::H => self.hl = (self.hl & 0xFF)   | ((val as u16) << 8),
            Reg::L => self.hl = (self.hl & 0xFF00) |  (val as u16),
            Reg::HL => panic!(),
            Reg::A => self.af = (self.af & 0xFF)   | ((val as u16) << 8),
        }
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
        Registers {pc: 0, af: 0, bc: 0, de: 0, hl: 0, sp: 0}
    }
    
    pub fn res_all_flags(&mut self) {
        self.af &= 0b1111_1111_0000_0000;
    }
    
    pub fn set_flag(&mut self, flag: Flag) {
        self.af |= flag.to_mask() as u16;
    }
        
    pub fn get_flag(&self, flag: Flag) -> bool {
        (self.af & (flag.to_mask() as u16)) > 0
    }
}