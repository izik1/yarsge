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
    pub pc: u16,
    pub a: u8,
    pub f: CpuFlags,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub sp: u16,
    pub ir: u8,
}

impl Registers {
    #[must_use]
    pub fn reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::B => (self.bc >> 8) as u8,
            Reg::C => self.bc as u8,
            Reg::D => (self.de >> 8) as u8,
            Reg::E => self.de as u8,
            Reg::H => (self.hl >> 8) as u8,
            Reg::L => self.hl as u8,
            Reg::A => self.a,
        }
    }

    #[must_use]
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
            Reg::B => self.bc = (self.bc & 0xFF) | (u16::from(val) << 8),
            Reg::C => self.bc = (self.bc & 0xFF00) | u16::from(val),
            Reg::D => self.de = (self.de & 0xFF) | (u16::from(val) << 8),
            Reg::E => self.de = (self.de & 0xFF00) | u16::from(val),
            Reg::H => self.hl = (self.hl & 0xFF) | (u16::from(val) << 8),
            Reg::L => self.hl = (self.hl & 0xFF00) | u16::from(val),
            Reg::A => self.a = val,
        }
    }

    #[must_use]
    pub fn get_af(&self) -> u16 {
        (u16::from(self.a) << 8) | u16::from(self.f.bits())
    }

    pub fn set_af(&mut self, val: u16) {
        self.f = CpuFlags::from_bits_truncate(val as u8);
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
}

impl Default for Registers {
    fn default() -> Self {
        Registers {
            pc: 0,
            a: 0,
            f: CpuFlags::empty(),
            bc: 0,
            de: 0,
            hl: 0,
            sp: 0,
            ir: 0,
        }
    }
}
