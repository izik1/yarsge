// Copyright Zachery Gyurkovitz 2017 MIT License, see lisence.md for more details.

use super::memory;
use super::memory::State;
use super::registers;
use super::registers::Reg;
use super::flags::Flag;
pub struct Cpu {
     cycle_counter: i64,
    pub mem: memory::Memory,
    pub regs: registers::Registers,
}

impl Cpu {
    fn read_pc(&self) -> u8 {
        self.mem.read_byte(self.regs.pc)
    }
    
    fn read_ipc(&mut self) -> u8{
        let val = self.read_pc();
        self.regs.pc += 1;
        val
    }
    
    fn read_ipc_cycle(&mut self) -> u8 {
        let val = self.mem.read_cycle(self.regs.pc);
        self.regs.pc += 1;
        val
    }
    
    fn write_hl_cycle(&mut self, val: u8) {
        // TODO: STUB
    }
    
    fn read_hl_cycle(&mut self) -> u8 {
        0xFF // TODO: STUB
    }
    
    fn instr_jr(&mut self, jump: bool) -> i64{
        let val = self.read_ipc_cycle() as i8;
        if !jump {
            4
        } else {
            self.mem.update(4);
            self.regs.pc = self.regs.pc.wrapping_add(val as u16);
            8
        }
    }
    
    fn instr_jp(&mut self, jump: bool) -> i64 {
        let low = self.read_ipc_cycle();
        let high = self.read_ipc_cycle();
        if !jump {
            8 
        } else{
            self.mem.update(4);
            self.regs.pc =  ((high as u16) <<  8) | (low as u16);
            12
        }
    }
    
    fn instr_ld(&mut self, dest: Reg, src: Reg) -> i64 {
        match (dest, src) {
            (Reg::HL, Reg::HL) => panic!(),
            (Reg::HL, src) =>     {let val = self.regs.get_reg(&src); self.write_hl_cycle(val); 4}
            (dest, Reg::HL) =>    {let val = self.read_hl_cycle(); self.regs.set_reg(dest, val); 0}
            (dest, src) =>        {let val = self.regs.get_reg(&src); self.regs.set_reg(dest, val); 0}
        }
    }
        
    fn run_instruction(&mut self) -> i64 {
        self.mem.update(1);
        let op = self.read_ipc();
        self.mem.update(1);
        
        // TODO: HALT bug missing here.
                
        2 + match op {
            0x00 => 0,
            0x18 => self.instr_jr(true),
            0x20 => {let j = !self.regs.get_flag(Flag::Z); self.instr_jr(j)}
            0x28 => {let j = self.regs.get_flag(Flag::Z); self.instr_jr(j)}
            0x30 => {let j = !self.regs.get_flag(Flag::C); self.instr_jr(j)}
            0x38 => {let j = self.regs.get_flag(Flag::C); self.instr_jr(j)}
                        
            0x40 => self.instr_ld(Reg::B, Reg::B), 0x41 => self.instr_ld(Reg::B, Reg::C), 0x42 => self.instr_ld(Reg::B, Reg::D) , 0x43 => self.instr_ld(Reg::B, Reg::E),
            0x44 => self.instr_ld(Reg::B, Reg::H), 0x45 => self.instr_ld(Reg::B, Reg::L), 0x46 => self.instr_ld(Reg::B, Reg::HL), 0x47 => self.instr_ld(Reg::B, Reg::A),
            0x48 => self.instr_ld(Reg::C, Reg::B), 0x49 => self.instr_ld(Reg::C, Reg::C), 0x4A => self.instr_ld(Reg::C, Reg::D) , 0x4B => self.instr_ld(Reg::C, Reg::E),
            0x4C => self.instr_ld(Reg::C, Reg::H), 0x4D => self.instr_ld(Reg::C, Reg::L), 0x4E => self.instr_ld(Reg::C, Reg::HL), 0x4F => self.instr_ld(Reg::C, Reg::A),
            
            0x50 => self.instr_ld(Reg::D, Reg::B), 0x51 => self.instr_ld(Reg::D, Reg::C), 0x52 => self.instr_ld(Reg::D, Reg::D) , 0x53 => self.instr_ld(Reg::D, Reg::E),
            0x54 => self.instr_ld(Reg::D, Reg::H), 0x55 => self.instr_ld(Reg::D, Reg::L), 0x56 => self.instr_ld(Reg::D, Reg::HL), 0x57 => self.instr_ld(Reg::D, Reg::A),
            0x58 => self.instr_ld(Reg::E, Reg::B), 0x59 => self.instr_ld(Reg::E, Reg::C), 0x5A => self.instr_ld(Reg::E, Reg::D) , 0x5B => self.instr_ld(Reg::E, Reg::E),
            0x5C => self.instr_ld(Reg::E, Reg::B), 0x5D => self.instr_ld(Reg::E, Reg::C), 0x5E => self.instr_ld(Reg::E, Reg::D) , 0x5F => self.instr_ld(Reg::E, Reg::E),
            
            0x60 => self.instr_ld(Reg::H, Reg::B), 0x61 => self.instr_ld(Reg::H, Reg::C), 0x62 => self.instr_ld(Reg::H, Reg::D) , 0x63 => self.instr_ld(Reg::H, Reg::E),
            0x64 => self.instr_ld(Reg::H, Reg::B), 0x65 => self.instr_ld(Reg::H, Reg::L), 0x66 => self.instr_ld(Reg::H, Reg::HL), 0x67 => self.instr_ld(Reg::H, Reg::A),
            0x68 => self.instr_ld(Reg::L, Reg::B), 0x69 => self.instr_ld(Reg::L, Reg::C), 0x6A => self.instr_ld(Reg::L, Reg::D) , 0x6B => self.instr_ld(Reg::L, Reg::E),
            0x6C => self.instr_ld(Reg::L, Reg::B), 0x6D => self.instr_ld(Reg::L, Reg::L), 0x6E => self.instr_ld(Reg::L, Reg::HL), 0x6F => self.instr_ld(Reg::L, Reg::A),
            
            0x70 => self.instr_ld(Reg::HL, Reg::B), 0x71 => self.instr_ld(Reg::HL, Reg::C), 0x72 => self.instr_ld(Reg::HL, Reg::D) , 0x73 => self.instr_ld(Reg::HL, Reg::E),
            0x74 => self.instr_ld(Reg::HL, Reg::H), 0x75 => self.instr_ld(Reg::HL, Reg::L), 0x76 => panic!() /*HALT*/              , 0x77 => self.instr_ld(Reg::HL, Reg::A),
            0x78 => self.instr_ld(Reg::A, Reg::B) , 0x79 => self.instr_ld(Reg::A,  Reg::C), 0x7A => self.instr_ld(Reg::A,  Reg::D) , 0x7B => self.instr_ld(Reg::A , Reg::E),
            0x7C => self.instr_ld(Reg::A, Reg::H) , 0x7D => self.instr_ld(Reg::A,  Reg::L), 0x7E => self.instr_ld(Reg::A,  Reg::HL), 0x7F => self.instr_ld(Reg::A , Reg::A),            

            0xC2 => {let j = !self.regs.get_flag(Flag::Z); self.instr_jp(j)}
            0xC3 => self.instr_jp(true),
            0xCA => {let j = self.regs.get_flag(Flag::Z); self.instr_jp(j)}
            0xCB => self.run_extended(),
            0xD2 => {let j = !self.regs.get_flag(Flag::C); self.instr_jp(j)}
            0xDA => {let j = self.regs.get_flag(Flag::C); self.instr_jp(j)}
            _ => panic!(),
        }
    }
    
    fn run_extended(&mut self) -> i64 {
        let op = self.read_ipc_cycle();
        4 + match op {
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
            _ => panic!(),
        }
        
    }
    
    // Affected flags: Z (set|res), N (res), H (set)
    fn instr_bit(&mut self, reg: Reg, mask: u8) -> i64 {
        self.regs.af = (self.regs.af & 0b1111_1111_0011) | Flag::H.to_bit() as u16;
        match reg {
            Reg::HL => {if (self.read_hl_cycle() & mask) == 0 {self.regs.af |= Flag::Z.to_bit() as u16}; 4}
            r => {if (self.regs.get_reg(&r) & mask) == 0 {self.regs.af |= Flag::Z.to_bit() as u16}; 0}
        }
    }
    
    fn instr_res(&mut self, reg: Reg, mask: u8) -> i64 {
        match reg {
            Reg::HL => {let val = self.read_hl_cycle(); self.write_hl_cycle(val & mask); 8}
            r => {let val = self.regs.get_reg(&r); self.regs.set_reg(r, val & mask) ; 0}
        }
    }
    
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
        self.mem.update(2);
        2 + self.handle_interrupts() + self.run_instruction()
    }
    
    pub fn new() -> Cpu {
        Cpu {
            cycle_counter: 0,
            mem: memory::Memory::new(),
            regs: registers::Registers::new()
        }
    }
    
    pub fn run(&mut self, ticks: i64) {
        self.cycle_counter += ticks;
        while self.cycle_counter > 0 {
            self.cycle_counter += match self.mem.status {
                State::Okay => self.handle_okay(),
                _ => panic!()
            }
        }
    }
}
