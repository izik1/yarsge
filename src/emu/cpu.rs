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
            8
        } else {
            self.mem.update(4);
            self.regs.pc = self.regs.pc.wrapping_add(val as u16);
            12
        }
    }
    
    fn instr_jp(&mut self, jump: bool) -> i64 {
        let low = self.read_ipc_cycle();
        let high = self.read_ipc_cycle();
        if !jump {
            12 
        } else{
            self.mem.update(4);
            self.regs.pc =  ((high as u16) <<  8) | (low as u16);
            16
        }
    }
    
    fn instr_ld(&mut self, dest: Reg, src: Reg) -> i64 {
        match (dest, src) {
            (Reg::HL, Reg::HL) => panic!(),
            (Reg::HL, src) =>     {let val = self.regs.get_reg(src); self.write_hl_cycle(val); 4}
            (dest, Reg::HL) =>    {let val = self.read_hl_cycle(); self.regs.set_reg(dest, val); 0}
            (dest, src) =>        {let val = self.regs.get_reg(src); self.regs.set_reg(dest, val); 0}
        }
    }
    
    fn run_instruction(&mut self) -> i64 {
        self.mem.update(1);
        let op = self.read_ipc();
        self.mem.update(1);
        
        // TODO: HALT bug missing here.
                
        2 + match  op {
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
            
            0x70 => self.instr_ld(Reg::HL, Reg::B), 0x71 => self.instr_ld(Reg::HL, Reg::C), 0x72 => self.instr_ld(Reg::HL, Reg::D), 0x73 => self.instr_ld(Reg::HL, Reg::E),
            0x74 => self.instr_ld(Reg::HL, Reg::H), 0x75 => self.instr_ld(Reg::HL, Reg::L), 0x76 => panic!() /*HALT*/             , 0x77 => self.instr_ld(Reg::HL, Reg::A),
            0x78 => self.instr_ld(Reg::A, Reg::B) , 0x79 => self.instr_ld(Reg::A, Reg::C) , 0x7A => self.instr_ld(Reg::A, Reg::D) , 0x7B => self.instr_ld(Reg::A , Reg::E),
            0x7C => self.instr_ld(Reg::A, Reg::H) , 0x7D => self.instr_ld(Reg::A, Reg::L) , 0x7E => self.instr_ld(Reg::A, Reg::HL), 0x7F => self.instr_ld(Reg::A , Reg::A),            
            
            0xC2 => {let j = !self.regs.get_flag(Flag::Z); self.instr_jp(j)}
            0xC3 => self.instr_jp(true),
            0xCA => {let j = self.regs.get_flag(Flag::Z); self.instr_jp(j)}
            0xD2 => {let j = !self.regs.get_flag(Flag::C); self.instr_jp(j)}
            0xDA => {let j = self.regs.get_flag(Flag::C); self.instr_jp(j)}
            
            _ => panic!(),
        }
    }

    fn handle_interrupts(&mut self) -> i64 {
        0 // STUB
    }
    
    
    fn handle_okay(&mut self) -> i64 {
        self.mem.update(2);
        2 + self.handle_interrupts()  + self.run_instruction()
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
