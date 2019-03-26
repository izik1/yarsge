use super::{ppu::{Ppu, DisplayPixel}, timer::Timer, dma::Dma, memory::Memory};

pub struct Hardware {
    ppu: Ppu,
    timer: Timer,
    dma: Dma,
    memory: Memory,
    pub r_if: u8,
    pub r_ier: u8,
    pub cycle_counter: i64,
    mid_check: bool,
}

impl Hardware {
    pub fn new(memory: Memory) -> Self {
        Self {
            cycle_counter: 0,
            ppu: Default::default(),
            memory,
            timer: Default::default(),
            r_if: 0,
            r_ier: 0,
            mid_check: false,
            dma: Default::default(),
        }
    }

    pub fn get_display(&self) -> &[DisplayPixel]{
        self.ppu.get_display()
    }

    pub fn read_cycle(&mut self, addr: u16) -> u8 {
        self.update(4);
        self.read_byte(addr)
    }

    pub fn stall(&mut self, m_cycles: i64) {
        self.update(m_cycles * 4);
    }

    fn update(&mut self, mut cycles: i64) {
        if self.mid_check {
            cycles += 2;
            self.mid_check = false;
        }

        for _ in 0..cycles {
            self.r_if |= (self.timer.update() | self.ppu.update()) & 0x1F;
            if let Some((oam_offset, addr)) = self.dma.update() {
                self.ppu.oam[oam_offset] = self.read_byte(addr)
            }
        }

        self.cycle_counter -= cycles;
    }

    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => self.memory.read_rom_low(addr),
            0x4000..=0x7FFF => self.memory.read_rom_high(addr - 0x4000),
            0x8000..=0x9FFF => self.ppu.get_vram(addr - 0x8000),
            0xC000..=0xDFFF => self.memory.wram[(addr - 0xC000) as usize],
            0xE000..=0xFDFF => self.memory.wram[(addr - 0xE000) as usize],
            0xFE00..=0xFE9F => self.ppu.read_oam(addr - 0xFE00).unwrap_or(0xFF),
            0xFF00..=0xFF7F => self.read_io(addr as u8),
            0xFEA0..=0xFEFF => 0,
            0xFF80..=0xFFFE => self.memory.hram[addr as usize - 0xFF80],
            0xFFFF => self.r_ier,
            _ => unimplemented!(
                "Unimplemented address range (read): (addr: {:01$X})",
                addr,
                4
            ),
        }
    }

    pub fn fetch(&mut self, addr: u16) -> u8 {
        self.update(3);
        let val = self.read_byte(addr);
        self.update(1);
        val
    }

    pub fn interrupt_check(&mut self) {
        self.update(2);
        self.mid_check = true;
    }

    fn read_io(&self, addr: u8) -> u8 {
        #[allow(clippy::match_same_arms)]
            match addr {
            0x00 => 0xFF, // joypad, not implemented, 0xFF = no buttons pressed down...
            0x04..=0x07 => self.timer.read_reg(addr),
            0x08..=0x0E => 0xFF, // Empty range.
            0x0F => self.r_if | 0xE0,
            0x10..=0x3F => 0xFF, // TODO: APU, silently ignore
            0x46 => (self.dma.addr >> 8) as u8,
            0x40..=0x45 | 0x47..=0x4B => self.ppu.get_reg(addr),
            0x4C..=0x7F => 0xFF, // Empty range.
            0x80..=0xFF => unreachable!("Invalid address range for IO regs! (read)"),
            _ => {
                eprintln!("Unimplemented IO reg (read): (addr: 0xFF{:01$X})", addr, 2);
                0xFF
            }
        }
    }

    pub fn write_u16_cycle(&mut self, address: u16, value: u16) {
        self.write_cycle(address, value as u8);
        self.write_cycle(address.wrapping_add(1), (value >> 8) as u8)
    }

    pub fn write_cycle(&mut self, addr: u16, val: u8) {
        self.update(4); // TODO (TEST): Hardware timing might be different.
        match addr {
            0x0000..=0x7FFF => self.memory.mbc_write(addr, val),
            0x8000..=0x9FFF => self.ppu.set_vram(addr - 0x8000, val),
            0xC000..=0xDFFF => self.memory.wram[(addr - 0xC000) as usize] = val,
            0xE000..=0xFDFF => self.memory.wram[(addr - 0xE000) as usize] = val,
            0xFE00..=0xFE9F => self.ppu.write_oam(addr - 0xFE00, val),
            0xFEA0..=0xFEFF => {}
            0xFF00..=0xFF7F => self.write_io(addr as u8, val),
            0xFF80..=0xFFFE => self.memory.hram[addr as usize - 0xFF80] = val,
            0xFFFF => self.r_ier = val,
            _ => unimplemented!(
                "Unimplemented address range (write): (addr: {:01$X} val: {2:03$X})",
                addr,
                4,
                val,
                2
            ),
        }
    }

    fn write_io(&mut self, addr: u8, val: u8) {
        #[allow(clippy::match_same_arms)]
            match addr {
            0x01 | 0x02 => {} // TODO: serial, silently ignore
            0x04..=0x07 => self.timer.write_reg(addr, val),
            0x08..=0x0E => {} // Empty range.
            0x0F => self.r_if = val & 0x1F,
            0x10..=0x3F => {} // TODO: APU, silently ignore
            0x46 => {
                self.dma.ld_addr = u16::from(val) << 8;
                self.dma.ld_timer = 4
            }
            0x40..=0x45 | 0x47..=0x4B => self.ppu.set_reg(addr, val),
            0x50 => self.memory.disable_boot_rom(),
            0x4C..=0x4F | 0x51..=0x7F => {}
            0x80..=0xFF => unreachable!("Invalid address range for IO regs! (write)"),
            _ => eprintln!(
                "Unimplemented IO reg (write): (addr: 0xFF{:01$X} val: {2:03$X})",
                addr, 2, val, 2
            ),
        }
    }
}
