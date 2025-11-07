use super::{
    dma::Dma,
    memory::Memory,
    pad::Pad,
    ppu::{DisplayPixel, Ppu},
    timer::Timer,
};
use crate::emu::{InterruptFlags, MCycle, TCycle};

pub struct Hardware {
    ppu: Ppu,
    timer: Timer,
    dma: Dma,
    memory: Memory,
    pad: Pad,
    pub reg_if: InterruptFlags,
    pub reg_ie: InterruptFlags,
    pub cycle_counter: TCycle,
}

impl Hardware {
    pub fn new(memory: Memory) -> Self {
        Self {
            cycle_counter: TCycle(0),
            ppu: Ppu::default(),
            memory,
            timer: Timer::default(),
            reg_if: InterruptFlags::empty(),
            reg_ie: InterruptFlags::empty(),
            dma: Dma::default(),
            pad: Pad::new(),
        }
    }

    pub fn set_keys(&mut self, val: u8) {
        self.pad.set_keys(val);
    }

    #[must_use]
    pub fn get_display(&self) -> &[DisplayPixel] {
        self.ppu.get_display()
    }

    #[must_use]
    pub fn read_cycle(&mut self, addr: u16) -> u8 {
        self.read_cycle_intr(addr).0
    }

    #[must_use]
    pub fn read_cycle_intr(&mut self, addr: u16) -> (u8, InterruptFlags) {
        self.tick_n(TCycle(2));
        let early_interrupts = self.reg_if & self.reg_ie;
        self.tick();
        // zzz bus
        self.tick();

        let val = self.read_byte(addr);
        (val, early_interrupts)
    }

    pub fn stall(&mut self, m_cycles: MCycle) {
        self.tick_n(m_cycles.into());
    }

    pub fn idle_cycle(&mut self) {
        self.stall(MCycle(1));
    }

    fn tick(&mut self) {
        self.tick_n(TCycle(1));
    }

    fn tick_n(&mut self, cycles: TCycle) {
        for _ in 0..cycles.0 {
            self.reg_if |= self.timer.tick() | self.ppu.tick();
            if let Some((oam_offset, addr)) = self.dma.tick() {
                self.ppu.oam[oam_offset] = self.read_byte(addr);
            }

            if self.pad.tick() {
                self.reg_if |= InterruptFlags::JOYPAD;
            }
        }

        self.cycle_counter -= cycles;
    }

    #[must_use]
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x4000 => self.memory.read_rom_low(addr),
            0x4000..0x8000 => self.memory.read_rom_high(addr - 0x4000),
            0x8000..0xa000 => self.ppu.get_vram(addr - 0x8000).unwrap_or(0xff),
            0xc000..0xe000 => self.memory.wram[(addr - 0xc000) as usize],
            0xe000..0xfe00 => self.memory.wram[(addr - 0xe000) as usize],
            0xfe00..0xfea0 => self.ppu.read_oam(addr - 0xfe00).unwrap_or(0xff),
            0xfea0..0xff00 => 0,
            0xff00..0xff80 => self.read_io(addr as u8),
            0xff80..0xffff => self.memory.hram[addr as usize - 0xff80],
            0xffff => self.reg_ie.bits(),
            _ => unimplemented!("Unimplemented address range (read): (addr: {addr:#04x})"),
        }
    }

    #[must_use]
    fn read_io(&self, addr: u8) -> u8 {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x00 => self.pad.get_selected(),
            0x04..0x08 => self.timer.read_reg(addr),
            0x08..0x0f => 0xff, // Empty range.
            0x0f => self.reg_if.bits() | 0xe0,
            0x10..0x40 => 0xff, // TODO: APU, silently ignore
            0x46 => (self.dma.addr >> 8) as u8,
            0x40..0x46 | 0x47..0x4c => self.ppu.get_reg(addr),
            0x4c..0x80 => 0xff, // Empty range.
            0x80.. => unreachable!("Invalid address range for IO regs! (read)"),
            _ => {
                eprintln!("Unimplemented IO reg (read): (addr: 0xff{addr:02x})");
                0xff
            }
        }
    }

    pub fn write_u16_cycle(&mut self, address: u16, value: u16) {
        self.write_cycle(address, value as u8);
        self.write_cycle(address.wrapping_add(1), (value >> 8) as u8);
    }

    pub fn write_cycle(&mut self, addr: u16, val: u8) {
        let _ = self.write_cycle_intr(addr, val);
    }

    #[must_use]
    pub fn write_cycle_intr(&mut self, addr: u16, val: u8) -> InterruptFlags {
        self.tick_n(TCycle(2));
        let early_interrupts = self.reg_if & self.reg_ie;
        self.tick();
        // zzz bus
        self.tick();
        match addr {
            0x0000..0x8000 => self.memory.mbc_write(addr, val),
            0x8000..0xa000 => self.ppu.set_vram(addr - 0x8000, val),
            0xc000..0xe000 => self.memory.wram[(addr - 0xc000) as usize] = val,
            0xe000..0xfe00 => self.memory.wram[(addr - 0xe000) as usize] = val,
            0xfe00..0xfea0 => self.ppu.write_oam(addr - 0xfe00, val),
            0xfea0..0xff00 => {}
            0xff00..0xff80 => self.write_io(addr as u8, val),
            0xff80..0xffff => self.memory.hram[addr as usize - 0xff80] = val,
            0xffff => self.reg_ie = InterruptFlags::from_bits_retain(val),
            _ => unimplemented!(
                "Unimplemented address range (write): (addr: {addr:#04x} val: {val:#02x})"
            ),
        }

        early_interrupts
    }

    fn write_io(&mut self, addr: u8, val: u8) {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x00 => self.pad.set_status(val),
            0x01 | 0x02 => {} // TODO: serial, silently ignore
            0x04..0x08 => self.timer.write_reg(addr, val),
            0x08..0x0f => {} // Empty range.
            0x0f => self.reg_if = InterruptFlags::from_bits_truncate(val),
            0x10..0x40 => {} // TODO: APU, silently ignore
            0x46 => {
                self.dma.ld_addr = u16::from(val) << 8;
                self.dma.ld_timer = 4;
            }
            0x40..0x46 | 0x47..0x4c => self.ppu.set_reg(addr, val),
            0x50 => self.memory.disable_boot_rom(),
            0x4c..0x50 | 0x51..0x80 => {}
            0x80.. => unreachable!("Invalid address range for IO regs! (write)"),
            _ => eprintln!("Unimplemented IO reg (write): (addr: 0xff{addr:02x} val: {val:#02x})"),
        }
    }
}
