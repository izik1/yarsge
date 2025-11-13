use super::{
    dma::Dma,
    memory::Memory,
    pad::Pad,
    ppu::{DisplayPixel, Ppu},
    timer::Timer,
};
use crate::Keys;
use crate::emu::bus::{BusState, ExternalBus};
use crate::emu::{InterruptFlags, TCycle};

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
    pub const fn new(memory: Memory) -> Self {
        Self {
            cycle_counter: TCycle(0),
            ppu: Ppu::new(),
            memory,
            timer: Timer::new(),
            reg_if: InterruptFlags::empty(),
            reg_ie: InterruptFlags::empty(),
            dma: Dma::new(),
            pad: Pad::new(),
        }
    }

    pub fn set_keys(&mut self, val: Keys) {
        self.pad.set_keys(val);
    }

    #[must_use]
    pub fn get_display(&self) -> impl IntoIterator<Item = DisplayPixel> {
        self.ppu.get_display()
    }

    #[must_use]
    pub fn read_cycle(&mut self, addr: u16) -> u8 {
        self.read_cycle_intr(addr).0
    }

    #[must_use]
    pub fn read_cycle_intr(&mut self, addr: u16) -> (u8, InterruptFlags) {
        let mut bus = ExternalBus::new();

        self.tick(&mut bus);
        bus.set_addr_cpu(addr);

        self.tick(&mut bus);

        let early_interrupts = self.reg_if & self.reg_ie;
        self.tick(&mut bus);

        let bus_val = self.memory.strobe_read(&mut bus);

        self.tick(&mut bus);

        let val = self.read_byte(addr, bus_val);
        (val, early_interrupts)
    }

    pub fn idle_cycle(&mut self) {
        self.tick_n::<4>(&mut ExternalBus::new());
    }

    fn tick(&mut self, bus: &mut ExternalBus) {
        self.tick_n::<1>(bus);
    }

    fn tick_n<const CYCLES: isize>(&mut self, bus: &mut ExternalBus) {
        const { assert!(CYCLES > 0) };

        for _ in 0..CYCLES {
            self.dma.tick(bus, &mut self.ppu, &mut self.memory);

            self.reg_if |= self.timer.tick() | self.ppu.tick();
        }

        // ACCURACY:
        // run joypad at lower frequency because realistically it only needs to run at 1 tick per input change
        // this is presumably inaccurate because the hardware that checks for an interrupt presumably does so every T-cycle.
        if const { CYCLES > 0 } && self.pad.tick() {
            self.reg_if |= InterruptFlags::JOYPAD;
        }

        self.cycle_counter -= TCycle(CYCLES);
    }

    #[must_use]
    fn read_byte(&self, addr: u16, bus_value: u8) -> u8 {
        match addr {
            0x0000..0x8000 | 0xa000..0xfe00 => bus_value,
            0x8000..0xa000 => self.ppu.get_vram(addr - 0x8000).unwrap_or(0xff),
            0xfe00..0xfea0 if self.dma.oam_blocked() => 0xff,
            0xfe00..0xfea0 => self.ppu.read_oam(addr - 0xfe00).unwrap_or(0xff),
            0xfea0..0xff00 => 0,
            0xff00..0xff80 => self.read_io(addr as u8),
            0xff80..0xffff => self.memory.hram[addr as usize - 0xff80],
            0xffff => self.reg_ie.bits(),
        }
    }

    #[must_use]
    fn read_io(&self, addr: u8) -> u8 {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x00 => self.pad.selected(),
            0x04..0x08 => self.timer.read_reg(addr),
            0x08..0x0f => 0xff, // Empty range.
            0x0f => self.reg_if.bits() | 0xe0,
            0x10..0x40 => 0xff, // TODO: APU, silently ignore
            0x46 => self.dma.read_src(),
            0x40..0x46 | 0x47..0x4c => self.ppu.get_reg(addr),
            0x4c..0x80 => 0xff, // Empty range.
            0x80.. => unreachable!("Invalid address range for IO regs! (read)"),
            _ => {
                log::warn!("Unimplemented IO reg (read): (addr: 0xff{addr:02x})");
                0xff
            }
        }
    }

    pub fn write_u16_cycle(&mut self, address: u16, value: u16) {
        let [high, low] = value.to_be_bytes();
        self.write_cycle(address, low);
        self.write_cycle(address.wrapping_add(1), high);
    }

    pub fn write_cycle(&mut self, addr: u16, val: u8) {
        let _ = self.write_cycle_intr(addr, val);
    }

    #[must_use]
    pub fn write_cycle_intr(&mut self, addr: u16, val: u8) -> InterruptFlags {
        let mut bus = ExternalBus::new();

        self.tick(&mut bus);

        if !bus.busy() {
            bus.set_addr_cpu(addr);
            bus.st.insert(BusState::PIN_NOT_READ);
        }

        self.tick(&mut bus);

        let early_interrupts = self.reg_if & self.reg_ie;

        self.tick(&mut bus);

        self.memory.strobe_write(&mut bus, val);

        self.tick(&mut bus);

        match addr {
            0x0000..0x8000 | 0xa000..0xfe00 => {}
            0x8000..0xa000 => self.ppu.set_vram(addr - 0x8000, val),
            0xfe00..0xfea0 if self.dma.oam_blocked() => {}
            0xfe00..0xfea0 => self.ppu.write_oam(addr - 0xfe00, val),
            0xfea0..0xff00 => {}
            0xff00..0xff80 => self.write_io(addr as u8, val),
            0xff80..0xffff => self.memory.hram[addr as usize - 0xff80] = val,
            0xffff => self.reg_ie = InterruptFlags::from_bits_retain(val),
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
                self.dma.write_src(val);
            }
            0x40..0x46 | 0x47..0x4c => self.ppu.set_reg(addr, val),
            0x50 if val != 0 => self.memory.disable_boot_rom(),
            0x50 => {}
            0x4c..0x50 | 0x51..0x80 => {}
            0x80.. => unreachable!("Invalid address range for IO regs! (write)"),
            _ => log::warn!("Unimplemented IO reg (write): (addr: 0xff{addr:02x} val: {val:#02x})"),
        }
    }
}
