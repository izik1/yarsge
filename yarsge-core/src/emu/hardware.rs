use super::{
    dma::Dma,
    memory::Memory,
    pad::Pad,
    ppu::{DisplayPixel, Ppu},
};
use crate::emu::apu::{self, ApuSampler};
use crate::emu::bus::{BusState, ExternalBus};
use crate::emu::{InterruptFlags, TCycle, timer};

pub(crate) trait CpuBus {
    fn clear_interrupt(&mut self, remove: InterruptFlags);

    #[must_use]
    fn has_interrupts(&self) -> bool;

    fn tick_cycle(&mut self);

    #[must_use]
    fn read_cycle_intr(&mut self, addr: u16) -> (u8, InterruptFlags);

    #[must_use]
    fn read_cycle(&mut self, addr: u16) -> u8 {
        self.read_cycle_intr(addr).0
    }

    #[must_use]
    fn read_hi_cycle(&mut self, addr: u8) -> u8 {
        self.read_cycle(u16::from_be_bytes([0xff, addr]))
    }

    #[must_use]
    fn write_cycle_intr(&mut self, addr: u16, value: u8) -> InterruptFlags;

    fn write_cycle(&mut self, addr: u16, value: u8) {
        let _ = self.write_cycle_intr(addr, value);
    }

    fn write_u16_cycle(&mut self, address: u16, value: u16) {
        let [high, low] = value.to_be_bytes();
        self.write_cycle(address, low);
        self.write_cycle(address.wrapping_add(1), high);
    }

    fn write_hi_cycle(&mut self, addr: u8, value: u8) {
        self.write_cycle(u16::from_be_bytes([0xff, addr]), value);
    }
}

#[non_exhaustive]
pub struct Hardware<S> {
    ppu: Ppu,
    timer: timer::Lazy,
    dma: Dma,
    memory: Memory,
    pub(crate) pad: Pad,
    pub(crate) apu: apu::Lazy<S>,
    pub reg_if: InterruptFlags,
    pub reg_ie: InterruptFlags,
    pub cycle_counter: TCycle,
}

impl<S: ApuSampler> Hardware<S> {
    pub const fn new(memory: Memory, apu_sampler: S) -> Self {
        Self {
            cycle_counter: TCycle(0),
            ppu: Ppu::new(),
            memory,
            timer: timer::Lazy::new(),
            reg_if: InterruptFlags::empty(),
            reg_ie: InterruptFlags::empty(),
            dma: Dma::new(),
            pad: Pad::new(),
            apu: apu::Lazy::new(apu_sampler),
        }
    }

    #[must_use]
    #[inline]
    pub fn display(&self) -> impl IntoIterator<Item = DisplayPixel> {
        self.ppu.display()
    }

    pub(crate) fn tick_pad(&mut self) {
        // ACCURACY:
        // run joypad at lower frequency because realistically it only needs to run at 1 tick per input change
        // this is presumably inaccurate because the hardware that checks for an interrupt presumably does so every T-cycle.
        if self.pad.tick() {
            self.reg_if |= InterruptFlags::JOYPAD;
            let _ = self.pad.tick();
        }
    }

    fn tick_n<const CYCLES: isize>(&mut self, bus: &mut ExternalBus) {
        const { assert!(CYCLES > 0) };

        for _ in 0..CYCLES {
            self.dma.tick(bus, &mut self.ppu, &mut self.memory);

            self.reg_if |= self.ppu.tick();
        }

        for x in 0..CYCLES {
            self.apu.tick(self.timer.lazy_div(x as u32));
        }

        self.reg_if |= self.timer.tick(CYCLES as u32);

        self.cycle_counter -= TCycle(CYCLES);
    }

    #[must_use]
    fn read_byte(&mut self, addr: u16, bus_value: u8) -> u8 {
        match addr {
            0x0000..0x8000 | 0xa000..0xfe00 => bus_value,
            0x8000..0xa000 => self.ppu.get_vram(addr - 0x8000).unwrap_or(0xff),
            0xfe00..0xfea0 if self.dma.oam_blocked() => 0xff,
            0xfe00..0xfea0 => self.ppu.read_oam(addr - 0xfe00).unwrap_or(0xff),
            0xfea0..0xff00 => 0,
            0xff00.. => self.read_byte_hi(addr as u8),
        }
    }

    #[must_use]
    fn read_byte_hi(&mut self, addr: u8) -> u8 {
        match addr {
            0x00..0x80 => self.read_io(addr),
            0x80..0xff => self.memory.hram[addr as usize - 0x80],
            0xff => self.reg_ie.bits(),
        }
    }

    #[must_use]
    fn read_io(&mut self, addr: u8) -> u8 {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x00 => self.pad.selected(),
            0x04..0x08 => self.timer.read_reg(addr),
            0x08..0x0f => 0xff, // Empty range.
            0x0f => self.reg_if.bits() | 0xe0,
            0x10..0x40 => self.apu.read_reg(addr),
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

    fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..0x8000 | 0xa000..0xfe00 => {}
            0x8000..0xa000 => self.ppu.set_vram(addr - 0x8000, value),
            0xfe00..0xfea0 if self.dma.oam_blocked() => {}
            0xfe00..0xfea0 => self.ppu.write_oam(addr - 0xfe00, value),
            0xfea0..0xff00 => {}
            0xff00.. => self.write_byte_hi(addr as u8, value),
        }
    }

    fn write_byte_hi(&mut self, addr: u8, value: u8) {
        match addr {
            0x00..0x80 => self.write_io(addr, value),
            0x80..0xff => self.memory.hram[addr as usize - 0x80] = value,
            0xff => self.reg_ie = InterruptFlags::from_bits_retain(value),
        }
    }

    fn write_io(&mut self, addr: u8, val: u8) {
        #[allow(clippy::match_same_arms)]
        match addr {
            0x00 => self.pad.set_status(val),
            0x01 | 0x02 => {} // TODO: serial, silently ignore
            0x04..0x08 => self.timer.write_reg(addr, val),
            0x08..0x0f => {} // Empty range.
            0x0f => self.reg_if = InterruptFlags::from_bits_truncate(val),
            0x10..0x40 => self.apu.write_reg(addr, val),
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

impl<S: ApuSampler> CpuBus for Hardware<S> {
    fn clear_interrupt(&mut self, remove: InterruptFlags) {
        self.reg_if.remove(remove);
    }

    fn has_interrupts(&self) -> bool {
        !(self.reg_ie & self.reg_if).is_empty()
    }

    fn tick_cycle(&mut self) {
        self.tick_n::<4>(&mut ExternalBus::new());
    }

    fn read_cycle_intr(&mut self, addr: u16) -> (u8, InterruptFlags) {
        // ACCURACY: technically the address should be on the bus on tick 1, and in the
        let mut bus = ExternalBus::new();

        self.tick_n::<2>(&mut bus);

        let early_interrupts = self.reg_if & self.reg_ie;
        self.tick_n::<2>(&mut bus);

        bus.set_addr_cpu(addr);
        let bus_val = self.memory.strobe_read(&mut bus);
        let val = self.read_byte(addr, bus_val);
        (val, early_interrupts)
    }

    fn read_cycle(&mut self, addr: u16) -> u8 {
        if addr >= 0xff00 {
            return self.read_hi_cycle(addr as u8);
        }

        let mut bus = ExternalBus::new();

        self.tick_n::<4>(&mut bus);

        bus.set_addr_cpu(addr);
        let bus_val = self.memory.strobe_read(&mut bus);

        self.read_byte(addr, bus_val)
    }

    fn read_hi_cycle(&mut self, addr: u8) -> u8 {
        // this never uses the external bus, nor checks for interrupts, so we can skip out on a bunch of work.
        self.tick_cycle();

        // and a bunch of address decoding.
        self.read_byte_hi(addr)
    }

    fn write_cycle_intr(&mut self, addr: u16, value: u8) -> InterruptFlags {
        let mut bus = ExternalBus::new();

        self.tick_n::<2>(&mut bus);

        let early_interrupts = self.reg_if & self.reg_ie;

        self.tick_n::<2>(&mut bus);

        if !bus.busy() {
            bus.set_addr_cpu(addr);
            bus.st.insert(BusState::PIN_NOT_READ);
        }

        self.memory.strobe_write(&mut bus, value);

        self.write_byte(addr, value);

        early_interrupts
    }

    fn write_cycle(&mut self, addr: u16, value: u8) {
        if addr >= 0xff00 {
            return self.write_hi_cycle(addr as u8, value);
        }

        let mut bus = ExternalBus::new();
        self.tick_n::<4>(&mut bus);

        if !bus.busy() {
            bus.set_addr_cpu(addr);
            bus.st.insert(BusState::PIN_NOT_READ);
        }

        self.memory.strobe_write(&mut bus, value);

        self.write_byte(addr, value);
    }

    fn write_hi_cycle(&mut self, addr: u8, value: u8) {
        // this never uses the external bus, nor checks for interrupts, so we can skip out on a bunch of work.
        self.tick_cycle();

        // and a bunch of address decoding.
        self.write_byte_hi(addr, value);
    }
}
