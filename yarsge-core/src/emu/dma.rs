use crate::emu::bus::{BusState, ExternalBus};
use crate::emu::memory::Memory;
use crate::emu::ppu::Ppu;

pub enum DmaAction {
    /// Read from The Bus.
    ReadBus,
    /// Read from VRAM.
    ReadVram(u16),
    /// Write to OAM.
    WriteOam(u8, u8),
}

pub struct Dma {
    tick: u8,
    new_src: Option<u8>,
    src_addr: u8,
    offset: u8,
    value: u8,
    last_running: bool,
}

impl Dma {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            tick: 0,
            src_addr: 0,
            new_src: None,
            // anything >= 0xa0
            // this signals "not active"
            offset: 0xff,
            value: 0xff,
            last_running: false,
        }
    }

    #[must_use]
    pub const fn running(&self) -> bool {
        self.offset < 0xa0
    }

    #[must_use]
    pub const fn oam_blocked(&self) -> bool {
        // since this is logically asked for "at the end of T-3", but `running` would reflect "at the start of T-0", we need to track the last value for running.
        self.last_running
    }

    pub const fn write_src(&mut self, value: u8) {
        self.new_src = Some(value);
    }

    #[must_use]
    pub const fn read_src(&self) -> u8 {
        self.src_addr
    }

    pub fn tick(&mut self, bus: &mut ExternalBus, ppu: &mut Ppu, memory: &mut Memory) {
        self.last_running = self.running();

        debug_assert!(self.tick < 4);
        let st = self.tick;
        self.tick = (self.tick + 1) % 4;

        if st == 3
            && let Some(new_src) = self.new_src
        {
            // start new DMA
            self.new_src = None;
            self.src_addr = new_src;
            self.offset = 0;
            return;
        }

        if !self.running() {
            return;
        }

        let src_addr = u16::from_be_bytes([self.src_addr, self.offset]);

        match st {
            0 => match self.src_addr {
                0x00..0x80 | 0xa0.. => {
                    bus.addr = src_addr;

                    if self.src_addr >= 0xa0 {
                        bus.st.remove(BusState::CHIP_SELECT);
                    }

                    self.value = memory.strobe_read(bus);
                }

                0x80..0xa0 => {
                    self.value = ppu.get_vram(src_addr - 0x8000).unwrap_or(0xff);
                }
            },

            1 => {}
            2 => {
                ppu.oam[self.offset as usize] = self.value;
            }

            3 => {
                self.offset += 1;
            }

            4.. => unreachable!(),
        }
    }
}

impl Default for Dma {
    fn default() -> Self {
        Self::new()
    }
}
