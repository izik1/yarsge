bitflags::bitflags! {
    #[derive(Clone, Copy, Eq, PartialEq)]
    pub struct BusState : u8 {
        const BUSY = 1 << 0;
        const PIN_NOT_READ = 1 << 1;
        const PIN_NOT_WRITE = 1 << 2;
        const CHIP_SELECT = 1 << 3;
    }
}

impl BusState {
    // CLK = 0, read=low, write=high, CS=high
    const DEFAULT: Self = Self::CHIP_SELECT.union(Self::PIN_NOT_WRITE);
}

pub struct ExternalBus {
    pub st: BusState,
    // addr[15] and addr[14:0]
    pub addr: u16,
    pub data: u8,
}

impl ExternalBus {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            st: BusState::DEFAULT,
            addr: 0xffff,
            data: 0xff,
        }
    }

    #[must_use]
    pub const fn a15(&self) -> bool {
        (self.addr >> 15) & 1 == 1
    }

    #[must_use]
    pub const fn busy(&self) -> bool {
        self.st.contains(BusState::BUSY)
    }

    pub fn set_addr_cpu(&mut self, addr: u16) {
        if self.busy() {
            return;
        }

        self.addr = addr;

        if (0xa000..0xfe00).contains(&addr) {
            self.st.remove(BusState::CHIP_SELECT);
        }
    }

    pub fn set_addr_dma(&mut self, addr: u16) {
        if self.busy() {
            return;
        }

        self.addr = addr;

        if addr >= 0xa000 {
            self.st.remove(BusState::CHIP_SELECT);
        }
    }
}

impl Default for ExternalBus {
    fn default() -> Self {
        Self::new()
    }
}
