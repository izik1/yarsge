use crate::emu::bus::{BusState, ExternalBus};

pub struct Memory {
    pub wram: [u8; 0x2000],
    pub hram: [u8; 0x007f],
    game_rom: Box<[u8]>,
    boot_rom: Option<Box<[u8]>>,
    cart_ram: Box<[u8]>,
    mbc: Mbc,
}

impl Memory {
    #[must_use]
    pub fn new(game_rom: Box<[u8]>, boot_rom: Box<[u8]>) -> Option<Self> {
        if game_rom.len() < 0x150 || boot_rom.len() != 0x100 {
            None
        } else {
            let mbc = Mbc::new_detect(game_rom[0x147], game_rom[0x148], game_rom[0x149])?;
            let cart_ram = mbc.make_ram();
            Some(Self {
                wram: [0; 0x2000],
                hram: [0; 0x007f],
                game_rom,
                boot_rom: Some(boot_rom),
                cart_ram,
                mbc,
            })
        }
    }

    fn strobe(&mut self, bus: &mut ExternalBus) {
        // pins are weird, `addr_15` is pulled low when it's supposed to be low, but `read=high` is
        // a zero.
        const BUS_READ_ROM: BusState = BusState::empty()
            .difference(BusState::BUSY)
            .difference(BusState::PIN_NOT_READ)
            .union(BusState::CHIP_SELECT)
            .union(BusState::PIN_NOT_WRITE);

        const BUS_WRITE_ROM: BusState = BusState::empty()
            .difference(BusState::BUSY)
            .union(BusState::PIN_NOT_READ)
            .union(BusState::CHIP_SELECT)
            .difference(BusState::PIN_NOT_WRITE);

        const BUS_READ_RAM: BusState = BusState::empty()
            .difference(BusState::BUSY)
            .difference(BusState::PIN_NOT_READ)
            .difference(BusState::CHIP_SELECT)
            .union(BusState::PIN_NOT_WRITE);

        const BUS_WRITE_RAM: BusState = BusState::empty()
            .difference(BusState::BUSY)
            .union(BusState::PIN_NOT_READ)
            .difference(BusState::CHIP_SELECT)
            .difference(BusState::PIN_NOT_WRITE);

        let st = bus.st;
        bus.st.insert(BusState::BUSY);

        match (bus.a15(), st) {
            (false, BUS_READ_ROM) => {
                bus.data = self.read_rom(bus.addr);
            }
            (false, BUS_WRITE_ROM) => {
                self.mbc_write(bus.addr, bus.data);
            }

            (true, BUS_READ_RAM) => {
                bus.data = match bus.addr & 0x7fff {
                    0x0000..0x2000 => unreachable!("VRAM space"),
                    // cart ram
                    addr @ 0x2000..0x4000 => {
                        let addr = self.mbc.map_ram_addr(addr - 0x2000);

                        self.cart_ram.get(addr).copied().unwrap_or(0xff)
                    }

                    // DMG specific behavior: WRAM (and therefore echo ram) is on the external bus
                    addr @ 0x4000..0x6000 => self.wram[(addr - 0x4000) as usize],
                    // echo ram / wram mirror would _normally_ end at ..0x7e00, but...
                    // OAM-DMA can read from the end of it...
                    // you really shouldn't DMA from there though!
                    // you can get nasty bus conflicts on actual hardware (depending on the specific cart).
                    addr @ 0x6000..0x8000 => self.wram[(addr - 0x6000) as usize],

                    0x8000.. => unreachable!(),
                };
            }

            (true, BUS_WRITE_RAM) => {
                match bus.addr & 0x7fff {
                    0x0000..0x2000 => unreachable!("VRAM space"),
                    // cart ram
                    addr @ 0x2000..0x4000 => {
                        let addr = self.mbc.map_ram_addr(addr - 0x2000);

                        if let Some(mem) = self.cart_ram.get_mut(addr) {
                            *mem = bus.data;
                        } else {
                            log::debug!("cart ram write failed: {addr:#08x} <- {:#02x}", bus.data);
                        }
                    }

                    // DMG specific behavior: WRAM (and therefore echo ram) is on the external bus
                    addr @ 0x4000..0x6000 => self.wram[(addr - 0x4000) as usize] = bus.data,
                    // echo ram / wram mirror would _normally_ end at ..0x7e00, but...
                    // OAM-DMA can read from there (but there probably isn't anything that can write)
                    // still, might as well have the bus correct for symmetry.
                    addr @ 0x6000..0x8000 => self.wram[(addr - 0x6000) as usize] = bus.data,

                    0x8000.. => unreachable!(),
                }
            }

            _ => {}
        }
    }

    #[must_use]
    pub fn strobe_read(&mut self, bus: &mut ExternalBus) -> u8 {
        self.strobe(bus);
        return bus.data;
    }

    pub fn strobe_write(&mut self, bus: &mut ExternalBus, val: u8) {
        if bus.busy() {
            return;
        }

        bus.st.remove(BusState::PIN_NOT_WRITE);
        bus.data = val;
        self.strobe(bus);
    }

    #[must_use]
    fn read_rom(&self, addr: u16) -> u8 {
        // inaccuracy: this shouldn't be visible on the bus.
        if let Some(boot_rom) = self.boot_rom.as_deref()
            && addr < 0x100
        {
            return boot_rom.get(addr as usize).copied().unwrap_or(0xff);
        }

        let addr = self.mbc.map_rom_addr(addr);

        self.game_rom.get(addr).copied().unwrap_or(0xff)
    }

    pub fn mbc_write(&mut self, addr: u16, val: u8) {
        match self.mbc {
            Mbc::Mbc0 => {}
            Mbc::Mbc1(ref mut desc) => match addr {
                0x0000..0x2000 => desc.ram_gate = val & 0xf == 0b1010,
                0x2000..0x4000 => desc.bank1 = if val & 0x1f == 0 { 1 } else { val & 0x1f },
                0x4000..0x6000 => desc.bank2 = val & 0b11,
                0x6000..0x8000 => desc.mode = super::bits::has_bit(val, 0),
                _ => unreachable!(),
            },
            Mbc::Mbc5(ref mut desc) => match addr {
                0x0000..0x2000 => desc.ram_gate = val == 0b0000_1010,
                0x2000..0x3000 => desc.rom_bank = (desc.rom_bank & 0xff00) | u16::from(val),
                0x3000..0x4000 => {
                    desc.rom_bank = (desc.rom_bank & 0x00ff) | (u16::from(val & 1) << 8)
                }
                0x4000..0x6000 => desc.ram_bank = val & 0xf,
                _ => {
                    log::warn!("invalid mbc write: {addr:#04x} ({val:#02x}");
                }
            },
        }
    }

    pub fn disable_boot_rom(&mut self) {
        self.boot_rom = None;
    }
}

#[derive(Debug)]
struct Mbc1 {
    banks_rom: u8,
    banks_ram: u8,
    ram_gate: bool,
    mode: bool,
    bank2: u8,
    bank1: u8,
}

impl Mbc1 {
    #[must_use]
    fn rom_bank_count(&self) -> usize {
        2 << (self.banks_rom as usize)
    }

    #[must_use]
    fn rom_bank_mask(&self) -> usize {
        self.rom_bank_count() - 1
    }

    #[must_use]
    fn ram_bank_count(&self) -> usize {
        match self.banks_ram {
            0x02 => 1,
            0x03 => 4,
            0x04 => 16,
            0x05 => 8,
            0x00 | 0x01 | _ => 0,
        }
    }

    #[must_use]
    fn ram_bank_mask(&self) -> usize {
        self.ram_bank_count() - 1
    }
}

#[derive(Debug)]
struct Mbc5 {
    banks_rom: u8,
    banks_ram: u8,
    ram_gate: bool,
    rom_bank: u16,
    ram_bank: u8,
}

impl Mbc5 {
    #[must_use]
    fn rom_bank_count(&self) -> usize {
        2 << (self.banks_rom as usize)
    }

    #[must_use]
    fn rom_bank_mask(&self) -> usize {
        self.rom_bank_count() - 1
    }

    #[must_use]
    fn ram_bank_count(&self) -> usize {
        match self.banks_ram {
            0x02 => 1,
            0x03 => 4,
            0x04 => 16,
            0x05 => 8,
            0x00 | 0x01 | _ => 0,
        }
    }

    #[must_use]
    fn ram_bank_mask(&self) -> usize {
        self.ram_bank_count() - 1
    }
}

#[derive(Debug)]
enum Mbc {
    Mbc0,
    Mbc1(Mbc1),
    Mbc5(Mbc5),
}

impl Mbc {
    #[must_use]
    fn make_ram(&self) -> Box<[u8]> {
        match self {
            Mbc::Mbc0 => Vec::new().into_boxed_slice(),
            Mbc::Mbc1(mbc1) => vec![0x00; 0x2000 * mbc1.ram_bank_count()].into_boxed_slice(),
            Mbc::Mbc5(mbc5) => vec![0x00; 0x2000 * mbc5.ram_bank_count()].into_boxed_slice(),
        }
    }

    fn new_detect(cart_type: u8, banks_rom: u8, banks_ram: u8) -> Option<Mbc> {
        match cart_type {
            0x00 => Some(Mbc::Mbc0),
            0x01..0x04 => Some(Mbc::Mbc1(Mbc1 {
                banks_rom,
                banks_ram,
                ram_gate: false,
                mode: false,
                bank1: 1,
                bank2: 0,
            })),
            0x19..0x1c => Some(Mbc::Mbc5(Mbc5 {
                banks_rom,
                banks_ram,
                ram_gate: false,
                rom_bank: 1,
                ram_bank: 0,
            })),
            _ => None,
        }
    }

    #[must_use]
    fn map_rom_addr(&self, addr: u16) -> usize {
        match *self {
            Mbc::Mbc0 => addr as usize,
            Mbc::Mbc1(ref desc) if addr < 0x4000 => {
                if !desc.mode {
                    return addr as usize;
                }

                (addr as usize)
                    .wrapping_add(((desc.rom_bank_mask()) & ((desc.bank2 << 5) as usize)) * 0x4000)
            }

            // if addr >= 0x4000
            Mbc::Mbc1(ref desc) => ((addr - 0x4000) as usize).wrapping_add(
                ((desc.rom_bank_mask()) & ((desc.bank2 << 5) | desc.bank1) as usize) * 0x4000,
            ),
            Mbc::Mbc5(_) if addr < 0x4000 => addr as usize,
            Mbc::Mbc5(ref desc) => ((addr - 0x4000) as usize)
                .wrapping_add(((desc.rom_bank_mask()) & (desc.rom_bank as usize)) * 0x4000),
        }
    }

    #[must_use]
    fn map_ram_addr(&self, addr: u16) -> usize {
        match *self {
            Mbc::Mbc0 => usize::MAX,
            Mbc::Mbc1(ref desc) if !desc.ram_gate => usize::MAX,
            Mbc::Mbc5(ref desc) if !desc.ram_gate => usize::MAX,
            Mbc::Mbc1(ref desc) if !desc.mode => usize::from(addr & 0x1fff),
            Mbc::Mbc1(ref desc) => {
                usize::from(addr & 0x1fff)
                    | ((desc.ram_bank_mask() & usize::from(desc.bank2)) << 13)
            }

            Mbc::Mbc5(ref desc) => {
                usize::from(addr & 0x1fff)
                    | ((desc.ram_bank_mask() & usize::from(desc.ram_bank)) << 13)
            }
        }
    }
}
