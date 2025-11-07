pub struct Memory {
    pub wram: [u8; 0x2000],
    pub hram: [u8; 0x007f],
    game_rom: Box<[u8]>,
    boot_rom: Box<[u8]>,
    boot_rom_enabled: bool,
    mbc: Mbc,
}

impl Memory {
    #[must_use]
    pub fn new(game_rom: Box<[u8]>, boot_rom: Box<[u8]>) -> Option<Self> {
        if game_rom.len() < 0x150 || boot_rom.len() != 0x100 {
            None
        } else {
            let mbc = Mbc::new_detect(game_rom[0x147], game_rom[0x148], game_rom[0x149])?;
            Some(Self {
                wram: [0; 0x2000],
                hram: [0; 0x007f],
                game_rom,
                boot_rom,
                boot_rom_enabled: true,
                mbc,
            })
        }
    }

    #[must_use]
    pub fn read_rom_low(&self, addr: u16) -> u8 {
        if self.boot_rom_enabled && addr < 0x100 {
            self.boot_rom[addr as usize]
        } else {
            let addr = self.mbc.get_physical_addr_low(addr);
            if addr < self.game_rom.len() {
                self.game_rom[addr]
            } else {
                0xff
            }
        }
    }

    #[must_use]
    pub fn read_rom_high(&self, addr: u16) -> u8 {
        let addr = self.mbc.get_physical_addr_high(addr);
        if addr < self.game_rom.len() {
            self.game_rom[addr]
        } else {
            0xff
        }
    }

    pub fn mbc_write(&mut self, addr: u16, val: u8) {
        match self.mbc {
            Mbc::Mbc0 => {}
            Mbc::Mbc1(ref mut desc) => match addr {
                0x0000..0x2000 => unimplemented!(),
                0x2000..0x4000 => desc.rom_bank = if val & 0x1f == 0 { 1 } else { val & 0x1f },
                0x4000..0x6000 => desc.ram_bank = val & 0b11,
                0x6000..0x8000 => desc.ram_bank_mode = super::bits::has_bit(val, 0),
                _ => unreachable!(),
            },
        }
    }

    pub fn disable_boot_rom(&mut self) {
        self.boot_rom_enabled = false;
    }
}

#[derive(Debug)]
struct MbcDescriptor {
    banks_rom: u8,
    banks_ram: u8,
    ram_bank_mode: bool,
    ram_bank: u8,
    rom_bank: u8,
}

impl MbcDescriptor {
    fn get_real_bank_count(&self) -> usize {
        2 << (self.banks_rom as usize)
    }
}

#[derive(Debug)]
enum Mbc {
    Mbc0,
    Mbc1(MbcDescriptor),
}

impl Mbc {
    #[must_use]
    fn new_detect(cart_type: u8, banks_rom: u8, banks_ram: u8) -> Option<Mbc> {
        match cart_type {
            0x00 => Some(Mbc::Mbc0),
            0x01 => Some(Mbc::Mbc1(MbcDescriptor {
                banks_rom,
                banks_ram,
                ram_bank_mode: false,
                rom_bank: 1,
                ram_bank: 0,
            })),
            _ => None,
        }
    }

    #[must_use]
    fn get_physical_addr_low(&self, addr: u16) -> usize {
        match *self {
            Mbc::Mbc0 => addr as usize,
            Mbc::Mbc1(ref desc) => {
                if desc.ram_bank_mode {
                    addr as usize
                } else {
                    (addr as usize).wrapping_add(
                        ((desc.get_real_bank_count() - 1) & ((desc.ram_bank << 5) as usize))
                            * 0x4000,
                    )
                }
            }
        }
    }

    #[must_use]
    fn get_physical_addr_high(&self, addr: u16) -> usize {
        match *self {
            Mbc::Mbc0 => addr.wrapping_add(0x4000) as usize,
            Mbc::Mbc1(ref desc) => (addr as usize).wrapping_add(
                ((desc.get_real_bank_count() - 1)
                    & (desc.rom_bank | (desc.ram_bank << 5)) as usize)
                    * 0x4000,
            ),
        }
    }
}
