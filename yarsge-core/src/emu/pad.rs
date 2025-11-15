use crate::{FallingEdge, Keys};

bitflags::bitflags! {
    struct Status : u8 {
        const NOT_BUTTONS = 1 << 5;
        const NOT_DPAD = 1 << 4;
    }
}

#[non_exhaustive]
pub struct Pad {
    pub keys: Keys,
    status: Status,
    keys_interrupt: FallingEdge,
}

impl Pad {
    pub const fn new() -> Self {
        Self {
            status: Status::empty(),
            keys: Keys::empty(),
            keys_interrupt: FallingEdge::new(true),
        }
    }

    #[inline]
    pub const fn set_status(&mut self, val: u8) {
        self.status = Status::from_bits_truncate(val);
    }

    #[must_use]
    pub const fn selected(&self) -> u8 {
        0xc0 | self.status.bits() | self.pad()
    }

    #[must_use]
    pub fn tick(&mut self) -> bool {
        self.keys_interrupt.tick(self.pad() == 0xf)
    }

    const fn pad(&self) -> u8 {
        if !self.status.contains(Status::NOT_BUTTONS) {
            !self.keys.bits() & 0xf
        } else if !self.status.contains(Status::NOT_DPAD) {
            !self.keys.bits() >> 4
        } else {
            0xf
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Keys;
    use crate::emu::pad::Pad;

    impl Pad {
        const fn with_status(status: u8) -> Self {
            let mut this = Self::new();
            this.set_status(status);

            this
        }

        fn tick_with_keys(&mut self, new_keys: Keys) -> bool {
            self.keys = new_keys;
            self.tick()
        }
    }

    fn all_keys() -> impl Iterator<Item = Keys> {
        (0..=0xff).map(Keys::from_bits_truncate)
    }

    #[test]
    fn tick_twice_never_interrupt() {
        let mut pad = Pad::new();

        // select buttons
        pad.set_status(!(0b10 << 4));

        assert!(
            !pad.tick() || !pad.tick(),
            "Interrupt should never occur two ticks in a row"
        );

        pad.keys = Keys::all();

        assert!(
            !pad.tick() || !pad.tick(),
            "Interrupt should never occur two ticks in a row"
        );

        pad.keys = Keys::empty();

        assert!(
            !pad.tick() || !pad.tick(),
            "Interrupt should never occur two ticks in a row"
        );

        pad.keys = Keys::A;

        let a = pad.tick();

        pad.keys = Keys::B;
        let b = pad.tick();

        assert!(!a || !b, "Interrupt should never occur two ticks in a row");
    }

    // this is kinda a trivial test, but... Who knows, maybe it'll catch a regression one day.
    #[test]
    fn tick_never_modifies_keys() {
        let mut pad = Pad::new();

        for keys in all_keys() {
            pad.keys = keys;
            let _ = pad.tick();
            assert_eq!(pad.keys, keys);
        }
    }

    #[test]
    fn interrupt_any_selected() {
        let mut pad_none = Pad::with_status(!(0b00 << 4));
        let mut pad_buttons = Pad::with_status(!(0b10 << 4));
        let mut pad_dpad = Pad::with_status(!(0b01 << 4));

        for keys in all_keys() {
            pad_none.keys = keys;
            pad_buttons.keys = keys;
            pad_dpad.keys = keys;

            assert!(
                !pad_none.tick_with_keys(keys),
                "empty pad should never interrupt"
            );

            assert_eq!(
                pad_buttons.tick_with_keys(keys),
                keys.intersects(Keys::A | Keys::B | Keys::SELECT | Keys::START)
            );

            assert_eq!(
                pad_dpad.tick_with_keys(keys),
                keys.intersects(Keys::RIGHT | Keys::LEFT | Keys::UP | Keys::DOWN)
            );

            assert!(
                !pad_none.tick_with_keys(Keys::empty()),
                "empty pad should never interrupt"
            );

            assert!(
                !pad_buttons.tick_with_keys(Keys::empty()),
                "pad shouldn't have interrupt on {{pad}} -> 0xf"
            );

            assert!(
                !pad_dpad.tick_with_keys(Keys::empty()),
                "pad shouldn't have interrupt on {{pad}} -> 0xf"
            );
        }
    }

    #[test]
    fn pad_all_acts_like_pad_buttons() {
        let mut pad_buttons = {
            let mut pad = Pad::new();
            pad.set_status(!(0b10 << 4));
            pad
        };

        let mut pad_all = {
            let mut pad = Pad::new();
            pad.set_status(!(0b11 << 4));
            pad
        };

        pad_buttons.keys = Keys::empty();
        pad_all.keys = Keys::empty();

        assert_eq!(
            pad_buttons.pad(),
            pad_all.pad(),
            "pad value mismatch (empty)"
        );

        assert_eq!(
            pad_buttons.tick(),
            pad_all.tick(),
            "pad interrupt mismatch (empty)"
        );

        for b in 0_u8..=0xff {
            let keys = Keys::from_bits_truncate(b);
            assert_eq!(
                pad_buttons.tick_with_keys(keys),
                pad_all.tick_with_keys(keys),
                "pad interrupt mismatch, byte: {b:#04x}"
            );

            assert_eq!(
                pad_buttons.pad(),
                pad_all.pad(),
                "pad value mismatch, byte: {b:#04x}"
            );

            pad_buttons.keys = Keys::empty();
            pad_all.keys = Keys::empty();

            assert!(!pad_buttons.tick(), "empty pad should never have interrupt");
            assert!(!pad_all.tick(), "empty pad should never have interrupt");
        }
    }

    #[test]
    fn status_bits() {
        let mut pad = Pad::new();
        assert_eq!(pad.status.bits(), 0);
        assert_eq!(pad.selected(), 0xc0 | 0xf);
        pad.set_status(0xff);
        assert_eq!(pad.status.bits() & !0x30, 0);
        assert_eq!(pad.selected(), 0xff);
    }
}
