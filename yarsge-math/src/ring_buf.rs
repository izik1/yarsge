#[derive(Clone)]
pub struct RingBuf<T> {
    /// Safety invariant: must *always* be in-bounds for `ring`.
    idx: usize,
    ring: Box<[T]>,
}

impl<T> RingBuf<T> {
    ///
    ///
    /// # Panics
    /// - ring must not be empty.
    #[must_use]
    pub const fn new(ring: Box<[T]>) -> Self {
        assert!(!ring.is_empty());

        // Safety: idx must always be in-bounds for ring, which means ring can't be empty.
        Self { idx: 0, ring }
    }

    pub fn push(&mut self, value: T) {
        // ensure safety preconditions
        debug_assert!(self.idx < self.ring.len());

        // Safety: `self.idx` is always in bounds
        unsafe {
            *self.ring.get_unchecked_mut(self.idx) = value;
        }

        self.idx = self.idx.checked_sub(1).unwrap_or(self.ring.len() - 1);
    }

    #[must_use]
    pub const fn split(&self) -> (&'_ [T], &'_ [T]) {
        debug_assert!(self.idx < self.ring.len());

        // Safety: `self.idx` is always in bounds (`self.idx < self.ring.len()`), this requires `0 <= mid <= len()`.
        unsafe { self.ring.split_at_unchecked(self.idx + 1) }
    }
}

impl<T> From<Box<[T]>> for RingBuf<T> {
    fn from(value: Box<[T]>) -> Self {
        Self::new(value)
    }
}

impl<T> From<Vec<T>> for RingBuf<T> {
    fn from(value: Vec<T>) -> Self {
        Self::new(value.into_boxed_slice())
    }
}
