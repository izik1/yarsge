#[derive(Debug, Clone)]
pub struct SparseVec<T> {
    pub pre: Vec<T>,
    pub zeros: usize,
    pub post: Vec<T>,
}

impl<T> SparseVec<T> {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            pre: Vec::new(),
            zeros: 0,
            post: Vec::new(),
        }
    }

    #[inline]
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.pre.len() + self.zeros + self.post.len()
    }
}

impl<T> Default for SparseVec<T> {
    fn default() -> Self {
        Self::new()
    }
}
