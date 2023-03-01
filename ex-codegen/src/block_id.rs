use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(NonZeroU64);

impl BlockId {
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).unwrap())
    }

    pub fn get(self) -> u64 {
        self.0.get()
    }
}
