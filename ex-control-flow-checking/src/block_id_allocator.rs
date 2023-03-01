use crate::BlockId;

#[derive(Default, Debug, Clone, Hash)]
pub struct BlockIdAllocator {
    next_id: u64,
}

impl BlockIdAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate(&mut self) -> BlockId {
        self.next_id += 1;
        BlockId::new(self.next_id)
    }
}
