use crate::TemporaryId;

#[derive(Default, Debug, Clone, Hash)]
pub struct TemporaryIdAllocator {
    next_id: u64,
}

impl TemporaryIdAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate(&mut self) -> TemporaryId {
        self.next_id += 1;
        TemporaryId::new(self.next_id)
    }
}
