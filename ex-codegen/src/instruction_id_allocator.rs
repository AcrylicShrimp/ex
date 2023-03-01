use crate::InstructionId;

#[derive(Default, Debug, Clone, Hash)]
pub struct InstructionIdAllocator {
    next_id: u64,
}

impl InstructionIdAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate(&mut self) -> InstructionId {
        self.next_id += 1;
        InstructionId::new(self.next_id)
    }
}
