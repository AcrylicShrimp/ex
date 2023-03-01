use crate::VariableId;

#[derive(Default, Debug, Clone, Hash)]
pub struct VariableIdAllocator {
    next_id: u64,
}

impl VariableIdAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate(&mut self) -> VariableId {
        self.next_id += 1;
        VariableId::new(self.next_id)
    }
}
