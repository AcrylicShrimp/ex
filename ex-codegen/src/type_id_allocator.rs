use crate::TypeId;

#[derive(Default, Debug, Clone, Hash)]
pub struct TypeIdAllocator {
    next_id: u64,
}

impl TypeIdAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate(&mut self) -> TypeId {
        self.next_id += 1;
        TypeId::new(self.next_id)
    }
}
