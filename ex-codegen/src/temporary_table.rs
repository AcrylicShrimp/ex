use crate::{Temporary, TemporaryId, TemporaryIdAllocator, TypeId};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TemporaryTable {
    pub temporaries: HashMap<TemporaryId, Temporary>,
    temporary_id_allocator: TemporaryIdAllocator,
}

impl TemporaryTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, type_id: TypeId) -> TemporaryId {
        let id = self.temporary_id_allocator.allocate();
        self.temporaries.insert(id, Temporary::new(id, type_id));
        id
    }
}
