use crate::{TypeId, TypeIdAllocator, TypeIdKind};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TypeIdTable {
    pub types: HashMap<TypeId, TypeIdKind>,
    reverse_types: HashMap<TypeIdKind, TypeId>,
    type_id_allocator: TypeIdAllocator,
}

impl TypeIdTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, type_kind: TypeIdKind) -> TypeId {
        if let Some(id) = self.reverse_types.get(&type_kind).cloned() {
            return id;
        }

        let id = self.type_id_allocator.allocate();
        self.types.insert(id, type_kind.clone());
        self.reverse_types.insert(type_kind, id);
        id
    }
}
