use crate::{TypeId, TypeIdAllocator, TypeKind};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TypeTable {
    pub types: HashMap<TypeId, TypeKind>,
    reverse_types: HashMap<TypeKind, TypeId>,
    type_id_allocator: TypeIdAllocator,
}

impl TypeTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, type_kind: TypeKind) -> TypeId {
        if let Some(id) = self.reverse_types.get(&type_kind).cloned() {
            return id;
        }

        let id = self.type_id_allocator.allocate();
        self.types.insert(id, type_kind.clone());
        self.reverse_types.insert(type_kind, id);
        id
    }
}
