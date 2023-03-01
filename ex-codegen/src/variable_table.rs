use crate::{TypeId, Variable, VariableId, VariableIdAllocator};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct VariableTable {
    pub variables: HashMap<VariableId, Variable>,
    variable_id_allocator: VariableIdAllocator,
}

impl VariableTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, type_id: TypeId) -> VariableId {
        let id = self.variable_id_allocator.allocate();
        self.variables.insert(id, Variable::new(id, type_id));
        id
    }
}
