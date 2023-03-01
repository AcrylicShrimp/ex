use crate::{TypeId, VariableId};

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub id: VariableId,
    pub type_id: TypeId,
}

impl Variable {
    pub fn new(id: VariableId, type_id: TypeId) -> Self {
        Self { id, type_id }
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Variable {}

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Variable {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl std::hash::Hash for Variable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
