use crate::{TemporaryId, TypeId};

#[derive(Debug, Clone, Copy)]
pub struct Temporary {
    pub id: TemporaryId,
    pub type_id: TypeId,
}

impl Temporary {
    pub fn new(id: TemporaryId, type_id: TypeId) -> Self {
        Self { id, type_id }
    }
}

impl PartialEq for Temporary {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Temporary {}

impl PartialOrd for Temporary {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Temporary {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl std::hash::Hash for Temporary {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
