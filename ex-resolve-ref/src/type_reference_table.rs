use ex_parser::{Id, NodeId};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TypeReferenceTable {
    pub references: HashMap<NodeId, TypeReference>,
}

impl TypeReferenceTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Hash)]
pub struct TypeReference {
    pub id: Id,
    pub kind: TypeReferenceKind,
}

impl TypeReference {
    pub fn new(id: Id, kind: TypeReferenceKind) -> Self {
        Self { id, kind }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum TypeReferenceKind {
    Function { node_id: NodeId },
    Parameter { node_id: NodeId, index: usize },
    Variable { node_id: NodeId, scope: NodeId },
}

impl TypeReferenceKind {
    pub fn function(node_id: NodeId) -> Self {
        Self::Function { node_id }
    }

    pub fn parameter(node_id: NodeId, index: usize) -> Self {
        Self::Parameter { node_id, index }
    }

    pub fn variable(node_id: NodeId, scope: NodeId) -> Self {
        Self::Variable { node_id, scope }
    }
}
