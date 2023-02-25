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
    pub kind: TypeKind,
}

impl TypeReference {
    pub fn new(id: Id, kind: TypeKind) -> Self {
        Self { id, kind }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum TypeKind {
    Unknown,
    Integer,
    Float,
    String,
}

impl TypeKind {
    pub fn unknown() -> Self {
        Self::Unknown
    }

    pub fn integer() -> Self {
        Self::Integer
    }

    pub fn float() -> Self {
        Self::Float
    }

    pub fn string() -> Self {
        Self::String
    }
}
