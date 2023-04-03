use crate::TypeId;
use ex_parser::NodeId;
use ex_symbol::Symbol;
use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UserStructId(NonZeroU64);

impl UserStructId {
    pub fn new(id: NodeId) -> Self {
        Self(NonZeroU64::new(id.get()).unwrap())
    }

    pub fn get(self) -> u64 {
        self.0.get()
    }
}

#[derive(Debug, Clone)]
pub struct UserStruct {
    pub id: UserStructId,
    pub name: Symbol,
    pub fields: Vec<UserStructField>,
}

impl UserStruct {
    pub fn new(id: NodeId, name: Symbol, fields: Vec<UserStructField>) -> Self {
        Self {
            id: UserStructId::new(id),
            name,
            fields,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserStructField {
    pub name: Symbol,
    pub type_id: TypeId,
}

impl UserStructField {
    pub fn new(name: Symbol, type_id: TypeId) -> Self {
        Self { name, type_id }
    }
}
