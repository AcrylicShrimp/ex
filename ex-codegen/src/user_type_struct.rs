use crate::TypeId;
use ex_symbol::Symbol;

#[derive(Debug, Clone)]
pub struct UserStruct {
    pub name: Symbol,
    pub fields: Vec<UserStructField>,
}

impl UserStruct {
    pub fn new(name: Symbol, fields: Vec<UserStructField>) -> Self {
        Self { name, fields }
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
