use crate::TypeId;
use ex_symbol::Symbol;

#[derive(Debug, Clone)]
pub struct UserTypeStruct {
    pub name: Symbol,
    pub fields: Vec<UserTypeStructField>,
}

impl UserTypeStruct {
    pub fn new(name: Symbol, fields: Vec<UserTypeStructField>) -> Self {
        Self { name, fields }
    }
}

#[derive(Debug, Clone)]
pub struct UserTypeStructField {
    pub name: Symbol,
    pub type_id: TypeId,
}

impl UserTypeStructField {
    pub fn new(name: Symbol, type_id: TypeId) -> Self {
        Self { name, type_id }
    }
}
