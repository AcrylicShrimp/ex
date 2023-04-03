use crate::{Function, TypeIdTable, UserStruct};
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub type_id_table: TypeIdTable,
    pub functions: HashMap<Symbol, Function>,
    pub user_structs: HashMap<Symbol, UserStruct>,
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }
}
