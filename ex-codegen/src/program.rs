use crate::{Function, TypeTable, UserTypeStruct};
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub functions: HashMap<Symbol, Function>,
    pub type_table: TypeTable,
    pub user_type_struct_table: HashMap<Symbol, UserTypeStruct>,
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }
}
