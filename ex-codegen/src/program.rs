use crate::{Function, TypeTable};
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub functions: HashMap<Symbol, Function>,
    pub type_table: TypeTable,
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }
}
