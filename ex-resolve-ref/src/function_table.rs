use crate::FunctionScopeTable;
use ex_parser::{Id, NodeId};
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct FunctionTable {
    pub functions: HashMap<Symbol, Function>,
    pub function_scopes: HashMap<Symbol, FunctionScopeTable>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Function {
    pub node: NodeId,
    pub name: Id,
    pub parameters: Vec<Id>,
}

impl Function {
    pub fn new(node: NodeId, name: Id, parameters: Vec<Id>) -> Self {
        Self {
            node,
            name,
            parameters,
        }
    }
}
