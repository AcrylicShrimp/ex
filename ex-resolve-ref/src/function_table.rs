use crate::FunctionScopeTable;
use ex_parser::{Id, NodeId, Typename};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct FunctionTable {
    pub functions: HashMap<NodeId, Function>,
    pub function_scopes: HashMap<NodeId, FunctionScopeTable>,
    pub function_symbols: HashMap<Symbol, NodeId>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn lookup_function(&self, symbol: Symbol) -> Option<&Function> {
        self.function_symbols
            .get(&symbol)
            .and_then(|node| self.functions.get(node))
    }

    pub fn lookup_scope(&self, symbol: Symbol) -> Option<&FunctionScopeTable> {
        self.function_symbols
            .get(&symbol)
            .and_then(|node| self.function_scopes.get(node))
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Function {
    pub node: NodeId,
    pub name: Id,
    pub params: Vec<Id>,
    pub param_typenames: Vec<Typename>,
    pub return_typename: Option<Typename>,
    pub span: Span,
}

impl Function {
    pub fn new(
        node: NodeId,
        name: Id,
        params: Vec<Id>,
        param_typenames: Vec<Typename>,
        return_typename: Option<Typename>,
        span: Span,
    ) -> Self {
        Self {
            node,
            name,
            params,
            param_typenames,
            return_typename,
            span,
        }
    }
}
