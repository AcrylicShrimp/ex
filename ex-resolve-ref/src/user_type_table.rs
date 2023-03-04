use ex_parser::{Id, NodeId, Typename};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct UserTypeTable {
    pub user_types: HashMap<NodeId, UserTypeKind>,
    pub user_type_symbols: HashMap<Symbol, NodeId>,
}

impl UserTypeTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<&UserTypeKind> {
        self.user_type_symbols
            .get(&symbol)
            .and_then(|node| self.user_types.get(node))
    }
}

#[derive(Debug, Clone, Hash)]
pub enum UserTypeKind {
    Struct(UserTypeStruct),
}

impl UserTypeKind {
    pub fn as_struct(&self) -> &UserTypeStruct {
        match self {
            Self::Struct(s) => s,
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct UserTypeStruct {
    pub node: NodeId,
    pub name: Id,
    pub fields: Vec<Id>,
    pub fields_typenames: Vec<Typename>,
    pub fields_spans: Vec<Span>,
    pub span: Span,
}

impl UserTypeStruct {
    pub fn new(
        node: NodeId,
        name: Id,
        fields: Vec<Id>,
        fields_typenames: Vec<Typename>,
        fields_spans: Vec<Span>,
        span: Span,
    ) -> Self {
        Self {
            node,
            name,
            fields,
            fields_typenames,
            fields_spans,
            span,
        }
    }
}
