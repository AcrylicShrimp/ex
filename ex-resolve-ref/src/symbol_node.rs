use ex_parser::{Id, NodeId};

use crate::ScopeId;

#[derive(Debug, Clone, Hash)]
pub struct SymbolNode {
    pub kind: SymbolNodeKind,
    pub node: NodeId,
    pub id: Id,
}

impl SymbolNode {
    pub fn new(kind: SymbolNodeKind, node: NodeId, id: Id) -> Self {
        Self { kind, node, id }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolNodeKind {
    Function,
    Parameter { index: usize },
    Variable { scope: ScopeId },
}

impl SymbolNodeKind {
    pub fn function() -> Self {
        Self::Function
    }

    pub fn parameter(index: usize) -> Self {
        Self::Parameter { index }
    }

    pub fn variable(scope: ScopeId) -> Self {
        Self::Variable { scope }
    }
}
