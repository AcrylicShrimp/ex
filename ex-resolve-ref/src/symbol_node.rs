use ex_parser::{Id, NodeId};

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
    Variable,
}

impl SymbolNodeKind {
    pub fn function() -> Self {
        Self::Function
    }

    pub fn parameter(index: usize) -> Self {
        Self::Parameter { index }
    }

    pub fn variable() -> Self {
        Self::Variable
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function)
    }

    pub fn is_parameter(&self) -> bool {
        matches!(self, Self::Parameter { .. })
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable { .. })
    }
}
