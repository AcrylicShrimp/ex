use ex_parser::NodeId;
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct AssignmentLhsTable {
    pub kinds: HashMap<NodeId, AssignmentLhsKind>,
}

impl AssignmentLhsTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Hash)]
pub enum AssignmentLhsKind {
    Param { index: usize },
    Variable { node: NodeId },
    Field { field: Symbol },
}
