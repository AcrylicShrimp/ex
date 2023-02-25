use crate::SymbolNode;
use ex_parser::NodeId;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct SymbolReferenceTable {
    pub references: HashMap<NodeId, SymbolNode>,
}

impl SymbolReferenceTable {
    pub fn new() -> Self {
        Default::default()
    }
}
