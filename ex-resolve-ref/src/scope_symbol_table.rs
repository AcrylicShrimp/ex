use crate::SymbolNode;
use ex_parser::NodeId;
use ex_symbol::Symbol;

#[derive(Debug, Clone, Hash)]
pub struct ScopeSymbolTable {
    pub scope_node: NodeId,
    pub symbols: Vec<SymbolNode>,
}

impl ScopeSymbolTable {
    pub fn new(scope_node: NodeId) -> Self {
        Self {
            scope_node,
            symbols: Vec::new(),
        }
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<&SymbolNode> {
        self.symbols
            .iter()
            .find(|symbol_node| symbol_node.id.symbol == symbol)
    }
}
