use crate::ScopeSymbolTable;
use ex_parser::NodeId;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(pub NodeId);

impl ScopeId {
    pub fn new(node: NodeId) -> Self {
        Self(node)
    }

    pub fn node(&self) -> NodeId {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct FunctionScopeTable {
    pub root: ScopeId,
    pub scopes: HashMap<ScopeId, Scope>,
    pub nodes: HashMap<NodeId, ScopeId>,
}

impl FunctionScopeTable {
    pub fn new(root: NodeId) -> Self {
        let root_scope = Scope::new(root, None);
        let root_scope_id = ScopeId::new(root);

        let mut scopes = HashMap::new();
        scopes.insert(root_scope_id, root_scope);

        let mut nodes = HashMap::new();
        nodes.insert(root, root_scope_id);

        Self {
            root: root_scope_id,
            scopes,
            nodes,
        }
    }

    pub fn scope(&self, node: ScopeId) -> &Scope {
        self.scopes.get(&node).unwrap()
    }

    pub fn scope_mut(&mut self, node: ScopeId) -> &mut Scope {
        self.scopes.get_mut(&node).unwrap()
    }

    pub fn node_scope(&self, node: NodeId) -> &Scope {
        self.scopes.get(self.nodes.get(&node).unwrap()).unwrap()
    }

    pub fn node_scope_mut(&mut self, node: NodeId) -> &mut Scope {
        self.scopes.get_mut(self.nodes.get(&node).unwrap()).unwrap()
    }

    pub fn new_scope(&mut self, node: NodeId, parent: ScopeId) -> ScopeId {
        let scope = Scope::new(node, Some(parent));
        let scope_id = ScopeId::new(node);
        self.scopes.insert(scope_id, scope);
        scope_id
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Scope {
    pub node: NodeId,
    pub parent: Option<ScopeId>,
    pub symbol_table: ScopeSymbolTable,
}

impl Scope {
    pub fn new(node: NodeId, parent: Option<ScopeId>) -> Self {
        Self {
            node,
            parent,
            symbol_table: ScopeSymbolTable::new(node),
        }
    }
}
