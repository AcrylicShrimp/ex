use ex_parser::NodeId;
use ex_resolve_ref::TypeKind;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TypeTable {
    pub types: HashMap<NodeId, TypeKind>,
}
impl TypeTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn assign(&mut self, node: NodeId, mut type_kind: TypeKind) -> bool {
        if let Some(old_type_kind) = self.types.get(&node) {
            if old_type_kind.eq(&type_kind) {
                return false;
            }
            type_kind = TypeKind::unknown();
        }
        self.types.insert(node, type_kind);
        true
    }

    pub fn purge_unknown(&mut self) {
        self.types.retain(|_, type_kind| !type_kind.is_unknown());
    }
}
