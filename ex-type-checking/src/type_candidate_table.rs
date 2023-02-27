use crate::TypeTable;
use ex_parser::NodeId;
use ex_resolve_ref::TypeKind;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TypeCandidateTable {
    pub candicates: HashMap<NodeId, Vec<TypeKind>>,
}

impl TypeCandidateTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add(&mut self, node_id: NodeId, type_kind: TypeKind) {
        self.candicates.entry(node_id).or_default().push(type_kind);
    }

    pub fn resolve(self) -> TypeTable {
        let mut type_table = TypeTable::new();

        for (node, mut candidates) in self.candicates {
            if candidates.is_empty() {
                continue;
            }

            if candidates.iter().any(|type_kind| type_kind.is_unknown()) {
                continue;
            }

            candidates.dedup();

            if candidates.len() != 1 {
                continue;
            }

            type_table.types.insert(node, candidates.pop().unwrap());
        }

        type_table
    }
}
