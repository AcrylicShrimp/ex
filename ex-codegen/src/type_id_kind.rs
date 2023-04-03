use crate::TypeId;
use ex_parser::NodeId;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeIdKind {
    Empty,
    Bool,
    Int,
    Float,
    String,
    Callable {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    UserStruct {
        id: NodeId,
    },
    Pointer {
        type_id: TypeId,
    },
    Reference {
        type_id: TypeId,
    },
}
