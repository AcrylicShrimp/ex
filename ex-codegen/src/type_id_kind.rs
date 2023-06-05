use crate::{TypeId, UserStructId};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeIdKind {
    Empty,
    Bool,
    Int,
    Float,
    String,
    Callable {
        param_types: Vec<TypeId>,
        return_type: TypeId,
    },
    UserStruct {
        id: UserStructId,
    },
    Pointer {
        type_id: TypeId,
    },
    Reference {
        type_id: TypeId,
    },
}
