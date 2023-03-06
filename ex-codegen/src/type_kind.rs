use crate::TypeId;
use ex_symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
    Empty,
    Boolean,
    Integer,
    Float,
    String,
    Callable {
        parameters: Vec<TypeId>,
        return_type: TypeId,
    },
    UserTypeStruct {
        symbol: Symbol,
    },
    Pointer {
        type_id: TypeId,
    },
    Reference {
        type_id: TypeId,
    },
}

impl TypeKind {
    pub fn as_user_type_struct(&self) -> Symbol {
        match self {
            TypeKind::UserTypeStruct { symbol } => *symbol,
            _ => unreachable!(),
        }
    }
}
