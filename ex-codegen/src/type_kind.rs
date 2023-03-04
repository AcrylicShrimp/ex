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
}
