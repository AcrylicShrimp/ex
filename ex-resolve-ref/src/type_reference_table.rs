use ex_parser::NodeId;
use ex_span::Span;
use ex_symbol::Symbol;
use std::{collections::HashMap, fmt::Display};

#[derive(Default, Debug, Clone)]
pub struct TypeReferenceTable {
    pub references: HashMap<NodeId, TypeReference>,
}

impl TypeReferenceTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Hash)]
pub struct TypeReference {
    pub kind: TypeKind,
    pub span: Span,
}

impl TypeReference {
    pub fn new(kind: TypeKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
    Unknown,
    Empty,
    Boolean,
    Integer,
    Float,
    String,
    Callable {
        params: Vec<TypeKind>,
        return_type: Box<TypeKind>,
    },
    UserTypeStruct {
        symbol: Symbol,
    },
    Pointer {
        type_kind: Box<TypeKind>,
    },
    Reference {
        type_kind: Box<TypeKind>,
    },
}

impl TypeKind {
    pub fn unknown() -> Self {
        Self::Unknown
    }

    pub fn empty() -> Self {
        Self::Empty
    }

    pub fn boolean() -> Self {
        Self::Boolean
    }

    pub fn integer() -> Self {
        Self::Integer
    }

    pub fn float() -> Self {
        Self::Float
    }

    pub fn string() -> Self {
        Self::String
    }

    pub fn callable(params: Vec<TypeKind>, return_type: TypeKind) -> Self {
        Self::Callable {
            params,
            return_type: Box::new(return_type),
        }
    }

    pub fn user_type_struct(symbol: Symbol) -> Self {
        Self::UserTypeStruct { symbol }
    }

    pub fn pointer(type_kind: TypeKind) -> Self {
        Self::Pointer {
            type_kind: Box::new(type_kind),
        }
    }

    pub fn reference(type_kind: TypeKind) -> Self {
        Self::Reference {
            type_kind: Box::new(type_kind),
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Boolean)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Integer)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String)
    }

    pub fn is_callable(&self) -> bool {
        matches!(self, Self::Callable { .. })
    }

    pub fn is_user_type_struct(&self) -> bool {
        matches!(self, Self::UserTypeStruct { .. })
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer { .. })
    }

    pub fn is_reference(&self) -> bool {
        matches!(self, Self::Reference { .. })
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Unknown => write!(f, "unknown"),
            TypeKind::Empty => write!(f, "()"),
            TypeKind::Boolean => write!(f, "bool"),
            TypeKind::Integer => write!(f, "int"),
            TypeKind::Float => write!(f, "float"),
            TypeKind::String => write!(f, "string"),
            TypeKind::Callable {
                params,
                return_type,
            } => {
                write!(f, "fn (")?;

                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", param)?;
                }

                write!(f, ") -> {}", return_type)
            }
            TypeKind::UserTypeStruct { symbol } => {
                write!(f, "struct {}", symbol)
            }
            TypeKind::Pointer { type_kind } => {
                write!(f, "{} ptr", type_kind)
            }
            TypeKind::Reference { type_kind } => {
                write!(f, "{} ref", type_kind)
            }
        }
    }
}
