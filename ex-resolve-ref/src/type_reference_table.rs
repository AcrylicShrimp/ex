use ex_parser::{Id, NodeId};
use ex_span::Span;
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
        parameters: Vec<TypeKind>,
        return_type: Box<TypeKind>,
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

    pub fn callable(parameters: Vec<TypeKind>, return_type: TypeKind) -> Self {
        Self::Callable {
            parameters,
            return_type: Box::new(return_type),
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
                parameters,
                return_type,
            } => {
                write!(f, "fn (")?;

                for (i, parameter) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", parameter)?;
                }

                write!(f, "): {}", return_type)
            }
        }
    }
}
