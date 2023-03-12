use crate::resolve::TopLevelTable;
use ex_parser::NodeId;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
    Unknown,
    Empty,
    Bool,
    Int,
    Float,
    String,
    Callable {
        params: Vec<TypeKind>,
        return_type: Box<TypeKind>,
    },
    UserStruct {
        id: NodeId,
    },
    Pointer {
        inner: Box<TypeKind>,
    },
    Reference {
        inner: Box<TypeKind>,
    },
}

impl TypeKind {
    pub fn unknown() -> Self {
        Self::Unknown
    }

    pub fn empty() -> Self {
        Self::Empty
    }

    pub fn bool() -> Self {
        Self::Bool
    }

    pub fn int() -> Self {
        Self::Int
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

    pub fn user_struct(id: NodeId) -> Self {
        Self::UserStruct { id }
    }

    pub fn pointer(inner: TypeKind) -> Self {
        Self::Pointer {
            inner: Box::new(inner),
        }
    }

    pub fn reference(inner: TypeKind) -> Self {
        Self::Reference {
            inner: Box::new(inner),
        }
    }

    pub fn is_subtype(from: &Self, to: &Self) -> bool {
        match (from, to) {
            (Self::Reference { inner: from }, Self::Reference { inner: to }) => {
                Self::is_subtype(from, to)
            }
            (Self::Reference { inner: from }, to) => Self::is_subtype(from, to),
            (_, Self::Reference { .. }) => false,
            (from, to) => {
                // TODO: Check if the types are compatible, not just equal.
                from == to && !from.is_unknown() && !to.is_unknown()
            }
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            TypeKind::Unknown => true,
            TypeKind::Pointer { inner } => inner.is_unknown(),
            TypeKind::Reference { inner } => inner.is_unknown(),
            _ => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, TypeKind::Empty)
    }

    pub fn is_reference(&self) -> bool {
        matches!(self, TypeKind::Reference { .. })
    }

    pub fn is_user_struct(&self) -> bool {
        matches!(self, TypeKind::UserStruct { .. })
    }

    pub fn unwrap_reference(&self) -> &Self {
        match self {
            TypeKind::Reference { inner } => inner,
            _ => self,
        }
    }

    pub fn display<'a>(&'a self, top_level_table: &'a TopLevelTable) -> TypeKindDisplay<'a> {
        TypeKindDisplay(top_level_table, self)
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Unknown => write!(f, "unknown"),
            TypeKind::Empty => write!(f, "()"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Int => write!(f, "int"),
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
            TypeKind::UserStruct { id } => {
                write!(f, "struct [id={:?}]", id)
            }
            TypeKind::Pointer { inner } => write!(f, "{} ptr", inner),
            TypeKind::Reference { inner } => write!(f, "{} ref", inner),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeKindDisplay<'a>(&'a TopLevelTable, &'a TypeKind);

impl<'a> Display for TypeKindDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            TypeKind::Unknown => write!(f, "unknown"),
            TypeKind::Empty => write!(f, "()"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Int => write!(f, "int"),
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
                    write!(f, "{}", Self(self.0, param))?;
                }
                write!(f, ") -> {}", Self(self.0, return_type))
            }
            TypeKind::UserStruct { id } => {
                let user_struct = self.0.user_types[id].as_user_struct().unwrap();
                write!(f, "struct {}", user_struct.name.symbol.to_str())
            }
            TypeKind::Pointer { inner } => write!(f, "{} ptr", Self(self.0, inner)),
            TypeKind::Reference { inner } => write!(f, "{} ref", Self(self.0, inner)),
        }
    }
}
