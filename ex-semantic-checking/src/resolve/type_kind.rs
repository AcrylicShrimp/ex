use ex_parser::NodeId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    pub fn is_reference(&self) -> bool {
        matches!(self, Self::Reference { .. })
    }
}
