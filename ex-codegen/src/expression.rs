use crate::{Pointer, TemporaryId, TypeId};
use ex_parser::TokenLiteral;

#[derive(Debug, Clone, Hash)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_id: TypeId,
}

impl Expression {
    pub fn new(kind: ExpressionKind, type_id: TypeId) -> Self {
        Self { kind, type_id }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum ExpressionKind {
    Binary {
        operator: BinaryOperator,
        left: TemporaryId,
        right: TemporaryId,
    },
    Unary {
        operator: UnaryOperator,
        right: TemporaryId,
    },
    Convert {
        expression: TemporaryId,
        from: TypeId,
        to: TypeId,
    },
    Call {
        expression: TemporaryId,
        args: Vec<TemporaryId>,
    },
    StructLiteral {
        struct_type: TypeId,
        fields: Vec<TemporaryId>,
    },
    Literal {
        literal: TokenLiteral,
    },
    ElementPointer {
        base: Pointer,
        indices: Vec<usize>,
    },
    Pointer {
        pointer: Pointer,
    },
    Load {
        pointer: Pointer,
    },
}

impl ExpressionKind {
    pub fn binary(operator: BinaryOperator, left: TemporaryId, right: TemporaryId) -> Self {
        Self::Binary {
            operator,
            left,
            right,
        }
    }

    pub fn unary(operator: UnaryOperator, right: TemporaryId) -> Self {
        Self::Unary { operator, right }
    }

    pub fn convert(expression: TemporaryId, from: TypeId, to: TypeId) -> Self {
        Self::Convert {
            expression,
            from,
            to,
        }
    }

    pub fn call(expression: TemporaryId, args: Vec<TemporaryId>) -> Self {
        Self::Call { expression, args }
    }

    pub fn struct_literal(struct_type: TypeId, fields: Vec<TemporaryId>) -> Self {
        Self::StructLiteral {
            struct_type,
            fields,
        }
    }

    pub fn literal(literal: TokenLiteral) -> Self {
        Self::Literal { literal }
    }

    pub fn element_pointer(base: Pointer, indices: Vec<usize>) -> Self {
        Self::ElementPointer { base, indices }
    }

    pub fn pointer(pointer: Pointer) -> Self {
        Self::Pointer { pointer }
    }

    pub fn load(pointer: Pointer) -> Self {
        Self::Load { pointer }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    LogOr,
    LogAnd,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Shl,
    Shr,
    BitOr,
    BitAnd,
    BitXor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    Minus,
    BitNot,
    LogNot,
}
