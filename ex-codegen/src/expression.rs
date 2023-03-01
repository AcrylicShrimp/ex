use crate::{BlockId, TemporaryId, TypeId, VariableId};
use ex_parser::TokenLiteral;
use ex_symbol::Symbol;

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
    // Phi {
    //     arguments: Vec<(BlockId, TemporaryId)>,
    // },
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
        arguments: Vec<TemporaryId>,
    },
    Literal {
        literal: TokenLiteral,
    },
    Variable {
        variable: VariableId,
    },
    Function {
        function: Symbol,
    },
    Temporary {
        temporary: TemporaryId,
    },
}

impl ExpressionKind {
    // pub fn phi(arguments: Vec<(BlockId, TemporaryId)>) -> Self {
    //     Self::Phi { arguments }
    // }

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

    pub fn call(expression: TemporaryId, arguments: Vec<TemporaryId>) -> Self {
        Self::Call {
            expression,
            arguments,
        }
    }

    pub fn literal(literal: TokenLiteral) -> Self {
        Self::Literal { literal }
    }

    pub fn variable(variable: VariableId) -> Self {
        Self::Variable { variable }
    }

    pub fn function(function: Symbol) -> Self {
        Self::Function { function }
    }

    pub fn temporary(temporary: TemporaryId) -> Self {
        Self::Temporary { temporary }
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
    Plus,
    Minus,
    BitNot,
    LogNot,
}
