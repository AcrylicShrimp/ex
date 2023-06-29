use crate::{
    hir::{HIRBinaryOperatorKind, HIRUnaryOperatorKind},
    resolve::TypeKind,
};
use lazy_static::lazy_static;
use std::collections::HashMap;

pub struct BuiltInBinaryOperator {
    operators: HashMap<(HIRBinaryOperatorKind, TypeKind, TypeKind), TypeKind>,
}

impl BuiltInBinaryOperator {
    pub fn new() -> Self {
        Self {
            operators: HashMap::from_iter([
                (
                    (
                        HIRBinaryOperatorKind::Eq,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Eq, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Eq,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Eq,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Ne,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Ne, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Ne,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Ne,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Lt,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Lt, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Lt,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Lt,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Gt,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Gt, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Gt,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Gt,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Le,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Le, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Le,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Le,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Ge,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Ge, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Ge,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Ge,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::LogOr,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::LogAnd,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (HIRBinaryOperatorKind::Add, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Add,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (HIRBinaryOperatorKind::Sub, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Sub,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (HIRBinaryOperatorKind::Mul, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Mul,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (HIRBinaryOperatorKind::Div, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Div,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (HIRBinaryOperatorKind::Mod, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Mod,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::Pow,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (HIRBinaryOperatorKind::Shl, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (HIRBinaryOperatorKind::Shr, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::BitOr,
                        TypeKind::int(),
                        TypeKind::int(),
                    ),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::BitAnd,
                        TypeKind::int(),
                        TypeKind::int(),
                    ),
                    TypeKind::int(),
                ),
                (
                    (
                        HIRBinaryOperatorKind::BitXor,
                        TypeKind::int(),
                        TypeKind::int(),
                    ),
                    TypeKind::int(),
                ),
            ]),
        }
    }

    pub fn result_type(
        &self,
        operator: HIRBinaryOperatorKind,
        left: TypeKind,
        right: TypeKind,
    ) -> Option<&TypeKind> {
        self.operators.get(&(operator, left, right))
    }
}

pub struct BuiltInUnaryOperator {
    operators: HashMap<(HIRUnaryOperatorKind, TypeKind), TypeKind>,
}

impl BuiltInUnaryOperator {
    pub fn new() -> Self {
        Self {
            operators: HashMap::from_iter([
                (
                    (HIRUnaryOperatorKind::Minus, TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (HIRUnaryOperatorKind::Minus, TypeKind::float()),
                    TypeKind::float(),
                ),
                (
                    (HIRUnaryOperatorKind::BitNot, TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (HIRUnaryOperatorKind::LogNot, TypeKind::bool()),
                    TypeKind::bool(),
                ),
            ]),
        }
    }

    pub fn result_type(
        &self,
        operator: HIRUnaryOperatorKind,
        operand: TypeKind,
    ) -> Option<&TypeKind> {
        self.operators.get(&(operator, operand))
    }
}

lazy_static! {
    pub static ref BUILT_IN_BINARY_OPERATOR: BuiltInBinaryOperator = BuiltInBinaryOperator::new();
    pub static ref BUILT_IN_UNARY_OPERATOR: BuiltInUnaryOperator = BuiltInUnaryOperator::new();
}
