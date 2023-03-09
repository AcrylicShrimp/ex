use crate::resolve::TypeKind;
use ex_parser::{ASTAssignmentOperatorKind, ASTBinaryOperatorKind, ASTUnaryOperatorKind};
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};

pub struct BuiltInAssignmentOperator {
    operators: HashSet<(ASTAssignmentOperatorKind, TypeKind, TypeKind)>,
}

impl BuiltInAssignmentOperator {
    pub fn new() -> Self {
        Self {
            operators: HashSet::from_iter([
                (
                    ASTAssignmentOperatorKind::Add,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Add,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Add,
                    TypeKind::string(),
                    TypeKind::string(),
                ),
                (
                    ASTAssignmentOperatorKind::Sub,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Sub,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Mul,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Mul,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Mul,
                    TypeKind::string(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Div,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Div,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Mod,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Mod,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Pow,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Pow,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Shl,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::Shr,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::BitOr,
                    TypeKind::bool(),
                    TypeKind::bool(),
                ),
                (
                    ASTAssignmentOperatorKind::BitOr,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::BitAnd,
                    TypeKind::bool(),
                    TypeKind::bool(),
                ),
                (
                    ASTAssignmentOperatorKind::BitAnd,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::BitXor,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
                (
                    ASTAssignmentOperatorKind::BitNot,
                    TypeKind::int(),
                    TypeKind::int(),
                ),
            ]),
        }
    }

    pub fn is_supported(
        &self,
        operator: ASTAssignmentOperatorKind,
        left: TypeKind,
        right: TypeKind,
    ) -> bool {
        self.operators.contains(&(operator, left, right))
    }
}

pub struct BuiltInBinaryOperator {
    operators: HashMap<(ASTBinaryOperatorKind, TypeKind, TypeKind), TypeKind>,
}

impl BuiltInBinaryOperator {
    pub fn new() -> Self {
        Self {
            operators: HashMap::from_iter([
                (
                    (
                        ASTBinaryOperatorKind::Eq,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Eq, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Eq,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Eq,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Ne, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Lt, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Gt, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Le, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Ge, TypeKind::int(), TypeKind::int()),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::LogOr,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::LogAnd,
                        TypeKind::bool(),
                        TypeKind::bool(),
                    ),
                    TypeKind::bool(),
                ),
                (
                    (ASTBinaryOperatorKind::Add, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Add,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Add,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::string(),
                ),
                (
                    (ASTBinaryOperatorKind::Sub, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Sub,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (ASTBinaryOperatorKind::Mul, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Mul,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Mul,
                        TypeKind::string(),
                        TypeKind::int(),
                    ),
                    TypeKind::string(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Mul,
                        TypeKind::int(),
                        TypeKind::string(),
                    ),
                    TypeKind::string(),
                ),
                (
                    (ASTBinaryOperatorKind::Div, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Div,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (ASTBinaryOperatorKind::Mod, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Mod,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (ASTBinaryOperatorKind::Pow, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Pow,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::float(),
                ),
                (
                    (ASTBinaryOperatorKind::Shl, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (ASTBinaryOperatorKind::Shr, TypeKind::int(), TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::BitOr,
                        TypeKind::int(),
                        TypeKind::int(),
                    ),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::BitAnd,
                        TypeKind::int(),
                        TypeKind::int(),
                    ),
                    TypeKind::int(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::BitXor,
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
        operator: ASTBinaryOperatorKind,
        left: TypeKind,
        right: TypeKind,
    ) -> Option<&TypeKind> {
        self.operators.get(&(operator, left, right))
    }
}

pub struct BuiltInUnaryOperator {
    operators: HashMap<(ASTUnaryOperatorKind, TypeKind), TypeKind>,
}

impl BuiltInUnaryOperator {
    pub fn new() -> Self {
        Self {
            operators: HashMap::from_iter([
                (
                    (ASTUnaryOperatorKind::Plus, TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (ASTUnaryOperatorKind::Plus, TypeKind::float()),
                    TypeKind::float(),
                ),
                (
                    (ASTUnaryOperatorKind::Minus, TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (ASTUnaryOperatorKind::Minus, TypeKind::float()),
                    TypeKind::float(),
                ),
                (
                    (ASTUnaryOperatorKind::BitNot, TypeKind::int()),
                    TypeKind::int(),
                ),
                (
                    (ASTUnaryOperatorKind::LogNot, TypeKind::bool()),
                    TypeKind::bool(),
                ),
            ]),
        }
    }

    pub fn result_type(
        &self,
        operator: ASTUnaryOperatorKind,
        operand: TypeKind,
    ) -> Option<&TypeKind> {
        self.operators.get(&(operator, operand))
    }
}

lazy_static! {
    pub static ref BUILT_IN_ASSIGNMENT_OPERATOR: BuiltInAssignmentOperator =
        BuiltInAssignmentOperator::new();
    pub static ref BUILT_IN_BINARY_OPERATOR: BuiltInBinaryOperator = BuiltInBinaryOperator::new();
    pub static ref BUILT_IN_UNARY_OPERATOR: BuiltInUnaryOperator = BuiltInUnaryOperator::new();
}
