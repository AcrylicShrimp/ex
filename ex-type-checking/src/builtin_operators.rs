use ex_parser::{ASTAssignmentOperatorKind, ASTBinaryOperatorKind, ASTUnaryOperatorKind};
use ex_resolve_ref::TypeKind;
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
                    TypeKind::integer(),
                    TypeKind::integer(),
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
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Sub,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Mul,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Mul,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Mul,
                    TypeKind::string(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Div,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Div,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Mod,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Mod,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Pow,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Pow,
                    TypeKind::float(),
                    TypeKind::float(),
                ),
                (
                    ASTAssignmentOperatorKind::Shl,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::Shr,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::BitOr,
                    TypeKind::boolean(),
                    TypeKind::boolean(),
                ),
                (
                    ASTAssignmentOperatorKind::BitOr,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::BitAnd,
                    TypeKind::boolean(),
                    TypeKind::boolean(),
                ),
                (
                    ASTAssignmentOperatorKind::BitAnd,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::BitXor,
                    TypeKind::integer(),
                    TypeKind::integer(),
                ),
                (
                    ASTAssignmentOperatorKind::BitNot,
                    TypeKind::integer(),
                    TypeKind::integer(),
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
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Eq,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Eq,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Eq,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ne,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Lt,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Gt,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Le,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::float(),
                        TypeKind::float(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Ge,
                        TypeKind::string(),
                        TypeKind::string(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::LogOr,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::LogAnd,
                        TypeKind::boolean(),
                        TypeKind::boolean(),
                    ),
                    TypeKind::boolean(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Add,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                    (
                        ASTBinaryOperatorKind::Sub,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                    (
                        ASTBinaryOperatorKind::Mul,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                        TypeKind::integer(),
                    ),
                    TypeKind::string(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Mul,
                        TypeKind::integer(),
                        TypeKind::string(),
                    ),
                    TypeKind::string(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Div,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                    (
                        ASTBinaryOperatorKind::Mod,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                    (
                        ASTBinaryOperatorKind::Pow,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                    (
                        ASTBinaryOperatorKind::Shl,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::Shr,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::BitOr,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::BitAnd,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
                ),
                (
                    (
                        ASTBinaryOperatorKind::BitXor,
                        TypeKind::integer(),
                        TypeKind::integer(),
                    ),
                    TypeKind::integer(),
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
                    (ASTUnaryOperatorKind::Plus, TypeKind::integer()),
                    TypeKind::integer(),
                ),
                (
                    (ASTUnaryOperatorKind::Plus, TypeKind::float()),
                    TypeKind::float(),
                ),
                (
                    (ASTUnaryOperatorKind::Minus, TypeKind::integer()),
                    TypeKind::integer(),
                ),
                (
                    (ASTUnaryOperatorKind::Minus, TypeKind::float()),
                    TypeKind::float(),
                ),
                (
                    (ASTUnaryOperatorKind::BitNot, TypeKind::integer()),
                    TypeKind::integer(),
                ),
                (
                    (ASTUnaryOperatorKind::LogNot, TypeKind::boolean()),
                    TypeKind::boolean(),
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
