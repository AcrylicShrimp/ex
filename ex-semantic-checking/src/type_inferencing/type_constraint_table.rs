use crate::resolve::TypeKind;
use ex_parser::{ASTBinaryOperatorKind, ASTUnaryOperatorKind, NodeId};
use ex_symbol::Symbol;
use std::{collections::HashMap, num::NonZeroU64};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(pub NonZeroU64);

impl TypeVariable {
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).unwrap())
    }
}

#[derive(Default, Debug, Clone)]
pub struct TypeConstraintTable {
    pub type_variables: HashMap<NodeId, TypeVariable>,
    pub type_contraints: Vec<TypeConstraint>,
    next_id: u64,
}

impl TypeConstraintTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_variable(&mut self, node: NodeId) -> TypeVariable {
        self.next_id += 1;
        let type_variable = TypeVariable::new(self.next_id);
        self.type_variables.insert(node, type_variable);
        type_variable
    }

    pub fn new_constraint(&mut self, constraint: TypeConstraint) {
        self.type_contraints.push(constraint);
    }
}

#[derive(Debug, Clone)]
pub struct TypeConstraint {
    pub kind: TypeConstraintKind,
    pub from: TypeVariable,
    pub to: TypeConstraintTarget,
}

impl TypeConstraint {
    pub fn equal(from: TypeVariable, to: TypeConstraintTarget) -> Self {
        Self {
            kind: TypeConstraintKind::Equal,
            from,
            to,
        }
    }

    pub fn subtype(from: TypeVariable, to: TypeConstraintTarget) -> Self {
        Self {
            kind: TypeConstraintKind::Subtype,
            from,
            to,
        }
    }

    pub fn is_equal(&self) -> bool {
        self.kind == TypeConstraintKind::Equal
    }

    pub fn is_subtype(&self) -> bool {
        self.kind == TypeConstraintKind::Subtype
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeConstraintKind {
    Equal,
    Subtype,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstraintTarget {
    Concrete {
        kind: TypeKind,
    },
    Variable {
        variable: TypeVariable,
    },
    BinaryOperation {
        operator: ASTBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    },
    UnaryOperation {
        operator: ASTUnaryOperatorKind,
        right: TypeVariable,
    },
    CallableParameterType {
        variable: TypeVariable,
        index: usize,
    },
    CallableReturnType {
        variable: TypeVariable,
    },
    MemberType {
        variable: TypeVariable,
        member: Symbol,
    },
    AddressOfType {
        variable: TypeVariable,
    },
    DereferenceType {
        variable: TypeVariable,
    },
}

impl TypeConstraintTarget {
    pub fn concrete(kind: TypeKind) -> Self {
        Self::Concrete { kind }
    }

    pub fn variable(variable: TypeVariable) -> Self {
        Self::Variable { variable }
    }

    pub fn binary_operation(
        operator: ASTBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    ) -> Self {
        Self::BinaryOperation {
            operator,
            left,
            right,
        }
    }

    pub fn unary_operation(operator: ASTUnaryOperatorKind, right: TypeVariable) -> Self {
        Self::UnaryOperation { operator, right }
    }

    pub fn callable_parameter_type(variable: TypeVariable, index: usize) -> Self {
        Self::CallableParameterType { variable, index }
    }

    pub fn callable_return_type(variable: TypeVariable) -> Self {
        Self::CallableReturnType { variable }
    }

    pub fn member_type(variable: TypeVariable, member: Symbol) -> Self {
        Self::MemberType { variable, member }
    }

    pub fn address_of_type(variable: TypeVariable) -> Self {
        Self::AddressOfType { variable }
    }

    pub fn dereference_type(variable: TypeVariable) -> Self {
        Self::DereferenceType { variable }
    }
}
