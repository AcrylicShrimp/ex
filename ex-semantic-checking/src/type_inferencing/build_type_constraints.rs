use crate::{
    hir::{
        HIRBinaryOperatorKind, HIRBlock, HIRExpression, HIRExpressionKind, HIRFunction, HIRProgram,
        HIRStatementKind, HIRUnaryOperatorKind,
    },
    resolve::{ReferenceTable, TopLevelTable, TypeKind},
};
use ex_parser::{NodeId, TokenLiteralKind};
use ex_symbol::Symbol;
use std::{collections::HashMap, num::NonZeroU64};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(pub NonZeroU64);

impl TypeVariable {
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).unwrap())
    }
}

pub type TypeVariableTable = HashMap<NodeId, TypeVariable>;
pub type ReverseTypeVariableTable = HashMap<TypeVariable, NodeId>;
pub type TypeConstraints = Vec<TypeConstraint>;

#[derive(Default, Debug, Clone)]
pub struct TypeConstraintTable {
    pub type_variables: TypeVariableTable,
    pub reverse_type_variables: ReverseTypeVariableTable,
    pub type_constraints: TypeConstraints,
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
        self.reverse_type_variables.insert(type_variable, node);
        type_variable
    }

    pub fn new_constraint(&mut self, constraint: TypeConstraint) {
        self.type_constraints.push(constraint);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        operator: HIRBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    },
    UnaryOperation {
        operator: HIRUnaryOperatorKind,
        right: TypeVariable,
    },
    CallableParamType {
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
}

impl TypeConstraintTarget {
    pub fn concrete(kind: TypeKind) -> Self {
        Self::Concrete { kind }
    }

    pub fn variable(variable: TypeVariable) -> Self {
        Self::Variable { variable }
    }

    pub fn binary_operation(
        operator: HIRBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    ) -> Self {
        Self::BinaryOperation {
            operator,
            left,
            right,
        }
    }

    pub fn unary_operation(operator: HIRUnaryOperatorKind, right: TypeVariable) -> Self {
        Self::UnaryOperation { operator, right }
    }

    pub fn callable_param_type(variable: TypeVariable, index: usize) -> Self {
        Self::CallableParamType { variable, index }
    }

    pub fn callable_return_type(variable: TypeVariable) -> Self {
        Self::CallableReturnType { variable }
    }

    pub fn member_type(variable: TypeVariable, member: Symbol) -> Self {
        Self::MemberType { variable, member }
    }
}

pub fn build_type_constraint_table(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    hir: &HIRProgram,
) -> TypeConstraintTable {
    let mut table = TypeConstraintTable::new();

    for function in &hir.functions {
        build_type_constraint_table_stmt_block(
            &mut table,
            top_level_table,
            reference_table,
            function,
            &function.body_block,
        );
    }

    table
}

fn build_type_constraint_table_stmt_block(
    constraint_table: &mut TypeConstraintTable,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    function: &HIRFunction,
    hir: &HIRBlock,
) {
    for statement in &hir.statements {
        match &statement.kind {
            HIRStatementKind::Block(hir) => {
                build_type_constraint_table_stmt_block(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    hir,
                );
            }
            HIRStatementKind::Let(hir) => {
                let type_var = constraint_table.new_variable(statement.id);

                if let Some(let_type) = &hir.let_type {
                    constraint_table.new_constraint(TypeConstraint::equal(
                        type_var,
                        TypeConstraintTarget::concrete(let_type.type_ref.kind.clone()),
                    ));
                }

                if let Some(let_assignment) = &hir.let_assignment {
                    let expr_type_var = build_type_constraint_table_expression(
                        constraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &let_assignment.expression,
                    );
                    constraint_table.new_constraint(TypeConstraint::subtype(
                        expr_type_var,
                        TypeConstraintTarget::variable(type_var),
                    ));
                }
            }
            HIRStatementKind::If(hir) => {
                let expr_type_var = build_type_constraint_table_expression(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &hir.expression,
                );
                constraint_table.new_constraint(TypeConstraint::subtype(
                    expr_type_var,
                    TypeConstraintTarget::concrete(TypeKind::bool()),
                ));

                for hir in &hir.single_else_ifs {
                    let expr_type_var = build_type_constraint_table_expression(
                        constraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &hir.expression,
                    );
                    constraint_table.new_constraint(TypeConstraint::subtype(
                        expr_type_var,
                        TypeConstraintTarget::concrete(TypeKind::bool()),
                    ));

                    build_type_constraint_table_stmt_block(
                        constraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &hir.body_block,
                    );
                }

                if let Some(hir) = &hir.single_else {
                    build_type_constraint_table_stmt_block(
                        constraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &hir.body_block,
                    );
                }
            }
            HIRStatementKind::Loop(hir) => {
                build_type_constraint_table_stmt_block(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &hir.body_block,
                );
            }
            HIRStatementKind::Break(_) => {}
            HIRStatementKind::Continue(_) => {}
            HIRStatementKind::Return(hir) => {
                if let Some(hir) = &hir.expression {
                    let expr_type_var = build_type_constraint_table_expression(
                        constraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &hir,
                    );
                    constraint_table.new_constraint(TypeConstraint::subtype(
                        expr_type_var,
                        TypeConstraintTarget::concrete(
                            function
                                .signature
                                .return_type
                                .as_ref()
                                .map(|return_type| return_type.type_ref.kind.clone())
                                .unwrap_or_else(|| TypeKind::empty()),
                        ),
                    ));
                }
            }
            HIRStatementKind::Assignment(hir) => {
                let left_expr_type_var = build_type_constraint_table_expression(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &hir.left,
                );
                let right_expr_type_var = build_type_constraint_table_expression(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &hir.right,
                );
                constraint_table.new_constraint(TypeConstraint::subtype(
                    right_expr_type_var,
                    TypeConstraintTarget::variable(left_expr_type_var),
                ));
            }
            HIRStatementKind::Row(hir) => {
                build_type_constraint_table_expression(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &hir.expression,
                );
            }
        }
    }
}

fn build_type_constraint_table_expression(
    constraint_table: &mut TypeConstraintTable,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    function: &HIRFunction,
    hir: &HIRExpression,
) -> TypeVariable {
    let type_var = constraint_table.new_variable(hir.id);

    match &hir.kind {
        HIRExpressionKind::Binary(hir) => {
            let left_expr_type_var = build_type_constraint_table_expression(
                constraint_table,
                top_level_table,
                reference_table,
                function,
                &hir.left,
            );
            let right_expr_type_var = build_type_constraint_table_expression(
                constraint_table,
                top_level_table,
                reference_table,
                function,
                &hir.right,
            );
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::binary_operation(
                    hir.operator_kind,
                    left_expr_type_var,
                    right_expr_type_var,
                ),
            ));
        }
        HIRExpressionKind::Unary(hir) => {
            let right_expr_type_var = build_type_constraint_table_expression(
                constraint_table,
                top_level_table,
                reference_table,
                function,
                &hir.right,
            );
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::unary_operation(hir.operator_kind, right_expr_type_var),
            ));
        }
        HIRExpressionKind::As(hir) => {
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::concrete(hir.type_ref.kind.clone()),
            ));

            build_type_constraint_table_expression(
                constraint_table,
                top_level_table,
                reference_table,
                function,
                &hir.expression,
            );
        }
        HIRExpressionKind::Call(hir) => {
            let expr_type_var = build_type_constraint_table_expression(
                constraint_table,
                top_level_table,
                reference_table,
                function,
                &hir.expression,
            );
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::callable_return_type(expr_type_var),
            ));

            for (index, arg) in hir.args.iter().enumerate() {
                let arg_type_var = build_type_constraint_table_expression(
                    constraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &arg,
                );
                constraint_table.new_constraint(TypeConstraint::equal(
                    arg_type_var,
                    TypeConstraintTarget::callable_param_type(expr_type_var, index),
                ));
            }
        }
        HIRExpressionKind::Member(hir) => {
            let expr_type_var = build_type_constraint_table_expression(
                constraint_table,
                top_level_table,
                reference_table,
                function,
                &hir.expression,
            );
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::member_type(expr_type_var, hir.member.symbol),
            ));
        }
        HIRExpressionKind::FunctionRef(hir) => {
            let function = &top_level_table.functions[&hir.function];
            let params = function.params.iter().cloned().collect();
            let return_type = function.return_type.clone();
            let type_kind = TypeKind::callable(params, return_type);
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::concrete(type_kind),
            ));
        }
        HIRExpressionKind::ParamRef(hir) => {
            let function = &top_level_table.functions[&hir.function];
            let param = function.params[hir.index].clone();
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::concrete(param),
            ));
        }
        HIRExpressionKind::VariableRef(hir) => {
            let variable_type_var = constraint_table.type_variables[&hir.statement];
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::variable(variable_type_var),
            ));
        }
        HIRExpressionKind::UnknownRef(..) => {}
        HIRExpressionKind::StructLiteral(hir) => {
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::concrete(hir.type_ref.kind.clone()),
            ));

            if let TypeKind::UserStruct { id } = &hir.type_ref.kind {
                let user_struct = top_level_table.user_types[id].as_user_struct().unwrap();
                for field in &hir.fields {
                    let field_type_var = build_type_constraint_table_expression(
                        constraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &field.expression,
                    );

                    if let Some(index) = user_struct.field_names.get(&field.name.symbol) {
                        constraint_table.new_constraint(TypeConstraint::subtype(
                            field_type_var,
                            TypeConstraintTarget::concrete(user_struct.fields[*index].clone()),
                        ));
                    }
                }
            }
        }
        HIRExpressionKind::Literal(literal) => {
            let type_kind = match literal.kind {
                TokenLiteralKind::Bool => TypeKind::bool(),
                TokenLiteralKind::IntegerBinary => TypeKind::int(),
                TokenLiteralKind::IntegerOctal => TypeKind::int(),
                TokenLiteralKind::IntegerHexadecimal => TypeKind::int(),
                TokenLiteralKind::IntegerDecimal => TypeKind::int(),
                TokenLiteralKind::Float => TypeKind::float(),
                TokenLiteralKind::Character { .. } => TypeKind::int(),
                TokenLiteralKind::String { .. } => TypeKind::string(),
            };
            constraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::concrete(type_kind),
            ));
        }
    }

    type_var
}
