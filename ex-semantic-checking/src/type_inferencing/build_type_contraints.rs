use crate::resolve::{Function, ReferenceTable, SymbolReferenceKind, TopLevelTable, TypeKind};
use ex_parser::{
    ASTBinaryOperatorKind, ASTBlock, ASTExpression, ASTExpressionKind, ASTProgram,
    ASTStatementKind, ASTTopLevelKind, ASTUnaryOperatorKind, NodeId, TokenLiteralKind,
};
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
pub type TypeContraints = Vec<TypeConstraint>;

#[derive(Default, Debug, Clone)]
pub struct TypeConstraintTable {
    pub type_variables: TypeVariableTable,
    pub reverse_type_variables: ReverseTypeVariableTable,
    pub type_contraints: TypeContraints,
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
        self.type_contraints.push(constraint);
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
        operator: ASTBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    },
    UnaryOperation {
        operator: ASTUnaryOperatorKind,
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

    pub fn callable_param_type(variable: TypeVariable, index: usize) -> Self {
        Self::CallableParamType { variable, index }
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

pub fn build_type_contraint_table(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    ast: &ASTProgram,
) -> TypeConstraintTable {
    let mut table = TypeConstraintTable::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let function = top_level_table.functions.get(&top_level.id).unwrap();
                build_type_contraint_table_stmt_block(
                    &mut table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.body_block,
                );
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    table
}

fn build_type_contraint_table_stmt_block(
    contraint_table: &mut TypeConstraintTable,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    function: &Function,
    ast: &ASTBlock,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                build_type_contraint_table_stmt_block(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    ast,
                );
            }
            ASTStatementKind::Let(ast) => {
                let type_var = contraint_table.new_variable(statement.id);

                if let Some(let_type) = &ast.let_type {
                    if let Some(type_ref) =
                        reference_table.type_references.get(&let_type.typename.id)
                    {
                        contraint_table.new_constraint(TypeConstraint::equal(
                            type_var,
                            TypeConstraintTarget::concrete(type_ref.kind.clone()),
                        ));
                    }
                }

                if let Some(let_assignment) = &ast.let_assignment {
                    let expr_type_var = build_type_contraint_table_expression(
                        contraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &let_assignment.expression,
                    );
                    contraint_table.new_constraint(TypeConstraint::subtype(
                        expr_type_var,
                        TypeConstraintTarget::variable(type_var),
                    ));
                }
            }
            ASTStatementKind::If(ast) => {
                let expr_type_var = build_type_contraint_table_expression(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.expression,
                );
                contraint_table.new_constraint(TypeConstraint::subtype(
                    expr_type_var,
                    TypeConstraintTarget::concrete(TypeKind::bool()),
                ));

                build_type_contraint_table_stmt_block(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.body_block,
                );

                for ast in &ast.single_else_ifs {
                    let expr_type_var = build_type_contraint_table_expression(
                        contraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &ast.expression,
                    );
                    contraint_table.new_constraint(TypeConstraint::subtype(
                        expr_type_var,
                        TypeConstraintTarget::concrete(TypeKind::bool()),
                    ));

                    build_type_contraint_table_stmt_block(
                        contraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &ast.body_block,
                    );
                }

                if let Some(ast) = &ast.single_else {
                    build_type_contraint_table_stmt_block(
                        contraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &ast.body_block,
                    );
                }
            }
            ASTStatementKind::Loop(ast) => {
                build_type_contraint_table_stmt_block(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.body_block,
                );
            }
            ASTStatementKind::While(ast) => {
                let expr_type_var = build_type_contraint_table_expression(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.expression,
                );
                contraint_table.new_constraint(TypeConstraint::subtype(
                    expr_type_var,
                    TypeConstraintTarget::concrete(TypeKind::bool()),
                ));

                build_type_contraint_table_stmt_block(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.body_block,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(ast) => {
                if let Some(ast) = &ast.expression {
                    let expr_type_var = build_type_contraint_table_expression(
                        contraint_table,
                        top_level_table,
                        reference_table,
                        function,
                        &ast,
                    );
                    contraint_table.new_constraint(TypeConstraint::subtype(
                        expr_type_var,
                        TypeConstraintTarget::concrete(function.return_type.clone()),
                    ));
                }
            }
            ASTStatementKind::Assignment(ast) => {
                let left_expr_type_var = build_type_contraint_table_expression(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.left,
                );
                let right_expr_type_var = build_type_contraint_table_expression(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.right,
                );
                contraint_table.new_constraint(TypeConstraint::subtype(
                    right_expr_type_var,
                    TypeConstraintTarget::variable(left_expr_type_var),
                ));
            }
            ASTStatementKind::Row(ast) => {
                build_type_contraint_table_expression(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &ast.expression,
                );
            }
        }
    }
}

fn build_type_contraint_table_expression(
    contraint_table: &mut TypeConstraintTable,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    function: &Function,
    ast: &ASTExpression,
) -> TypeVariable {
    let type_var = contraint_table.new_variable(ast.id);

    match &ast.kind {
        ASTExpressionKind::Binary(ast) => {
            let left_expr_type_var = build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.left,
            );
            let right_expr_type_var = build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.right,
            );
            contraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::binary_operation(
                    ast.operator_kind,
                    left_expr_type_var,
                    right_expr_type_var,
                ),
            ));
        }
        ASTExpressionKind::Unary(ast) => {
            let right_expr_var = build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.right,
            );

            let target = match ast.operator_kind {
                ASTUnaryOperatorKind::Plus
                | ASTUnaryOperatorKind::Minus
                | ASTUnaryOperatorKind::BitNot
                | ASTUnaryOperatorKind::LogNot => {
                    TypeConstraintTarget::unary_operation(ast.operator_kind, right_expr_var)
                }
                ASTUnaryOperatorKind::AddressOf => {
                    TypeConstraintTarget::address_of_type(right_expr_var)
                }
                ASTUnaryOperatorKind::Dereference => {
                    TypeConstraintTarget::dereference_type(right_expr_var)
                }
            };

            contraint_table.new_constraint(TypeConstraint::equal(type_var, target));
        }
        ASTExpressionKind::As(ast) => {
            if let Some(type_ref) = reference_table.type_references.get(&ast.typename.id) {
                contraint_table.new_constraint(TypeConstraint::subtype(
                    type_var,
                    TypeConstraintTarget::concrete(type_ref.kind.clone()),
                ));
            }

            build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.expression,
            );
        }
        ASTExpressionKind::Call(ast) => {
            let callee_type_var = build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.expression,
            );
            contraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::callable_return_type(callee_type_var),
            ));

            for (index, arg) in ast.args.iter().enumerate() {
                let arg_type_var = build_type_contraint_table_expression(
                    contraint_table,
                    top_level_table,
                    reference_table,
                    function,
                    &arg.expression,
                );
                contraint_table.new_constraint(TypeConstraint::subtype(
                    arg_type_var,
                    TypeConstraintTarget::callable_param_type(callee_type_var, index),
                ));
            }
        }
        ASTExpressionKind::Member(ast) => {
            let base_type_var = build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.expression,
            );
            contraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::member_type(base_type_var, ast.member.symbol),
            ));
        }
        ASTExpressionKind::Paren(ast) => {
            let expr_type_var = build_type_contraint_table_expression(
                contraint_table,
                top_level_table,
                reference_table,
                function,
                &ast.expression,
            );
            contraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::variable(expr_type_var),
            ));
        }
        ASTExpressionKind::Literal(ast) => {
            let type_kind = match ast.literal.kind {
                TokenLiteralKind::Bool => TypeKind::bool(),
                TokenLiteralKind::IntegerBinary => TypeKind::int(),
                TokenLiteralKind::IntegerOctal => TypeKind::int(),
                TokenLiteralKind::IntegerHexadecimal => TypeKind::int(),
                TokenLiteralKind::IntegerDecimal => TypeKind::int(),
                TokenLiteralKind::Float => TypeKind::float(),
                TokenLiteralKind::Character { .. } => TypeKind::int(),
                TokenLiteralKind::String { .. } => TypeKind::string(),
            };
            contraint_table.new_constraint(TypeConstraint::equal(
                type_var,
                TypeConstraintTarget::concrete(type_kind),
            ));
        }
        ASTExpressionKind::IdReference(ast) => {
            if let Some(symbol_ref) = reference_table.symbol_references.get(&ast.id) {
                match &symbol_ref.kind {
                    SymbolReferenceKind::Function { function } => {
                        let function = top_level_table.functions.get(function).unwrap();
                        let params = function.params.iter().cloned().collect();
                        let return_type = function.return_type.clone();
                        let type_kind = TypeKind::callable(params, return_type);
                        contraint_table.new_constraint(TypeConstraint::equal(
                            type_var,
                            TypeConstraintTarget::concrete(type_kind),
                        ));
                    }
                    SymbolReferenceKind::Param { function, index } => {
                        let function = top_level_table.functions.get(function).unwrap();
                        let param = function.params[*index].clone();
                        contraint_table.new_constraint(TypeConstraint::equal(
                            type_var,
                            TypeConstraintTarget::concrete(param),
                        ));
                    }
                    SymbolReferenceKind::Variable {
                        function,
                        scope,
                        index,
                    } => {
                        let function_scope = &reference_table.function_scopes[function];
                        let scope = &function_scope.scope_table.scopes[scope];
                        let variable = &scope.variables[*index];
                        let variable_type_var = contraint_table.type_variables[&variable.id];
                        contraint_table.new_constraint(TypeConstraint::equal(
                            type_var,
                            TypeConstraintTarget::variable(variable_type_var),
                        ));
                    }
                }
            }
        }
        ASTExpressionKind::StructLiteral(ast) => {
            if let Some(type_ref) = reference_table.type_references.get(&ast.typename.id) {
                contraint_table.new_constraint(TypeConstraint::equal(
                    type_var,
                    TypeConstraintTarget::concrete(type_ref.kind.clone()),
                ));

                if let TypeKind::UserStruct { id } = &type_ref.kind {
                    let user_struct = top_level_table.user_types[id].as_user_struct().unwrap();
                    for field in &ast.fields {
                        let field_type_var = build_type_contraint_table_expression(
                            contraint_table,
                            top_level_table,
                            reference_table,
                            function,
                            &field.expression,
                        );

                        if let Some(index) = user_struct.field_names.get(&field.name.symbol) {
                            contraint_table.new_constraint(TypeConstraint::subtype(
                                field_type_var,
                                TypeConstraintTarget::concrete(user_struct.fields[*index].clone()),
                            ));
                        }
                    }
                }
            }
        }
    }

    type_var
}
