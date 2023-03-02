mod builtin_operators;
mod type_table;
mod type_table_builder;

pub use builtin_operators::*;
pub use type_table::*;
pub use type_table_builder::*;

use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin, SubDiagnostics};
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTProgram, ASTStatementKind,
    ASTTopLevelKind, TokenLiteralKind,
};
use ex_resolve_ref::{
    FunctionTable, SymbolNodeKind, SymbolReferenceTable, TypeKind, TypeReferenceTable,
};
use ex_span::SourceFile;
use std::sync::{mpsc::Sender, Arc};

pub fn check_types(
    type_table: &TypeTable,
    type_reference_table: &TypeReferenceTable,
    ast: &ASTProgram,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                check_types_stmt_block(
                    type_table,
                    type_reference_table,
                    ast,
                    &ast.body_block,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

pub fn check_types_stmt_block(
    type_table: &TypeTable,
    type_reference_table: &TypeReferenceTable,
    ast_function: &ASTFunction,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                check_types_stmt_block(
                    type_table,
                    type_reference_table,
                    ast_function,
                    &stmt_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(stmt_let) => {
                let variable_type_kind = match type_table.types.get(&statement.id) {
                    Some(type_kind) => type_kind,
                    None => {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!("cannot deduce type"),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: stmt_let.span,
                                }),
                                sub_diagnostics: vec![SubDiagnostics {
                                    level: DiagnosticsLevel::Hint,
                                    message: format!("consider adding a type annotation"),
                                    origin: None,
                                }],
                            })
                            .unwrap();
                        continue;
                    }
                };

                if let Some(let_assignment) = &stmt_let.let_assignment {
                    check_types_expression(
                        type_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );

                    if let Some(expression_type_kind) =
                        type_table.types.get(&let_assignment.expression.id)
                    {
                        if variable_type_kind != expression_type_kind {
                            diagnostics
                                .send(Diagnostics {
                                    level: DiagnosticsLevel::Error,
                                    message: format!(
                                        "expected type `{}`, found `{}`",
                                        variable_type_kind, expression_type_kind
                                    ),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: let_assignment.span,
                                    }),
                                    sub_diagnostics: vec![],
                                })
                                .unwrap();
                        }
                    }
                }
            }
            ASTStatementKind::If(stmt_if) => {
                check_types_expression(type_table, &stmt_if.expression, file, diagnostics);

                if let Some(expression_type_kind) = type_table.types.get(&stmt_if.expression.id) {
                    if !expression_type_kind.is_boolean() {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!(
                                    "expected type `{}`, found `{}`",
                                    TypeKind::boolean(),
                                    expression_type_kind
                                ),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: stmt_if.expression.span,
                                }),
                                sub_diagnostics: vec![],
                            })
                            .unwrap();
                    }
                }

                check_types_stmt_block(
                    type_table,
                    type_reference_table,
                    ast_function,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    check_types_expression(
                        type_table,
                        &single_else_if.expression,
                        file,
                        diagnostics,
                    );

                    if let Some(expression_type_kind) =
                        type_table.types.get(&single_else_if.expression.id)
                    {
                        if !expression_type_kind.is_boolean() {
                            diagnostics
                                .send(Diagnostics {
                                    level: DiagnosticsLevel::Error,
                                    message: format!(
                                        "expected type `{}`, found `{}`",
                                        TypeKind::boolean(),
                                        expression_type_kind
                                    ),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: single_else_if.expression.span,
                                    }),
                                    sub_diagnostics: vec![],
                                })
                                .unwrap();
                        }
                    }

                    check_types_stmt_block(
                        type_table,
                        type_reference_table,
                        ast_function,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    check_types_stmt_block(
                        type_table,
                        type_reference_table,
                        ast_function,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expression) = &stmt_return.expression {
                    check_types_expression(type_table, &expression, file, diagnostics);
                }

                let function_return_type_reference = ast_function
                    .signature
                    .return_type
                    .as_ref()
                    .and_then(|return_type| {
                        type_reference_table
                            .references
                            .get(&return_type.typename.id)
                    });

                match (function_return_type_reference, &stmt_return.expression) {
                    (Some(function_return_type_reference), Some(expression)) => {
                        if let Some(expression_type_kind) = type_table.types.get(&expression.id) {
                            if !function_return_type_reference.kind.eq(expression_type_kind) {
                                diagnostics
                                    .send(Diagnostics {
                                        level: DiagnosticsLevel::Error,
                                        message: format!(
                                            "expected type `{}`, found `{}`",
                                            function_return_type_reference.kind,
                                            expression_type_kind
                                        ),
                                        origin: Some(DiagnosticsOrigin {
                                            file: file.clone(),
                                            span: expression.span,
                                        }),
                                        sub_diagnostics: vec![SubDiagnostics {
                                            level: DiagnosticsLevel::Hint,
                                            message: format!(
                                                "the function returns a value of type `{}`",
                                                function_return_type_reference.kind
                                            ),
                                            origin: Some(DiagnosticsOrigin {
                                                file: file.clone(),
                                                span: function_return_type_reference.span,
                                            }),
                                        }],
                                    })
                                    .unwrap();
                            }
                        }
                    }
                    (Some(function_return_type_reference), None) => {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!("this return statement must have a value"),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: statement.span,
                                }),
                                sub_diagnostics: vec![SubDiagnostics {
                                    level: DiagnosticsLevel::Hint,
                                    message: format!("the function returns a value"),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: function_return_type_reference.span,
                                    }),
                                }],
                            })
                            .unwrap();
                    }
                    (None, Some(_)) => {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!("this return statement must not have a value"),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: statement.span,
                                }),
                                sub_diagnostics: vec![SubDiagnostics {
                                    level: DiagnosticsLevel::Hint,
                                    message: format!("the function does not return a value"),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: ast_function.signature.span,
                                    }),
                                }],
                            })
                            .unwrap();
                    }
                    _ => {}
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                check_types_expression(
                    type_table,
                    &stmt_assignment.left.expression,
                    file,
                    diagnostics,
                );
                check_types_expression(type_table, &stmt_assignment.right, file, diagnostics);

                match (
                    type_table.types.get(&stmt_assignment.left.expression.id),
                    type_table.types.get(&stmt_assignment.right.id),
                ) {
                    (Some(left_type_kind), Some(right_type_kind)) => {
                        if !left_type_kind.eq(right_type_kind) {
                            diagnostics
                                .send(Diagnostics {
                                    level: DiagnosticsLevel::Error,
                                    message: format!(
                                        "expected type `{}`, found `{}`",
                                        left_type_kind, right_type_kind
                                    ),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: stmt_assignment.right.span,
                                    }),
                                    sub_diagnostics: vec![],
                                })
                                .unwrap();
                        }
                    }
                    _ => {}
                }
            }
            ASTStatementKind::Row(stmt_row) => {
                check_types_expression(type_table, &stmt_row.expression, file, diagnostics);
            }
        }
    }
}

pub fn check_types_expression(
    type_table: &TypeTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            check_types_expression(type_table, &expr_binary.left, file, diagnostics);
            check_types_expression(type_table, &expr_binary.right, file, diagnostics);
        }
        ASTExpressionKind::Unary(expr_unary) => {
            check_types_expression(type_table, &expr_unary.right, file, diagnostics);
        }
        ASTExpressionKind::As(expr_as) => {
            check_types_expression(type_table, &expr_as.expression, file, diagnostics);
        }
        ASTExpressionKind::Call(expr_call) => {
            for argument in &expr_call.arguments {
                check_types_expression(type_table, &argument.expression, file, diagnostics);
            }

            if let Some(callee_type_kind) = type_table.types.get(&expr_call.expression.id) {
                match callee_type_kind {
                    TypeKind::Callable { parameters, .. } => {
                        if parameters.len() != expr_call.arguments.len() {
                            diagnostics
                                .send(Diagnostics {
                                    level: DiagnosticsLevel::Error,
                                    message: format!(
                                        "expected {} arguments, found {}",
                                        parameters.len(),
                                        expr_call.arguments.len()
                                    ),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: expr_call.expression.span,
                                    }),
                                    sub_diagnostics: vec![],
                                })
                                .unwrap();
                        } else {
                            for (parameter, argument) in
                                parameters.iter().zip(expr_call.arguments.iter())
                            {
                                if let Some(argument_type_kind) =
                                    type_table.types.get(&argument.expression.id)
                                {
                                    if !parameter.eq(argument_type_kind) {
                                        diagnostics
                                            .send(Diagnostics {
                                                level: DiagnosticsLevel::Error,
                                                message: format!(
                                                    "expected type `{}`, found `{}`",
                                                    parameter, argument_type_kind
                                                ),
                                                origin: Some(DiagnosticsOrigin {
                                                    file: file.clone(),
                                                    span: argument.expression.span,
                                                }),
                                                sub_diagnostics: vec![],
                                            })
                                            .unwrap();
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!(
                                    "expected a callable type, found `{}`",
                                    callee_type_kind
                                ),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: expr_call.expression.span,
                                }),
                                sub_diagnostics: vec![],
                            })
                            .unwrap();
                    }
                }
            }
        }
        ASTExpressionKind::Paren(expr_paren) => {
            check_types_expression(type_table, &expr_paren.expression, file, diagnostics);
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(..) => {}
    }

    if !type_table.types.contains_key(&ast.id) {
        diagnostics
            .send(Diagnostics {
                level: DiagnosticsLevel::Error,
                message: format!("could not resolve type of expression"),
                origin: Some(DiagnosticsOrigin {
                    file: file.clone(),
                    span: ast.span,
                }),
                sub_diagnostics: vec![],
            })
            .unwrap();
    }
}

pub fn propagate_type_variables(
    function_table: &FunctionTable,
    symbol_reference_table: &SymbolReferenceTable,
    type_reference_table: &TypeReferenceTable,
    ast: &ASTProgram,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> TypeTableBuilder {
    let mut type_table_builder = TypeTableBuilder::new();

    for top_levels in &ast.top_levels {
        match &top_levels.kind {
            ASTTopLevelKind::Function(ast) => {
                propagate_type_variables_stmt_block(
                    &mut type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    ast,
                    &ast.body_block,
                    file,
                    diagnostics,
                );
            }
        }
    }

    type_table_builder
}

fn propagate_type_variables_stmt_block(
    type_table_builder: &mut TypeTableBuilder,
    function_table: &FunctionTable,
    symbol_reference_table: &SymbolReferenceTable,
    type_reference_table: &TypeReferenceTable,
    ast_function: &ASTFunction,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                propagate_type_variables_stmt_block(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    ast_function,
                    stmt_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(stmt_let) => {
                let statement_type_var = type_table_builder.new_variable(statement.id);

                if let Some(let_type) = &stmt_let.let_type {
                    if let Some(type_kind) = type_reference_table
                        .references
                        .get(&let_type.typename.id)
                        .map(|type_reference| type_reference.kind.clone())
                    {
                        type_table_builder.variable_constraints.push(
                            TypeVariableConstraint::equal(
                                statement_type_var,
                                TypeVariableConstraintOperand::concrete(type_kind),
                            ),
                        );
                    }
                }

                if let Some(let_assignment) = &stmt_let.let_assignment {
                    let expression_type_var = propagate_type_variables_expression(
                        type_table_builder,
                        function_table,
                        symbol_reference_table,
                        type_reference_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );
                    type_table_builder
                        .variable_constraints
                        .push(TypeVariableConstraint::subtype(
                            expression_type_var,
                            TypeVariableConstraintOperand::variable(statement_type_var),
                        ));
                }
            }
            ASTStatementKind::If(stmt_if) => {
                let type_var = propagate_type_variables_expression(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    &stmt_if.expression,
                    file,
                    diagnostics,
                );
                type_table_builder
                    .variable_constraints
                    .push(TypeVariableConstraint::subtype(
                        type_var,
                        TypeVariableConstraintOperand::concrete(TypeKind::boolean()),
                    ));

                propagate_type_variables_stmt_block(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    ast_function,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    let type_var = propagate_type_variables_expression(
                        type_table_builder,
                        function_table,
                        symbol_reference_table,
                        type_reference_table,
                        &single_else_if.expression,
                        file,
                        diagnostics,
                    );
                    type_table_builder
                        .variable_constraints
                        .push(TypeVariableConstraint::subtype(
                            type_var,
                            TypeVariableConstraintOperand::concrete(TypeKind::boolean()),
                        ));

                    propagate_type_variables_stmt_block(
                        type_table_builder,
                        function_table,
                        symbol_reference_table,
                        type_reference_table,
                        ast_function,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    propagate_type_variables_stmt_block(
                        type_table_builder,
                        function_table,
                        symbol_reference_table,
                        type_reference_table,
                        ast_function,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expression) = &stmt_return.expression {
                    let type_var = propagate_type_variables_expression(
                        type_table_builder,
                        function_table,
                        symbol_reference_table,
                        type_reference_table,
                        expression,
                        file,
                        diagnostics,
                    );

                    let function_return_type_reference = ast_function
                        .signature
                        .return_type
                        .as_ref()
                        .and_then(|return_type| {
                            type_reference_table
                                .references
                                .get(&return_type.typename.id)
                        });

                    let function_return_type_kind = match function_return_type_reference {
                        Some(function_return_type_reference) => {
                            function_return_type_reference.kind.clone()
                        }
                        None => TypeKind::empty(),
                    };

                    type_table_builder
                        .variable_constraints
                        .push(TypeVariableConstraint::subtype(
                            type_var,
                            TypeVariableConstraintOperand::concrete(function_return_type_kind),
                        ));
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                let left_type_var = propagate_type_variables_expression(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    &stmt_assignment.left.expression,
                    file,
                    diagnostics,
                );
                let right_type_var = propagate_type_variables_expression(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    &stmt_assignment.right,
                    file,
                    diagnostics,
                );
                type_table_builder
                    .variable_constraints
                    .push(TypeVariableConstraint::subtype(
                        right_type_var,
                        TypeVariableConstraintOperand::variable(left_type_var),
                    ));
            }
            ASTStatementKind::Row(stmt_row) => {
                propagate_type_variables_expression(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    &stmt_row.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

fn propagate_type_variables_expression(
    type_table_builder: &mut TypeTableBuilder,
    function_table: &FunctionTable,
    symbol_reference_table: &SymbolReferenceTable,
    type_reference_table: &TypeReferenceTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> TypeVariable {
    let type_var = type_table_builder.new_variable(ast.id);

    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            let left_type_var = propagate_type_variables_expression(
                type_table_builder,
                function_table,
                symbol_reference_table,
                type_reference_table,
                &expr_binary.left,
                file,
                diagnostics,
            );
            let right_type_var = propagate_type_variables_expression(
                type_table_builder,
                function_table,
                symbol_reference_table,
                type_reference_table,
                &expr_binary.right,
                file,
                diagnostics,
            );
            type_table_builder
                .variable_constraints
                .push(TypeVariableConstraint::equal(
                    type_var,
                    TypeVariableConstraintOperand::binary_operation(
                        expr_binary.operator_kind,
                        left_type_var,
                        right_type_var,
                    ),
                ));
        }
        ASTExpressionKind::Unary(expr_unary) => {
            let right_type_var = propagate_type_variables_expression(
                type_table_builder,
                function_table,
                symbol_reference_table,
                type_reference_table,
                &expr_unary.right,
                file,
                diagnostics,
            );
            type_table_builder
                .variable_constraints
                .push(TypeVariableConstraint::equal(
                    type_var,
                    TypeVariableConstraintOperand::unary_operation(
                        expr_unary.operator_kind,
                        right_type_var,
                    ),
                ));
        }
        ASTExpressionKind::As(expr_as) => {
            if let Some(type_reference) = type_reference_table.references.get(&expr_as.typename.id)
            {
                type_table_builder
                    .variable_constraints
                    .push(TypeVariableConstraint::equal(
                        type_var,
                        TypeVariableConstraintOperand::concrete(type_reference.kind.clone()),
                    ));
            }

            propagate_type_variables_expression(
                type_table_builder,
                function_table,
                symbol_reference_table,
                type_reference_table,
                &expr_as.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(expr_call) => {
            let callee_type_var = propagate_type_variables_expression(
                type_table_builder,
                function_table,
                symbol_reference_table,
                type_reference_table,
                &expr_call.expression,
                file,
                diagnostics,
            );
            type_table_builder
                .variable_constraints
                .push(TypeVariableConstraint::equal(
                    type_var,
                    TypeVariableConstraintOperand::callable_return_type(callee_type_var),
                ));

            for (index, argument) in expr_call.arguments.iter().enumerate() {
                let argument_type_var = propagate_type_variables_expression(
                    type_table_builder,
                    function_table,
                    symbol_reference_table,
                    type_reference_table,
                    &argument.expression,
                    file,
                    diagnostics,
                );
                type_table_builder
                    .variable_constraints
                    .push(TypeVariableConstraint::subtype(
                        argument_type_var,
                        TypeVariableConstraintOperand::callable_parameter_type(
                            callee_type_var,
                            index,
                        ),
                    ));
            }
        }
        ASTExpressionKind::Paren(expr_paren) => {
            let expression_type_var = propagate_type_variables_expression(
                type_table_builder,
                function_table,
                symbol_reference_table,
                type_reference_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
            type_table_builder
                .variable_constraints
                .push(TypeVariableConstraint::equal(
                    type_var,
                    TypeVariableConstraintOperand::variable(expression_type_var),
                ));
        }
        ASTExpressionKind::Literal(expr_literal) => {
            let type_kind = match expr_literal.literal.kind {
                TokenLiteralKind::Bool => TypeKind::boolean(),
                TokenLiteralKind::IntegerBinary => TypeKind::integer(),
                TokenLiteralKind::IntegerOctal => TypeKind::integer(),
                TokenLiteralKind::IntegerHexadecimal => TypeKind::integer(),
                TokenLiteralKind::IntegerDecimal => TypeKind::integer(),
                TokenLiteralKind::Float => TypeKind::float(),
                TokenLiteralKind::Character { .. } => TypeKind::integer(),
                TokenLiteralKind::String { .. } => TypeKind::string(),
            };
            type_table_builder
                .variable_constraints
                .push(TypeVariableConstraint::equal(
                    type_var,
                    TypeVariableConstraintOperand::concrete(type_kind),
                ));
        }
        ASTExpressionKind::IdReference(expr_id_ref) => {
            if let Some(symbol_reference) = symbol_reference_table.references.get(&expr_id_ref.id) {
                match symbol_reference.kind {
                    SymbolNodeKind::Function => {
                        let function = function_table
                            .functions
                            .get(&symbol_reference.node)
                            .unwrap();
                        let arguments = function
                            .parameter_typenames
                            .iter()
                            .map(|parameter| {
                                if let Some(type_reference) =
                                    type_reference_table.references.get(&parameter.id)
                                {
                                    type_reference.kind.clone()
                                } else {
                                    TypeKind::unknown()
                                }
                            })
                            .collect();
                        let return_type = if let Some(return_typename) = &function.return_typename {
                            if let Some(type_reference) =
                                type_reference_table.references.get(&return_typename.id)
                            {
                                type_reference.kind.clone()
                            } else {
                                TypeKind::unknown()
                            }
                        } else {
                            TypeKind::empty()
                        };
                        let type_kind = TypeKind::callable(arguments, return_type);
                        type_table_builder.variable_constraints.push(
                            TypeVariableConstraint::equal(
                                type_var,
                                TypeVariableConstraintOperand::concrete(type_kind),
                            ),
                        );
                    }
                    SymbolNodeKind::Parameter { index } => {
                        let function = function_table
                            .functions
                            .get(&symbol_reference.node)
                            .unwrap();

                        if let Some(type_reference) = type_reference_table
                            .references
                            .get(&function.parameter_typenames[index].id)
                        {
                            type_table_builder.variable_constraints.push(
                                TypeVariableConstraint::equal(
                                    type_var,
                                    TypeVariableConstraintOperand::concrete(
                                        type_reference.kind.clone(),
                                    ),
                                ),
                            );
                        }
                    }
                    SymbolNodeKind::Variable { .. } => {
                        let symbol_type_var = type_table_builder
                            .variables
                            .get(&symbol_reference.node)
                            .cloned()
                            .unwrap();
                        type_table_builder.variable_constraints.push(
                            TypeVariableConstraint::equal(
                                type_var,
                                TypeVariableConstraintOperand::variable(symbol_type_var),
                            ),
                        );
                    }
                };
            };
        }
    }

    type_var
}
