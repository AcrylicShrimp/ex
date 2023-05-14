use crate::{
    hir::{HIRBlock, HIRExpression, HIRExpressionKind, HIRFunction, HIRProgram, HIRStatementKind},
    resolve::{TopLevelTable, TypeKind},
    type_inferencing::TypeTable,
};
use ex_diagnostics::DiagnosticsSender;
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::{hash_map::Entry, HashMap};

pub fn check_types(
    type_table: &TypeTable,
    top_level_table: &TopLevelTable,
    hir: &HIRProgram,
    diagnostics: &DiagnosticsSender,
) {
    for function in &hir.functions {
        check_types_stmt_block(
            type_table,
            top_level_table,
            function,
            &function.body_block,
            diagnostics,
        );
    }
}

pub fn check_types_stmt_block(
    type_table: &TypeTable,
    top_level_table: &TopLevelTable,
    function: &HIRFunction,
    hir: &HIRBlock,
    diagnostics: &DiagnosticsSender,
) {
    for statement in &hir.statements {
        match &statement.kind {
            HIRStatementKind::Block(hir) => {
                check_types_stmt_block(type_table, top_level_table, function, hir, diagnostics);
            }
            HIRStatementKind::Let(hir) => {
                if let Some(let_assignment) = &hir.let_assignment {
                    check_types_expression(
                        type_table,
                        top_level_table,
                        &let_assignment.expression,
                        diagnostics,
                    );
                }

                let type_kind = match type_table.types.get(&statement.id) {
                    Some(type_kind) => type_kind,
                    None => {
                        diagnostics.error_sub(
                            hir.span,
                            format!("cannot infer type for variable {}", hir.name.symbol),
                            vec![diagnostics
                                .sub_hint_simple(format!("consider adding a type annotation"))],
                        );
                        continue;
                    }
                };

                if let Some(let_assignment) = &hir.let_assignment {
                    if let Some(expression_type_kind) =
                        type_table.types.get(&let_assignment.expression.id)
                    {
                        if !TypeKind::is_subtype(expression_type_kind, type_kind) {
                            diagnostics.error_sub(
                                let_assignment.expression.span,
                                format!(
                                    "cannot assign type `{}` to variable {}",
                                    expression_type_kind.display(top_level_table),
                                    hir.name.symbol
                                ),
                                vec![diagnostics.sub_hint_simple(format!(
                                    "expected type `{}`",
                                    type_kind.display(top_level_table)
                                ))],
                            );
                        }
                    }
                }
            }
            HIRStatementKind::If(hir) => {
                check_types_expression(type_table, top_level_table, &hir.expression, diagnostics);

                if let Some(type_kind) = type_table.types.get(&hir.expression.id) {
                    if !TypeKind::is_subtype(type_kind, &TypeKind::bool()) {
                        diagnostics.error(
                            hir.expression.span,
                            format!(
                                "expected type `{}`, found `{}`",
                                TypeKind::bool().display(top_level_table),
                                type_kind.display(top_level_table)
                            ),
                        );
                    }
                }

                check_types_stmt_block(
                    type_table,
                    top_level_table,
                    function,
                    &hir.body_block,
                    diagnostics,
                );

                for ast in &hir.single_else_ifs {
                    check_types_expression(
                        type_table,
                        top_level_table,
                        &ast.expression,
                        diagnostics,
                    );

                    if let Some(type_kind) = type_table.types.get(&ast.expression.id) {
                        if !TypeKind::is_subtype(type_kind, &TypeKind::bool()) {
                            diagnostics.error(
                                ast.expression.span,
                                format!(
                                    "expected type `{}`, found `{}`",
                                    TypeKind::bool().display(top_level_table),
                                    type_kind.display(top_level_table)
                                ),
                            );
                        }
                    }

                    check_types_stmt_block(
                        type_table,
                        top_level_table,
                        function,
                        &ast.body_block,
                        diagnostics,
                    );
                }

                if let Some(ast) = &hir.single_else {
                    check_types_stmt_block(
                        type_table,
                        top_level_table,
                        function,
                        &ast.body_block,
                        diagnostics,
                    );
                }
            }
            HIRStatementKind::Loop(hir) => {
                check_types_stmt_block(
                    type_table,
                    top_level_table,
                    function,
                    &hir.body_block,
                    diagnostics,
                );
            }
            HIRStatementKind::Break(..) => {}
            HIRStatementKind::Continue(..) => {}
            HIRStatementKind::Return(hir) => {
                if let Some(expression) = &hir.expression {
                    check_types_expression(type_table, top_level_table, expression, diagnostics);
                }

                match (&function.signature.return_type, &hir.expression) {
                    (Some(return_type), Some(expression)) => {
                        if let Some(type_kind) = type_table.types.get(&expression.id) {
                            if !TypeKind::is_subtype(type_kind, &return_type.type_ref.kind) {
                                diagnostics.error_sub(
                                    expression.span,
                                    format!(
                                        "return type `{}` is incompatible",
                                        type_kind.display(top_level_table),
                                    ),
                                    // TODO: Do not use `expected type` here, use `subtypes of` instead.
                                    vec![diagnostics.sub_hint_simple(format!(
                                        "expected type `{}`",
                                        return_type.type_ref.kind.display(top_level_table)
                                    ))],
                                );
                            }
                        }
                    }
                    (Some(return_type), None) => {
                        diagnostics.error_sub(
                            statement.span,
                            format!("this return statement must have a value"),
                            vec![diagnostics.sub_hint(
                                return_type.span,
                                format!("the function returns a value"),
                            )],
                        );
                    }
                    (None, Some(_)) => {
                        diagnostics.error_sub(
                            statement.span,
                            format!("this return statement must not have a value"),
                            vec![diagnostics.sub_hint(
                                function.signature.span,
                                format!("the function does not return a value"),
                            )],
                        );
                    }
                    (None, None) => {}
                }
            }
            HIRStatementKind::Assignment(hir) => {
                check_types_expression(type_table, top_level_table, &hir.left, diagnostics);
                check_types_expression(type_table, top_level_table, &hir.right, diagnostics);

                if let (Some(left), Some(right)) = (
                    type_table.types.get(&hir.left.id),
                    type_table.types.get(&hir.right.id),
                ) {
                    if !TypeKind::is_subtype(right, left) {
                        diagnostics.error_sub(
                            hir.right.span,
                            format!(
                                "cannot assign incompatible type `{}`",
                                right.display(top_level_table)
                            ),
                            vec![diagnostics.sub_hint_simple(format!(
                                "expected type `{}`",
                                left.display(top_level_table)
                            ))],
                        );
                    }
                }
            }
            HIRStatementKind::Row(hir) => {
                check_types_expression(type_table, top_level_table, &hir.expression, diagnostics);
            }
        }
    }
}

pub fn check_types_expression(
    type_table: &TypeTable,
    top_level_table: &TopLevelTable,
    hir: &HIRExpression,
    diagnostics: &DiagnosticsSender,
) {
    let hir_id = hir.id;
    match &hir.kind {
        HIRExpressionKind::Binary(hir) => {
            check_types_expression(type_table, top_level_table, &hir.left, diagnostics);
            check_types_expression(type_table, top_level_table, &hir.right, diagnostics);
        }
        HIRExpressionKind::Unary(hir) => {
            check_types_expression(type_table, top_level_table, &hir.right, diagnostics);
        }
        HIRExpressionKind::As(hir) => {
            check_types_expression(type_table, top_level_table, &hir.expression, diagnostics);
        }
        HIRExpressionKind::Call(hir) => {
            check_types_expression(type_table, top_level_table, &hir.expression, diagnostics);

            for arg in &hir.args {
                check_types_expression(type_table, top_level_table, arg, diagnostics);
            }

            if let Some(type_kind) = type_table.types.get(&hir.expression.id) {
                match type_kind.unwrap_reference() {
                    TypeKind::Callable { params, .. } => {
                        if params.len() != hir.args.len() {
                            diagnostics.error(
                                hir.expression.span,
                                format!("expected {} args, found {}", params.len(), hir.args.len()),
                            );
                        } else {
                            for (param, arg) in params.iter().zip(hir.args.iter()) {
                                if let Some(arg_type_kind) = type_table.types.get(&arg.id) {
                                    if !TypeKind::is_subtype(arg_type_kind, param) {
                                        diagnostics.error_sub(
                                            arg.span,
                                            format!(
                                                "cannot pass incompatible type `{}`",
                                                arg_type_kind.display(top_level_table)
                                            ),
                                            vec![diagnostics.sub_hint_simple(format!(
                                                "expected type `{}`",
                                                param.display(top_level_table)
                                            ))],
                                        );
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        diagnostics.error(
                            hir.expression.span,
                            format!(
                                "given type `{}` is not callable",
                                type_kind.display(top_level_table)
                            ),
                        );
                    }
                }
            }
        }
        HIRExpressionKind::Member(hir) => {
            check_types_expression(type_table, top_level_table, &hir.expression, diagnostics);

            if let Some(type_kind) = type_table.types.get(&hir.expression.id) {
                match type_kind.unwrap_reference() {
                    TypeKind::UserStruct { id } => {
                        let user_struct = top_level_table.user_types[id].as_user_struct().unwrap();
                        if !user_struct.field_names.contains_key(&hir.member.symbol) {
                            diagnostics.error_sub(
                                hir.member.span,
                                format!(
                                    "struct {} does not have a field named {}",
                                    user_struct.name.symbol, hir.member.symbol
                                ),
                                vec![diagnostics.sub_hint(
                                    user_struct.span,
                                    format!("struct definition is here"),
                                )],
                            );
                        }
                    }
                    _ => {
                        diagnostics.error(
                            hir.expression.span,
                            format!(
                                "given type `{}` is not a struct",
                                type_kind.display(top_level_table)
                            ),
                        );
                    }
                }
            }
        }
        HIRExpressionKind::FunctionRef(..) => {}
        HIRExpressionKind::ParamRef(..) => {}
        HIRExpressionKind::VariableRef(..) => {}
        HIRExpressionKind::UnknownRef(..) => {}
        HIRExpressionKind::StructLiteral(hir) => {
            for field in &hir.fields {
                check_types_expression(type_table, top_level_table, &field.expression, diagnostics);
            }

            if let Some(type_kind) = type_table.types.get(&hir_id) {
                let mut handled_fields = HashMap::<Symbol, Span>::new();
                let user_struct = if let TypeKind::UserStruct { id } = type_kind {
                    top_level_table.user_types[id].as_user_struct().unwrap()
                } else {
                    unreachable!()
                };

                for field in &hir.fields {
                    match handled_fields.entry(field.name.symbol) {
                        Entry::Occupied(entry) => {
                            diagnostics.error_sub(
                                field.name.span,
                                format!("field {} is already set", field.name.symbol),
                                vec![diagnostics
                                    .sub_hint(*entry.get(), format!("preivous field is here",))],
                            );
                            continue;
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(field.name.span);
                        }
                    }

                    let type_kind =
                        if let Some(type_kind) = type_table.types.get(&field.expression.id) {
                            type_kind
                        } else {
                            continue;
                        };
                    let index = if let Some(index) = user_struct.field_names.get(&field.name.symbol)
                    {
                        *index
                    } else {
                        diagnostics.error_sub(
                            field.name.span,
                            format!(
                                "struct {} does not have a field named {}",
                                user_struct.name.symbol, field.name.symbol
                            ),
                            vec![diagnostics
                                .sub_hint(user_struct.span, format!("struct definition is here"))],
                        );
                        continue;
                    };
                    let field_type_kind = &user_struct.fields[index];

                    if !TypeKind::is_subtype(type_kind, field_type_kind) {
                        diagnostics.error_sub(
                            field.name.span,
                            format!(
                                "cannot assign incompatible type `{}`",
                                field_type_kind.display(top_level_table)
                            ),
                            vec![diagnostics.sub_hint_simple(format!(
                                "expected type `{}`",
                                field_type_kind.display(top_level_table)
                            ))],
                        );
                    }
                }

                let mut field_names = user_struct
                    .field_names
                    .iter()
                    .map(|(key, value)| (*key, *value))
                    .collect::<Vec<_>>();
                field_names.sort_unstable_by_key(|(_, value)| *value);

                for (field, index) in field_names {
                    if !handled_fields.contains_key(&field) {
                        diagnostics.error_sub(
                            hir.span,
                            format!("field {} is not set", field),
                            vec![diagnostics.sub_hint(
                                user_struct.field_spans[index],
                                format!("struct field is defined here"),
                            )],
                        );
                    }
                }
            }
        }
        HIRExpressionKind::Literal(..) => {}
    }

    if !type_table.types.contains_key(&hir.id) {
        diagnostics.error(hir.span, format!("could not infer type for expression"));
    }
}
