use crate::{
    resolve::{Function, TopLevelTable, TypeKind},
    type_inferencing::{TypeTable, BUILT_IN_ASSIGNMENT_OPERATOR},
};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTProgram, ASTStatementKind, ASTTopLevelKind,
};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::{hash_map::Entry, HashMap};

pub fn check_types(
    type_table: &TypeTable,
    top_level_table: &TopLevelTable,
    ast: &ASTProgram,
    diagnostics: &DiagnosticsSender,
) {
    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let function = &top_level_table.functions[&top_level.id];
                check_types_stmt_block(
                    type_table,
                    top_level_table,
                    function,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }
}

pub fn check_types_stmt_block(
    type_table: &TypeTable,
    top_level_table: &TopLevelTable,
    function: &Function,
    ast: &ASTBlock,
    diagnostics: &DiagnosticsSender,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                check_types_stmt_block(type_table, top_level_table, function, ast, diagnostics);
            }
            ASTStatementKind::Let(ast) => {
                if let Some(let_assignment) = &ast.let_assignment {
                    check_types_expression(
                        type_table,
                        top_level_table,
                        function,
                        &let_assignment.expression,
                        diagnostics,
                    );
                }

                let type_kind = match type_table.types.get(&statement.id) {
                    Some(type_kind) => type_kind,
                    None => {
                        diagnostics.error_sub(
                            ast.span,
                            format!("cannot infer type for variable {}", ast.name.symbol),
                            vec![diagnostics
                                .sub_hint_simple(format!("consider adding a type annotation"))],
                        );
                        continue;
                    }
                };

                if let Some(let_assignment) = &ast.let_assignment {
                    if let Some(expression_type_kind) =
                        type_table.types.get(&let_assignment.expression.id)
                    {
                        if !TypeKind::is_subtype(expression_type_kind, type_kind) {
                            diagnostics.error_sub(
                                let_assignment.expression.span,
                                format!(
                                    "cannot assign type `{}` to variable {}",
                                    expression_type_kind.display(top_level_table),
                                    ast.name.symbol
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
            ASTStatementKind::If(ast) => {
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
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

                for ast in &ast.single_else_ifs {
                    check_types_expression(
                        type_table,
                        top_level_table,
                        function,
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

                if let Some(ast) = &ast.single_else {
                    check_types_stmt_block(
                        type_table,
                        top_level_table,
                        function,
                        &ast.body_block,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(ast) => {
                check_types_stmt_block(
                    type_table,
                    top_level_table,
                    function,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTStatementKind::While(ast) => {
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
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
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(ast) => {
                if let Some(expression) = &ast.expression {
                    check_types_expression(
                        type_table,
                        top_level_table,
                        function,
                        expression,
                        diagnostics,
                    );
                }

                match (function.return_type.is_empty(), &ast.expression) {
                    (true, Some(expression)) => {
                        if let Some(type_kind) = type_table.types.get(&expression.id) {
                            if !TypeKind::is_subtype(type_kind, &function.return_type) {
                                diagnostics.error_sub(
                                    expression.span,
                                    format!(
                                        "return type `{}` is incompatible",
                                        type_kind.display(top_level_table),
                                    ),
                                    vec![diagnostics.sub_hint_simple(format!(
                                        "expected type `{}`",
                                        function.return_type.display(top_level_table)
                                    ))],
                                );
                            }
                        }
                    }
                    (true, None) => {
                        diagnostics.error_sub(
                            statement.span,
                            format!("this return statement must have a value"),
                            vec![diagnostics
                                .sub_hint(function.span, format!("the function returns a value"))],
                        );
                    }
                    (false, Some(_)) => {
                        diagnostics.error_sub(
                            statement.span,
                            format!("this return statement must not have a value"),
                            vec![diagnostics.sub_hint(
                                function.span,
                                format!("the function does not return a value"),
                            )],
                        );
                    }
                    (false, None) => {}
                }
            }
            ASTStatementKind::Assignment(ast) => {
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
                    &ast.left,
                    diagnostics,
                );
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
                    &ast.right,
                    diagnostics,
                );

                if let (Some(left), Some(right)) = (
                    type_table.types.get(&ast.left.id),
                    type_table.types.get(&ast.right.id),
                ) {
                    match ast.operator_kind {
                        Some(operator) => {
                            if !BUILT_IN_ASSIGNMENT_OPERATOR.is_supported(
                                operator,
                                left.clone(),
                                right.clone(),
                            ) {
                                diagnostics.error(
                                    ast.span,
                                    format!(
                                        "`{}` {} `{}` is not supported",
                                        left.display(top_level_table),
                                        ast.operator.symbol,
                                        right.display(top_level_table)
                                    ),
                                );
                            }
                        }
                        None => {
                            if !TypeKind::is_subtype(right, left) {
                                diagnostics.error_sub(
                                    ast.right.span,
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
                }
            }
            ASTStatementKind::Row(ast) => {
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
                    &ast.expression,
                    diagnostics,
                );
            }
        }
    }
}

pub fn check_types_expression(
    type_table: &TypeTable,
    top_level_table: &TopLevelTable,
    function: &Function,
    ast: &ASTExpression,
    diagnostics: &DiagnosticsSender,
) {
    let ast_id = ast.id;
    match &ast.kind {
        ASTExpressionKind::Binary(ast) => {
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.left,
                diagnostics,
            );
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.right,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(ast) => {
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.right,
                diagnostics,
            );
        }
        ASTExpressionKind::As(ast) => {
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(ast) => {
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.expression,
                diagnostics,
            );

            for argument in &ast.arguments {
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
                    &argument.expression,
                    diagnostics,
                );
            }

            if let Some(type_kind) = type_table.types.get(&ast.expression.id) {
                match type_kind.unwrap_reference() {
                    TypeKind::Callable { params, .. } => {
                        if params.len() != ast.arguments.len() {
                            diagnostics.error(
                                ast.expression.span,
                                format!(
                                    "expected {} arguments, found {}",
                                    params.len(),
                                    ast.arguments.len()
                                ),
                            );
                        } else {
                            for (param, argument) in params.iter().zip(ast.arguments.iter()) {
                                if let Some(argument_type_kind) =
                                    type_table.types.get(&argument.expression.id)
                                {
                                    if !TypeKind::is_subtype(argument_type_kind, param) {
                                        diagnostics.error_sub(
                                            argument.expression.span,
                                            format!(
                                                "cannot pass incompatible type `{}`",
                                                argument_type_kind.display(top_level_table)
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
                            ast.expression.span,
                            format!(
                                "given type `{}` is not callable",
                                type_kind.display(top_level_table)
                            ),
                        );
                    }
                }
            }
        }
        ASTExpressionKind::Member(ast) => {
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.expression,
                diagnostics,
            );

            if let Some(type_kind) = type_table.types.get(&ast.expression.id) {
                match type_kind.unwrap_reference() {
                    TypeKind::UserStruct { id } => {
                        let user_struct = top_level_table.user_types[id].as_user_struct().unwrap();
                        if !user_struct.field_names.contains_key(&ast.member.symbol) {
                            diagnostics.error_sub(
                                ast.member.span,
                                format!(
                                    "struct {} does not have a field named {}",
                                    user_struct.name.symbol, ast.member.symbol
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
                            ast.expression.span,
                            format!(
                                "given type `{}` is not a struct",
                                type_kind.display(top_level_table)
                            ),
                        );
                    }
                }
            }
        }
        ASTExpressionKind::Paren(ast) => {
            check_types_expression(
                type_table,
                top_level_table,
                function,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(..) => {}
        ASTExpressionKind::StructLiteral(ast) => {
            for field in &ast.fields {
                check_types_expression(
                    type_table,
                    top_level_table,
                    function,
                    &field.expression,
                    diagnostics,
                );
            }

            if let Some(type_kind) = type_table.types.get(&ast_id) {
                let mut handled_fields = HashMap::<Symbol, Span>::new();
                let user_struct = if let TypeKind::UserStruct { id } = type_kind {
                    top_level_table.user_types[id].as_user_struct().unwrap()
                } else {
                    unreachable!()
                };

                for field in &ast.fields {
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
                            ast.span,
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
    }

    if !type_table.types.contains_key(&ast.id) {
        diagnostics.error(ast.span, format!("could not infer type for expression"));
    }
}
