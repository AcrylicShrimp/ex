mod assignment_lhs_table;
mod function_scope_table;
mod function_table;
mod scope_symbol_table;
mod symbol_node;
mod symbol_reference_table;
mod type_reference_table;

pub use assignment_lhs_table::*;
pub use function_scope_table::*;
pub use function_table::*;
pub use scope_symbol_table::*;
pub use symbol_node::*;
pub use symbol_reference_table::*;
pub use type_reference_table::*;

use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin, SubDiagnostics};
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTProgram, ASTStatementKind,
    ASTTopLevelKind, Typename, TypenameKind,
};
use ex_span::SourceFile;
use std::{
    collections::hash_map::Entry,
    sync::{mpsc::Sender, Arc},
};

pub fn resolve_ast(
    ast: &ASTProgram,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> (
    FunctionTable,
    SymbolReferenceTable,
    TypeReferenceTable,
    AssignmentLhsTable,
) {
    let mut functions = Vec::new();
    let mut function_table = FunctionTable::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let function = Function::new(
                    top_level.id,
                    ast.signature.name,
                    ast.signature
                        .parameters
                        .iter()
                        .map(|parameter| parameter.name)
                        .collect(),
                    ast.signature
                        .parameters
                        .iter()
                        .map(|parameter| parameter.typename.clone())
                        .collect(),
                    ast.signature
                        .return_type
                        .as_ref()
                        .map(|return_type| return_type.typename.clone()),
                    ast.span,
                );
                let scope_table = resolve_scopes(&function, ast, file, diagnostics);

                function_table.functions.insert(top_level.id, function);
                function_table
                    .function_scopes
                    .insert(top_level.id, scope_table);

                match function_table
                    .function_symbols
                    .entry(ast.signature.name.symbol)
                {
                    Entry::Occupied(entry) => {
                        let previous = function_table.functions.get(entry.get()).unwrap();
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!(
                                    "duplicated function {}",
                                    ast.signature.name.symbol
                                ),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: ast.signature.name.span,
                                }),
                                sub_diagnostics: vec![SubDiagnostics {
                                    level: DiagnosticsLevel::Hint,
                                    message: format!("previous definition here"),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: previous.name.span,
                                    }),
                                }],
                            })
                            .unwrap();
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(top_level.id);
                    }
                }

                functions.push(ast);
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    let mut symbol_reference_table = SymbolReferenceTable::new();
    let mut type_reference_table = TypeReferenceTable::new();
    let mut assignment_lhs_table = AssignmentLhsTable::new();

    for ast in functions {
        let scope_table = function_table
            .lookup_scope(ast.signature.name.symbol)
            .unwrap();
        resolve_symbol_references(
            &mut symbol_reference_table,
            scope_table,
            &function_table,
            ast,
            file,
            diagnostics,
        );
        resolve_type_references(&mut type_reference_table, ast, file, diagnostics);
        resolve_assignment_lhs(
            &mut assignment_lhs_table,
            &symbol_reference_table,
            ast,
            file,
            diagnostics,
        );
    }

    (
        function_table,
        symbol_reference_table,
        type_reference_table,
        assignment_lhs_table,
    )
}

fn resolve_scopes(
    function: &Function,
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> FunctionScopeTable {
    let mut scope_table = FunctionScopeTable::new(function.node);
    let scopes = scope_table.scope_mut(scope_table.root);

    for (index, parameter) in ast.signature.parameters.iter().enumerate() {
        scopes.symbol_table.symbols.push(SymbolNode::new(
            SymbolNodeKind::parameter(index),
            function.node,
            parameter.name,
        ));
    }

    resolve_scopes_stmt_block(
        scope_table.root,
        &mut scope_table,
        &ast.body_block,
        file,
        diagnostics,
    );

    scope_table
}

fn resolve_scopes_stmt_block(
    mut scope: ScopeId,
    scope_table: &mut FunctionScopeTable,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        scope_table.nodes.insert(statement.id, scope);

        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                let new_scope = scope_table.new_scope(statement.id, scope);
                resolve_scopes_stmt_block(new_scope, scope_table, stmt_block, file, diagnostics);
            }
            ASTStatementKind::Let(stmt_let) => {
                if let Some(let_type) = &stmt_let.let_type {
                    scope_table.nodes.insert(let_type.typename.id, scope);
                }

                let new_scope = scope_table.new_scope(statement.id, scope);
                scope_table
                    .scope_mut(new_scope)
                    .symbol_table
                    .symbols
                    .push(SymbolNode::new(
                        SymbolNodeKind::variable(),
                        statement.id,
                        stmt_let.name,
                    ));

                if let Some(let_assignment) = &stmt_let.let_assignment {
                    resolve_scopes_expression(
                        scope,
                        scope_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );
                }

                scope = new_scope;
            }
            ASTStatementKind::If(stmt_if) => {
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &stmt_if.expression,
                    file,
                    diagnostics,
                );

                let new_scope = scope_table.new_scope(statement.id, scope);
                resolve_scopes_stmt_block(
                    new_scope,
                    scope_table,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    resolve_scopes_expression(
                        scope,
                        scope_table,
                        &single_else_if.expression,
                        file,
                        diagnostics,
                    );

                    let new_scope = scope_table.new_scope(statement.id, scope);
                    resolve_scopes_stmt_block(
                        new_scope,
                        scope_table,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    let new_scope = scope_table.new_scope(statement.id, scope);
                    resolve_scopes_stmt_block(
                        new_scope,
                        scope_table,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(stmt_loop) => {
                let new_scope = scope_table.new_scope(statement.id, scope);
                resolve_scopes_stmt_block(
                    new_scope,
                    scope_table,
                    &stmt_loop.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::While(stmt_while) => {
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &stmt_while.expression,
                    file,
                    diagnostics,
                );

                let new_scope = scope_table.new_scope(statement.id, scope);
                resolve_scopes_stmt_block(
                    new_scope,
                    scope_table,
                    &stmt_while.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expr) = &stmt_return.expression {
                    resolve_scopes_expression(scope, scope_table, expr, file, diagnostics);
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &stmt_assignment.left.expression,
                    file,
                    diagnostics,
                );
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &stmt_assignment.right,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(stmt_row) => {
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &stmt_row.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_scopes_expression(
    scope: ScopeId,
    scope_table: &mut FunctionScopeTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    scope_table.nodes.insert(ast.id, scope);

    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            resolve_scopes_expression(scope, scope_table, &expr_binary.left, file, diagnostics);
            resolve_scopes_expression(scope, scope_table, &expr_binary.right, file, diagnostics);
        }
        ASTExpressionKind::Unary(expr_unary) => {
            resolve_scopes_expression(scope, scope_table, &expr_unary.right, file, diagnostics);
        }
        ASTExpressionKind::As(expr_as) => {
            resolve_scopes_expression(scope, scope_table, &expr_as.expression, file, diagnostics);
            scope_table.nodes.insert(expr_as.typename.id, scope);
        }
        ASTExpressionKind::Call(expr_call) => {
            resolve_scopes_expression(scope, scope_table, &expr_call.expression, file, diagnostics);

            for argument in &expr_call.arguments {
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &argument.expression,
                    file,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Paren(expr_paren) => {
            resolve_scopes_expression(
                scope,
                scope_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(expr_id_ref) => {
            scope_table.nodes.insert(expr_id_ref.id, scope);
        }
    }
}

fn resolve_symbol_references(
    symbol_reference_table: &mut SymbolReferenceTable,
    scope_table: &FunctionScopeTable,
    function_table: &FunctionTable,
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    resolve_symbol_references_stmt_block(
        symbol_reference_table,
        scope_table,
        function_table,
        &ast.body_block,
        file,
        diagnostics,
    );
}

fn resolve_symbol_references_stmt_block(
    symbol_reference_table: &mut SymbolReferenceTable,
    scope_table: &FunctionScopeTable,
    function_table: &FunctionTable,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                resolve_symbol_references_stmt_block(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(stmt_let) => {
                if let Some(let_assignment) = &stmt_let.let_assignment {
                    resolve_symbol_references_expression(
                        symbol_reference_table,
                        scope_table,
                        function_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::If(stmt_if) => {
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_if.expression,
                    file,
                    diagnostics,
                );
                resolve_symbol_references_stmt_block(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    resolve_symbol_references_expression(
                        symbol_reference_table,
                        scope_table,
                        function_table,
                        &single_else_if.expression,
                        file,
                        diagnostics,
                    );
                    resolve_symbol_references_stmt_block(
                        symbol_reference_table,
                        scope_table,
                        function_table,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    resolve_symbol_references_stmt_block(
                        symbol_reference_table,
                        scope_table,
                        function_table,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(stmt_loop) => {
                resolve_symbol_references_stmt_block(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_loop.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::While(stmt_while) => {
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_while.expression,
                    file,
                    diagnostics,
                );
                resolve_symbol_references_stmt_block(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_while.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expression) = &stmt_return.expression {
                    resolve_symbol_references_expression(
                        symbol_reference_table,
                        scope_table,
                        function_table,
                        expression,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_assignment.left.expression,
                    file,
                    diagnostics,
                );
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_assignment.right,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(stmt_row) => {
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &stmt_row.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_symbol_references_expression(
    symbol_reference_table: &mut SymbolReferenceTable,
    scope_table: &FunctionScopeTable,
    function_table: &FunctionTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_binary.left,
                file,
                diagnostics,
            );
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_binary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(expr_unary) => {
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_unary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::As(expr_as) => {
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_as.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(expr_call) => {
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_call.expression,
                file,
                diagnostics,
            );

            for argument in &expr_call.arguments {
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &argument.expression,
                    file,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Paren(expr_paren) => {
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(expr_id_ref) => {
            let mut scope = scope_table.node_scope(expr_id_ref.id);

            loop {
                if let Some(symbol_node) = scope.symbol_table.lookup(expr_id_ref.reference.symbol) {
                    symbol_reference_table
                        .references
                        .insert(expr_id_ref.id, symbol_node.clone());
                    return;
                }

                scope = if let Some(parent) = scope.parent {
                    scope_table.scope(parent)
                } else {
                    break;
                }
            }

            if let Some(function) = function_table.lookup_function(expr_id_ref.reference.symbol) {
                symbol_reference_table.references.insert(
                    expr_id_ref.id,
                    SymbolNode::new(SymbolNodeKind::function(), function.node, function.name),
                );
                return;
            }

            diagnostics
                .send(Diagnostics {
                    level: DiagnosticsLevel::Error,
                    message: format!("unresolved reference {}", expr_id_ref.reference.symbol),
                    origin: Some(DiagnosticsOrigin {
                        file: file.clone(),
                        span: expr_id_ref.span,
                    }),
                    sub_diagnostics: vec![],
                })
                .unwrap();
        }
    }
}

fn resolve_type_references(
    type_reference_table: &mut TypeReferenceTable,
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for parameter in &ast.signature.parameters {
        resolve_type_reference(type_reference_table, &parameter.typename, file, diagnostics);
    }

    if let Some(return_type) = &ast.signature.return_type {
        resolve_type_reference(
            type_reference_table,
            &return_type.typename,
            file,
            diagnostics,
        );
    }

    resolve_type_references_stmt_block(type_reference_table, &ast.body_block, file, diagnostics);
}

fn resolve_type_references_stmt_block(
    type_reference_table: &mut TypeReferenceTable,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                resolve_type_references_stmt_block(
                    type_reference_table,
                    stmt_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(stmt_let) => {
                if let Some(let_type) = &stmt_let.let_type {
                    resolve_type_reference(
                        type_reference_table,
                        &let_type.typename,
                        file,
                        diagnostics,
                    );
                }

                if let Some(let_assignment) = &stmt_let.let_assignment {
                    resolve_type_references_expression(
                        type_reference_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::If(stmt_if) => {
                resolve_type_references_expression(
                    type_reference_table,
                    &stmt_if.expression,
                    file,
                    diagnostics,
                );
                resolve_type_references_stmt_block(
                    type_reference_table,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    resolve_type_references_expression(
                        type_reference_table,
                        &single_else_if.expression,
                        file,
                        diagnostics,
                    );
                    resolve_type_references_stmt_block(
                        type_reference_table,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    resolve_type_references_stmt_block(
                        type_reference_table,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(stmt_loop) => {
                resolve_type_references_stmt_block(
                    type_reference_table,
                    &stmt_loop.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::While(stmt_while) => {
                resolve_type_references_expression(
                    type_reference_table,
                    &stmt_while.expression,
                    file,
                    diagnostics,
                );
                resolve_type_references_stmt_block(
                    type_reference_table,
                    &stmt_while.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expression) = &stmt_return.expression {
                    resolve_type_references_expression(
                        type_reference_table,
                        expression,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                resolve_type_references_expression(
                    type_reference_table,
                    &stmt_assignment.left.expression,
                    file,
                    diagnostics,
                );
                resolve_type_references_expression(
                    type_reference_table,
                    &stmt_assignment.right,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(stmt_row) => {
                resolve_type_references_expression(
                    type_reference_table,
                    &stmt_row.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_type_references_expression(
    type_reference_table: &mut TypeReferenceTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            resolve_type_references_expression(
                type_reference_table,
                &expr_binary.left,
                file,
                diagnostics,
            );
            resolve_type_references_expression(
                type_reference_table,
                &expr_binary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(expr_unary) => {
            resolve_type_references_expression(
                type_reference_table,
                &expr_unary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::As(expr_as) => {
            resolve_type_references_expression(
                type_reference_table,
                &expr_as.expression,
                file,
                diagnostics,
            );
            resolve_type_reference(type_reference_table, &expr_as.typename, file, diagnostics);
        }
        ASTExpressionKind::Call(expr_call) => {
            resolve_type_references_expression(
                type_reference_table,
                &expr_call.expression,
                file,
                diagnostics,
            );

            for argument in &expr_call.arguments {
                resolve_type_references_expression(
                    type_reference_table,
                    &argument.expression,
                    file,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Paren(expr_paren) => {
            resolve_type_references_expression(
                type_reference_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(..) => {}
    }
}

fn resolve_type_reference(
    type_reference_table: &mut TypeReferenceTable,
    typename: &Typename,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    let type_kind = resolve_type_kind(typename, file, diagnostics);
    type_reference_table
        .references
        .insert(typename.id, TypeReference::new(type_kind, typename.span));
}

fn resolve_type_kind(
    typename: &Typename,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> TypeKind {
    match &typename.kind {
        TypenameKind::Id(id) => match id.symbol {
            symbol if symbol == *ex_parser::TYPENAME_BOOL => TypeKind::boolean(),
            symbol if symbol == *ex_parser::TYPENAME_INT => TypeKind::integer(),
            symbol if symbol == *ex_parser::TYPENAME_FLOAT => TypeKind::float(),
            symbol if symbol == *ex_parser::TYPENAME_STRING => TypeKind::string(),
            symbol => {
                diagnostics
                    .send(Diagnostics {
                        level: DiagnosticsLevel::Error,
                        message: format!("unresolved type reference {}", symbol),
                        origin: Some(DiagnosticsOrigin {
                            file: file.clone(),
                            span: typename.span,
                        }),
                        sub_diagnostics: vec![SubDiagnostics {
                            level: DiagnosticsLevel::Hint,
                            message: format!(
                                "type must be one of {}, {}, {}, {}, or {}",
                                *ex_parser::TYPENAME_BOOL,
                                *ex_parser::TYPENAME_INT,
                                *ex_parser::TYPENAME_FLOAT,
                                *ex_parser::TYPENAME_STRING,
                                *ex_parser::KEYWORD_FN
                            ),
                            origin: None,
                        }],
                    })
                    .unwrap();
                TypeKind::unknown()
            }
        },
        TypenameKind::Function(function) => TypeKind::callable(
            function
                .parameters
                .iter()
                .map(|parameter| resolve_type_kind(&parameter.typename, file, diagnostics))
                .collect(),
            function
                .return_type
                .as_ref()
                .map(|return_type| resolve_type_kind(&return_type.typename, file, diagnostics))
                .unwrap_or_else(|| TypeKind::empty()),
        ),
    }
}

fn resolve_assignment_lhs(
    assignment_lhs_table: &mut AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    resolve_assignment_lhs_stmt_block(
        assignment_lhs_table,
        symbol_reference_table,
        &ast.body_block,
        file,
        diagnostics,
    );
}

fn resolve_assignment_lhs_stmt_block(
    assignment_lhs_table: &mut AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                resolve_assignment_lhs_stmt_block(
                    assignment_lhs_table,
                    symbol_reference_table,
                    stmt_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(..) => {}
            ASTStatementKind::If(stmt_if) => {
                resolve_assignment_lhs_stmt_block(
                    assignment_lhs_table,
                    symbol_reference_table,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    resolve_assignment_lhs_stmt_block(
                        assignment_lhs_table,
                        symbol_reference_table,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    resolve_assignment_lhs_stmt_block(
                        assignment_lhs_table,
                        symbol_reference_table,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(stmt_loop) => {
                resolve_assignment_lhs_stmt_block(
                    assignment_lhs_table,
                    symbol_reference_table,
                    &stmt_loop.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::While(stmt_while) => {
                resolve_assignment_lhs_stmt_block(
                    assignment_lhs_table,
                    symbol_reference_table,
                    &stmt_while.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(..) => {}
            ASTStatementKind::Assignment(stmt_assignment) => {
                if let Some(lhs_kind) = resolve_assignment_lhs_expression(
                    symbol_reference_table,
                    &stmt_assignment.left.expression,
                    file,
                    diagnostics,
                ) {
                    assignment_lhs_table
                        .kinds
                        .insert(stmt_assignment.left.id, lhs_kind);
                }
            }
            ASTStatementKind::Row(..) => {}
        }
    }
}

fn resolve_assignment_lhs_expression(
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Option<AssignmentLhsKind> {
    if let ASTExpressionKind::IdReference(expr_id_ref) = &ast.kind {
        if let Some(symbol_reference) = symbol_reference_table.references.get(&expr_id_ref.id) {
            match &symbol_reference.kind {
                SymbolNodeKind::Function => {}
                SymbolNodeKind::Parameter { index } => {
                    return Some(AssignmentLhsKind::Parameter { index: *index });
                }
                SymbolNodeKind::Variable => {
                    return Some(AssignmentLhsKind::Variable {
                        node: symbol_reference.node,
                    });
                }
            }
        }
    }

    diagnostics
        .send(Diagnostics {
            level: DiagnosticsLevel::Error,
            message: format!("invalid assignment left-hand side"),
            origin: Some(DiagnosticsOrigin {
                file: file.clone(),
                span: ast.span,
            }),
            sub_diagnostics: vec![SubDiagnostics {
                level: DiagnosticsLevel::Hint,
                message: format!("left-hand side must be a variable or a parameter"),
                origin: None,
            }],
        })
        .unwrap();
    None
}
