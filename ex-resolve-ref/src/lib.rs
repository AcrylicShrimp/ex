mod function_scope_table;
mod function_table;
mod scope_symbol_table;
mod symbol_node;
mod symbol_reference_table;
mod type_reference_table;

pub use function_scope_table::*;
pub use function_table::*;
pub use scope_symbol_table::*;
pub use symbol_node::*;
pub use symbol_reference_table::*;
pub use type_reference_table::*;

use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin, SubDiagnostics};
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTProgram, ASTStatementKind,
    ASTTopLevelKind, Typename,
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
) -> (FunctionTable, SymbolReferenceTable, TypeReferenceTable) {
    let mut functions = Vec::new();
    let mut function_table = FunctionTable::new();
    let mut type_reference_table = TypeReferenceTable::new();

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
                );
                let scope_table = resolve_scopes(&function, ast, file, diagnostics);

                match function_table.functions.entry(ast.signature.name.symbol) {
                    Entry::Occupied(entry) => {
                        let previous = entry.get();
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
                        entry.insert(function);
                    }
                }

                function_table
                    .function_scopes
                    .insert(ast.signature.name.symbol, scope_table);

                functions.push(ast);
            }
        }
    }

    let mut symbol_reference_table = SymbolReferenceTable::new();

    for ast in functions {
        let scope_table = function_table
            .function_scopes
            .get(&ast.signature.name.symbol)
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
    }

    (function_table, symbol_reference_table, type_reference_table)
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
                        SymbolNodeKind::Variable,
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
                    &stmt_if.condition,
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
                        &single_else_if.condition,
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
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expr) = &stmt_return.expression {
                    resolve_scopes_expression(scope, scope_table, expr, file, diagnostics);
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                resolve_scopes_expression(
                    scope,
                    scope_table,
                    &stmt_assignment.left,
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
                    &stmt_if.condition,
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
                        &single_else_if.condition,
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
                    &stmt_assignment.left,
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

            if let Some(function) = function_table.functions.get(&expr_id_ref.reference.symbol) {
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
            }
            ASTStatementKind::If(stmt_if) => {
                resolve_type_references_expression(
                    type_reference_table,
                    &stmt_if.condition,
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
                        &single_else_if.condition,
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
                    &stmt_assignment.left,
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
    let type_kind = match typename.typename.symbol {
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
                        span: typename.typename.span,
                    }),
                    sub_diagnostics: vec![SubDiagnostics {
                        level: DiagnosticsLevel::Hint,
                        message: format!(
                            "type must be one of {}, {}, or {}",
                            *ex_parser::TYPENAME_INT,
                            *ex_parser::TYPENAME_FLOAT,
                            *ex_parser::TYPENAME_STRING
                        ),
                        origin: None,
                    }],
                })
                .unwrap();
            TypeKind::unknown()
        }
    };

    type_reference_table.references.insert(
        typename.id,
        TypeReference::new(typename.typename, type_kind),
    );
}
