mod assignment_lhs_table;
mod function_scope_table;
mod function_table;
mod scope_symbol_table;
mod symbol_node;
mod symbol_reference_table;
mod type_reference_table;
mod user_type_table;

pub use assignment_lhs_table::*;
pub use function_scope_table::*;
pub use function_table::*;
pub use scope_symbol_table::*;
pub use symbol_node::*;
pub use symbol_reference_table::*;
pub use type_reference_table::*;
pub use user_type_table::*;

use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin, SubDiagnostics};
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTProgram, ASTStatementKind,
    ASTStruct, ASTTopLevelKind, NodeId, Typename, TypenameKind,
};
use ex_span::{SourceFile, Span};
use ex_symbol::Symbol;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::{mpsc::Sender, Arc},
};

pub fn resolve_ast(
    ast: &ASTProgram,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> (
    FunctionTable,
    UserTypeTable,
    SymbolReferenceTable,
    TypeReferenceTable,
    AssignmentLhsTable,
) {
    let mut top_level_table = HashMap::<Symbol, Span>::new();
    let mut function_table = FunctionTable::new();
    let mut user_type_table = UserTypeTable::new();
    let mut functions = Vec::new();
    let mut structs = Vec::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                match top_level_table.entry(ast.signature.name.symbol) {
                    Entry::Occupied(entry) => {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!(
                                    "duplicated top level {}",
                                    ast.signature.name.symbol
                                ),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: ast.signature.name.span,
                                }),
                                sub_diagnostics: vec![
                                    SubDiagnostics {
                                        level: DiagnosticsLevel::Hint,
                                        message: format!("previous definition here"),
                                        origin: Some(DiagnosticsOrigin {
                                            file: file.clone(),
                                            span: *entry.get(),
                                        }),
                                    },
                                    SubDiagnostics {
                                        level: DiagnosticsLevel::Warning,
                                        message: format!("duplicated one will be ignored"),
                                        origin: None,
                                    },
                                ],
                            })
                            .unwrap();
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(ast.signature.name.span);
                    }
                }

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
                function_table
                    .function_symbols
                    .insert(ast.signature.name.symbol, top_level.id);

                functions.push(ast);
            }
            ASTTopLevelKind::Struct(ast) => {
                match top_level_table.entry(ast.name.symbol) {
                    Entry::Occupied(entry) => {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!("duplicated top level {}", ast.name.symbol),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: ast.name.span,
                                }),
                                sub_diagnostics: vec![SubDiagnostics {
                                    level: DiagnosticsLevel::Hint,
                                    message: format!("previous definition here"),
                                    origin: Some(DiagnosticsOrigin {
                                        file: file.clone(),
                                        span: *entry.get(),
                                    }),
                                }],
                            })
                            .unwrap();
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(ast.name.span);
                    }
                }

                let user_type_struct = UserTypeStruct::new(
                    top_level.id,
                    ast.name,
                    ast.fields.iter().map(|field| field.name).collect(),
                    ast.fields
                        .iter()
                        .map(|field| field.typename.clone())
                        .collect(),
                    ast.fields.iter().map(|field| field.span).collect(),
                    ast.span,
                );

                user_type_table
                    .user_types
                    .insert(top_level.id, UserTypeKind::Struct(user_type_struct));
                user_type_table
                    .user_type_symbols
                    .insert(ast.name.symbol, top_level.id);

                structs.push(ast);
            }
        }
    }

    let mut symbol_reference_table = SymbolReferenceTable::new();
    let mut type_reference_table = TypeReferenceTable::new();
    let mut assignment_lhs_table = AssignmentLhsTable::new();

    for ast in functions {
        check_function_duplicated_parameters(ast, file, diagnostics);

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
        resolve_type_references(
            &mut type_reference_table,
            &user_type_table,
            ast,
            file,
            diagnostics,
        );
        resolve_assignment_lhs(
            &mut assignment_lhs_table,
            &symbol_reference_table,
            ast,
            file,
            diagnostics,
        );
    }

    for ast in structs {
        check_user_type_struct_duplicated_fields(ast, file, diagnostics);
        resolve_type_references_user_type_struct(
            &mut type_reference_table,
            &user_type_table,
            ast,
            file,
            diagnostics,
        );
    }

    check_user_type_struct_circular_dependencies(
        &type_reference_table,
        &user_type_table,
        file,
        diagnostics,
    );

    (
        function_table,
        user_type_table,
        symbol_reference_table,
        type_reference_table,
        assignment_lhs_table,
    )
}

fn check_function_duplicated_parameters(
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    let mut parameters = HashMap::<Symbol, Span>::new();

    for parameter in &ast.signature.parameters {
        match parameters.entry(parameter.name.symbol) {
            Entry::Occupied(entry) => {
                diagnostics
                    .send(Diagnostics {
                        level: DiagnosticsLevel::Error,
                        message: format!("duplicated parameter {}", parameter.name.symbol),
                        origin: Some(DiagnosticsOrigin {
                            file: file.clone(),
                            span: parameter.name.span,
                        }),
                        sub_diagnostics: vec![SubDiagnostics {
                            level: DiagnosticsLevel::Hint,
                            message: format!("previous definition here"),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: *entry.get(),
                            }),
                        }],
                    })
                    .unwrap();
            }
            Entry::Vacant(entry) => {
                entry.insert(parameter.span);
            }
        }
    }
}

fn check_user_type_struct_duplicated_fields(
    ast: &ASTStruct,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    let mut fields = HashMap::<Symbol, Span>::new();

    for field in &ast.fields {
        match fields.entry(field.name.symbol) {
            Entry::Occupied(entry) => {
                diagnostics
                    .send(Diagnostics {
                        level: DiagnosticsLevel::Error,
                        message: format!("duplicated field {}", field.name.symbol),
                        origin: Some(DiagnosticsOrigin {
                            file: file.clone(),
                            span: field.name.span,
                        }),
                        sub_diagnostics: vec![SubDiagnostics {
                            level: DiagnosticsLevel::Hint,
                            message: format!("previous definition here"),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: *entry.get(),
                            }),
                        }],
                    })
                    .unwrap();
            }
            Entry::Vacant(entry) => {
                entry.insert(field.span);
            }
        }
    }
}

fn check_user_type_struct_circular_dependencies(
    type_reference_table: &TypeReferenceTable,
    user_type_table: &UserTypeTable,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    let mut dependencies = HashMap::<NodeId, Vec<(NodeId, Span)>>::new();
    let mut handled = HashSet::<(NodeId, NodeId)>::new();

    for (id, user_type) in &user_type_table.user_types {
        #[allow(irrefutable_let_patterns)]
        let user_type_struct = if let UserTypeKind::Struct(user_type_struct) = user_type {
            user_type_struct
        } else {
            continue;
        };

        for (index, typename) in user_type_struct.fields_typenames.iter().enumerate() {
            let symbol = if let TypeKind::UserTypeStruct { symbol } =
                &type_reference_table.references[&typename.id].kind
            {
                symbol
            } else {
                continue;
            };
            let user_type_id = user_type_table.user_type_symbols[symbol];
            dependencies
                .entry(user_type_id)
                .or_default()
                .push((*id, user_type_struct.fields_spans[index]));
            handled.insert((*id, user_type_id));
        }
    }

    for (id, dependencies) in &mut dependencies {
        let mut added_dependencies = Vec::new();

        for (dep_id, _) in dependencies.iter() {
            let user_type_struct = user_type_table.user_types[dep_id].as_struct();

            for (index, typename) in user_type_struct.fields_typenames.iter().enumerate() {
                let symbol = if let TypeKind::UserTypeStruct { symbol } =
                    &type_reference_table.references[&typename.id].kind
                {
                    symbol
                } else {
                    continue;
                };
                let user_type_id = user_type_table.user_type_symbols[symbol];

                if handled.contains(&(*id, user_type_id)) {
                    continue;
                }

                added_dependencies.push((*id, user_type_struct.fields_spans[index]));
                handled.insert((*id, user_type_id));
            }
        }

        dependencies.extend(added_dependencies);
    }

    for (id, dependencies) in &dependencies {
        if let Some((_, dep_span)) = dependencies.iter().find(|(dep_id, _)| id == dep_id) {
            diagnostics
                .send(Diagnostics {
                    level: DiagnosticsLevel::Error,
                    message: format!(
                        "struct {} has circular dependency",
                        user_type_table.user_types[id].as_struct().name.symbol,
                    ),
                    origin: Some(DiagnosticsOrigin {
                        file: file.clone(),
                        span: user_type_table.user_types[id].as_struct().span,
                    }),
                    sub_diagnostics: vec![SubDiagnostics {
                        level: DiagnosticsLevel::Hint,
                        message: format!("dependency here"),
                        origin: Some(DiagnosticsOrigin {
                            file: file.clone(),
                            span: *dep_span,
                        }),
                    }],
                })
                .unwrap();
        }
    }
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
        ASTExpressionKind::Member(expr_member) => {
            resolve_scopes_expression(
                scope,
                scope_table,
                &expr_member.expression,
                file,
                diagnostics,
            );
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
        ASTExpressionKind::StructLiteral(expr_struct_literal) => {
            for field in &expr_struct_literal.fields {
                resolve_scopes_expression(scope, scope_table, &field.expression, file, diagnostics);
            }
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
        ASTExpressionKind::Member(expr_member) => {
            resolve_symbol_references_expression(
                symbol_reference_table,
                scope_table,
                function_table,
                &expr_member.expression,
                file,
                diagnostics,
            );
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
        ASTExpressionKind::StructLiteral(expr_struct_literal) => {
            for field in &expr_struct_literal.fields {
                resolve_symbol_references_expression(
                    symbol_reference_table,
                    scope_table,
                    function_table,
                    &field.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_type_references(
    type_reference_table: &mut TypeReferenceTable,
    user_type_table: &UserTypeTable,
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for parameter in &ast.signature.parameters {
        resolve_type_reference(
            type_reference_table,
            user_type_table,
            &parameter.typename,
            file,
            diagnostics,
        );
    }

    if let Some(return_type) = &ast.signature.return_type {
        resolve_type_reference(
            type_reference_table,
            user_type_table,
            &return_type.typename,
            file,
            diagnostics,
        );
    }

    resolve_type_references_stmt_block(
        type_reference_table,
        user_type_table,
        &ast.body_block,
        file,
        diagnostics,
    );
}

fn resolve_type_references_stmt_block(
    type_reference_table: &mut TypeReferenceTable,
    user_type_table: &UserTypeTable,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                resolve_type_references_stmt_block(
                    type_reference_table,
                    user_type_table,
                    stmt_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(stmt_let) => {
                if let Some(let_type) = &stmt_let.let_type {
                    resolve_type_reference(
                        type_reference_table,
                        user_type_table,
                        &let_type.typename,
                        file,
                        diagnostics,
                    );
                }

                if let Some(let_assignment) = &stmt_let.let_assignment {
                    resolve_type_references_expression(
                        type_reference_table,
                        user_type_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::If(stmt_if) => {
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
                    &stmt_if.expression,
                    file,
                    diagnostics,
                );
                resolve_type_references_stmt_block(
                    type_reference_table,
                    user_type_table,
                    &stmt_if.body_block,
                    file,
                    diagnostics,
                );

                for single_else_if in &stmt_if.single_else_ifs {
                    resolve_type_references_expression(
                        type_reference_table,
                        user_type_table,
                        &single_else_if.expression,
                        file,
                        diagnostics,
                    );
                    resolve_type_references_stmt_block(
                        type_reference_table,
                        user_type_table,
                        &single_else_if.body_block,
                        file,
                        diagnostics,
                    );
                }

                if let Some(single_else) = &stmt_if.single_else {
                    resolve_type_references_stmt_block(
                        type_reference_table,
                        user_type_table,
                        &single_else.body_block,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(stmt_loop) => {
                resolve_type_references_stmt_block(
                    type_reference_table,
                    user_type_table,
                    &stmt_loop.body_block,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::While(stmt_while) => {
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
                    &stmt_while.expression,
                    file,
                    diagnostics,
                );
                resolve_type_references_stmt_block(
                    type_reference_table,
                    user_type_table,
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
                        user_type_table,
                        expression,
                        file,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
                    &stmt_assignment.left,
                    file,
                    diagnostics,
                );
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
                    &stmt_assignment.right,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(stmt_row) => {
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
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
    user_type_table: &UserTypeTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_binary.left,
                file,
                diagnostics,
            );
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_binary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(expr_unary) => {
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_unary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::As(expr_as) => {
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_as.expression,
                file,
                diagnostics,
            );
            resolve_type_reference(
                type_reference_table,
                user_type_table,
                &expr_as.typename,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(expr_call) => {
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_call.expression,
                file,
                diagnostics,
            );

            for argument in &expr_call.arguments {
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
                    &argument.expression,
                    file,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Member(expr_member) => {
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_member.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Paren(expr_paren) => {
            resolve_type_references_expression(
                type_reference_table,
                user_type_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(..) => {}
        ASTExpressionKind::StructLiteral(expr_struct_literal) => {
            resolve_type_reference(
                type_reference_table,
                user_type_table,
                &expr_struct_literal.typename,
                file,
                diagnostics,
            );

            if let Some(type_reference) = type_reference_table
                .references
                .get(&expr_struct_literal.typename.id)
            {
                if !type_reference.kind.is_unknown() && !type_reference.kind.is_user_type_struct() {
                    diagnostics
                        .send(Diagnostics {
                            level: DiagnosticsLevel::Error,
                            message: format!("expected struct type, found {}", type_reference.kind),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: expr_struct_literal.typename.span,
                            }),
                            sub_diagnostics: vec![],
                        })
                        .unwrap();
                }
            }

            for field in &expr_struct_literal.fields {
                resolve_type_references_expression(
                    type_reference_table,
                    user_type_table,
                    &field.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_type_references_user_type_struct(
    type_reference_table: &mut TypeReferenceTable,
    user_type_table: &UserTypeTable,
    ast: &ASTStruct,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for field in &ast.fields {
        resolve_type_reference(
            type_reference_table,
            user_type_table,
            &field.typename,
            file,
            diagnostics,
        );
    }
}

fn resolve_type_reference(
    type_reference_table: &mut TypeReferenceTable,
    user_type_table: &UserTypeTable,
    typename: &Typename,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    let type_kind = resolve_type_kind(user_type_table, typename, file, diagnostics);
    type_reference_table
        .references
        .insert(typename.id, TypeReference::new(type_kind, typename.span));
}

fn resolve_type_kind(
    user_type_table: &UserTypeTable,
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
            symbol => match user_type_table.lookup(id.symbol) {
                Some(kind) => match kind {
                    UserTypeKind::Struct(..) => TypeKind::user_type_struct(symbol),
                },
                None => {
                    diagnostics
                        .send(Diagnostics {
                            level: DiagnosticsLevel::Error,
                            message: format!("unresolved type reference {}", symbol),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: typename.span,
                            }),
                            sub_diagnostics: vec![],
                        })
                        .unwrap();
                    TypeKind::unknown()
                }
            },
        },
        TypenameKind::Function(function) => TypeKind::callable(
            function
                .parameters
                .iter()
                .map(|parameter| {
                    resolve_type_kind(user_type_table, &parameter.typename, file, diagnostics)
                })
                .collect(),
            function
                .return_type
                .as_ref()
                .map(|return_type| {
                    resolve_type_kind(user_type_table, &return_type.typename, file, diagnostics)
                })
                .unwrap_or_else(|| TypeKind::empty()),
        ),
        TypenameKind::Pointer(pointer) => {
            let type_kind =
                resolve_type_kind(user_type_table, &pointer.typename, file, diagnostics);
            TypeKind::pointer(type_kind)
        }
        TypenameKind::Reference(reference) => {
            let type_kind =
                resolve_type_kind(user_type_table, &reference.typename, file, diagnostics);
            TypeKind::reference(type_kind)
        }
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
                resolve_assignment_lhs_expression(
                    assignment_lhs_table,
                    symbol_reference_table,
                    &stmt_assignment.left,
                    file,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(..) => {}
        }
    }
}

fn resolve_assignment_lhs_expression(
    assignment_lhs_table: &mut AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    match &ast.kind {
        ASTExpressionKind::Member(expr_member) => {
            resolve_assignment_lhs_expression(
                assignment_lhs_table,
                symbol_reference_table,
                &expr_member.expression,
                file,
                diagnostics,
            );
            assignment_lhs_table.kinds.insert(
                ast.id,
                AssignmentLhsKind::Field {
                    field: expr_member.member.symbol,
                },
            );
            return;
        }
        ASTExpressionKind::Paren(expr_paren) => {
            resolve_assignment_lhs_expression(
                assignment_lhs_table,
                symbol_reference_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
            let kind = assignment_lhs_table
                .kinds
                .get(&expr_paren.expression.id)
                .unwrap();
            assignment_lhs_table.kinds.insert(ast.id, kind.clone());
            return;
        }
        ASTExpressionKind::IdReference(expr_id_ref) => {
            if let Some(symbol_reference) = symbol_reference_table.references.get(&expr_id_ref.id) {
                match &symbol_reference.kind {
                    SymbolNodeKind::Function => {}
                    SymbolNodeKind::Parameter { index } => {
                        assignment_lhs_table
                            .kinds
                            .insert(ast.id, AssignmentLhsKind::Parameter { index: *index });
                        return;
                    }
                    SymbolNodeKind::Variable => {
                        assignment_lhs_table.kinds.insert(
                            ast.id,
                            AssignmentLhsKind::Variable {
                                node: symbol_reference.node,
                            },
                        );
                        return;
                    }
                }
            }
        }
        _ => {}
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
                message: format!("left-hand side must be a member, a variable or a parameter"),
                origin: None,
            }],
        })
        .unwrap();
}
