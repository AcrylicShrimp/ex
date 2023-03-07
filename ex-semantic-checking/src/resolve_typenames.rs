use crate::{typename_to_type_kind, UnresolvedTopLevelTable};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTProgram, ASTStatementKind, ASTTopLevelKind,
    NodeId,
};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TypenameTable {
    pub types: HashMap<NodeId, TypeKind>,
}

#[derive(Debug, Clone, Hash)]
pub enum TypeKind {
    Unknown,
    Empty,
    Bool,
    Int,
    Float,
    String,
    Callable {
        params: Vec<TypeKind>,
        return_type: Box<TypeKind>,
    },
    UserStruct {
        id: NodeId,
    },
    Pointer {
        inner: Box<TypeKind>,
    },
    Reference {
        inner: Box<TypeKind>,
    },
}

impl TypeKind {
    pub fn unknown() -> Self {
        Self::Unknown
    }

    pub fn empty() -> Self {
        Self::Empty
    }

    pub fn bool() -> Self {
        Self::Bool
    }

    pub fn int() -> Self {
        Self::Int
    }

    pub fn float() -> Self {
        Self::Float
    }

    pub fn string() -> Self {
        Self::String
    }

    pub fn callable(params: Vec<TypeKind>, return_type: TypeKind) -> Self {
        Self::Callable {
            params,
            return_type: Box::new(return_type),
        }
    }

    pub fn user_struct(id: NodeId) -> Self {
        Self::UserStruct { id }
    }

    pub fn pointer(inner: TypeKind) -> Self {
        Self::Pointer {
            inner: Box::new(inner),
        }
    }

    pub fn reference(inner: TypeKind) -> Self {
        Self::Reference {
            inner: Box::new(inner),
        }
    }
}

pub fn resolve_typenames(
    ast: &ASTProgram,
    unresolved_top_level_table: &UnresolvedTopLevelTable,
    diagnostics: &DiagnosticsSender,
) -> TypenameTable {
    let mut table = TypenameTable::default();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                resolve_typenames_stmt_block(
                    &mut table,
                    &ast.body_block,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    table
}

fn resolve_typenames_stmt_block(
    table: &mut TypenameTable,
    ast: &ASTBlock,
    unresolved_top_level_table: &UnresolvedTopLevelTable,
    diagnostics: &DiagnosticsSender,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                resolve_typenames_stmt_block(table, ast, unresolved_top_level_table, diagnostics);
            }
            ASTStatementKind::Let(ast) => {
                if let Some(let_type) = &ast.let_type {
                    let type_kind = typename_to_type_kind(
                        &let_type.typename,
                        unresolved_top_level_table,
                        diagnostics,
                    );
                    table.types.insert(let_type.typename.id, type_kind);
                }

                if let Some(let_assignment) = &ast.let_assignment {
                    resolve_typenames_expression(
                        table,
                        &let_assignment.expression,
                        unresolved_top_level_table,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::If(ast) => {
                resolve_typenames_expression(
                    table,
                    &ast.expression,
                    unresolved_top_level_table,
                    diagnostics,
                );
                resolve_typenames_stmt_block(
                    table,
                    &ast.body_block,
                    unresolved_top_level_table,
                    diagnostics,
                );

                for ast in &ast.single_else_ifs {
                    resolve_typenames_expression(
                        table,
                        &ast.expression,
                        unresolved_top_level_table,
                        diagnostics,
                    );
                    resolve_typenames_stmt_block(
                        table,
                        &ast.body_block,
                        unresolved_top_level_table,
                        diagnostics,
                    );
                }

                if let Some(ast) = &ast.single_else {
                    resolve_typenames_stmt_block(
                        table,
                        &ast.body_block,
                        unresolved_top_level_table,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(ast) => {
                resolve_typenames_stmt_block(
                    table,
                    &ast.body_block,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
            ASTStatementKind::While(ast) => {
                resolve_typenames_expression(
                    table,
                    &ast.expression,
                    unresolved_top_level_table,
                    diagnostics,
                );
                resolve_typenames_stmt_block(
                    table,
                    &ast.body_block,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(ast) => {
                if let Some(ast) = &ast.expression {
                    resolve_typenames_expression(
                        table,
                        &ast,
                        unresolved_top_level_table,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Assignment(ast) => {
                resolve_typenames_expression(
                    table,
                    &ast.left,
                    unresolved_top_level_table,
                    diagnostics,
                );
                resolve_typenames_expression(
                    table,
                    &ast.right,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(ast) => {
                resolve_typenames_expression(
                    table,
                    &ast.expression,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_typenames_expression(
    table: &mut TypenameTable,
    ast: &ASTExpression,
    unresolved_top_level_table: &UnresolvedTopLevelTable,
    diagnostics: &DiagnosticsSender,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(ast) => {
            resolve_typenames_expression(table, &ast.left, unresolved_top_level_table, diagnostics);
            resolve_typenames_expression(
                table,
                &ast.right,
                unresolved_top_level_table,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(ast) => {
            resolve_typenames_expression(
                table,
                &ast.right,
                unresolved_top_level_table,
                diagnostics,
            );
        }
        ASTExpressionKind::As(ast) => {
            resolve_typenames_expression(
                table,
                &ast.expression,
                unresolved_top_level_table,
                diagnostics,
            );

            let type_kind =
                typename_to_type_kind(&ast.typename, unresolved_top_level_table, diagnostics);
            table.types.insert(ast.typename.id, type_kind);
        }
        ASTExpressionKind::Call(ast) => {
            resolve_typenames_expression(
                table,
                &ast.expression,
                unresolved_top_level_table,
                diagnostics,
            );

            for argument in &ast.arguments {
                resolve_typenames_expression(
                    table,
                    &argument.expression,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Member(ast) => {
            resolve_typenames_expression(
                table,
                &ast.expression,
                unresolved_top_level_table,
                diagnostics,
            );
        }
        ASTExpressionKind::Paren(ast) => {
            resolve_typenames_expression(
                table,
                &ast.expression,
                unresolved_top_level_table,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(..) => {}
        ASTExpressionKind::StructLiteral(ast) => {
            let type_kind =
                typename_to_type_kind(&ast.typename, unresolved_top_level_table, diagnostics);
            table.types.insert(ast.typename.id, type_kind);

            for field in &ast.fields {
                resolve_typenames_expression(
                    table,
                    &field.expression,
                    unresolved_top_level_table,
                    diagnostics,
                );
            }
        }
    }
}
