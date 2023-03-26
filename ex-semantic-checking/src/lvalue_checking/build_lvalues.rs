use crate::{
    resolve::{ReferenceTable, ScopeId, SymbolReferenceKind},
    type_inferencing::TypeTable,
};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTProgram, ASTStatementKind, ASTTopLevelKind,
    NodeId,
};
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct LValueTable {
    pub lvalues: HashMap<NodeId, LValue>,
}

impl LValueTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Hash)]
pub struct LValue {
    pub kind: LValueKind,
    pub members: Vec<LValueMember>,
}

impl LValue {
    pub fn new(kind: LValueKind) -> Self {
        Self {
            kind,
            members: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LValueKind {
    Param {
        function: NodeId,
        index: usize,
    },
    Variable {
        function: NodeId,
        scope: ScopeId,
        index: usize,
    },
}

impl LValueKind {
    pub fn param(function: NodeId, index: usize) -> Self {
        Self::Param { function, index }
    }

    pub fn variable(function: NodeId, scope: ScopeId, index: usize) -> Self {
        Self::Variable {
            function,
            scope,
            index,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LValueMember {
    Field { field: Symbol },
}

impl LValueMember {
    pub fn field(field: Symbol) -> Self {
        Self::Field { field }
    }
}

pub fn build_lvalues(
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    ast: &ASTProgram,
    diagnostics: &DiagnosticsSender,
) -> LValueTable {
    let mut table = LValueTable::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                build_lvalues_stmt_block(
                    &mut table,
                    reference_table,
                    type_table,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    table
}

fn build_lvalues_stmt_block(
    lvalue_table: &mut LValueTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    ast: &ASTBlock,
    diagnostics: &DiagnosticsSender,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                build_lvalues_stmt_block(
                    lvalue_table,
                    reference_table,
                    type_table,
                    ast,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(..) => {}
            ASTStatementKind::If(ast) => {
                build_lvalues_stmt_block(
                    lvalue_table,
                    reference_table,
                    type_table,
                    &ast.body_block,
                    diagnostics,
                );

                for ast in &ast.single_else_ifs {
                    build_lvalues_stmt_block(
                        lvalue_table,
                        reference_table,
                        type_table,
                        &ast.body_block,
                        diagnostics,
                    );
                }

                if let Some(ast) = &ast.single_else {
                    build_lvalues_stmt_block(
                        lvalue_table,
                        reference_table,
                        type_table,
                        &ast.body_block,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(ast) => {
                build_lvalues_stmt_block(
                    lvalue_table,
                    reference_table,
                    type_table,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTStatementKind::While(ast) => {
                build_lvalues_stmt_block(
                    lvalue_table,
                    reference_table,
                    type_table,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(..) => {}
            ASTStatementKind::Assignment(ast) => {
                if let Some(lvalue) = build_lvalues_expression(reference_table, &ast.left) {
                    lvalue_table.lvalues.insert(statement.id, lvalue);
                    continue;
                }

                if let Some(type_kind) = type_table.types.get(&ast.left.id) {
                    if type_kind.is_reference() {
                        continue;
                    }
                }

                diagnostics.error(
                    ast.left.span,
                    format!("invalid assignment left-hand side expression"),
                );
            }
            ASTStatementKind::Row(..) => {}
        }
    }
}

fn build_lvalues_expression(
    reference_table: &ReferenceTable,
    ast: &ASTExpression,
) -> Option<LValue> {
    match &ast.kind {
        ASTExpressionKind::Binary(..) => None,
        ASTExpressionKind::Unary(..) => None,
        ASTExpressionKind::As(..) => None,
        ASTExpressionKind::Call(..) => None,
        ASTExpressionKind::Member(ast) => {
            let mut lvalue = build_lvalues_expression(reference_table, &ast.expression)?;
            lvalue.members.push(LValueMember::field(ast.member.symbol));
            Some(lvalue)
        }
        ASTExpressionKind::Paren(ast) => build_lvalues_expression(reference_table, &ast.expression),
        ASTExpressionKind::Literal(..) => None,
        ASTExpressionKind::IdReference(ast) => {
            let symbol_ref = reference_table.symbol_references.get(&ast.id)?;
            match &symbol_ref.kind {
                SymbolReferenceKind::Function { .. } => None,
                SymbolReferenceKind::Param { function, index } => {
                    Some(LValue::new(LValueKind::param(*function, *index)))
                }
                SymbolReferenceKind::Variable {
                    function,
                    scope,
                    index,
                } => Some(LValue::new(LValueKind::variable(*function, *scope, *index))),
            }
        }
        ASTExpressionKind::StructLiteral(..) => None,
    }
}
