use crate::{HIRBlock, HIRExpression, HIRExpressionKind, HIRProgram, HIRStatementKind, TypeTable};
use ex_diagnostics::DiagnosticsSender;

pub fn check_lvalues(type_table: &TypeTable, hir: &HIRProgram, diagnostics: &DiagnosticsSender) {
    for function in &hir.functions {
        check_lvalues_stmt_block(type_table, &function.body_block, diagnostics);
    }
}

fn check_lvalues_stmt_block(
    type_table: &TypeTable,
    hir: &HIRBlock,
    diagnostics: &DiagnosticsSender,
) {
    for statement in &hir.statements {
        match &statement.kind {
            HIRStatementKind::Block(hir) => {
                check_lvalues_stmt_block(type_table, hir, diagnostics);
            }
            HIRStatementKind::Let(..) => {}
            HIRStatementKind::If(hir) => {
                check_lvalues_stmt_block(type_table, &hir.body_block, diagnostics);

                for hir in &hir.single_else_ifs {
                    check_lvalues_stmt_block(type_table, &hir.body_block, diagnostics);
                }

                if let Some(hir) = &hir.single_else {
                    check_lvalues_stmt_block(type_table, &hir.body_block, diagnostics);
                }
            }
            HIRStatementKind::Loop(hir) => {
                check_lvalues_stmt_block(type_table, &hir.body_block, diagnostics);
            }
            HIRStatementKind::Break(..) => {}
            HIRStatementKind::Continue(..) => {}
            HIRStatementKind::Return(..) => {}
            HIRStatementKind::Assignment(hir) => {
                if !check_lvalues_expression(type_table, &hir.left, diagnostics) {
                    diagnostics.error(
                        hir.left.span,
                        format!("invalid assignment left-hand side expression"),
                    );
                }
            }
            HIRStatementKind::Row(..) => {}
        }
    }
}

fn check_lvalues_expression(
    type_table: &TypeTable,
    hir: &HIRExpression,
    diagnostics: &DiagnosticsSender,
) -> bool {
    if let Some(type_kind) = type_table.types.get(&hir.id) {
        if type_kind.is_reference() {
            return true;
        }
    }

    match &hir.kind {
        HIRExpressionKind::Binary(..) => false,
        HIRExpressionKind::Unary(..) => false,
        HIRExpressionKind::As(..) => false,
        HIRExpressionKind::Call(..) => false,
        HIRExpressionKind::Member(hir) => {
            check_lvalues_expression(type_table, &hir.expression, diagnostics)
        }
        HIRExpressionKind::FunctionRef(..) => false,
        HIRExpressionKind::ParamRef(..) => true,
        HIRExpressionKind::VariableRef(..) => true,
        HIRExpressionKind::UnknownRef(..) => false,
        HIRExpressionKind::StructLiteral(..) => false,
        HIRExpressionKind::Literal(..) => false,
    }
}
