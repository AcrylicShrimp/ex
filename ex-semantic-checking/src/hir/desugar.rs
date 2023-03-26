use super::{
    build_hir_expression, build_hir_stmt_block, HIRBinaryExpression, HIRBinaryOperatorKind,
    HIRBlock, HIRBreak, HIRExpression, HIRExpressionKind, HIRIf, HIRLoop, HIRStatement,
    HIRStatementKind, HIRUnaryExpression, HIRUnaryOperatorKind,
};
use crate::resolve::{ReferenceTable, TopLevelTable};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{ASTAssignment, ASTAssignmentOperatorKind, ASTWhile, NodeIdAllocator};

pub fn desugar_stmt_while(
    id_alloc: &mut NodeIdAllocator,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    ast: &ASTWhile,
    diagnostics: &DiagnosticsSender,
) -> HIRLoop {
    HIRLoop {
        body_block: HIRBlock {
            statements: vec![
                HIRStatement {
                    id: id_alloc.allocate(),
                    kind: HIRStatementKind::If(HIRIf {
                        expression: HIRExpression {
                            id: id_alloc.allocate(),
                            kind: HIRExpressionKind::Unary(HIRUnaryExpression {
                                operator_kind: HIRUnaryOperatorKind::LogNot,
                                right: Box::new(build_hir_expression(
                                    id_alloc,
                                    top_level_table,
                                    reference_table,
                                    &ast.expression,
                                    diagnostics,
                                )),
                                span: ast.expression.span,
                            }),
                            span: ast.expression.span,
                        },
                        body_block: HIRBlock {
                            statements: vec![HIRStatement {
                                id: id_alloc.allocate(),
                                kind: HIRStatementKind::Break(HIRBreak {
                                    span: ast.expression.span,
                                }),
                                span: ast.expression.span,
                            }],
                            span: ast.expression.span,
                        },
                        single_else_ifs: Vec::new(),
                        single_else: None,
                        span: ast.expression.span,
                    }),
                    span: ast.expression.span,
                },
                HIRStatement {
                    id: id_alloc.allocate(),
                    kind: HIRStatementKind::Block(build_hir_stmt_block(
                        id_alloc,
                        top_level_table,
                        reference_table,
                        &ast.body_block,
                        diagnostics,
                    )),
                    span: ast.body_block.span,
                },
            ],
            span: ast.span,
        },
        span: ast.span,
    }
}

pub fn desugar_stmt_assignment_right(
    id_alloc: &mut NodeIdAllocator,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    ast: &ASTAssignment,
    diagnostics: &DiagnosticsSender,
) -> HIRExpression {
    let operator_kind = match ast.operator_kind {
        Some(operator_kind) => match operator_kind {
            ASTAssignmentOperatorKind::Add => HIRBinaryOperatorKind::Add,
            ASTAssignmentOperatorKind::Sub => HIRBinaryOperatorKind::Sub,
            ASTAssignmentOperatorKind::Mul => HIRBinaryOperatorKind::Mul,
            ASTAssignmentOperatorKind::Div => HIRBinaryOperatorKind::Div,
            ASTAssignmentOperatorKind::Mod => HIRBinaryOperatorKind::Mod,
            ASTAssignmentOperatorKind::Pow => HIRBinaryOperatorKind::Pow,
            ASTAssignmentOperatorKind::Shl => HIRBinaryOperatorKind::Shl,
            ASTAssignmentOperatorKind::Shr => HIRBinaryOperatorKind::Shr,
            ASTAssignmentOperatorKind::BitOr => HIRBinaryOperatorKind::BitOr,
            ASTAssignmentOperatorKind::BitAnd => HIRBinaryOperatorKind::BitAnd,
            ASTAssignmentOperatorKind::BitXor => HIRBinaryOperatorKind::BitXor,
        },
        None => {
            return build_hir_expression(
                id_alloc,
                top_level_table,
                reference_table,
                &ast.right,
                diagnostics,
            )
        }
    };
    let left = build_hir_expression(
        id_alloc,
        top_level_table,
        reference_table,
        &ast.left,
        diagnostics,
    );
    let right = build_hir_expression(
        id_alloc,
        top_level_table,
        reference_table,
        &ast.right,
        diagnostics,
    );

    HIRExpression {
        id: id_alloc.allocate(),
        kind: HIRExpressionKind::Binary(HIRBinaryExpression {
            operator_kind,
            left: Box::new(left),
            right: Box::new(right),
            span: ast.span,
        }),
        span: ast.span,
    }
}
