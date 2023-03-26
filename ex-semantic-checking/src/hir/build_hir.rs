use super::{desugar_stmt_assignment_right, desugar_stmt_while};
use crate::resolve::{ReferenceTable, ScopeId, SymbolReferenceKind, TopLevelTable, TypeReference};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{
    ASTBinaryOperatorKind, ASTBlock, ASTExpression, ASTExpressionKind, ASTProgram,
    ASTStatementKind, ASTTopLevelKind, ASTUnaryOperatorKind, Id, NodeId, NodeIdAllocator,
    TokenLiteral, Typename,
};
use ex_span::Span;

#[derive(Debug, Clone, Hash)]
pub struct HIRProgram {
    pub functions: Vec<HIRFunction>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRFunction {
    pub id: NodeId,
    pub signature: HIRFunctionSignature,
    pub body_block: HIRBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRFunctionSignature {
    pub name: Id,
    pub params: Vec<HIRFunctionParam>,
    pub return_type: Option<HIRFunctionReturnType>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRFunctionParam {
    pub name: Id,
    pub type_ref: Option<TypeReference>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRFunctionReturnType {
    pub type_ref: Option<TypeReference>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRStatement {
    pub id: NodeId,
    pub kind: HIRStatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum HIRStatementKind {
    Block(HIRBlock),
    Let(HIRLet),
    If(HIRIf),
    Loop(HIRLoop),
    Break(HIRBreak),
    Continue(HIRContinue),
    Return(HIRReturn),
    Assignment(HIRAssignment),
    Row(HIRRow),
}

#[derive(Debug, Clone, Hash)]
pub struct HIRBlock {
    pub statements: Vec<HIRStatement>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRLet {
    pub name: Id,
    pub let_type: Option<HIRLetType>,
    pub let_assignment: Option<HIRLetAssignment>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRLetType {
    pub type_ref: Option<TypeReference>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRLetAssignment {
    pub expression: HIRExpression,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRIf {
    pub expression: HIRExpression,
    pub body_block: HIRBlock,
    pub single_else_ifs: Vec<HIRSingleElseIf>,
    pub single_else: Option<HIRSingleElse>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRSingleElseIf {
    pub expression: HIRExpression,
    pub body_block: HIRBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRSingleElse {
    pub body_block: HIRBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRLoop {
    pub body_block: HIRBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRBreak {
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRContinue {
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRReturn {
    pub expression: Option<HIRExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRAssignment {
    pub left: HIRExpression,
    pub right: HIRExpression,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRRow {
    pub expression: HIRExpression,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRExpression {
    pub id: NodeId,
    pub kind: HIRExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum HIRExpressionKind {
    Binary(HIRBinaryExpression),
    Unary(HIRUnaryExpression),
    As(HIRAsExpression),
    Call(HIRCallExpression),
    Member(HIRMemberExpression),
    FunctionRef(HIRFunctionRef), // symbol reference of function
    ParamRef(HIRParamRef),       // symbol reference of param
    VariableRef(HIRVariableRef), // symbol reference of variable
    UnknownRef(HIRUnknownRef),   // symbol reference of unknown; used for error recovery
    StructLiteral(HIRStructLiteral),
    Literal(TokenLiteral),
}

#[derive(Debug, Clone, Hash)]
pub struct HIRBinaryExpression {
    pub left: Box<HIRExpression>,
    pub operator_kind: HIRBinaryOperatorKind,
    pub right: Box<HIRExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HIRBinaryOperatorKind {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    LogOr,
    LogAnd,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Shl,
    Shr,
    BitOr,
    BitAnd,
    BitXor,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRUnaryExpression {
    pub operator_kind: HIRUnaryOperatorKind,
    pub right: Box<HIRExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HIRUnaryOperatorKind {
    Minus,
    BitNot,
    LogNot,
    AddressOf,
    Dereference,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRAsExpression {
    pub expression: Box<HIRExpression>,
    pub type_ref: Option<TypeReference>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRCallExpression {
    pub expression: Box<HIRExpression>,
    pub args: Vec<HIRExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRMemberExpression {
    pub expression: Box<HIRExpression>,
    pub member: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRFunctionRef {
    pub reference: Id,
    pub function: NodeId,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRParamRef {
    pub reference: Id,
    pub function: NodeId,
    pub index: usize,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRVariableRef {
    pub id: NodeId,
    pub reference: Id,
    pub function: NodeId,
    pub scope: ScopeId,
    pub index: usize,
    pub statement: NodeId,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRUnknownRef {
    pub id: NodeId,
    pub reference: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRStructLiteral {
    pub type_ref: Option<TypeReference>,
    pub fields: Vec<HIRStructLiteralField>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct HIRStructLiteralField {
    pub name: Id,
    pub expression: HIRExpression,
    pub span: Span,
}

pub fn build_hir(
    id_alloc: &mut NodeIdAllocator,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    ast: &ASTProgram,
    diagnostics: &DiagnosticsSender,
) -> HIRProgram {
    let mut functions = Vec::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => functions.push(HIRFunction {
                id: top_level.id,
                signature: HIRFunctionSignature {
                    name: ast.signature.name,
                    params: ast
                        .signature
                        .params
                        .iter()
                        .map(|param| HIRFunctionParam {
                            name: param.name,
                            type_ref: typename_to_type_ref(&param.typename, reference_table),
                            span: param.span,
                        })
                        .collect(),
                    return_type: ast.signature.return_type.as_ref().map(|return_type| {
                        HIRFunctionReturnType {
                            type_ref: typename_to_type_ref(&return_type.typename, reference_table),
                            span: return_type.span,
                        }
                    }),
                    span: ast.signature.span,
                },
                body_block: build_hir_stmt_block(
                    id_alloc,
                    top_level_table,
                    reference_table,
                    &ast.body_block,
                    diagnostics,
                ),
                span: ast.span,
            }),
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    HIRProgram {
        functions,
        span: ast.span,
    }
}

pub fn build_hir_stmt_block(
    id_alloc: &mut NodeIdAllocator,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    ast: &ASTBlock,
    diagnostics: &DiagnosticsSender,
) -> HIRBlock {
    let mut statements = Vec::new();

    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                let block = build_hir_stmt_block(
                    id_alloc,
                    top_level_table,
                    reference_table,
                    &ast,
                    diagnostics,
                );
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Block(block),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Let(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Let(HIRLet {
                        name: ast.name,
                        let_type: ast.let_type.as_ref().map(|let_type| HIRLetType {
                            type_ref: typename_to_type_ref(&let_type.typename, reference_table),
                            span: let_type.span,
                        }),
                        let_assignment: ast.let_assignment.as_ref().map(|let_assignment| {
                            HIRLetAssignment {
                                expression: build_hir_expression(
                                    id_alloc,
                                    top_level_table,
                                    reference_table,
                                    &let_assignment.expression,
                                    diagnostics,
                                ),
                                span: let_assignment.span,
                            }
                        }),
                        span: ast.span,
                    }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::If(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::If(HIRIf {
                        expression: build_hir_expression(
                            id_alloc,
                            top_level_table,
                            reference_table,
                            &ast.expression,
                            diagnostics,
                        ),
                        body_block: build_hir_stmt_block(
                            id_alloc,
                            top_level_table,
                            reference_table,
                            &ast.body_block,
                            diagnostics,
                        ),
                        single_else_ifs: ast
                            .single_else_ifs
                            .iter()
                            .map(|ast| HIRSingleElseIf {
                                expression: build_hir_expression(
                                    id_alloc,
                                    top_level_table,
                                    reference_table,
                                    &ast.expression,
                                    diagnostics,
                                ),
                                body_block: build_hir_stmt_block(
                                    id_alloc,
                                    top_level_table,
                                    reference_table,
                                    &ast.body_block,
                                    diagnostics,
                                ),
                                span: ast.span,
                            })
                            .collect(),
                        single_else: ast.single_else.as_ref().map(|ast| HIRSingleElse {
                            body_block: build_hir_stmt_block(
                                id_alloc,
                                top_level_table,
                                reference_table,
                                &ast.body_block,
                                diagnostics,
                            ),
                            span: ast.span,
                        }),
                        span: ast.span,
                    }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Loop(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Loop(HIRLoop {
                        body_block: build_hir_stmt_block(
                            id_alloc,
                            top_level_table,
                            reference_table,
                            &ast.body_block,
                            diagnostics,
                        ),
                        span: ast.span,
                    }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::While(ast) => {
                let desugared_loop = desugar_stmt_while(
                    id_alloc,
                    top_level_table,
                    reference_table,
                    ast,
                    diagnostics,
                );
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Loop(desugared_loop),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Break(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Break(HIRBreak { span: ast.span }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Continue(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Continue(HIRContinue { span: ast.span }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Return(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Return(HIRReturn {
                        expression: ast.expression.as_ref().map(|expression| {
                            build_hir_expression(
                                id_alloc,
                                top_level_table,
                                reference_table,
                                expression,
                                diagnostics,
                            )
                        }),
                        span: ast.span,
                    }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Assignment(ast) => {
                let left = build_hir_expression(
                    id_alloc,
                    top_level_table,
                    reference_table,
                    &ast.left,
                    diagnostics,
                );
                let desugared_right = desugar_stmt_assignment_right(
                    id_alloc,
                    top_level_table,
                    reference_table,
                    ast,
                    diagnostics,
                );
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Assignment(HIRAssignment {
                        left,
                        right: desugared_right,
                        span: ast.span,
                    }),
                    span: statement.span,
                };
                statements.push(statement);
            }
            ASTStatementKind::Row(ast) => {
                let statement = HIRStatement {
                    id: statement.id,
                    kind: HIRStatementKind::Row(HIRRow {
                        expression: build_hir_expression(
                            id_alloc,
                            top_level_table,
                            reference_table,
                            &ast.expression,
                            diagnostics,
                        ),
                        span: ast.span,
                    }),
                    span: statement.span,
                };
                statements.push(statement);
            }
        }
    }

    HIRBlock {
        statements,
        span: ast.span,
    }
}

pub fn build_hir_expression(
    id_alloc: &mut NodeIdAllocator,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    ast: &ASTExpression,
    diagnostics: &DiagnosticsSender,
) -> HIRExpression {
    let ast_id = ast.id;

    match &ast.kind {
        ASTExpressionKind::Binary(ast) => {
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
            let operator_kind = match ast.operator_kind {
                ASTBinaryOperatorKind::Eq => HIRBinaryOperatorKind::Eq,
                ASTBinaryOperatorKind::Ne => HIRBinaryOperatorKind::Ne,
                ASTBinaryOperatorKind::Lt => HIRBinaryOperatorKind::Lt,
                ASTBinaryOperatorKind::Gt => HIRBinaryOperatorKind::Gt,
                ASTBinaryOperatorKind::Le => HIRBinaryOperatorKind::Le,
                ASTBinaryOperatorKind::Ge => HIRBinaryOperatorKind::Ge,
                ASTBinaryOperatorKind::LogOr => HIRBinaryOperatorKind::LogOr,
                ASTBinaryOperatorKind::LogAnd => HIRBinaryOperatorKind::LogAnd,
                ASTBinaryOperatorKind::Add => HIRBinaryOperatorKind::Add,
                ASTBinaryOperatorKind::Sub => HIRBinaryOperatorKind::Sub,
                ASTBinaryOperatorKind::Mul => HIRBinaryOperatorKind::Mul,
                ASTBinaryOperatorKind::Div => HIRBinaryOperatorKind::Div,
                ASTBinaryOperatorKind::Mod => HIRBinaryOperatorKind::Mod,
                ASTBinaryOperatorKind::Pow => HIRBinaryOperatorKind::Pow,
                ASTBinaryOperatorKind::Shl => HIRBinaryOperatorKind::Shl,
                ASTBinaryOperatorKind::Shr => HIRBinaryOperatorKind::Shr,
                ASTBinaryOperatorKind::BitOr => HIRBinaryOperatorKind::BitOr,
                ASTBinaryOperatorKind::BitAnd => HIRBinaryOperatorKind::BitAnd,
                ASTBinaryOperatorKind::BitXor => HIRBinaryOperatorKind::BitXor,
            };
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::Binary(HIRBinaryExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator_kind,
                    span: ast.span,
                }),
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::Unary(ast) => {
            let right = build_hir_expression(
                id_alloc,
                top_level_table,
                reference_table,
                &ast.right,
                diagnostics,
            );
            let operator_kind = match ast.operator_kind {
                ASTUnaryOperatorKind::Plus => {
                    return right;
                }
                ASTUnaryOperatorKind::Minus => HIRUnaryOperatorKind::Minus,
                ASTUnaryOperatorKind::BitNot => HIRUnaryOperatorKind::BitNot,
                ASTUnaryOperatorKind::LogNot => HIRUnaryOperatorKind::LogNot,
                ASTUnaryOperatorKind::AddressOf => HIRUnaryOperatorKind::AddressOf,
                ASTUnaryOperatorKind::Dereference => HIRUnaryOperatorKind::Dereference,
            };
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::Unary(HIRUnaryExpression {
                    right: Box::new(right),
                    operator_kind,
                    span: ast.span,
                }),
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::As(ast) => {
            let expression = build_hir_expression(
                id_alloc,
                top_level_table,
                reference_table,
                &ast.expression,
                diagnostics,
            );
            let type_ref = typename_to_type_ref(&ast.typename, reference_table);
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::As(HIRAsExpression {
                    expression: Box::new(expression),
                    type_ref,
                    span: ast.span,
                }),
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::Call(ast) => {
            let expression = build_hir_expression(
                id_alloc,
                top_level_table,
                reference_table,
                &ast.expression,
                diagnostics,
            );
            let args = ast
                .args
                .iter()
                .map(|arg| {
                    build_hir_expression(
                        id_alloc,
                        top_level_table,
                        reference_table,
                        &arg.expression,
                        diagnostics,
                    )
                })
                .collect();
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::Call(HIRCallExpression {
                    expression: Box::new(expression),
                    args,
                    span: ast.span,
                }),
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::Member(ast) => {
            let expression = build_hir_expression(
                id_alloc,
                top_level_table,
                reference_table,
                &ast.expression,
                diagnostics,
            );
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::Member(HIRMemberExpression {
                    expression: Box::new(expression),
                    member: ast.member.clone(),
                    span: ast.span,
                }),
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::Paren(ast) => build_hir_expression(
            id_alloc,
            top_level_table,
            reference_table,
            &ast.expression,
            diagnostics,
        ),
        ASTExpressionKind::IdReference(ast) => {
            let expression_kind = match reference_table.symbol_references.get(&ast.id) {
                Some(symbol_ref) => match symbol_ref.kind {
                    SymbolReferenceKind::Function { function } => {
                        HIRExpressionKind::FunctionRef(HIRFunctionRef {
                            reference: ast.reference,
                            function,
                            span: ast.span,
                        })
                    }
                    SymbolReferenceKind::Param { function, index } => {
                        HIRExpressionKind::ParamRef(HIRParamRef {
                            reference: ast.reference,
                            function,
                            index,
                            span: ast.span,
                        })
                    }
                    SymbolReferenceKind::Variable {
                        function,
                        scope,
                        index,
                    } => {
                        let variable = &reference_table.function_scopes[&function]
                            .scope_table
                            .scopes[&scope]
                            .variables[index];
                        HIRExpressionKind::VariableRef(HIRVariableRef {
                            id: ast.id,
                            reference: ast.reference,
                            function,
                            scope,
                            index,
                            statement: variable.id,
                            span: ast.span,
                        })
                    }
                },
                None => HIRExpressionKind::UnknownRef(HIRUnknownRef {
                    id: ast.id,
                    reference: ast.reference,
                    span: ast.span,
                }),
            };
            let expression = HIRExpression {
                id: ast_id,
                kind: expression_kind,
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::StructLiteral(ast) => {
            let type_ref = typename_to_type_ref(&ast.typename, reference_table);
            let fields = ast
                .fields
                .iter()
                .map(|field| {
                    let expression = build_hir_expression(
                        id_alloc,
                        top_level_table,
                        reference_table,
                        &field.expression,
                        diagnostics,
                    );
                    HIRStructLiteralField {
                        name: field.name.clone(),
                        expression,
                        span: field.span,
                    }
                })
                .collect();
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::StructLiteral(HIRStructLiteral {
                    type_ref,
                    fields,
                    span: ast.span,
                }),
                span: ast.span,
            };
            expression
        }
        ASTExpressionKind::Literal(ast) => {
            let expression = HIRExpression {
                id: ast_id,
                kind: HIRExpressionKind::Literal(ast.literal.clone()),
                span: ast.span,
            };
            expression
        }
    }
}

fn typename_to_type_ref(
    typename: &Typename,
    reference_table: &ReferenceTable,
) -> Option<TypeReference> {
    reference_table.type_references.get(&typename.id).cloned()
}
