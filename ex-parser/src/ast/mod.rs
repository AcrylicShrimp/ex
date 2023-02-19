use crate::TokenLiteral;
use ex_span::Span;
use ex_symbol::Symbol;

#[derive(Debug, Clone, Copy, Hash)]
pub struct Id {
    pub symbol: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTProgram {
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTTopLevel {
    pub kind: ASTTopLevelKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTTopLevelKind {
    Function(ASTFunction),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFunction {
    pub signature: ASTFunctionSignature,
    pub body_block: ASTBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFunctionSignature {
    pub keyword_fn: Id,
    pub name: Id,
    pub paren_open: Id,
    pub parameters: Vec<ASTFunctionParameter>,
    pub paren_close: Id,
    pub return_type: Option<ASTFunctionReturnType>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFunctionParameter {
    pub name: Id,
    pub colon: Id,
    pub typename: Id,
    pub comma: Option<Id>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFunctionReturnType {
    pub colon: Id,
    pub typename: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStatement {
    pub kind: ASTStatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTStatementKind {
    Let(ASTLet),
    Block(ASTBlock),
    If(ASTIf),
    Return(ASTReturn),
    Assignment(ASTAssignment),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTLet {
    pub keyword_let: Id,
    pub name: Id,
    pub let_type: Option<ASTLetType>,
    pub let_expression: Option<ASTLetExpression>,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTLetType {
    pub colon: Id,
    pub typename: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTLetExpression {
    pub assignment: Id,
    pub expression: ASTExpression,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTBlock {
    pub brace_open: Id,
    pub statements: Vec<ASTStatement>,
    pub brace_close: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTIf {
    pub keyword_if: Id,
    pub condition: ASTExpression,
    pub body_block: ASTBlock,
    pub single_else_if: Vec<ASTSingleElseIf>,
    pub single_else: Option<ASTSingleElse>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTSingleElseIf {
    pub keyword_else: Id,
    pub keyword_if: Id,
    pub condition: ASTExpression,
    pub body_block: ASTBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTSingleElse {
    pub keyword_else: Id,
    pub body_block: ASTBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTReturn {
    pub keyword_return: Id,
    pub expression: Option<ASTExpression>,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTAssignment {
    pub left: ASTExpression,
    pub operator: Id,
    pub right: ASTExpression,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTExpressionKind {
    Binary(ASTBinaryExpression),
    Literal(ASTLiteral),
    Id(Id),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTBinaryExpression {
    pub left: Box<ASTExpression>,
    pub operator: Id,
    pub right: Box<ASTExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTLiteral {
    pub literal: TokenLiteral,
    pub span: Span,
}
