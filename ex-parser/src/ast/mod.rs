use crate::TokenLiteral;
use ex_span::Span;
use ex_symbol::Symbol;

#[derive(Debug, Clone, Copy, Hash)]
pub struct Id {
    pub symbol: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct Literal {
    pub literal: TokenLiteral,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTProgram {
    pub top_levels: Vec<ASTTopLevel>,
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
    Block(ASTBlock),
    Let(ASTLet),
    If(ASTIf),
    Return(ASTReturn),
    Assignment(ASTAssignment),
    Row(ASTRow),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTLet {
    pub keyword_let: Id,
    pub name: Id,
    pub let_type: Option<ASTLetType>,
    pub let_assignment: Option<ASTLetAssignment>,
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
pub struct ASTLetAssignment {
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
    pub single_else_ifs: Vec<ASTSingleElseIf>,
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
    pub operator_kind: ASTAssignmentOperatorKind,
    pub right: ASTExpression,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ASTAssignmentOperatorKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BitOr,
    BitAnd,
    BitXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTRow {
    pub expression: ASTExpression,
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
    Unary(ASTUnaryExpression),
    As(ASTAsExpression),
    Call(ASTCallExpression),
    Paren(ASTParenExpression), // single
    Literal(Literal),          // single
    Id(Id),                    // single
}

#[derive(Debug, Clone, Hash)]
pub struct ASTBinaryExpression {
    pub left: Box<ASTExpression>,
    pub operator: Id,
    pub operator_kind: ASTBinaryOperatorKind,
    pub right: Box<ASTExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ASTBinaryOperatorKind {
    Eq,     // Precedence 1 : compare
    Ne,     // Precedence 1 : compare
    Lt,     // Precedence 1 : compare
    Gt,     // Precedence 1 : compare
    Le,     // Precedence 1 : compare
    Ge,     // Precedence 1 : compare
    LogOr,  // Precedence 2 : logical_or_and
    LogAnd, // Precedence 2 : logical_or_and
    Add,    // Precedence 3 : arithmetic_add_sub
    Sub,    // Precedence 3 : arithmetic_add_sub
    Mul,    // Precedence 4 : arithmetic_mul_div_mod
    Div,    // Precedence 4 : arithmetic_mul_div_mod
    Mod,    // Precedence 4 : arithmetic_mul_div_mod
    Pow,    // Precedence 5 : arithmetic_pow
    Shl,    // Precedence 6 : bit_shift
    Shr,    // Precedence 6 : bit_shift
    BitOr,  // Precedence 7 : bit_or_and_xor
    BitAnd, // Precedence 7 : bit_or_and_xor
    BitXor, // Precedence 7 : bit_or_and_xor
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUnaryExpression {
    pub operator: Id,
    pub operator_kind: ASTUnaryOperatorKind,
    pub right: Box<ASTExpression>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ASTUnaryOperatorKind {
    BitNot,
    LogNot,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTAsExpression {
    pub expression: Box<ASTExpression>,
    pub keyword_as: Id,
    pub typename: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTCallExpression {
    pub expression: Box<ASTExpression>,
    pub paren_open: Id,
    pub arguments: Vec<ASTArgumentExpression>,
    pub paren_close: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTArgumentExpression {
    pub expression: ASTExpression,
    pub comma: Option<Id>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTParenExpression {
    pub paren_open: Id,
    pub expression: Box<ASTExpression>,
    pub paren_close: Id,
    pub span: Span,
}
