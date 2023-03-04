mod node_id;
mod node_id_allocator;

pub use node_id::*;
pub use node_id_allocator::*;

use crate::TokenLiteral;
use ex_span::Span;
use ex_symbol::Symbol;

#[derive(Debug, Clone, Hash)]
pub struct Typename {
    pub id: NodeId,
    pub kind: TypenameKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum TypenameKind {
    Id(Id),
    Function(TypenameFunction),
}

#[derive(Debug, Clone, Hash)]
pub struct TypenameFunction {
    pub keyword_fn: Id,
    pub paren_open: Id,
    pub parameters: Vec<TypenameFunctionParameter>,
    pub paren_close: Id,
    pub return_type: Option<TypenameFunctionReturnType>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct TypenameFunctionParameter {
    pub typename: Box<Typename>,
    pub comma: Option<Id>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct TypenameFunctionReturnType {
    pub arrow: Id,
    pub typename: Box<Typename>,
    pub span: Span,
}

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
    pub id: NodeId,
    pub kind: ASTTopLevelKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTTopLevelKind {
    Function(ASTFunction),
    Struct(ASTStruct),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFunction {
    pub signature: ASTFunctionSignature,
    pub body_block: ASTBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStruct {
    pub keyword_struct: Id,
    pub name: Id,
    pub brace_open: Id,
    pub fields: Vec<ASTStructField>,
    pub brace_close: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStructField {
    pub name: Id,
    pub colon: Id,
    pub typename: Typename,
    pub semicolon: Id,
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
    pub typename: Typename,
    pub comma: Option<Id>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFunctionReturnType {
    pub arrow: Id,
    pub typename: Typename,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStatement {
    pub id: NodeId,
    pub kind: ASTStatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTStatementKind {
    Block(ASTBlock),
    Let(ASTLet),
    If(ASTIf),
    Loop(ASTLoop),
    While(ASTWhile),
    Break(ASTBreak),
    Continue(ASTContinue),
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
    pub typename: Typename,
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
    pub expression: ASTExpression,
    pub body_block: ASTBlock,
    pub single_else_ifs: Vec<ASTSingleElseIf>,
    pub single_else: Option<ASTSingleElse>,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTLoop {
    pub keyword_loop: Id,
    pub body_block: ASTBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTWhile {
    pub keyword_while: Id,
    pub expression: ASTExpression,
    pub body_block: ASTBlock,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTSingleElseIf {
    pub keyword_else: Id,
    pub keyword_if: Id,
    pub expression: ASTExpression,
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
pub struct ASTBreak {
    pub keyword_break: Id,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTContinue {
    pub keyword_continue: Id,
    pub semicolon: Id,
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
    pub left: ASTAssignmentLeft,
    pub operator: Id,
    pub operator_kind: Option<ASTAssignmentOperatorKind>,
    pub right: ASTExpression,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ASTAssignmentOperatorKind {
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
    BitNot,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTAssignmentLeft {
    pub id: NodeId,
    pub expression: ASTExpression,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTRow {
    pub expression: ASTExpression,
    pub semicolon: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExpression {
    pub id: NodeId,
    pub kind: ASTExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTExpressionKind {
    Binary(ASTBinaryExpression),
    Unary(ASTUnaryExpression),
    As(ASTAsExpression),
    Call(ASTCallExpression),
    Paren(ASTParenExpression),       // single
    Literal(Literal),                // single
    IdReference(ASTIdReference),     // single
    StructLiteral(ASTStructLiteral), // single
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
    Plus,
    Minus,
    BitNot,
    LogNot,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTAsExpression {
    pub expression: Box<ASTExpression>,
    pub keyword_as: Id,
    pub typename: Typename,
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

#[derive(Debug, Clone, Hash)]
pub struct ASTIdReference {
    pub id: NodeId,
    pub reference: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStructLiteral {
    pub typename: Typename,
    pub brace_open: Id,
    pub fields: Vec<ASTStructLiteralField>,
    pub brace_close: Id,
    pub span: Span,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStructLiteralField {
    pub name: Id,
    pub colon: Id,
    pub expression: ASTExpression,
    pub comma: Option<Id>,
    pub span: Span,
}
