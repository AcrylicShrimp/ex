use crate::{NodeId, Puncuated, Token, TokenLiteral, PUNCUATION_KIND_COMMA};
use ex_span::Span;
use ex_symbol::Symbol;

#[derive(Debug, Clone, Copy, Hash)]
pub struct Id {
    pub span: Span,
    pub symbol: Symbol,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTProgram {
    pub id: NodeId,
    pub span: Span,
    pub top_levels: Vec<ASTTopLevel>,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTTopLevel {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTTopLevelKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTTopLevelKind {
    Use(ASTUse),
    TypeDef(ASTTypeDef),
    ModuleDef(ASTModuleDef),
    ExternBlockDef(ASTExternBlockDef),
    FnDef(ASTFnDef),
    StructDef(ASTStructDef),
    InterfaceDef(ASTInterfaceDef),
    ImplBlock(ASTImplBlock),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUse {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>, // pub
    pub keyword_use: Id,         // use
    pub path: ASTUsePath,        // self::super::identifier::*
    pub token_semicolon: Token,  // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePath {
    pub id: NodeId,
    pub span: Span,
    pub prefix: Option<ASTUsePathPrefix>, // self::super::identifier::
    pub item: ASTUsePathItem,             // * | identifier | { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePathPrefix {
    pub id: NodeId,
    pub span: Span,
    pub segments: Vec<ASTUsePathPrefixSegment>, // self::super::identifier::
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePathPrefixSegment {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTUsePathPrefixSegmentKind, // self | super | identifier
    pub token_path_sep: Token,             // ::
}

#[derive(Debug, Clone, Hash)]
pub enum ASTUsePathPrefixSegmentKind {
    Self_(Id),      // self
    Super_(Id),     // super
    Identifier(Id), // identifier
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePathItem {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTUsePathItemKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTUsePathItemKind {
    All(Token),                   // *
    Single(ASTUsePathItemSingle), // identifier | identifier as identifier
    Group(ASTUsePathItemGroup),   // { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePathItemSingle {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,                           // identifier
    pub alias: Option<ASTUsePathItemSingleAlias>, // as identifier
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePathItemSingleAlias {
    pub id: NodeId,
    pub span: Span,
    pub token_as: Token, // as
    pub identifier: Id,  // identifier
}

#[derive(Debug, Clone, Hash)]
pub struct ASTUsePathItemGroup {
    pub id: NodeId,
    pub span: Span,
    pub token_brace_open: Token, // {
    pub items: Puncuated<ASTUsePath, { PUNCUATION_KIND_COMMA }>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTTypeDef {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>, // pub
    pub keyword_type: Id,        // type
    pub identifier: Id,          // identifier
    pub token_equal: Token,      // =
    pub typename: ASTTypename,   // path::to::type::Type<...>
    pub token_semicolon: Token,  // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTModuleDef {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>, // pub
    pub keyword_module: Id,      // module
    pub identifier: Id,          // identifier
    pub token_brace_open: Token, // {
    pub top_levels: Vec<ASTTopLevel>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExternBlockDef {
    pub id: NodeId,
    pub span: Span,
    pub keyword_extern: Id,      // extern
    pub token_brace_open: Token, // {
    pub items: Vec<ASTExternBlockDefItem>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExternBlockDefItem {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTExternBlockDefItemKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTExternBlockDefItemKind {
    FnDecl(ASTFnDecl),
    FnDef(ASTFnDef),
    StructDef(ASTStructDef),
    ImplBlock(ASTImplBlock),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFnDecl {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>,                // pub
    pub keyword_fn: Id,                         // fn
    pub identifier: Id,                         // identifier
    pub generic_param: Option<ASTGenericParam>, // <...>
    pub token_paren_open: Token,                // (
    pub params: Puncuated<ASTFnParam, { PUNCUATION_KIND_COMMA }>,
    pub token_paren_close: Token,               // )
    pub result: Option<ASTFnResult>,            // -> path::to::type::Type<...>
    pub generic_where: Option<ASTGenericWhere>, // where ...
    pub token_semicolon: Token,                 // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFnDef {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>,                // pub
    pub keyword_fn: Id,                         // fn
    pub identifier: Id,                         // identifier
    pub generic_param: Option<ASTGenericParam>, // <...>
    pub token_paren_open: Token,                // (
    pub params: Puncuated<ASTFnParam, { PUNCUATION_KIND_COMMA }>,
    pub token_paren_close: Token,               // )
    pub result: Option<ASTFnResult>,            // -> path::to::type::Type<...>
    pub generic_where: Option<ASTGenericWhere>, // where ...
    pub stmt_block: ASTStmtBlock,               // { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFnParam {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,        // identifier
    pub token_colon: Token,    // :
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTFnResult {
    pub id: NodeId,
    pub span: Span,
    pub token_arrow: Token,    // ->
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStructDef {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>,                // pub
    pub keyword_struct: Id,                     // struct
    pub identifier: Id,                         // identifier
    pub generic_param: Option<ASTGenericParam>, // <...>
    pub generic_where: Option<ASTGenericWhere>, // where ...
    pub token_brace_open: Token,                // {
    pub fields: Puncuated<ASTStructDefField, { PUNCUATION_KIND_COMMA }>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStructDefField {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,        // identifier
    pub token_colon: Token,    // :
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTInterfaceDef {
    pub id: NodeId,
    pub span: Span,
    pub keyword_pub: Option<Id>,                // pub
    pub keyword_interface: Id,                  // interface
    pub identifier: Id,                         // identifier
    pub generic_param: Option<ASTGenericParam>, // <...>
    pub generic_where: Option<ASTGenericWhere>, // where ...
    pub token_brace_open: Token,                // {
    pub items: Vec<ASTFnDecl>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTImplBlock {
    pub id: NodeId,
    pub span: Span,
    pub keyword_impl: Id,                       // impl
    pub typename: ASTTypename,                  // path::to::type::Type<...>
    pub for_type: Option<ASTImplBlockForType>,  // for path::to::type::Type<...>
    pub generic_where: Option<ASTGenericWhere>, // where ...
    pub token_brace_open: Token,                // {
    pub items: Vec<ASTImplBlockItem>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTImplBlockForType {
    pub id: NodeId,
    pub span: Span,
    pub keyword_for: Id,       // for
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTImplBlockItem {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTImplBlockItemKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTImplBlockItemKind {
    FnDef(ASTFnDef),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericParam {
    pub id: NodeId,
    pub span: Span,
    pub token_angle_open: Token, // <
    pub items: Puncuated<ASTGenericParamItem, { PUNCUATION_KIND_COMMA }>,
    pub token_angle_close: Token, // >
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericParamItem {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id, // identifier
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericWhere {
    pub id: NodeId,
    pub span: Span,
    pub token_where: Token, // where
    pub items: Puncuated<ASTGenericWhereItem, { PUNCUATION_KIND_COMMA }>,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericWhereItem {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,                          // identifier
    pub token_colon: Token,                      // :
    pub condition: ASTGenericWhereItemCondition, // path::to::type::Type<...> + path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericWhereItemCondition {
    pub id: NodeId,
    pub span: Span,
    pub typename: ASTTypename, // path::to::type::Type<...>
    pub extra_items: Vec<ASTGenericWhereItemConditionItem>, // + path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericWhereItemConditionItem {
    pub id: NodeId,
    pub span: Span,
    pub token_plus: Token,     // +
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTGenericArg {
    pub id: NodeId,
    pub span: Span,
    pub token_angle_open: Token, // <
    pub items: Puncuated<ASTTypename, { PUNCUATION_KIND_COMMA }>,
    pub token_angle_close: Token, // >
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtBlock {
    pub id: NodeId,
    pub span: Span,
    pub token_brace_open: Token, // {
    pub stmts: Vec<ASTStmt>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtLet {
    pub id: NodeId,
    pub span: Span,
    pub keyword_let: Id,                      // let
    pub identifier: Id,                       // identifier
    pub typename: Option<ASTStmtLetTypename>, // : path::to::type::Type<...>
    pub expr: Option<ASTStmtLetExpr>,         // = expression
    pub token_semicolon: Token,               // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtLetTypename {
    pub id: NodeId,
    pub span: Span,
    pub token_colon: Token,    // :
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtLetExpr {
    pub id: NodeId,
    pub span: Span,
    pub token_equal: Token, // =
    pub expr: ASTExpr,      // expression
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtIf {
    pub id: NodeId,
    pub span: Span,
    pub keyword_if: Id,           // if
    pub expr: ASTExpr,            // expression
    pub stmt_block: ASTStmtBlock, // { ... }
    pub else_ifs: Vec<ASTStmtIfElseIf>,
    pub else_: Option<ASTStmtIfElse>,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtIfElseIf {
    pub id: NodeId,
    pub span: Span,
    pub keyword_else: Id,         // else
    pub keyword_if: Id,           // if
    pub expr: ASTExpr,            // expression
    pub stmt_block: ASTStmtBlock, // { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtIfElse {
    pub id: NodeId,
    pub span: Span,
    pub keyword_else: Id,         // else
    pub stmt_block: ASTStmtBlock, // { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtLoop {
    pub id: NodeId,
    pub span: Span,
    pub keyword_loop: Id,         // loop
    pub stmt_block: ASTStmtBlock, // { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtWhile {
    pub id: NodeId,
    pub span: Span,
    pub keyword_while: Id,        // while
    pub expr: ASTExpr,            // expression
    pub stmt_block: ASTStmtBlock, // { ... }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtBreak {
    pub id: NodeId,
    pub span: Span,
    pub keyword_break: Id,      // break
    pub token_semicolon: Token, // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtContinue {
    pub id: NodeId,
    pub span: Span,
    pub keyword_continue: Id,   // continue
    pub token_semicolon: Token, // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtReturn {
    pub id: NodeId,
    pub span: Span,
    pub keyword_return: Id,     // return
    pub token_semicolon: Token, // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtAssignment {
    pub id: NodeId,
    pub span: Span,
    pub expr: ASTExpr, // expression
    pub operator: ASTStmtAssignmentOperator,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtAssignmentOperator {
    pub id: NodeId,
    pub span: Span,
    pub operator: Id,
    pub kind: ASTStmtAssignmentOperatorKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTStmtAssignmentOperatorKind {
    Assignment, // =
    Add,        // +=
    Sub,        // -=
    Mul,        // *=
    Div,        // /=
    Mod,        // %=
    Pow,        // **=
    Shl,        // <<=
    Shr,        // >>=
    BitOr,      // |=
    BitAnd,     // &=
    BitXor,     // ^=
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmtExpr {
    pub id: NodeId,
    pub span: Span,
    pub expr: ASTExpr,          // expression
    pub token_semicolon: Token, // ;
}

#[derive(Debug, Clone, Hash)]
pub struct ASTStmt {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTStmtKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTStmtKind {
    Block(ASTStmtBlock),
    Let(ASTStmtLet),
    If(ASTStmtIf),
    Loop(ASTStmtLoop),
    While(ASTStmtWhile),
    Break(ASTStmtBreak),
    Continue(ASTStmtContinue),
    Return(ASTStmtReturn),
    Assignment(ASTStmtAssignment),
    Expr(ASTStmtExpr),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExpr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ASTExprKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTExprKind {
    StructLiteral(ASTExprStructLiteral),
    Literal(ASTExprLiteral),
    Ident(ASTExprIdent),
    Paren(ASTExprParen),
    Member(ASTExprMember),
    Call(ASTExprCall),
    As(ASTExprAs),
    Unary(ASTExprUnary),
    Binary(ASTExprBinary),
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprStructLiteral {
    pub id: NodeId,
    pub span: Span,
    pub typename: ASTTypename,   // path::to::type::Type<...>
    pub token_brace_open: Token, // {
    pub fields: Puncuated<ASTExprStructLiteralField, { PUNCUATION_KIND_COMMA }>,
    pub token_brace_close: Token, // }
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprStructLiteralField {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,     // identifier
    pub token_colon: Token, // :
    pub expr: ASTExpr,      // expression
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprLiteral {
    pub id: NodeId,
    pub span: Span,
    pub token: TokenLiteral,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprIdent {
    pub id: NodeId,
    pub span: Span,
    pub prefix: Option<ASTExprIdentPrefix>, // path::to::identifier::
    pub item: ASTExprIdentItem,             // identifier<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprIdentPrefix {
    pub id: NodeId,
    pub span: Span,
    pub segments: Vec<ASTExprIdentPrefixSegment>, // path::to::identifier::
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprIdentPrefixSegment {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,        // identifier
    pub token_path_sep: Token, // ::
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprIdentItem {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,                 // identifier
    pub generic: Option<ASTGenericArg>, // < ... >
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprParen {
    pub id: NodeId,
    pub span: Span,
    pub token_paren_open: Token, // (
    pub expr: Box<ASTExpr>,
    pub token_paren_close: Token, // )
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprMember {
    pub id: NodeId,
    pub span: Span,
    pub expr: Box<ASTExpr>,
    pub token_dot: Token, // .
    pub member: Id,       // identifier
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprCall {
    pub id: NodeId,
    pub span: Span,
    pub callee: ASTExprCallCallee,
    pub token_paren_open: Token, // (
    pub args: Puncuated<ASTExpr, { PUNCUATION_KIND_COMMA }>,
    pub token_paren_close: Token, // )
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprCallCallee {
    pub id: NodeId,
    pub span: Span,
    pub expr: Box<ASTExpr>,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprAs {
    pub id: NodeId,
    pub span: Span,
    pub expr: Box<ASTExpr>,
    pub token_as: Token,       // as
    pub typename: ASTTypename, // path::to::type::Type<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprUnary {
    pub id: NodeId,
    pub span: Span,
    pub operator: ASTExprUnaryOperator,
    pub operand_lhs: Box<ASTExpr>,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprUnaryOperator {
    pub id: NodeId,
    pub span: Span,
    pub token_operator: Token,
    pub kind: ASTExprUnaryOperatorKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTExprUnaryOperatorKind {
    Plus,
    Minus,
    BitNot,
    LogNot,
    AddressOf,
    Dereference,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprBinary {
    pub id: NodeId,
    pub span: Span,
    pub operand_lhs: Box<ASTExpr>,
    pub operator: ASTExprBinaryOperator,
    pub operand_rhs: Box<ASTExpr>,
}

#[derive(Debug, Clone, Hash)]
pub struct ASTExprBinaryOperator {
    pub id: NodeId,
    pub span: Span,
    pub token_operator: Token,
    pub kind: ASTExprBinaryOperatorKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ASTExprBinaryOperatorKind {
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
pub struct ASTTypename {
    pub id: NodeId,
    pub span: Span,
    pub prefix: Option<ASTTypenamePrefix>, // path::to::type::
    pub item: ASTTypenameItem,             // Typename<...>
}

#[derive(Debug, Clone, Hash)]
pub struct ASTTypenamePrefix {
    pub id: NodeId,
    pub span: Span,
    pub segments: Vec<ASTTypenamePrefixSegment>, // path::to::type::
}

#[derive(Debug, Clone, Hash)]
pub struct ASTTypenamePrefixSegment {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,        // identifier
    pub token_path_sep: Token, // ::
}

#[derive(Debug, Clone, Hash)]
pub struct ASTTypenameItem {
    pub id: NodeId,
    pub span: Span,
    pub identifier: Id,                 // identifier
    pub generic: Option<ASTGenericArg>, // < ... >
}
