use crate::TokenLiteral;
use ex_symbol::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Unknown { symbol: Symbol },
    Comment,      // "#"
    OpenParen,    // "("
    CloseParen,   // ")"
    OpenBrace,    // "{"
    CloseBrace,   // "}"
    OpenBracket,  // "["
    CloseBracket, // "]"
    Dot,          // "."
    Comma,        // ","
    Colon,        // ":"
    Semicolon,    // ";"
    Arrow,        // "->"
    // Assignment operators
    Assign,       // "="
    AssignAdd,    // "+="
    AssignSub,    // "-="
    AssignMul,    // "*="
    AssignDiv,    // "/="
    AssignMod,    // "%="
    AssignPow,    // "**="
    AssignShl,    // "<<="
    AssignShr,    // ">>="
    AssignBitOr,  // "|="
    AssignBitAnd, // "&="
    AssignBitXor, // "^="
    AssignBitNot, // "~="
    // Range operators
    Rng,          // ".."
    RngInclusive, // "..="
    // Cmp operators
    Eq, // "=="
    Ne, // "!="
    Lt, // "<"
    Gt, // ">"
    Le, // "<="
    Ge, // ">="
    // Binary operators
    Add,    // "+"
    Sub,    // "-"
    Mul,    // "*"
    Div,    // "/"
    Mod,    // "%"
    Pow,    // "**"
    Shl,    // "<<"
    Shr,    // ">>"
    BitOr,  // "|"
    BitAnd, // "&"
    BitXor, // "^"
    LogOr,  // "||"
    LogAnd, // "&&"
    // Unary operators
    BitNot, // "~"
    LogNot, // "!"
    // Module access operators
    ModuleMember, // "::"
    Id { symbol: Symbol },
    Literal(TokenLiteral),
}

impl TokenKind {
    pub fn into_symbol(self) -> Symbol {
        match self {
            TokenKind::Unknown { .. } => *crate::UNKNOWN,
            TokenKind::Comment => *crate::COMMENT,
            TokenKind::OpenParen => *crate::OPEN_PAREN,
            TokenKind::CloseParen => *crate::CLOSE_PAREN,
            TokenKind::OpenBrace => *crate::OPEN_BRACE,
            TokenKind::CloseBrace => *crate::CLOSE_BRACE,
            TokenKind::OpenBracket => *crate::OPEN_BRACKET,
            TokenKind::CloseBracket => *crate::CLOSE_BRACKET,
            TokenKind::Dot => *crate::DOT,
            TokenKind::Comma => *crate::COMMA,
            TokenKind::Colon => *crate::COLON,
            TokenKind::Semicolon => *crate::SEMICOLON,
            TokenKind::Arrow => *crate::ARROW,
            TokenKind::Assign => *crate::ASSIGN,
            TokenKind::AssignAdd => *crate::ASSIGN_ADD,
            TokenKind::AssignSub => *crate::ASSIGN_SUB,
            TokenKind::AssignMul => *crate::ASSIGN_MUL,
            TokenKind::AssignDiv => *crate::ASSIGN_DIV,
            TokenKind::AssignMod => *crate::ASSIGN_MOD,
            TokenKind::AssignPow => *crate::ASSIGN_POW,
            TokenKind::AssignShl => *crate::ASSIGN_SHL,
            TokenKind::AssignShr => *crate::ASSIGN_SHR,
            TokenKind::AssignBitOr => *crate::ASSIGN_BIT_OR,
            TokenKind::AssignBitAnd => *crate::ASSIGN_BIT_AND,
            TokenKind::AssignBitXor => *crate::ASSIGN_BIT_XOR,
            TokenKind::AssignBitNot => *crate::ASSIGN_BIT_NOT,
            TokenKind::Rng => *crate::RNG,
            TokenKind::RngInclusive => *crate::RNG_INCLUSIVE,
            TokenKind::Eq => *crate::EQ,
            TokenKind::Ne => *crate::NE,
            TokenKind::Lt => *crate::LT,
            TokenKind::Gt => *crate::GT,
            TokenKind::Le => *crate::LE,
            TokenKind::Ge => *crate::GE,
            TokenKind::Add => *crate::ADD,
            TokenKind::Sub => *crate::SUB,
            TokenKind::Mul => *crate::MUL,
            TokenKind::Div => *crate::DIV,
            TokenKind::Mod => *crate::MOD,
            TokenKind::Pow => *crate::POW,
            TokenKind::Shl => *crate::SHL,
            TokenKind::Shr => *crate::SHR,
            TokenKind::BitOr => *crate::BIT_OR,
            TokenKind::BitAnd => *crate::BIT_AND,
            TokenKind::BitXor => *crate::BIT_XOR,
            TokenKind::LogOr => *crate::LOG_OR,
            TokenKind::LogAnd => *crate::LOG_AND,
            TokenKind::BitNot => *crate::BIT_NOT,
            TokenKind::LogNot => *crate::LOG_NOT,
            TokenKind::ModuleMember => *crate::MODULE_MEMBER,
            TokenKind::Id { .. } => *crate::ID,
            TokenKind::Literal(..) => *crate::LITERAL,
        }
    }
}
