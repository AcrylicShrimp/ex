use crate::LowTokenLiteralKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LowTokenKind {
    Unknown,
    Whitespace,
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
    Eq,           // "="
    Bang,         // "!"
    Lt,           // "<"
    Gt,           // ">"
    Plus,         // "+"
    Minus,        // "-"
    Star,         // "*"
    Slash,        // "/"
    Percent,      // "%"
    Or,           // "|"
    And,          // "&"
    Caret,        // "^"
    Tilde,        // "~"
    Id,           // identifier or keyword
    Literal { kind: LowTokenLiteralKind },
}
