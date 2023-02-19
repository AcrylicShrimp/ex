mod token;
mod token_kind;
mod token_literal;
mod token_literal_kind;

pub use token::*;
pub use token_kind::*;
pub use token_literal::*;
pub use token_literal_kind::*;

use crate::{
    low_token_iter, LowToken, LowTokenKind, LowTokenLiteralKind, LowTokenNumberLiteralKind,
};
use ex_span::{Pos, SourceFile, Span};
use ex_symbol::Symbol;
use std::iter::from_fn as iter_from_fn;

pub fn token_iter(file: &SourceFile) -> impl Iterator<Item = Token> + '_ {
    let mut iter = unglued_token_iter(file);
    let mut current = iter.next();
    let mut next = iter.next();

    iter_from_fn(move || {
        let mut token = match current.take() {
            Some(token) => token,
            None => return None,
        };

        while let Some(next_token) = next.take() {
            if let Some(glued) = token.glue(&next_token) {
                next = iter.next();
                token = glued;
            } else {
                next = Some(next_token);
                break;
            }
        }

        current = next.take();
        next = iter.next();
        Some(token)
    })
}

fn unglued_token_iter(file: &SourceFile) -> impl Iterator<Item = Token> + '_ {
    let mut low = file.span().low;
    let mut iter = low_token_iter(file.content());

    iter_from_fn(move || loop {
        let token = match iter.next() {
            Some(token) => token,
            None => return None,
        };
        let length = token.len;
        let token = convert(token, low, file);

        low += length;

        match token {
            Some(token) => {
                return Some(token);
            }
            None => {}
        }
    })
}

fn convert(token: LowToken, low: Pos, file: &SourceFile) -> Option<Token> {
    let span = Span::new(low, low + token.len);
    let kind = match token.kind {
        LowTokenKind::Whitespace | LowTokenKind::Comment => return None,
        LowTokenKind::Unknown => TokenKind::Unknown,
        LowTokenKind::OpenParen => TokenKind::OpenParen,
        LowTokenKind::CloseParen => TokenKind::CloseParen,
        LowTokenKind::OpenBrace => TokenKind::OpenBrace,
        LowTokenKind::CloseBrace => TokenKind::CloseBrace,
        LowTokenKind::OpenBracket => TokenKind::OpenBracket,
        LowTokenKind::CloseBracket => TokenKind::CloseBracket,
        LowTokenKind::Dot => TokenKind::Dot,
        LowTokenKind::Comma => TokenKind::Comma,
        LowTokenKind::Colon => TokenKind::Colon,
        LowTokenKind::Semicolon => TokenKind::Semicolon,
        LowTokenKind::Eq => TokenKind::Eq,
        LowTokenKind::Bang => TokenKind::LogNot,
        LowTokenKind::Lt => TokenKind::Lt,
        LowTokenKind::Gt => TokenKind::Gt,
        LowTokenKind::Plus => TokenKind::Add,
        LowTokenKind::Minus => TokenKind::Sub,
        LowTokenKind::Star => TokenKind::Mul,
        LowTokenKind::Slash => TokenKind::Div,
        LowTokenKind::Percent => TokenKind::Mod,
        LowTokenKind::Or => TokenKind::BitOr,
        LowTokenKind::And => TokenKind::BitAnd,
        LowTokenKind::Caret => TokenKind::BitXor,
        LowTokenKind::Tilde => TokenKind::BitNot,
        LowTokenKind::Id => match file.slice(span) {
            "true" => TokenKind::Literal(TokenLiteral::new(
                TokenLiteralKind::Bool,
                Symbol::from_str("true"),
                None,
            )),
            "false" => TokenKind::Literal(TokenLiteral::new(
                TokenLiteralKind::Bool,
                Symbol::from_str("false"),
                None,
            )),
            id => TokenKind::Id {
                symbol: Symbol::from_str(id),
            },
        },
        LowTokenKind::Literal { kind } => {
            let content = file.slice(span);
            let literal = match kind {
                LowTokenLiteralKind::Number { kind, suffix_start } => {
                    let suffix_start = suffix_start as usize;
                    let kind = match kind {
                        LowTokenNumberLiteralKind::IntegerBinary => TokenLiteralKind::IntegerBinary,
                        LowTokenNumberLiteralKind::IntegerOctal => TokenLiteralKind::IntegerOctal,
                        LowTokenNumberLiteralKind::IntegerHexadecimal => {
                            TokenLiteralKind::IntegerHexadecimal
                        }
                        LowTokenNumberLiteralKind::IntegerDecimal => {
                            TokenLiteralKind::IntegerDecimal
                        }
                        LowTokenNumberLiteralKind::Float => TokenLiteralKind::Float,
                    };
                    let non_prefix_content = Symbol::from_str(&content[..suffix_start]);
                    let suffix = if suffix_start == content.len() {
                        None
                    } else {
                        Some(Symbol::from_str(&content[suffix_start..]))
                    };
                    TokenLiteral::new(kind, non_prefix_content, suffix)
                }
                LowTokenLiteralKind::Character { terminated } => TokenLiteral::new(
                    TokenLiteralKind::Character { terminated },
                    Symbol::from_str(content),
                    None,
                ),
                LowTokenLiteralKind::String { terminated } => TokenLiteral::new(
                    TokenLiteralKind::Character { terminated },
                    Symbol::from_str(content),
                    None,
                ),
            };
            TokenKind::Literal(literal)
        }
    };
    Some(Token::new(kind, span))
}
