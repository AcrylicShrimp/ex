mod cursor;
mod low_token;
mod low_token_kind;
mod low_token_literal_kind;
mod low_token_number_literal_kind;

pub use cursor::*;
pub use low_token::*;
pub use low_token_kind::*;
pub use low_token_literal_kind::*;
pub use low_token_number_literal_kind::*;

use std::iter::from_fn as iter_from_fn;
use unicode_xid::UnicodeXID;

pub fn low_token_iter(mut input: &str) -> impl Iterator<Item = LowToken> + '_ {
    iter_from_fn(move || {
        if input.is_empty() {
            return None;
        }

        let token = next(input);
        input = &input[token.len as usize..];
        Some(token)
    })
}

fn next(input: impl AsRef<str>) -> LowToken {
    let mut cursor = Cursor::new(input.as_ref());
    let kind = match cursor.consume().unwrap() {
        char if char.is_whitespace() => {
            consume_while(&mut cursor, |char| char.is_whitespace());
            LowTokenKind::Whitespace
        }
        char if is_id_start(char) => {
            consume_while(&mut cursor, |char| is_id_continue(char));
            LowTokenKind::Id
        }
        char @ '0'..='9' => {
            let kind = consume_literal_number(&mut cursor, char);
            let suffix_start = cursor.len_consumed();

            if is_id_start(cursor.first()) {
                cursor.consume();
                consume_while(&mut cursor, |char| is_id_continue(char));
            }

            LowTokenKind::Literal {
                kind: LowTokenLiteralKind::Number { kind, suffix_start },
            }
        }
        '#' => {
            consume_while(&mut cursor, |char| char != '\n');
            LowTokenKind::Comment
        }
        '(' => LowTokenKind::OpenParen,
        ')' => LowTokenKind::CloseParen,
        '{' => LowTokenKind::OpenBrace,
        '}' => LowTokenKind::CloseBrace,
        '[' => LowTokenKind::OpenBracket,
        ']' => LowTokenKind::CloseBracket,
        '.' => LowTokenKind::Dot,
        ',' => LowTokenKind::Comma,
        ':' => LowTokenKind::Colon,
        ';' => LowTokenKind::Semicolon,
        '=' => LowTokenKind::Eq,
        '!' => LowTokenKind::Bang,
        '<' => LowTokenKind::Lt,
        '>' => LowTokenKind::Gt,
        '+' => LowTokenKind::Plus,
        '-' => LowTokenKind::Minus,
        '*' => LowTokenKind::Star,
        '/' => LowTokenKind::Slash,
        '%' => LowTokenKind::Percent,
        '|' => LowTokenKind::Or,
        '&' => LowTokenKind::And,
        '^' => LowTokenKind::Caret,
        '~' => LowTokenKind::Tilde,
        '\'' => LowTokenKind::Literal {
            kind: LowTokenLiteralKind::Character {
                terminated: consume_literal_character(&mut cursor),
            },
        },
        '"' => LowTokenKind::Literal {
            kind: LowTokenLiteralKind::String {
                terminated: consume_literal_string(&mut cursor),
            },
        },
        _ => LowTokenKind::Unknown,
    };
    LowToken::new(kind, cursor.len_consumed())
}

fn consume_while(cursor: &mut Cursor, mut pred: impl FnMut(char) -> bool) {
    while pred(cursor.first()) {
        cursor.consume();
    }
}

fn is_id_start(char: char) -> bool {
    ('a'..='z').contains(&char)
        || ('A'..='Z').contains(&char)
        || (char == '_')
        || (char > '\x7f' && char.is_xid_start())
}

fn is_id_continue(char: char) -> bool {
    ('a'..='z').contains(&char)
        || ('A'..='Z').contains(&char)
        || ('0'..='9').contains(&char)
        || (char == '_')
        || (char > '\x7f' && char.is_xid_continue())
}

fn consume_literal_number(cursor: &mut Cursor, first_char: char) -> LowTokenNumberLiteralKind {
    let kind = if first_char == '0' {
        match cursor.first() {
            'b' if cursor.second().is_digit(2) => {
                cursor.consume();
                consume_while(cursor, |char| char.is_digit(2));
                LowTokenNumberLiteralKind::IntegerBinary
            }
            'o' if cursor.second().is_digit(8) => {
                cursor.consume();
                consume_while(cursor, |char| char.is_digit(8));
                LowTokenNumberLiteralKind::IntegerOctal
            }
            'x' if cursor.second().is_digit(16) => {
                cursor.consume();
                consume_while(cursor, |char| char.is_digit(16));
                LowTokenNumberLiteralKind::IntegerHexadecimal
            }
            '0'..='9' => {
                cursor.consume();
                consume_while(cursor, |char| char.is_digit(10));
                LowTokenNumberLiteralKind::IntegerDecimal
            }
            '.' | 'e' | 'E' => LowTokenNumberLiteralKind::IntegerDecimal,
            _ => return LowTokenNumberLiteralKind::IntegerDecimal,
        }
    } else {
        LowTokenNumberLiteralKind::IntegerDecimal
    };

    if kind != LowTokenNumberLiteralKind::IntegerDecimal {
        return kind;
    }

    match cursor.first() {
        '.' if cursor.second().is_digit(10) => {
            cursor.consume();
            consume_while(cursor, |char| char.is_digit(10));

            match (cursor.first(), cursor.second(), cursor.lookup(2)) {
                ('e' | 'E', '+' | '-', digit) if digit.is_digit(10) => {
                    cursor.consume();
                    cursor.consume();
                    consume_while(cursor, |char| char.is_digit(10));
                }
                ('e' | 'E', digit, _) if digit.is_digit(10) => {
                    cursor.consume();
                    consume_while(cursor, |char| char.is_digit(10));
                }
                _ => {}
            }

            LowTokenNumberLiteralKind::Float
        }
        'e' | 'E'
            if match cursor.second() {
                '+' | '-' if cursor.lookup(2).is_digit(10) => true,
                digit if digit.is_digit(10) => true,
                _ => false,
            } =>
        {
            cursor.consume();

            match cursor.first() {
                '+' | '-' => {
                    cursor.consume();
                }
                _ => {}
            }

            consume_while(cursor, |char| char.is_digit(10));
            LowTokenNumberLiteralKind::Float
        }
        _ => {
            consume_while(cursor, |char| char.is_digit(10));
            LowTokenNumberLiteralKind::IntegerDecimal
        }
    }
}

fn consume_literal_character(cursor: &mut Cursor) -> bool {
    // Normal case e.g. 'a'
    if cursor.first() != '\\' && cursor.second() == '\'' {
        cursor.consume();
        cursor.consume();
        return true;
    }

    match cursor.first() {
        '\'' => {
            cursor.consume();
            return true;
        }
        '\\' => {
            cursor.consume();
            cursor.consume();

            if cursor.first() == '\'' {
                cursor.consume();
                return true;
            }

            return false;
        }
        _ => {
            cursor.consume();

            if cursor.first() == '\'' {
                cursor.consume();
                return true;
            }

            return false;
        }
    }
}

fn consume_literal_string(cursor: &mut Cursor) -> bool {
    while let Some(char) = cursor.consume() {
        match char {
            '"' => return true,
            '\\' => {
                cursor.consume();
            }
            _ => {}
        }
    }

    false
}
