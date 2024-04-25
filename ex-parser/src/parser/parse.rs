use crate::{new::ASTTypenamePrefixSegment, Lookup, Token, TokenKind, TokenLiteralKind};
use ex_diagnostics::DiagnosticsSender;
use ex_span::Span;
use ex_symbol::Symbol;

/// Defines how the lexer should interpret the next token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexicalContext {
    /// Normal context; no special rules apply.
    Normal,
    /// Isolates `<` and `>` from shift operators.
    Typename,
}

/// A stream of tokens.
pub trait TokenStream {
    fn next(&mut self, context: LexicalContext) -> Option<Token>;
}

/// A cursor that can peek and consume tokens from a stream.
pub struct Cursor<'d, T>
where
    T: TokenStream,
{
    token: Option<Token>,
    stream: T,
    diagnostics: &'d DiagnosticsSender,
}

impl<'d, T> Cursor<'d, T>
where
    T: TokenStream,
{
    pub fn new(stream: T, diagnostics: &'d DiagnosticsSender) -> Self {
        Self {
            token: None,
            stream,
            diagnostics,
        }
    }

    /// Peek a new token from the stream. The token is cached and will be returned again
    /// in subsequent `peek` or `next` methods.
    pub fn peek(&mut self, context: LexicalContext) -> Lookup {
        match self.token.clone() {
            Some(token) => Lookup::new(Some(token)),
            None => {
                self.token = self.next_token(context);
                Lookup::new(self.token.clone())
            }
        }
    }

    /// Gets a new token from the stream. Note that the retuned token may not be fresh.
    /// It drops the cached token to read fresh one in subsequent `peek` or `next` methods.
    pub fn next(&mut self, context: LexicalContext) -> Lookup {
        match self.token.take() {
            Some(token) => Lookup::new(Some(token)),
            None => Lookup::new(self.next_token(context)),
        }
    }

    fn next_token(&mut self, context: LexicalContext) -> Option<Token> {
        let token = self.stream.next(context);

        if let Some(token) = token.clone() {
            check_token(&token, self.diagnostics);
        }

        token
    }
}

/// Checks if the given token is valid. Emits a diagnostic if it is not.
fn check_token(token: &Token, diagnostics: &DiagnosticsSender) {
    match token.kind {
        TokenKind::Unknown { .. } => {
            diagnostics.error_sub(
                token.span,
                format!("unknown token found"),
                vec![diagnostics.sub_hint(token.span, format!("remove this token"))],
            );
        }
        TokenKind::Literal(literal) => {
            if literal.suffix.is_some() {
                diagnostics.error_sub(
                    token.span,
                    format!("unknown suffix found"),
                    vec![diagnostics.sub_hint(token.span, format!("remove this suffix"))],
                );
            }

            match literal.kind {
                TokenLiteralKind::Character { terminated }
                | TokenLiteralKind::String { terminated }
                    if !terminated =>
                {
                    diagnostics.error_sub(
                        token.span,
                        format!("unterminated literal found"),
                        vec![diagnostics.sub_hint(token.span, format!("add a closing delimiter"))],
                    );
                }
                _ => {}
            }
        }
        _ => {}
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub expected: Vec<TokenKind>,
    pub found: Option<Token>,
}

pub trait Parse
where
    Self: Sized,
{
    const START_TOKENS: &'static [TokenKind];

    fn parse(
        cursor: &mut Cursor<impl TokenStream>,
        context: LexicalContext,
    ) -> Result<Self, ParseError>;
}

impl Parse for ASTTypenamePrefixSegment {
    const START_TOKENS: &'static [TokenKind] = &[];

    fn parse(
        cursor: &mut Cursor<impl TokenStream>,
        context: LexicalContext,
    ) -> Result<Self, ParseError> {
        let identifier = cursor.next(context);
        let identifier = identifier.id().ok_or_else(|| ParseError {
            expected: vec![TokenKind::Id {
                symbol: Symbol::EMPTY,
            }],
            found: identifier.token().clone(),
        })?;

        let token_path_sep = cursor.next(context);
        let token_path_sep = token_path_sep
            .token_kind(TokenKind::PathSep)
            .ok_or_else(|| ParseError {
                expected: vec![TokenKind::PathSep],
                found: token_path_sep.token().clone(),
            })?;

        Ok(Self {
            identifier,
            token_path_sep,
        })
    }
}
