use crate::{Lookup, Token, TokenKind, TokenLiteralKind};
use ex_diagnostics::DiagnosticsSender;
use ex_span::Span;

pub struct Parser<'d, T>
where
    T: Iterator<Item = Token>,
{
    first: Lookup,
    second: Lookup,
    tok: T,
    diagnostics: &'d DiagnosticsSender,
}

impl<'d, T> Parser<'d, T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(mut tok: T, diagnostics: &'d DiagnosticsSender) -> Self {
        let first = tok.next();

        Self {
            first: Lookup::new(first),
            second: Lookup::new(tok.next()),
            tok,
            diagnostics,
        }
    }

    pub fn span(&self) -> Span {
        self.first
            .token()
            .as_ref()
            .map_or_else(|| self.diagnostics.file().span_end(), |token| token.span)
    }

    pub fn first(&self) -> &Lookup {
        &self.first
    }

    pub fn second(&self) -> &Lookup {
        &self.second
    }

    pub fn is_exists(&self) -> bool {
        self.first.is_exists()
    }

    pub fn consume(&mut self) {
        if let Some(token) = self.first.token() {
            check_token(&token, &self.diagnostics);
        }

        self.first = self.second.take();
        self.second = Lookup::new(self.tok.next());
    }
}

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
