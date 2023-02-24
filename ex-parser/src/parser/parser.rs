use crate::{Lookup, Token, TokenKind, TokenLiteralKind};
use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin, SubDiagnostics};
use ex_span::{SourceFile, Span};
use std::sync::{mpsc::Sender, Arc};

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    first: Lookup,
    second: Lookup,
    tok: T,
    file: Arc<SourceFile>,
    diagnostics: Arc<Sender<Diagnostics>>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(mut tok: T, file: Arc<SourceFile>, diagnostics: Arc<Sender<Diagnostics>>) -> Self {
        let first = tok.next();

        Self {
            first: Lookup::new(first),
            second: Lookup::new(tok.next()),
            tok,
            file,
            diagnostics,
        }
    }

    pub fn span(&self) -> Span {
        self.first
            .token()
            .as_ref()
            .map_or_else(|| self.file.span_end(), |token| token.span)
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
            check_token(token, &self.file, &self.diagnostics);
        }

        self.first = self.second.take();
        self.second = Lookup::new(self.tok.next());
    }
}

fn check_token(token: &Token, file: &Arc<SourceFile>, diagnostics: &Sender<Diagnostics>) {
    match token.kind {
        TokenKind::Unknown { .. } => {
            diagnostics
                .send(Diagnostics {
                    level: DiagnosticsLevel::Error,
                    message: format!("unknown token found"),
                    origin: Some(DiagnosticsOrigin {
                        file: file.clone(),
                        span: token.span,
                    }),
                    sub_diagnostics: vec![SubDiagnostics {
                        level: DiagnosticsLevel::Hint,
                        message: format!("remove this token"),
                        origin: Some(DiagnosticsOrigin {
                            file: file.clone(),
                            span: token.span,
                        }),
                    }],
                })
                .unwrap();
        }
        TokenKind::Literal(literal) => {
            if literal.suffix.is_some() {
                diagnostics
                    .send(Diagnostics {
                        level: DiagnosticsLevel::Error,
                        message: format!("unknown suffix found"),
                        origin: Some(DiagnosticsOrigin {
                            file: file.clone(),
                            span: token.span,
                        }),
                        sub_diagnostics: vec![SubDiagnostics {
                            level: DiagnosticsLevel::Hint,
                            message: format!("remove the suffix"),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: token.span,
                            }),
                        }],
                    })
                    .unwrap();
            }

            match literal.kind {
                TokenLiteralKind::Character { terminated }
                | TokenLiteralKind::String { terminated }
                    if !terminated =>
                {
                    diagnostics
                        .send(Diagnostics {
                            level: DiagnosticsLevel::Error,
                            message: format!("unterminated literal found"),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: token.span,
                            }),
                            sub_diagnostics: vec![SubDiagnostics {
                                level: DiagnosticsLevel::Hint,
                                message: format!("add a closing delimiter"),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: token.span,
                                }),
                            }],
                        })
                        .unwrap();
                }
                _ => {}
            }
        }
        _ => {}
    }
}
