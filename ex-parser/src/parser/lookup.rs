use crate::{Id, Literal, Token, TokenKind};
use ex_symbol::Symbol;

#[derive(Debug, Clone, Hash)]
pub struct Lookup {
    token: Option<Token>,
}

impl Lookup {
    pub fn new(token: Option<Token>) -> Self {
        Self { token }
    }

    pub fn token(&self) -> &Option<Token> {
        &self.token
    }

    pub fn is_exists(&self) -> bool {
        self.token.is_some()
    }

    pub fn is_kind(&self, kind: TokenKind) -> bool {
        self.token
            .as_ref()
            .and_then(|token| Some(token.kind == kind))
            .unwrap_or_default()
    }

    pub fn is_keyword(&self, keyword: Symbol) -> bool {
        self.id()
            .and_then(|id| Some(id.symbol == keyword))
            .unwrap_or_default()
    }

    pub fn kind(&self, kind: TokenKind) -> Option<Id> {
        self.token.as_ref().and_then(|token| {
            if token.kind == kind {
                Some(Id {
                    symbol: kind.into_symbol(),
                    span: token.span,
                })
            } else {
                None
            }
        })
    }

    pub fn keyword(&self, keyword: Symbol) -> Option<Id> {
        self.id()
            .and_then(|id| if id.symbol == keyword { Some(id) } else { None })
    }

    pub fn id(&self) -> Option<Id> {
        self.token.as_ref().and_then(|token| {
            if let TokenKind::Id { symbol } = token.kind {
                Some(Id {
                    symbol,
                    span: token.span,
                })
            } else {
                None
            }
        })
    }

    pub fn literal(&self) -> Option<Literal> {
        self.token.as_ref().and_then(|token| {
            if let TokenKind::Literal(literal) = token.kind {
                Some(Literal {
                    literal,
                    span: token.span,
                })
            } else {
                None
            }
        })
    }

    pub fn take(&mut self) -> Self {
        Self::new(self.token.take())
    }
}
