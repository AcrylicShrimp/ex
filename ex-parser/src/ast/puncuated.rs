use crate::Token;

pub const PUNCUATION_KIND_COMMA: u8 = PuncuationKind::Comma as u8; // ,

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PuncuationKind {
    Comma,
}

// TODO: T must implement Parse to be able to parse the puncuated items
#[derive(Debug, Clone, Hash)]
pub struct Puncuated<T: std::fmt::Debug + Clone + std::hash::Hash, const KIND: u8> {
    pub items: Vec<PuncuatedItem<T>>,
}

#[derive(Debug, Clone, Hash)]
pub enum PuncuatedItem<T: std::fmt::Debug + Clone + std::hash::Hash> {
    Puncuanated { item: T, puncuation: Token },
    NotPuncuated { item: T },
}
