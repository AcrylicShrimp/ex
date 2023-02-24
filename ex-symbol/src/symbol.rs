#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub u32);

impl Symbol {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn new_const(index: u32) -> Self {
        Self(index)
    }
}

impl From<Symbol> for u32 {
    fn from(idx: Symbol) -> Self {
        idx.0
    }
}

impl From<u32> for Symbol {
    fn from(idx: u32) -> Self {
        Self::new(idx)
    }
}
