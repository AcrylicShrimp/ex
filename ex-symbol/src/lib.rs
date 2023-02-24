mod chunk;
mod interner;
mod symbol;

pub use chunk::*;
pub use interner::*;
pub use symbol::*;

use lazy_static::lazy_static;
use parking_lot::Mutex;
use std::fmt::{Debug, Display};

lazy_static! {
    pub(crate) static ref STR_INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}

impl Symbol {
    pub fn from_str(str: impl AsRef<str>) -> Self {
        STR_INTERNER.lock().intern(str)
    }

    pub fn to_str(self) -> &'static str {
        STR_INTERNER.lock().str(self)
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.to_str())
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.to_str())
    }
}

impl From<Symbol> for &'static str {
    fn from(symbol: Symbol) -> Self {
        symbol.to_str()
    }
}

impl From<Symbol> for String {
    fn from(symbol: Symbol) -> Self {
        symbol.to_str().to_owned()
    }
}

impl From<&'static str> for Symbol {
    fn from(str: &'static str) -> Self {
        Symbol::from_str(str)
    }
}

impl From<String> for Symbol {
    fn from(string: String) -> Self {
        Symbol::from_str(string)
    }
}
