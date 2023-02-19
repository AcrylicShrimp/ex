use crate::LowTokenNumberLiteralKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LowTokenLiteralKind {
    Number {
        kind: LowTokenNumberLiteralKind,
        suffix_start: u32,
    },
    Character {
        terminated: bool,
    },
    String {
        terminated: bool,
    },
}
