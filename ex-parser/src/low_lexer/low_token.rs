use crate::LowTokenKind;

#[derive(Debug, Clone, Copy, Hash)]
pub struct LowToken {
    pub kind: LowTokenKind,
    pub len: u32,
}

impl LowToken {
    pub fn new(kind: LowTokenKind, len: u32) -> Self {
        Self { kind, len }
    }
}
