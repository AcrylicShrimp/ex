use crate::Pos;
use std::cmp::{max, min};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub low: Pos,
    pub high: Pos,
}

impl Span {
    pub const ZERO: Span = Span {
        low: Pos::ZERO,
        high: Pos::ZERO,
    };

    pub fn new(low: impl Into<Pos>, high: impl Into<Pos>) -> Self {
        let low = low.into();
        let high = high.into();
        debug_assert!(low <= high);

        Self { low, high }
    }

    pub fn len(self) -> u32 {
        self.high.get() - self.low.get()
    }

    pub fn contains_pos(self, other: Pos) -> bool {
        self.low <= other && self.high >= other
    }

    pub fn contains_span(self, other: Span) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    pub fn to(self, to: Span) -> Span {
        debug_assert!(self.low <= to.high);

        Span {
            low: self.low,
            high: to.high,
        }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            low: min(self.low, other.low),
            high: max(self.high, other.high),
        }
    }
}
