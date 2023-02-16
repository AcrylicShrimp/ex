use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos(u32);

impl Pos {
    pub const ZERO: Self = Self(0);

    pub fn new(pos: u32) -> Self {
        Self(pos)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

impl From<u32> for Pos {
    fn from(pos: u32) -> Self {
        Self(pos)
    }
}

impl From<Pos> for u32 {
    fn from(pos: Pos) -> Self {
        pos.0
    }
}

impl Add<Self> for Pos {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::new(self.0 + other.0)
    }
}

impl Add<u32> for Pos {
    type Output = Self;

    fn add(self, other: u32) -> Self {
        Self::new(self.0 + other)
    }
}

impl Add<Pos> for u32 {
    type Output = Pos;

    fn add(self, other: Pos) -> Pos {
        Pos::new(self + other.0)
    }
}

impl Sub<Self> for Pos {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::new(self.0 - other.0)
    }
}

impl Sub<u32> for Pos {
    type Output = Self;

    fn sub(self, other: u32) -> Self {
        Self::new(self.0 - other)
    }
}

impl Sub<Pos> for u32 {
    type Output = Pos;

    fn sub(self, other: Pos) -> Pos {
        Pos::new(self - other.0)
    }
}
