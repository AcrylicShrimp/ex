use std::ops::{Add, AddAssign, Sub, SubAssign};

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

impl AddAssign for Pos {
    fn add_assign(&mut self, other: Self) {
        *self = Self::new(self.0 + other.0);
    }
}

impl AddAssign<u32> for Pos {
    fn add_assign(&mut self, other: u32) {
        *self = Self::new(self.0 + other);
    }
}

impl AddAssign<Pos> for u32 {
    fn add_assign(&mut self, other: Pos) {
        *self += other.0;
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

impl SubAssign for Pos {
    fn sub_assign(&mut self, other: Self) {
        *self = Self::new(self.0 - other.0);
    }
}

impl SubAssign<u32> for Pos {
    fn sub_assign(&mut self, other: u32) {
        *self = Self::new(self.0 - other);
    }
}

impl SubAssign<Pos> for u32 {
    fn sub_assign(&mut self, other: Pos) {
        *self -= other.0;
    }
}
