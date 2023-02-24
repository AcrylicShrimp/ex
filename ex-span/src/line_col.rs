#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: u32,
    pub column: u32,
}

impl LineCol {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}
