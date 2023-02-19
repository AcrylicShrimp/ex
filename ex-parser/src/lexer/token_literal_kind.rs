#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenLiteralKind {
    Bool,
    IntegerBinary,
    IntegerOctal,
    IntegerHexadecimal,
    IntegerDecimal,
    Float,
    Character { terminated: bool },
    String { terminated: bool },
}
