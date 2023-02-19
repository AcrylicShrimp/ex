#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LowTokenNumberLiteralKind {
    IntegerBinary,
    IntegerOctal,
    IntegerHexadecimal,
    IntegerDecimal,
    Float,
}
