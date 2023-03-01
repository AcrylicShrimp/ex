use std::fmt::Display;

use ex_symbol::Symbol;

#[derive(Debug, Clone)]
pub enum Value {
    Empty,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Callable(Symbol),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Boolean(value) => *value,
            _ => unreachable!(),
        }
    }

    pub fn as_integer(&self) -> i64 {
        match self {
            Value::Integer(value) => *value,
            _ => unreachable!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            Value::Float(value) => *value,
            _ => unreachable!(),
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            Value::String(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn as_callable(&self) -> Symbol {
        match self {
            Value::Callable(value) => *value,
            _ => unreachable!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Empty => write!(f, "empty"),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::Integer(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Callable(value) => write!(f, "{}", value),
        }
    }
}
