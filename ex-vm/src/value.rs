use ex_codegen::{Program, TypeId, TypeKind};
use ex_symbol::Symbol;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Value {
    Empty,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Callable(Symbol),
    Struct(TypeId, Vec<Value>),
    Pointer(usize),
    Reference(usize),
}

impl Value {
    pub fn empty_from_type_id(program: &Program, type_id: TypeId) -> Self {
        match &program.type_table.types[&type_id] {
            TypeKind::Empty => Value::Empty,
            TypeKind::Boolean => Value::Empty,
            TypeKind::Integer => Value::Empty,
            TypeKind::Float => Value::Empty,
            TypeKind::String => Value::Empty,
            TypeKind::Callable { .. } => Value::Empty,
            TypeKind::UserTypeStruct { symbol } => {
                let fields = program.user_type_struct_table[symbol].fields.clone();
                let values = fields
                    .iter()
                    .map(|field| Value::empty_from_type_id(program, field.type_id))
                    .collect();
                Value::Struct(type_id, values)
            }
            TypeKind::Pointer { .. } => Value::Empty,
            TypeKind::Reference { .. } => Value::Empty,
        }
    }

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

    pub fn as_struct(&self) -> (TypeId, &Vec<Value>) {
        match self {
            Value::Struct(type_id, fields) => (*type_id, fields),
            _ => unreachable!(),
        }
    }

    pub fn as_struct_mut(&mut self) -> (TypeId, &mut Vec<Value>) {
        match self {
            Value::Struct(type_id, fields) => (*type_id, fields),
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
            Value::Struct(_, values) => {
                write!(f, "(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, ")")
            }
            Value::Pointer(ptr) => write!(f, "ptr {}", ptr),
            Value::Reference(ptr) => write!(f, "ref {}", ptr),
        }
    }
}
