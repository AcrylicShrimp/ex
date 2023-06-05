use ex_codegen::{FunctionId, Program, TypeId, TypeIdKind, VariableId};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Value {
    Empty,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Callable(FunctionId),
    Struct(TypeId, Vec<Value>),
    Pointer(VariableId, Vec<usize>),
    Reference(VariableId, Vec<usize>),
}

impl Value {
    pub fn empty_from_type_id(program: &Program, type_id: TypeId) -> Self {
        match &program.type_id_table.types[&type_id] {
            TypeIdKind::Empty => Value::Empty,
            TypeIdKind::Bool => Value::Empty,
            TypeIdKind::Int => Value::Empty,
            TypeIdKind::Float => Value::Empty,
            TypeIdKind::String => Value::Empty,
            TypeIdKind::Callable { .. } => Value::Empty,
            TypeIdKind::UserStruct { id } => {
                let fields = program.user_structs[id].fields.clone();
                let values = fields
                    .iter()
                    .map(|field| Value::empty_from_type_id(program, field.type_id))
                    .collect();
                Value::Struct(type_id, values)
            }
            TypeIdKind::Pointer { .. } => Value::Empty,
            TypeIdKind::Reference { .. } => Value::Empty,
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

    pub fn as_callable(&self) -> FunctionId {
        match self {
            Value::Callable(id) => *id,
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
            Value::Callable(id) => write!(f, "function[id={}]", id.get()),
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
            Value::Pointer(var, indices) => {
                write!(f, "ptr[var={}, indices={:?}]", var.get(), indices)
            }
            Value::Reference(var, indices) => {
                write!(f, "ref[var={}, indices={:?}]", var.get(), indices)
            }
        }
    }
}
