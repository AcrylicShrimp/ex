use crate::{FunctionId, TemporaryId, VariableId};

#[derive(Debug, Clone, Copy, Hash)]
pub enum Pointer {
    Function(FunctionId),
    Variable(VariableId),
    RawPointer(TemporaryId),
}

impl Pointer {
    pub fn function(function: FunctionId) -> Self {
        Self::Function(function)
    }

    pub fn variable(variable: VariableId) -> Self {
        Self::Variable(variable)
    }

    pub fn raw_pointer(temporary: TemporaryId) -> Self {
        Self::RawPointer(temporary)
    }
}
