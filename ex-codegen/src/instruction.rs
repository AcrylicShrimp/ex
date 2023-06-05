use crate::{InstructionId, Pointer, TemporaryId};

#[derive(Debug, Clone, Hash)]
pub struct Instruction {
    pub id: InstructionId,
    pub kind: InstructionKind,
}

impl Instruction {
    pub fn new(id: InstructionId, kind: InstructionKind) -> Self {
        Self { id, kind }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum InstructionKind {
    Store {
        pointer: Pointer,
        temporary: TemporaryId,
    },
    Expression {
        temporary: TemporaryId,
    },
}

impl InstructionKind {
    pub fn store(pointer: Pointer, temporary: TemporaryId) -> Self {
        Self::Store { pointer, temporary }
    }

    pub fn expression(temporary: TemporaryId) -> Self {
        Self::Expression { temporary }
    }
}
