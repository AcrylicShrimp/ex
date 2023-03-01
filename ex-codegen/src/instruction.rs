use crate::{BlockId, Expression, InstructionId, TemporaryId, VariableId};

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
        variable: VariableId,
        temporary: TemporaryId,
    },
    Assign {
        temporary: TemporaryId,
        expression: Expression,
    },
    Jump {
        block: BlockId,
    },
    Branch {
        condition: TemporaryId,
        then_block: BlockId,
        else_block: BlockId,
    },
    Terminate {
        temporary: Option<TemporaryId>,
    },
}

impl InstructionKind {
    pub fn store(variable: VariableId, temporary: TemporaryId) -> Self {
        Self::Store {
            variable,
            temporary,
        }
    }

    pub fn assign(temporary: TemporaryId, expression: Expression) -> Self {
        Self::Assign {
            temporary,
            expression,
        }
    }

    pub fn jump(block: BlockId) -> Self {
        Self::Jump { block }
    }

    pub fn branch(condition: TemporaryId, then_block: BlockId, else_block: BlockId) -> Self {
        Self::Branch {
            condition,
            then_block,
            else_block,
        }
    }

    pub fn terminate(temporary: Option<TemporaryId>) -> Self {
        Self::Terminate { temporary }
    }
}
