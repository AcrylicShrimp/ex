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
    Load {
        pointer: Pointer,
        temporary: TemporaryId,
    },
    Store {
        pointer: Pointer,
        temporary: TemporaryId,
        operator: Option<AssignmentOperator>,
    },
    Assign {
        temporary: TemporaryId,
        expression: Expression,
    },
    Extract {
        from: TemporaryId,
        indices: Vec<usize>,
        temporary: TemporaryId,
    },
    Jump {
        block: BlockId,
        args: Vec<TemporaryId>,
    },
    Branch {
        condition: TemporaryId,
        then_block: BlockId,
        then_args: Vec<TemporaryId>,
        else_block: BlockId,
        else_args: Vec<TemporaryId>,
    },
    Terminate {
        temporary: Option<TemporaryId>,
    },
}

impl InstructionKind {
    pub fn load(pointer: Pointer, temporary: TemporaryId) -> Self {
        Self::Load { pointer, temporary }
    }

    pub fn store(
        pointer: Pointer,
        temporary: TemporaryId,
        operator: Option<AssignmentOperator>,
    ) -> Self {
        Self::Store {
            pointer,
            temporary,
            operator,
        }
    }

    pub fn assign(temporary: TemporaryId, expression: Expression) -> Self {
        Self::Assign {
            temporary,
            expression,
        }
    }

    pub fn extract(from: TemporaryId, indices: Vec<usize>, temporary: TemporaryId) -> Self {
        Self::Extract {
            from,
            indices,
            temporary,
        }
    }

    pub fn jump(block: BlockId, args: Vec<TemporaryId>) -> Self {
        Self::Jump { block, args }
    }

    pub fn branch(
        condition: TemporaryId,
        then_block: BlockId,
        then_args: Vec<TemporaryId>,
        else_block: BlockId,
        else_args: Vec<TemporaryId>,
    ) -> Self {
        Self::Branch {
            condition,
            then_block,
            then_args,
            else_block,
            else_args,
        }
    }

    pub fn terminate(temporary: Option<TemporaryId>) -> Self {
        Self::Terminate { temporary }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Pointer {
    pub variable: VariableId,
    pub indices: Vec<usize>,
}

impl Pointer {
    pub fn new(variable: VariableId, indices: Vec<usize>) -> Self {
        Self { variable, indices }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AssignmentOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Shl,
    Shr,
    BitOr,
    BitAnd,
    BitXor,
}
