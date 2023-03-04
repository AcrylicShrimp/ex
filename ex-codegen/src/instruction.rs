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
        operator: Option<AssignmentOperator>,
    },
    Assign {
        temporary: TemporaryId,
        expression: Expression,
    },
    Jump {
        block: BlockId,
        arguments: Vec<TemporaryId>,
    },
    Branch {
        condition: TemporaryId,
        then_block: BlockId,
        then_arguments: Vec<TemporaryId>,
        else_block: BlockId,
        else_arguments: Vec<TemporaryId>,
    },
    Terminate {
        temporary: Option<TemporaryId>,
    },
}

impl InstructionKind {
    pub fn store(
        variable: VariableId,
        temporary: TemporaryId,
        operator: Option<AssignmentOperator>,
    ) -> Self {
        Self::Store {
            variable,
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

    pub fn jump(block: BlockId, arguments: Vec<TemporaryId>) -> Self {
        Self::Jump { block, arguments }
    }

    pub fn branch(
        condition: TemporaryId,
        then_block: BlockId,
        then_arguments: Vec<TemporaryId>,
        else_block: BlockId,
        else_arguments: Vec<TemporaryId>,
    ) -> Self {
        Self::Branch {
            condition,
            then_block,
            then_arguments,
            else_block,
            else_arguments,
        }
    }

    pub fn terminate(temporary: Option<TemporaryId>) -> Self {
        Self::Terminate { temporary }
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
    BitNot,
}
