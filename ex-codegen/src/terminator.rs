use crate::{BlockId, TemporaryId};

#[derive(Debug, Clone, Hash)]
pub enum Terminator {
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

impl Terminator {
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
