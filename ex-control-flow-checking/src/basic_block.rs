use ex_parser::NodeId;
use ex_span::Span;

use crate::BlockId;

#[derive(Debug, Clone, Hash)]
pub struct BasicBlock {
    pub id: BlockId,
    pub exit: Option<BasicBlockExit>, // NOTE: This field must be set before the entire graph is built.
    pub statements: Vec<(NodeId, Span)>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            exit: None,
            statements: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum BasicBlockExit {
    Terminate {
        has_value: bool,
    },
    Jump {
        block: BlockId,
    },
    Branch {
        left_block: BlockId,
        right_block: BlockId,
    },
}

impl BasicBlockExit {
    pub fn terminate(has_value: bool) -> Self {
        Self::Terminate { has_value }
    }

    pub fn jump(block: BlockId) -> Self {
        Self::Jump { block }
    }

    pub fn branch(left_block: BlockId, right_block: BlockId) -> Self {
        Self::Branch {
            left_block,
            right_block,
        }
    }
}
