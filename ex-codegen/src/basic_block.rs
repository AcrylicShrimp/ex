use crate::{BlockId, InstructionId, TemporaryId};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub temporaries: HashSet<TemporaryId>,
    pub instructions: Vec<InstructionId>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            temporaries: HashSet::new(),
            instructions: Vec::new(),
        }
    }
}
