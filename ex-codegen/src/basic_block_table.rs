use crate::{BasicBlock, BlockId};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct BasicBlockTable {
    pub blocks: HashMap<BlockId, BasicBlock>,
}

impl BasicBlockTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, block: BasicBlock) {
        self.blocks.insert(block.id, block);
    }
}
