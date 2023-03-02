use crate::{Instruction, InstructionId, InstructionIdAllocator, InstructionKind};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct InstructionTable {
    pub instructions: HashMap<InstructionId, Instruction>,
    instruction_id_allocator: InstructionIdAllocator,
}

impl InstructionTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, kind: InstructionKind) -> InstructionId {
        let id = self.instruction_id_allocator.allocate();
        self.instructions.insert(id, Instruction::new(id, kind));
        id
    }
}
