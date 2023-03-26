use crate::{
    BlockId, Instruction, InstructionId, InstructionKind, InstructionTable, TemporaryId,
    TemporaryTable, TypeId,
};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub params: Vec<TemporaryId>,
    pub temporary_table: TemporaryTable,
    pub instruction_table: InstructionTable,
    pub instructions: Vec<InstructionId>,
}

impl BasicBlock {
    pub fn new(id: BlockId, params: impl Iterator<Item = TypeId>) -> Self {
        let mut temporary_table = TemporaryTable::new();
        let params = params
            .map(|type_id| temporary_table.insert(type_id))
            .collect();

        Self {
            id,
            params,
            temporary_table,
            instruction_table: InstructionTable::new(),
            instructions: Vec::new(),
        }
    }

    pub fn new_temporary(&mut self, type_id: TypeId) -> TemporaryId {
        self.temporary_table.insert(type_id)
    }

    pub fn new_instruction(&mut self, kind: InstructionKind) -> InstructionId {
        let id = self.instruction_table.insert(kind);
        self.instructions.push(id);
        id
    }

    pub fn replace_instruction(&mut self, id: InstructionId, kind: InstructionKind) {
        self.instruction_table
            .instructions
            .insert(id, Instruction::new(id, kind));
    }
}
