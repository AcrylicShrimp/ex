use crate::{
    BasicBlock, BasicBlockTable, BlockId, BlockIdAllocator, InstructionId, InstructionKind,
    InstructionTable, TemporaryTable, TypeId, VariableId, VariableTable,
};
use ex_symbol::Symbol;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub parameters: Vec<FunctionParameter>,
    pub return_type_id: TypeId,
    pub instruction_table: InstructionTable,
    pub temporary_table: TemporaryTable,
    pub variable_table: VariableTable,
    pub block_table: BasicBlockTable,
    block_id_allocator: BlockIdAllocator,
    pub entry_block: BlockId,
    pub exit_block: Option<BlockId>,
}

impl Function {
    pub fn new(name: Symbol, parameter_type_ids: Vec<TypeId>, return_type_id: TypeId) -> Self {
        let mut variable_table = VariableTable::new();
        let parameters = parameter_type_ids
            .into_iter()
            .map(|type_id| FunctionParameter::new(type_id, variable_table.insert(type_id)))
            .collect();

        let mut block_table = BasicBlockTable::new();
        let mut block_id_allocator = BlockIdAllocator::new();

        let entry_block_id = block_id_allocator.allocate();
        let entry_block = BasicBlock::new(entry_block_id);
        block_table.blocks.insert(entry_block_id, entry_block);

        Self {
            name,
            parameters,
            return_type_id,
            instruction_table: InstructionTable::new(),
            temporary_table: TemporaryTable::new(),
            variable_table,
            block_table,
            block_id_allocator,
            entry_block: entry_block_id,
            exit_block: None,
        }
    }

    pub fn new_block(&mut self) -> BasicBlock {
        let id = self.block_id_allocator.allocate();
        let block = BasicBlock::new(id);
        block
    }

    pub fn new_instruction(
        &mut self,
        block: &mut BasicBlock,
        kind: InstructionKind,
    ) -> InstructionId {
        let id = self.instruction_table.insert(kind);
        block.instructions.push(id);
        id
    }

    pub fn new_instruction_indirect(
        &mut self,
        block_id: BlockId,
        kind: InstructionKind,
    ) -> InstructionId {
        let id = self.instruction_table.insert(kind);
        self.block_table
            .blocks
            .get_mut(&block_id)
            .unwrap()
            .instructions
            .push(id);
        id
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub type_id: TypeId,
    pub variable_id: VariableId,
}

impl FunctionParameter {
    pub fn new(type_id: TypeId, variable_id: VariableId) -> Self {
        Self {
            type_id,
            variable_id,
        }
    }
}
