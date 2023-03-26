use crate::{
    BasicBlock, BasicBlockTable, BlockId, BlockIdAllocator, TypeId, VariableId, VariableTable,
};
use ex_symbol::Symbol;
use std::iter::empty as iter_empty;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<FunctionParam>,
    pub return_type_id: TypeId,
    pub variable_table: VariableTable,
    pub block_table: BasicBlockTable,
    block_id_allocator: BlockIdAllocator,
    pub entry_block_id: BlockId,
}

impl Function {
    pub fn new(name: Symbol, param_type_ids: Vec<TypeId>, return_type_id: TypeId) -> Self {
        let mut variable_table = VariableTable::new();
        let params = param_type_ids
            .into_iter()
            .map(|type_id| FunctionParam::new(type_id, variable_table.insert(type_id)))
            .collect();

        let mut block_table = BasicBlockTable::new();
        let mut block_id_allocator = BlockIdAllocator::new();

        let entry_block_id = block_id_allocator.allocate();
        let entry_block = BasicBlock::new(entry_block_id, iter_empty());
        block_table.blocks.insert(entry_block_id, entry_block);

        Self {
            name,
            params,
            return_type_id,
            variable_table,
            block_table,
            block_id_allocator,
            entry_block_id,
        }
    }

    pub fn new_variable(&mut self, type_id: TypeId) -> VariableId {
        self.variable_table.insert(type_id)
    }

    pub fn new_block(&mut self, params: impl Iterator<Item = TypeId>) -> BasicBlock {
        let id = self.block_id_allocator.allocate();
        let block = BasicBlock::new(id, params);
        block
    }

    pub fn insert_block(&mut self, block: BasicBlock) {
        self.block_table.insert(block)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub type_id: TypeId,
    pub variable_id: VariableId,
}

impl FunctionParam {
    pub fn new(type_id: TypeId, variable_id: VariableId) -> Self {
        Self {
            type_id,
            variable_id,
        }
    }
}
