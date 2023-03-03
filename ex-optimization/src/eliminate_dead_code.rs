use ex_codegen::{BlockId, Function, InstructionKind};
use std::collections::HashSet;

pub fn eliminate_dead_code(function: &mut Function) {
    loop {
        let unreachable_blocks = compute_unreachable_blocks(function);

        if unreachable_blocks.is_empty() {
            break;
        }

        for block_id in unreachable_blocks {
            function.block_table.blocks.remove(&block_id);
        }
    }
}

fn compute_unreachable_blocks(function: &Function) -> Vec<BlockId> {
    let mut visited = HashSet::new();
    let mut worklist = vec![function.entry_block_id];

    while let Some(block_id) = worklist.pop() {
        if visited.contains(&block_id) {
            continue;
        }

        visited.insert(block_id);

        let block = &function.block_table.blocks[&block_id];
        let last_instruction = if let Some(last_instruction) = block.instructions.last() {
            *last_instruction
        } else {
            continue;
        };

        match block.instruction_table.instructions[&last_instruction].kind {
            InstructionKind::Jump { block, .. } => {
                worklist.push(block);
            }
            InstructionKind::Branch {
                then_block,
                else_block,
                ..
            } => {
                worklist.push(then_block);
                worklist.push(else_block);
            }
            _ => {}
        }
    }

    function
        .block_table
        .blocks
        .keys()
        .filter(|block_id| !visited.contains(block_id))
        .copied()
        .collect()
}
