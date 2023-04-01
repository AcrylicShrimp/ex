use crate::{
    cfg_checking::ControlFlowGraph, BasicBlock, BasicBlockExit, BlockId, FunctionControlFlowGraph,
    TopLevelTable,
};
use ex_diagnostics::DiagnosticsSender;
use std::collections::HashSet;

pub fn check_cfg(
    cfg: &mut ControlFlowGraph,
    top_level_table: &TopLevelTable,
    diagnostics: &DiagnosticsSender,
) {
    for (_, function) in &mut cfg.functions {
        validate(function, top_level_table, diagnostics);
    }
}

fn validate(
    cfg: &mut FunctionControlFlowGraph,
    top_level_table: &TopLevelTable,
    diagnostics: &DiagnosticsSender,
) {
    let function = &top_level_table.functions[&cfg.id];
    let unreachable_blocks = purge_unreachable_blocks(cfg);

    for unreachable_block in unreachable_blocks {
        let mut iter = unreachable_block
            .instructions
            .iter()
            .filter_map(|instruction| instruction.as_hir_statement());
        let last = iter.clone().last();
        let first = iter.next();
        let (first, last) = match (first, last) {
            (Some(first), Some(last)) => (first, last),
            _ => continue,
        };

        diagnostics.warning(
            first.to(last),
            format!("unreachable code in function {}", function.name.symbol),
        );
    }

    if let Some(exit_block_id) = cfg.exit_block_id {
        if cfg.blocks.contains_key(&exit_block_id) {
            if !function.return_type.is_empty() {
                // TODO: Add a span to the function return type.
                diagnostics.error(
                    function.name.span,
                    format!(
                        "function {} has a return type but does not return a value",
                        function.name.symbol
                    ),
                );
            }
        }
    }
}

fn compute_unreachable_blocks(cfg: &FunctionControlFlowGraph) -> HashSet<BlockId> {
    let mut reachable_blocks = HashSet::new();
    let mut worklist = vec![cfg.entry_block_id];

    while let Some(block_id) = worklist.pop() {
        if reachable_blocks.contains(&block_id) {
            continue;
        }

        reachable_blocks.insert(block_id);

        let block = cfg.blocks.get(&block_id).unwrap();
        match block.exit.as_ref().unwrap() {
            BasicBlockExit::Terminate { .. } => {}
            BasicBlockExit::Jump { block } => {
                worklist.push(*block);
            }
            BasicBlockExit::Branch {
                left_block,
                right_block,
            } => {
                worklist.push(*left_block);
                worklist.push(*right_block);
            }
        }
    }

    let mut unreachable_blocks = HashSet::new();

    for (block_id, _) in &cfg.blocks {
        if !reachable_blocks.contains(block_id) {
            unreachable_blocks.insert(*block_id);
        }
    }

    unreachable_blocks
}

fn purge_unreachable_blocks(cfg: &mut FunctionControlFlowGraph) -> Vec<BasicBlock> {
    let unreachable_blocks = compute_unreachable_blocks(cfg);
    unreachable_blocks
        .into_iter()
        .map(|block_id| cfg.blocks.remove(&block_id).unwrap())
        .collect()
}
