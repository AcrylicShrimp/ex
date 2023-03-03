mod basic_block;
mod block_id;
mod block_id_allocator;
mod function_control_flow_graph;

pub use basic_block::*;
pub use block_id::*;
pub use block_id_allocator::*;
pub use function_control_flow_graph::*;

use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin};
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTIf, ASTLoop, ASTProgram,
    ASTStatementKind, ASTTopLevelKind, NodeId,
};
use ex_resolve_ref::{AssignmentLhsKind, AssignmentLhsTable, FunctionTable, SymbolReferenceTable};
use ex_span::SourceFile;
use std::sync::{mpsc::Sender, Arc};

pub fn check_control_flow(
    function_table: &FunctionTable,
    assignment_lhs_table: &AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTProgram,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    for top_level in &ast.top_levels {
        let function = function_table.functions.get(&top_level.id).unwrap();

        match &top_level.kind {
            ASTTopLevelKind::Function(ast_function) => {
                let function_cfg = build_function_cfg(
                    assignment_lhs_table,
                    symbol_reference_table,
                    top_level.id,
                    ast_function,
                    file,
                    diagnostics,
                );
                function_cfg.validate(function, file, diagnostics);
            }
        }
    }
}

fn build_function_cfg(
    assignment_lhs_table: &AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast_id: NodeId,
    ast: &ASTFunction,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> FunctionControlFlowGraph {
    let mut block_id_alloc = BlockIdAllocator::new();
    let mut function_cfg = FunctionControlFlowGraph::new(&mut block_id_alloc, ast_id, ast.span);

    let (inner_entry_id, mut inner_exit_block) = build_function_cfg_stmt_block(
        &mut block_id_alloc,
        &mut function_cfg,
        None,
        None,
        assignment_lhs_table,
        symbol_reference_table,
        &ast.body_block,
        file,
        diagnostics,
    );

    function_cfg
        .blocks
        .get_mut(&function_cfg.entry_block)
        .unwrap()
        .exit = Some(BasicBlockExit::jump(inner_entry_id));
    inner_exit_block.exit = Some(BasicBlockExit::terminate(false));
    function_cfg.exit_block = Some(inner_exit_block.id);
    function_cfg.insert_block(inner_exit_block);

    function_cfg
}

fn build_function_cfg_stmt_block(
    block_id_alloc: &mut BlockIdAllocator,
    cfg: &mut FunctionControlFlowGraph,
    loop_entry_block_id: Option<BlockId>,
    loop_exit_block_id: Option<BlockId>,
    assignment_lhs_table: &AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTBlock,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> (BlockId, BasicBlock) {
    let entry_id = block_id_alloc.allocate();
    let mut block = BasicBlock::new(entry_id);

    for statement in &ast.statements {
        block.statements.push((statement.id, statement.span));

        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                let (inner_entry_id, inner_exit_block) = build_function_cfg_stmt_block(
                    block_id_alloc,
                    cfg,
                    loop_entry_block_id,
                    loop_exit_block_id,
                    assignment_lhs_table,
                    symbol_reference_table,
                    stmt_block,
                    file,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_id));
                cfg.insert_block(block);

                block = inner_exit_block;
            }
            ASTStatementKind::Let(stmt_let) => {
                if let Some(let_assignment) = &stmt_let.let_assignment {
                    check_variable_usage(
                        cfg,
                        symbol_reference_table,
                        &let_assignment.expression,
                        file,
                        diagnostics,
                    );
                    cfg.initialized_variables.insert(statement.id);
                }
            }
            ASTStatementKind::If(stmt_if) => {
                let (inner_entry_id, inner_exit_block) = build_function_cfg_stmt_if(
                    block_id_alloc,
                    cfg,
                    loop_entry_block_id,
                    loop_exit_block_id,
                    assignment_lhs_table,
                    symbol_reference_table,
                    stmt_if,
                    file,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_id));
                cfg.insert_block(block);

                block = inner_exit_block;
            }
            ASTStatementKind::Loop(stmt_loop) => {
                let (inner_entry_id, inner_exit_block) = build_function_cfg_stmt_loop(
                    block_id_alloc,
                    cfg,
                    assignment_lhs_table,
                    symbol_reference_table,
                    stmt_loop,
                    file,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_id));
                cfg.insert_block(block);

                block = inner_exit_block;
            }
            ASTStatementKind::Break(stmt_break) => match loop_exit_block_id {
                Some(loop_exit_block_id) => {
                    block.exit = Some(BasicBlockExit::jump(loop_exit_block_id));
                    cfg.insert_block(block);

                    block = BasicBlock::new(block_id_alloc.allocate());
                }
                None => {
                    diagnostics
                        .send(Diagnostics {
                            level: DiagnosticsLevel::Error,
                            message: format!("break statement outside of loop"),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: stmt_break.span,
                            }),
                            sub_diagnostics: vec![],
                        })
                        .unwrap();
                }
            },
            ASTStatementKind::Continue(stmt_continue) => match loop_entry_block_id {
                Some(loop_entry_block_id) => {
                    block.exit = Some(BasicBlockExit::jump(loop_entry_block_id));
                    cfg.insert_block(block);

                    block = BasicBlock::new(block_id_alloc.allocate());
                }
                None => {
                    diagnostics
                        .send(Diagnostics {
                            level: DiagnosticsLevel::Error,
                            message: format!("continue statement outside of loop"),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: stmt_continue.span,
                            }),
                            sub_diagnostics: vec![],
                        })
                        .unwrap();
                }
            },
            ASTStatementKind::Return(stmt_return) => {
                if let Some(expression) = &stmt_return.expression {
                    check_variable_usage(
                        cfg,
                        symbol_reference_table,
                        &expression,
                        file,
                        diagnostics,
                    );
                }

                block.exit = Some(BasicBlockExit::terminate(stmt_return.expression.is_some()));
                cfg.insert_block(block);

                block = BasicBlock::new(block_id_alloc.allocate());
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                // NOTE: We don't need to check the left side of the assignment, because it's a variable that is being assigned to.
                check_variable_usage(
                    cfg,
                    symbol_reference_table,
                    &stmt_assignment.right,
                    file,
                    diagnostics,
                );

                if let Some(lhs_kind) = assignment_lhs_table.kinds.get(&stmt_assignment.left.id) {
                    if let AssignmentLhsKind::Variable { node } = lhs_kind {
                        cfg.initialized_variables.insert(*node);
                    }
                }
            }
            ASTStatementKind::Row(stmt_row) => {
                check_variable_usage(
                    cfg,
                    symbol_reference_table,
                    &stmt_row.expression,
                    file,
                    diagnostics,
                );
            }
        }
    }

    (entry_id, block)
}

fn build_function_cfg_stmt_if(
    block_id_alloc: &mut BlockIdAllocator,
    cfg: &mut FunctionControlFlowGraph,
    loop_entry_block_id: Option<BlockId>,
    loop_exit_block_id: Option<BlockId>,
    assignment_lhs_table: &AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTIf,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> (BlockId, BasicBlock) {
    let mut stack = Vec::new();

    check_variable_usage(
        cfg,
        symbol_reference_table,
        &ast.expression,
        file,
        diagnostics,
    );

    let mut block = BasicBlock::new(block_id_alloc.allocate());
    let (then_entry_id, mut then_exit_block) = build_function_cfg_stmt_block(
        block_id_alloc,
        cfg,
        loop_entry_block_id,
        loop_exit_block_id,
        assignment_lhs_table,
        symbol_reference_table,
        &ast.body_block,
        file,
        diagnostics,
    );
    let end_block = BasicBlock::new(block_id_alloc.allocate());
    block.exit = Some(BasicBlockExit::branch(then_entry_id, end_block.id));
    then_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
    cfg.insert_block(then_exit_block);
    stack.push((block, then_entry_id, end_block));

    for single_else_if in &ast.single_else_ifs {
        check_variable_usage(
            cfg,
            symbol_reference_table,
            &single_else_if.expression,
            file,
            diagnostics,
        );

        let mut block = BasicBlock::new(block_id_alloc.allocate());
        let (then_entry_id, mut then_exit_block) = build_function_cfg_stmt_block(
            block_id_alloc,
            cfg,
            loop_entry_block_id,
            loop_exit_block_id,
            assignment_lhs_table,
            symbol_reference_table,
            &single_else_if.body_block,
            file,
            diagnostics,
        );
        let end_block = BasicBlock::new(block_id_alloc.allocate());
        block.exit = Some(BasicBlockExit::branch(then_entry_id, end_block.id));
        then_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
        cfg.insert_block(then_exit_block);
        stack.push((block, then_entry_id, end_block));
    }

    if let Some(single_else) = &ast.single_else {
        let (else_entry_id, mut else_exit_block) = build_function_cfg_stmt_block(
            block_id_alloc,
            cfg,
            loop_entry_block_id,
            loop_exit_block_id,
            assignment_lhs_table,
            symbol_reference_table,
            &single_else.body_block,
            file,
            diagnostics,
        );
        let (block, then_entry_id, end_block) = stack.last_mut().unwrap();
        block.exit = Some(BasicBlockExit::branch(*then_entry_id, else_entry_id));
        else_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
        cfg.insert_block(else_exit_block);
    }

    let (mut last_block, _, mut last_end_block) = stack.pop().unwrap();

    while let Some((mut block, then_entry_id, end_block)) = stack.pop() {
        block.exit = Some(BasicBlockExit::branch(then_entry_id, last_block.id));
        last_end_block.exit = Some(BasicBlockExit::jump(end_block.id));

        cfg.insert_block(last_block);
        cfg.insert_block(last_end_block);

        last_block = block;
        last_end_block = end_block;
    }

    let entry_id = last_block.id;
    cfg.insert_block(last_block);

    (entry_id, last_end_block)
}

fn build_function_cfg_stmt_loop(
    block_id_alloc: &mut BlockIdAllocator,
    cfg: &mut FunctionControlFlowGraph,
    assignment_lhs_table: &AssignmentLhsTable,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTLoop,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> (BlockId, BasicBlock) {
    let entry_block_id = block_id_alloc.allocate();
    let mut entry_block = BasicBlock::new(entry_block_id);
    let exit_block = BasicBlock::new(block_id_alloc.allocate());

    let (inner_entry_id, mut inner_exit_block) = build_function_cfg_stmt_block(
        block_id_alloc,
        cfg,
        Some(entry_block_id),
        Some(exit_block.id),
        assignment_lhs_table,
        symbol_reference_table,
        &ast.body_block,
        file,
        diagnostics,
    );
    entry_block.exit = Some(BasicBlockExit::jump(inner_entry_id));
    cfg.insert_block(entry_block);

    inner_exit_block.exit = Some(BasicBlockExit::jump(entry_block_id));
    cfg.insert_block(inner_exit_block);

    (entry_block_id, exit_block)
}

fn check_variable_usage(
    cfg: &FunctionControlFlowGraph,
    symbol_reference_table: &SymbolReferenceTable,
    ast: &ASTExpression,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            check_variable_usage(
                cfg,
                symbol_reference_table,
                &expr_binary.left,
                file,
                diagnostics,
            );
            check_variable_usage(
                cfg,
                symbol_reference_table,
                &expr_binary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(expr_unary) => {
            check_variable_usage(
                cfg,
                symbol_reference_table,
                &expr_unary.right,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::As(expr_as) => {
            check_variable_usage(
                cfg,
                symbol_reference_table,
                &expr_as.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(expr_call) => {
            check_variable_usage(
                cfg,
                symbol_reference_table,
                &expr_call.expression,
                file,
                diagnostics,
            );

            for argument in &expr_call.arguments {
                check_variable_usage(
                    cfg,
                    symbol_reference_table,
                    &argument.expression,
                    file,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Paren(expr_paren) => {
            check_variable_usage(
                cfg,
                symbol_reference_table,
                &expr_paren.expression,
                file,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(expr_id_ref) => {
            if let Some(symbol_reference) = symbol_reference_table.references.get(&expr_id_ref.id) {
                if symbol_reference.kind.is_variable() {
                    if !cfg.initialized_variables.contains(&symbol_reference.node) {
                        diagnostics
                            .send(Diagnostics {
                                level: DiagnosticsLevel::Error,
                                message: format!(
                                    "variable {} is used before being initialized",
                                    expr_id_ref.reference.symbol
                                ),
                                origin: Some(DiagnosticsOrigin {
                                    file: file.clone(),
                                    span: ast.span,
                                }),
                                sub_diagnostics: vec![],
                            })
                            .unwrap();
                    }
                }
            }
        }
    }
}
