use crate::{
    resolve::{ReferenceTable, TopLevelTable, TypeKind},
    type_inferencing::TypeTable,
    HIRBlock, HIRFunction, HIRIf, HIRLoop, HIRProgram, HIRStatementKind,
};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::NodeId;
use ex_span::Span;
use std::{collections::HashMap, num::NonZeroU64};

#[derive(Default, Debug, Clone)]
pub struct ControlFlowGraph {
    pub functions: HashMap<NodeId, FunctionControlFlowGraph>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionControlFlowGraph {
    pub id: NodeId,
    pub span: Span,
    pub entry_block_id: BlockId,
    pub exit_block_id: Option<BlockId>,
    pub blocks: HashMap<BlockId, BasicBlock>,
    pub stack_allocations: Vec<TypeKind>,
    next_id: u64,
}

impl FunctionControlFlowGraph {
    pub fn new(id: NodeId, span: Span) -> Self {
        let entry_block_id = BlockId::new(0);
        let entry_block = BasicBlock::new(entry_block_id);

        let blocks = HashMap::from_iter(vec![(entry_block_id, entry_block)]);

        Self {
            id,
            span,
            entry_block_id,
            exit_block_id: None,
            blocks,
            stack_allocations: Vec::new(),
            next_id: 0,
        }
    }

    pub fn new_block(&mut self) -> BasicBlock {
        self.next_id += 1;
        let block = BasicBlock::new(BlockId::new(self.next_id));
        block
    }

    pub fn insert_block(&mut self, block: BasicBlock) {
        self.blocks.insert(block.id, block);
    }
}

#[derive(Debug, Clone, Hash)]
pub struct BasicBlock {
    pub id: BlockId,
    pub exit: Option<BasicBlockExit>, // NOTE: This field must be set before the entire graph is built.
    pub instructions: Vec<BasicBlockInstruction>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            exit: None,
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(NonZeroU64);

impl BlockId {
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).unwrap())
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

#[derive(Debug, Clone, Hash)]
pub enum BasicBlockInstruction {
    HIRStatement { span: Span },
    DeallocateStack { type_kind: TypeKind },
}

#[derive(Debug, Clone, Hash)]
pub struct Scope {
    pub variables: Vec<TypeKind>,
}

#[derive(Debug, Clone, Hash)]
pub struct LoopContext {
    pub entry_block_id: BlockId,
    pub exit_block_id: BlockId,
    pub stack_count: usize,
}

pub fn build_cfg(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRProgram,
    diagnostics: &DiagnosticsSender,
) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();

    for function in &hir.functions {
        let function_cfg = build_cfg_function(
            top_level_table,
            reference_table,
            type_table,
            &function,
            diagnostics,
        );
        cfg.functions.insert(function.id, function_cfg);
    }

    cfg
}

fn build_cfg_function(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRFunction,
    diagnostics: &DiagnosticsSender,
) -> FunctionControlFlowGraph {
    let mut cfg = FunctionControlFlowGraph::new(hir.id, hir.span);
    let mut scope_stack = Vec::new();

    let (inner_entry_block_id, mut inner_exit_block) = build_cfg_stmt_block(
        &mut cfg,
        &mut scope_stack,
        None,
        top_level_table,
        reference_table,
        type_table,
        &hir.body_block,
        diagnostics,
    );
    cfg.blocks.get_mut(&cfg.entry_block_id).unwrap().exit =
        Some(BasicBlockExit::jump(inner_entry_block_id));
    inner_exit_block.exit = Some(BasicBlockExit::Terminate { has_value: false });
    cfg.exit_block_id = Some(inner_exit_block.id);
    cfg.insert_block(inner_exit_block);

    cfg
}

fn build_cfg_stmt_block(
    cfg: &mut FunctionControlFlowGraph,
    scope_stack: &mut Vec<Scope>,
    loop_context: Option<&LoopContext>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRBlock,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    scope_stack.push(Scope {
        variables: Vec::new(),
    });

    let mut block = cfg.new_block();
    let entry_block_id = block.id;

    for statement in &hir.statements {
        match &statement.kind {
            HIRStatementKind::Block(hir) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_block(
                    cfg,
                    scope_stack,
                    loop_context,
                    top_level_table,
                    reference_table,
                    type_table,
                    hir,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(block);

                block = inner_exit_block;
            }
            HIRStatementKind::Let(_) => {
                let type_kind = type_table
                    .types
                    .get(&statement.id)
                    .cloned()
                    .unwrap_or_else(|| TypeKind::Unknown);
                cfg.stack_allocations.push(type_kind.clone());
                scope_stack.last_mut().unwrap().variables.push(type_kind);
            }
            HIRStatementKind::If(hir) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_if(
                    cfg,
                    scope_stack,
                    loop_context,
                    top_level_table,
                    reference_table,
                    type_table,
                    hir,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(block);

                block = inner_exit_block;
            }
            HIRStatementKind::Loop(hir) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_loop(
                    cfg,
                    scope_stack,
                    top_level_table,
                    reference_table,
                    type_table,
                    hir,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(block);

                block = inner_exit_block;
            }
            HIRStatementKind::Break(hir) => match loop_context {
                Some(loop_context) => {
                    for scope in scope_stack[loop_context.stack_count..].iter().rev() {
                        for variable in scope.variables.iter().rev() {
                            block
                                .instructions
                                .push(BasicBlockInstruction::DeallocateStack {
                                    type_kind: variable.clone(),
                                });
                        }
                    }

                    block.exit = Some(BasicBlockExit::jump(loop_context.exit_block_id));
                    cfg.insert_block(block);

                    block = cfg.new_block();
                }
                None => {
                    diagnostics.error(hir.span, format!("break statement outside of loop"));
                }
            },
            HIRStatementKind::Continue(hir) => match loop_context {
                Some(loop_context) => {
                    for scope in scope_stack[loop_context.stack_count..].iter().rev() {
                        for variable in scope.variables.iter().rev() {
                            block
                                .instructions
                                .push(BasicBlockInstruction::DeallocateStack {
                                    type_kind: variable.clone(),
                                });
                        }
                    }

                    block.exit = Some(BasicBlockExit::jump(loop_context.entry_block_id));
                    cfg.insert_block(block);

                    block = cfg.new_block();
                }
                None => {
                    diagnostics.error(hir.span, format!("continue statement outside of loop"));
                }
            },
            HIRStatementKind::Return(hir) => {
                for variable in cfg.stack_allocations.iter().rev() {
                    block
                        .instructions
                        .push(BasicBlockInstruction::DeallocateStack {
                            type_kind: variable.clone(),
                        });
                }

                block.exit = Some(BasicBlockExit::terminate(hir.expression.is_some()));
                cfg.insert_block(block);

                block = cfg.new_block();
            }
            HIRStatementKind::Assignment(..) => {}
            HIRStatementKind::Row(..) => {}
        }
    }

    for variable in scope_stack.pop().unwrap().variables.into_iter().rev() {
        block
            .instructions
            .push(BasicBlockInstruction::DeallocateStack {
                type_kind: variable,
            });
    }

    (entry_block_id, block)
}

fn build_cfg_stmt_if(
    cfg: &mut FunctionControlFlowGraph,
    scope_stack: &mut Vec<Scope>,
    loop_context: Option<&LoopContext>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRIf,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    let mut stack = Vec::new();

    let mut block = cfg.new_block();
    let (then_entry_block_id, mut then_exit_block) = build_cfg_stmt_block(
        cfg,
        scope_stack,
        loop_context,
        top_level_table,
        reference_table,
        type_table,
        &hir.body_block,
        diagnostics,
    );
    let end_block = cfg.new_block();
    block.exit = Some(BasicBlockExit::branch(then_entry_block_id, end_block.id));
    then_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
    cfg.insert_block(then_exit_block);
    stack.push((block, then_entry_block_id, end_block));

    for hir in &hir.single_else_ifs {
        let mut block = cfg.new_block();
        let (then_entry_block_id, mut then_exit_block) = build_cfg_stmt_block(
            cfg,
            scope_stack,
            loop_context,
            top_level_table,
            reference_table,
            type_table,
            &hir.body_block,
            diagnostics,
        );
        let end_block = cfg.new_block();
        block.exit = Some(BasicBlockExit::branch(then_entry_block_id, end_block.id));
        then_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
        cfg.insert_block(then_exit_block);
        stack.push((block, then_entry_block_id, end_block));
    }

    if let Some(hir) = &hir.single_else {
        let (else_entry_block_id, mut else_exit_block) = build_cfg_stmt_block(
            cfg,
            scope_stack,
            loop_context,
            top_level_table,
            reference_table,
            type_table,
            &hir.body_block,
            diagnostics,
        );
        let (block, then_entry_block_id, end_block) = stack.last_mut().unwrap();
        block.exit = Some(BasicBlockExit::branch(
            *then_entry_block_id,
            else_entry_block_id,
        ));
        else_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
        cfg.insert_block(else_exit_block);
    }

    let (mut last_block, _, mut last_end_block) = stack.pop().unwrap();

    while let Some((mut block, then_entry_block_id, end_block)) = stack.pop() {
        block.exit = Some(BasicBlockExit::branch(then_entry_block_id, last_block.id));
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

fn build_cfg_stmt_loop(
    cfg: &mut FunctionControlFlowGraph,
    scope_stack: &mut Vec<Scope>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRLoop,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    let mut entry_block = cfg.new_block();
    let entry_block_id = entry_block.id;
    let exit_block = cfg.new_block();

    let loop_context = LoopContext {
        entry_block_id,
        exit_block_id: exit_block.id,
        stack_count: scope_stack.len(),
    };
    let (inner_entry_block_id, mut inner_exit_block) = build_cfg_stmt_block(
        cfg,
        scope_stack,
        Some(&loop_context),
        top_level_table,
        reference_table,
        type_table,
        &hir.body_block,
        diagnostics,
    );
    entry_block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
    cfg.insert_block(entry_block);

    inner_exit_block.exit = Some(BasicBlockExit::jump(entry_block_id));
    cfg.insert_block(inner_exit_block);

    (entry_block_id, exit_block)
}

// fn check_variable_usage(
//     cfg: &FunctionControlFlowGraph,
//     top_level_table: &TopLevelTable,
//     reference_table: &ReferenceTable,
//     type_table: &TypeTable,
//     ast: &ASTExpression,
//     diagnostics: &DiagnosticsSender,
// ) {
//     match &ast.kind {
//         ASTExpressionKind::Binary(ast) => {
//             check_variable_usage(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 &ast.left,
//                 diagnostics,
//             );
//             check_variable_usage(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 &ast.right,
//                 diagnostics,
//             );
//         }
//         ASTExpressionKind::Unary(ast) => {
//             check_variable_usage(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 &ast.right,
//                 diagnostics,
//             );
//         }
//         ASTExpressionKind::As(ast) => {
//             check_variable_usage(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 &ast.expression,
//                 diagnostics,
//             );
//         }
//         ASTExpressionKind::Call(ast) => {
//             check_variable_usage(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 &ast.expression,
//                 diagnostics,
//             );

//             for arg in &ast.args {
//                 check_variable_usage(
//                     cfg,
//                     top_level_table,
//                     reference_table,
//                     type_table,
//                     &arg.expression,
//                     diagnostics,
//                 );
//             }
//         }
//         ASTExpressionKind::Member(ast_member) => {
//             let variable = pick_variable(reference_table, ast);
//             let members = list_members(ast);

//             match (variable, members) {
//                 (Some(variable), Some(members)) => {
//                     if !cfg.initialized_variables.contains(&variable)
//                         && !cfg
//                             .partially_initialized_variables
//                             .get(&variable)
//                             .map(|member_set| member_set.contains(&members))
//                             .unwrap_or_default()
//                     {
//                         diagnostics.error(
//                             ast.span,
//                             format!(
//                                 "member {} is used before being initialized",
//                                 ast_member.member.symbol
//                             ),
//                         );
//                     }
//                 }
//                 _ => {
//                     // Do not check when the variable is not found or when the member is not found.
//                 }
//             }
//         }
//         ASTExpressionKind::Paren(ast) => {
//             check_variable_usage(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 &ast.expression,
//                 diagnostics,
//             );
//         }
//         ASTExpressionKind::Literal(..) => {}
//         ASTExpressionKind::IdReference(ast_id_ref) => {
//             let variable = pick_variable(reference_table, ast);

//             match variable {
//                 Some(variable) => {
//                     if !cfg.initialized_variables.contains(&variable) {
//                         diagnostics.error(
//                             ast.span,
//                             format!(
//                                 "variable {} is used before being initialized",
//                                 ast_id_ref.reference.symbol
//                             ),
//                         );
//                     }
//                 }
//                 None => {
//                     // Do not check when the variable is not found.
//                 }
//             }
//         }
//         ASTExpressionKind::StructLiteral(ast) => {
//             for field in &ast.fields {
//                 check_variable_usage(
//                     cfg,
//                     top_level_table,
//                     reference_table,
//                     type_table,
//                     &field.expression,
//                     diagnostics,
//                 );
//             }
//         }
//     }
// }

// fn pick_variable(reference_table: &ReferenceTable, ast: &ASTExpression) -> Option<NodeId> {
//     match &ast.kind {
//         ASTExpressionKind::Binary(..) => None,
//         ASTExpressionKind::Unary(..) => None,
//         ASTExpressionKind::As(..) => None,
//         ASTExpressionKind::Call(..) => None,
//         ASTExpressionKind::Member(ast) => pick_variable(reference_table, &ast.expression),
//         ASTExpressionKind::Paren(ast) => pick_variable(reference_table, &ast.expression),
//         ASTExpressionKind::Literal(..) => None,
//         ASTExpressionKind::IdReference(ast) => {
//             let symbol_ref = reference_table.symbol_references.get(&ast.id)?;
//             match &symbol_ref.kind {
//                 SymbolReferenceKind::Function { .. } => None,
//                 SymbolReferenceKind::Param { .. } => None,
//                 SymbolReferenceKind::Variable {
//                     function,
//                     scope,
//                     index,
//                 } => {
//                     let variable = &reference_table.function_scopes[function].scope_table.scopes
//                         [scope]
//                         .variables[*index];
//                     Some(variable.id)
//                 }
//             }
//         }
//         ASTExpressionKind::StructLiteral(..) => None,
//     }
// }

// fn list_members(ast: &ASTExpression) -> Option<Vec<LValueMember>> {
//     match &ast.kind {
//         ASTExpressionKind::Binary(..) => None,
//         ASTExpressionKind::Unary(..) => None,
//         ASTExpressionKind::As(..) => None,
//         ASTExpressionKind::Call(..) => None,
//         ASTExpressionKind::Member(ast) => {
//             let mut members = list_members(&ast.expression)?;
//             members.push(LValueMember::field(ast.member.symbol));
//             Some(members)
//         }
//         ASTExpressionKind::Paren(ast) => list_members(&ast.expression),
//         ASTExpressionKind::Literal(..) => None,
//         ASTExpressionKind::IdReference(..) => Some(vec![]),
//         ASTExpressionKind::StructLiteral(..) => None,
//     }
// }

// fn mark_variable_as_initialized(
//     cfg: &mut FunctionControlFlowGraph,
//     top_level_table: &TopLevelTable,
//     reference_table: &ReferenceTable,
//     type_table: &TypeTable,
//     lvalue: &LValue,
//     ast: &ASTExpression,
//     diagnostics: &DiagnosticsSender,
// ) {
//     match &ast.kind {
//         ASTExpressionKind::Binary(..) => {}
//         ASTExpressionKind::Unary(..) => {}
//         ASTExpressionKind::As(..) => {}
//         ASTExpressionKind::Call(..) => {}
//         ASTExpressionKind::Member(ast) => {
//             match &lvalue.kind {
//                 LValueKind::Param { .. } => {
//                     return;
//                 }
//                 LValueKind::Variable {
//                     function,
//                     scope,
//                     index,
//                 } => {
//                     let scope_variable =
//                         &reference_table.function_scopes[function].scope_table.scopes[scope]
//                             .variables[*index];
//                     let partially_initialized = cfg
//                         .partially_initialized_variables
//                         .entry(scope_variable.id)
//                         .or_default();
//                     partially_initialized.insert(lvalue.members.clone());

//                     let type_kind = match type_table.types.get(&ast.expression.id) {
//                         Some(type_kind) => type_kind,
//                         None => return,
//                     };
//                     let user_struct = match type_kind.unwrap_reference() {
//                         TypeKind::UserStruct { id } => {
//                             top_level_table.user_types[id].as_user_struct().unwrap()
//                         }
//                         _ => return,
//                     };

//                     if partially_initialized
//                         .iter()
//                         .filter(|initialized_members| {
//                             initialized_members.len() == lvalue.members.len()
//                         })
//                         .count()
//                         != user_struct.fields.len()
//                     {
//                         return;
//                     }
//                 }
//             }

//             mark_variable_as_initialized(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 lvalue,
//                 &ast.expression,
//                 diagnostics,
//             );
//         }
//         ASTExpressionKind::Paren(ast) => {
//             mark_variable_as_initialized(
//                 cfg,
//                 top_level_table,
//                 reference_table,
//                 type_table,
//                 lvalue,
//                 &ast.expression,
//                 diagnostics,
//             );
//         }
//         ASTExpressionKind::Literal(..) => {}
//         ASTExpressionKind::IdReference(_) => match &lvalue.kind {
//             LValueKind::Param { .. } => {
//                 // Params are always initialized.
//             }
//             LValueKind::Variable {
//                 function,
//                 scope,
//                 index,
//             } => {
//                 let scope_variable = &reference_table.function_scopes[function].scope_table.scopes
//                     [scope]
//                     .variables[*index];
//                 cfg.initialized_variables.insert(scope_variable.id);
//             }
//         },
//         ASTExpressionKind::StructLiteral(..) => {}
//     }
// }
