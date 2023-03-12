use crate::{
    lvalue_checking::{LValue, LValueKind, LValueMember, LValueTable},
    resolve::{ReferenceTable, SymbolReferenceKind, TopLevelTable, TypeKind},
    type_inferencing::TypeTable,
};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTIf, ASTLoop, ASTProgram,
    ASTStatementKind, ASTTopLevelKind, ASTWhile, NodeId,
};
use ex_span::Span;
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU64,
};

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
    pub initialized_variables: HashSet<NodeId>,
    pub partially_initialized_variables: HashMap<NodeId, HashSet<Vec<LValueMember>>>,
    pub entry_block_id: BlockId,
    pub exit_block_id: Option<BlockId>,
    pub blocks: HashMap<BlockId, BasicBlock>,
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
            initialized_variables: Default::default(),
            partially_initialized_variables: Default::default(),
            entry_block_id,
            exit_block_id: None,
            blocks,
            next_id: 1,
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
    pub statements: Vec<(NodeId, Span)>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            exit: None,
            statements: Vec::new(),
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

pub fn build_cfg(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue_table: &LValueTable,
    ast: &ASTProgram,
    diagnostics: &DiagnosticsSender,
) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let function_cfg = build_cfg_function(
                    top_level_table,
                    reference_table,
                    type_table,
                    lvalue_table,
                    top_level.id,
                    ast,
                    diagnostics,
                );
                cfg.functions.insert(top_level.id, function_cfg);
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    cfg
}

fn build_cfg_function(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue_table: &LValueTable,
    id: NodeId,
    ast: &ASTFunction,
    diagnostics: &DiagnosticsSender,
) -> FunctionControlFlowGraph {
    let mut cfg = FunctionControlFlowGraph::new(id, ast.span);

    let (inner_entry_block_id, mut inner_exit_block) = build_cfg_stmt_block(
        &mut cfg,
        None,
        None,
        top_level_table,
        reference_table,
        type_table,
        lvalue_table,
        &ast.body_block,
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
    loop_entry_block_id: Option<BlockId>,
    loop_exit_block_id: Option<BlockId>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue_table: &LValueTable,
    ast: &ASTBlock,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    let mut block = cfg.new_block();
    let entry_block_id = block.id;

    for statement in &ast.statements {
        block.statements.push((statement.id, statement.span));

        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_block(
                    cfg,
                    loop_entry_block_id,
                    loop_exit_block_id,
                    top_level_table,
                    reference_table,
                    type_table,
                    lvalue_table,
                    ast,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(inner_exit_block);

                block = cfg.new_block();
            }
            ASTStatementKind::Let(ast) => {
                if let Some(let_assignment) = &ast.let_assignment {
                    check_variable_usage(
                        cfg,
                        top_level_table,
                        reference_table,
                        type_table,
                        &let_assignment.expression,
                        diagnostics,
                    );
                    cfg.initialized_variables.insert(statement.id);
                }
            }
            ASTStatementKind::If(ast) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_if(
                    cfg,
                    loop_entry_block_id,
                    loop_exit_block_id,
                    top_level_table,
                    reference_table,
                    type_table,
                    lvalue_table,
                    ast,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(inner_exit_block);

                block = cfg.new_block();
            }
            ASTStatementKind::Loop(ast) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_loop(
                    cfg,
                    top_level_table,
                    reference_table,
                    type_table,
                    lvalue_table,
                    ast,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(inner_exit_block);

                block = cfg.new_block();
            }
            ASTStatementKind::While(ast) => {
                let (inner_entry_block_id, inner_exit_block) = build_cfg_stmt_while(
                    cfg,
                    top_level_table,
                    reference_table,
                    type_table,
                    lvalue_table,
                    ast,
                    diagnostics,
                );
                block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
                cfg.insert_block(inner_exit_block);

                block = cfg.new_block();
            }
            ASTStatementKind::Break(ast) => match loop_exit_block_id {
                Some(loop_exit_block_id) => {
                    block.exit = Some(BasicBlockExit::jump(loop_exit_block_id));
                    cfg.insert_block(block);

                    block = cfg.new_block();
                }
                None => {
                    diagnostics.error(ast.span, format!("break statement outside of loop"));
                }
            },
            ASTStatementKind::Continue(ast) => match loop_entry_block_id {
                Some(loop_entry_block_id) => {
                    block.exit = Some(BasicBlockExit::jump(loop_entry_block_id));
                    cfg.insert_block(block);

                    block = cfg.new_block();
                }
                None => {
                    diagnostics.error(ast.span, format!("continue statement outside of loop"));
                }
            },
            ASTStatementKind::Return(ast) => {
                if let Some(return_expression) = &ast.expression {
                    check_variable_usage(
                        cfg,
                        top_level_table,
                        reference_table,
                        type_table,
                        return_expression,
                        diagnostics,
                    );
                }
                block.exit = Some(BasicBlockExit::terminate(ast.expression.is_some()));
                cfg.insert_block(block);

                block = cfg.new_block();
            }
            ASTStatementKind::Assignment(ast) => {
                check_variable_usage(
                    cfg,
                    top_level_table,
                    reference_table,
                    type_table,
                    &ast.right,
                    diagnostics,
                );

                if let Some(lvalue) = lvalue_table.lvalues.get(&statement.id) {
                    mark_variable_as_initialized(
                        cfg,
                        top_level_table,
                        reference_table,
                        type_table,
                        lvalue,
                        &ast.left,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Row(ast) => {
                check_variable_usage(
                    cfg,
                    top_level_table,
                    reference_table,
                    type_table,
                    &ast.expression,
                    diagnostics,
                );
            }
        }
    }

    (entry_block_id, block)
}

fn build_cfg_stmt_if(
    cfg: &mut FunctionControlFlowGraph,
    loop_entry_block_id: Option<BlockId>,
    loop_exit_block_id: Option<BlockId>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue_table: &LValueTable,
    ast: &ASTIf,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    let mut stack = Vec::new();

    check_variable_usage(
        cfg,
        top_level_table,
        reference_table,
        type_table,
        &ast.expression,
        diagnostics,
    );

    let mut block = cfg.new_block();
    let (then_entry_block_id, mut then_exit_block) = build_cfg_stmt_block(
        cfg,
        loop_entry_block_id,
        loop_exit_block_id,
        top_level_table,
        reference_table,
        type_table,
        lvalue_table,
        &ast.body_block,
        diagnostics,
    );
    let end_block = cfg.new_block();
    block.exit = Some(BasicBlockExit::branch(then_entry_block_id, end_block.id));
    then_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
    cfg.insert_block(then_exit_block);
    stack.push((block, then_entry_block_id, end_block));

    for ast in &ast.single_else_ifs {
        check_variable_usage(
            cfg,
            top_level_table,
            reference_table,
            type_table,
            &ast.expression,
            diagnostics,
        );

        let mut block = cfg.new_block();
        let (then_entry_block_id, mut then_exit_block) = build_cfg_stmt_block(
            cfg,
            loop_entry_block_id,
            loop_exit_block_id,
            top_level_table,
            reference_table,
            type_table,
            lvalue_table,
            &ast.body_block,
            diagnostics,
        );
        let end_block = cfg.new_block();
        block.exit = Some(BasicBlockExit::branch(then_entry_block_id, end_block.id));
        then_exit_block.exit = Some(BasicBlockExit::jump(end_block.id));
        cfg.insert_block(then_exit_block);
        stack.push((block, then_entry_block_id, end_block));
    }

    if let Some(ast) = &ast.single_else {
        let (else_entry_block_id, mut else_exit_block) = build_cfg_stmt_block(
            cfg,
            loop_entry_block_id,
            loop_exit_block_id,
            top_level_table,
            reference_table,
            type_table,
            lvalue_table,
            &ast.body_block,
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
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue_table: &LValueTable,
    ast: &ASTLoop,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    let mut entry_block = cfg.new_block();
    let entry_block_id = entry_block.id;
    let exit_block = cfg.new_block();

    let (inner_entry_block_id, mut inner_exit_block) = build_cfg_stmt_block(
        cfg,
        Some(entry_block_id),
        Some(exit_block.id),
        top_level_table,
        reference_table,
        type_table,
        lvalue_table,
        &ast.body_block,
        diagnostics,
    );
    entry_block.exit = Some(BasicBlockExit::jump(inner_entry_block_id));
    cfg.insert_block(entry_block);

    inner_exit_block.exit = Some(BasicBlockExit::jump(entry_block_id));
    cfg.insert_block(inner_exit_block);

    (entry_block_id, exit_block)
}

fn build_cfg_stmt_while(
    cfg: &mut FunctionControlFlowGraph,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue_table: &LValueTable,
    ast: &ASTWhile,
    diagnostics: &DiagnosticsSender,
) -> (BlockId, BasicBlock) {
    let mut entry_block = cfg.new_block();
    let entry_block_id = entry_block.id;
    let exit_block = cfg.new_block();

    let (inner_entry_block_id, mut inner_exit_block) = build_cfg_stmt_block(
        cfg,
        Some(entry_block_id),
        Some(exit_block.id),
        top_level_table,
        reference_table,
        type_table,
        lvalue_table,
        &ast.body_block,
        diagnostics,
    );
    entry_block.exit = Some(BasicBlockExit::branch(inner_entry_block_id, exit_block.id));
    cfg.insert_block(entry_block);

    inner_exit_block.exit = Some(BasicBlockExit::jump(entry_block_id));
    cfg.insert_block(inner_exit_block);

    (entry_block_id, exit_block)
}

fn check_variable_usage(
    cfg: &FunctionControlFlowGraph,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    ast: &ASTExpression,
    diagnostics: &DiagnosticsSender,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(ast) => {
            check_variable_usage(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                &ast.left,
                diagnostics,
            );
            check_variable_usage(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                &ast.right,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(ast) => {
            check_variable_usage(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                &ast.right,
                diagnostics,
            );
        }
        ASTExpressionKind::As(ast) => {
            check_variable_usage(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(ast) => {
            check_variable_usage(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                &ast.expression,
                diagnostics,
            );

            for argument in &ast.arguments {
                check_variable_usage(
                    cfg,
                    top_level_table,
                    reference_table,
                    type_table,
                    &argument.expression,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Member(ast_member) => {
            let variable = pick_variable(reference_table, ast);
            let members = list_members(ast);

            match (variable, members) {
                (Some(variable), Some(members)) => {
                    if !cfg.initialized_variables.contains(&variable)
                        && !cfg
                            .partially_initialized_variables
                            .get(&variable)
                            .map(|member_set| member_set.contains(&members))
                            .unwrap_or_default()
                    {
                        diagnostics.error(
                            ast.span,
                            format!(
                                "member {} is used before being initialized",
                                ast_member.member.symbol
                            ),
                        );
                    }
                }
                _ => {
                    // Do not check when the variable is not found or when the member is not found.
                }
            }
        }
        ASTExpressionKind::Paren(ast) => {
            check_variable_usage(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(ast_id_ref) => {
            let variable = pick_variable(reference_table, ast);

            match variable {
                Some(variable) => {
                    if !cfg.initialized_variables.contains(&variable) {
                        diagnostics.error(
                            ast.span,
                            format!(
                                "variable {} is used before being initialized",
                                ast_id_ref.reference.symbol
                            ),
                        );
                    }
                }
                None => {
                    // Do not check when the variable is not found.
                }
            }
        }
        ASTExpressionKind::StructLiteral(ast) => {
            for field in &ast.fields {
                check_variable_usage(
                    cfg,
                    top_level_table,
                    reference_table,
                    type_table,
                    &field.expression,
                    diagnostics,
                );
            }
        }
    }
}

fn pick_variable(reference_table: &ReferenceTable, ast: &ASTExpression) -> Option<NodeId> {
    match &ast.kind {
        ASTExpressionKind::Binary(..) => None,
        ASTExpressionKind::Unary(..) => None,
        ASTExpressionKind::As(..) => None,
        ASTExpressionKind::Call(..) => None,
        ASTExpressionKind::Member(ast) => pick_variable(reference_table, &ast.expression),
        ASTExpressionKind::Paren(ast) => pick_variable(reference_table, &ast.expression),
        ASTExpressionKind::Literal(..) => None,
        ASTExpressionKind::IdReference(ast) => {
            let symbol_ref = reference_table.symbol_references.get(&ast.id)?;
            match &symbol_ref.kind {
                SymbolReferenceKind::Function { .. } => None,
                SymbolReferenceKind::Parameter { .. } => None,
                SymbolReferenceKind::Variable {
                    function,
                    scope,
                    index,
                } => {
                    let variable = &reference_table.function_scopes[function].scope_table.scopes
                        [scope]
                        .variables[*index];
                    Some(variable.id)
                }
            }
        }
        ASTExpressionKind::StructLiteral(..) => None,
    }
}

fn list_members(ast: &ASTExpression) -> Option<Vec<LValueMember>> {
    match &ast.kind {
        ASTExpressionKind::Binary(..) => None,
        ASTExpressionKind::Unary(..) => None,
        ASTExpressionKind::As(..) => None,
        ASTExpressionKind::Call(..) => None,
        ASTExpressionKind::Member(ast) => {
            let mut members = list_members(&ast.expression)?;
            members.push(LValueMember::field(ast.member.symbol));
            Some(members)
        }
        ASTExpressionKind::Paren(ast) => list_members(&ast.expression),
        ASTExpressionKind::Literal(..) => None,
        ASTExpressionKind::IdReference(..) => Some(vec![]),
        ASTExpressionKind::StructLiteral(..) => None,
    }
}

fn mark_variable_as_initialized(
    cfg: &mut FunctionControlFlowGraph,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    lvalue: &LValue,
    ast: &ASTExpression,
    diagnostics: &DiagnosticsSender,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(..) => {}
        ASTExpressionKind::Unary(..) => {}
        ASTExpressionKind::As(..) => {}
        ASTExpressionKind::Call(..) => {}
        ASTExpressionKind::Member(ast) => {
            match &lvalue.kind {
                LValueKind::Parameter { .. } => {
                    return;
                }
                LValueKind::Variable {
                    function,
                    scope,
                    index,
                } => {
                    let scope_variable =
                        &reference_table.function_scopes[function].scope_table.scopes[scope]
                            .variables[*index];
                    let partially_initialized = cfg
                        .partially_initialized_variables
                        .entry(scope_variable.id)
                        .or_default();
                    partially_initialized.insert(lvalue.members.clone());

                    let type_kind = match type_table.types.get(&ast.expression.id) {
                        Some(type_kind) => type_kind,
                        None => return,
                    };
                    let user_struct = match type_kind.unwrap_reference() {
                        TypeKind::UserStruct { id } => {
                            top_level_table.user_types[id].as_user_struct().unwrap()
                        }
                        _ => return,
                    };

                    if partially_initialized
                        .iter()
                        .filter(|initialized_members| {
                            initialized_members.len() == lvalue.members.len()
                        })
                        .count()
                        != user_struct.fields.len()
                    {
                        return;
                    }
                }
            }

            mark_variable_as_initialized(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                lvalue,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Paren(ast) => {
            mark_variable_as_initialized(
                cfg,
                top_level_table,
                reference_table,
                type_table,
                lvalue,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(_) => match &lvalue.kind {
            LValueKind::Parameter { .. } => {
                // Parameters are always initialized.
            }
            LValueKind::Variable {
                function,
                scope,
                index,
            } => {
                let scope_variable = &reference_table.function_scopes[function].scope_table.scopes
                    [scope]
                    .variables[*index];
                cfg.initialized_variables.insert(scope_variable.id);
            }
        },
        ASTExpressionKind::StructLiteral(..) => {}
    }
}
