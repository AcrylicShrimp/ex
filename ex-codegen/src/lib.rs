mod basic_block;
mod basic_block_table;
mod block_id;
mod block_id_allocator;
mod expression;
mod function;
mod instruction;
mod instruction_id;
mod instruction_id_allocator;
mod instruction_table;
mod program;
mod temporary;
mod temporary_id;
mod temporary_id_allocator;
mod temporary_table;
mod type_id;
mod type_id_allocator;
mod type_kind;
mod type_table;
mod user_type_struct;
mod variable;
mod variable_id;
mod variable_id_allocator;
mod variable_table;

pub use basic_block::*;
pub use basic_block_table::*;
pub use block_id::*;
pub use block_id_allocator::*;
pub use expression::*;
pub use function::*;
pub use instruction::*;
pub use instruction_id::*;
pub use instruction_id_allocator::*;
pub use instruction_table::*;
pub use program::*;
pub use temporary::*;
pub use temporary_id::*;
pub use temporary_id_allocator::*;
pub use temporary_table::*;
pub use type_id::*;
pub use type_id_allocator::*;
pub use type_kind::*;
pub use type_table::*;
pub use user_type_struct::*;
pub use variable::*;
pub use variable_id::*;
pub use variable_id_allocator::*;
pub use variable_table::*;

use ex_parser::{
    ASTAssignmentOperatorKind, ASTBinaryOperatorKind, ASTBlock, ASTExpression, ASTExpressionKind,
    ASTFunction, ASTIf, ASTLoop, ASTProgram, ASTStatementKind, ASTTopLevelKind,
    ASTUnaryOperatorKind, ASTWhile, NodeId, Typename,
};
use ex_resolve_ref::{
    AssignmentLhsKind, AssignmentLhsTable, Function as NodeFunction, FunctionTable, SymbolNodeKind,
    SymbolReferenceTable, TypeKind as NodeTypeKind, TypeReferenceTable,
};
use ex_symbol::Symbol;
use ex_type_checking::TypeTable as NodeTypeTable;
use std::{collections::HashMap, iter::empty as iter_empty};

pub fn codegen(
    ast: &ASTProgram,
    function_table: &FunctionTable,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
    assignment_lhs_table: &AssignmentLhsTable,
) -> Program {
    let mut program = Program::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(..) => {}
            ASTTopLevelKind::Struct(ast) => {
                let user_type_struct = UserTypeStruct::new(
                    ast.name.symbol,
                    ast.fields
                        .iter()
                        .map(|field| {
                            UserTypeStructField::new(
                                field.name.symbol,
                                typename_to_type_id(
                                    &mut program.type_table,
                                    type_reference_table,
                                    &field.typename,
                                ),
                            )
                        })
                        .collect(),
                );
                program
                    .user_type_struct_table
                    .insert(ast.name.symbol, user_type_struct);
            }
        }
    }

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let function = function_table.functions.get(&top_level.id).unwrap();
                let function = codegen_function(
                    &mut program.type_table,
                    &program.user_type_struct_table,
                    ast,
                    function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                    assignment_lhs_table,
                );
                program.functions.insert(function.name, function);
            }
            ASTTopLevelKind::Struct(..) => {}
        }
    }

    program
}

fn codegen_function(
    type_table: &mut TypeTable,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    ast: &ASTFunction,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
    assignment_lhs_table: &AssignmentLhsTable,
) -> Function {
    let mut function = Function::new(
        ast.signature.name.symbol,
        ast.signature
            .parameters
            .iter()
            .map(|parameter| {
                typename_to_type_id(type_table, type_reference_table, &parameter.typename)
            })
            .collect(),
        ast.signature
            .return_type
            .as_ref()
            .map(|return_type| {
                typename_to_type_id(type_table, type_reference_table, &return_type.typename)
            })
            .unwrap_or_else(|| type_table.insert(TypeKind::Empty)),
    );
    let mut node_variable_table = HashMap::new();

    let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_block(
        &mut node_variable_table,
        type_table,
        &mut function,
        None,
        None,
        user_type_struct_table,
        &ast.body_block,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
        assignment_lhs_table,
    );

    function
        .block_table
        .blocks
        .get_mut(&function.entry_block_id)
        .unwrap()
        .new_instruction(InstructionKind::jump(inner_entry_block_id, vec![]));
    inner_exit_block.new_instruction(InstructionKind::terminate(None));
    function.block_table.insert(inner_exit_block);

    function
}

fn codegen_function_stmt_block(
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    loop_entry_block_id: Option<BlockId>,
    loop_exit_block_id: Option<BlockId>,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    ast: &ASTBlock,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
    assignment_lhs_table: &AssignmentLhsTable,
) -> (BlockId, BasicBlock) {
    let mut block = function.new_block(iter_empty());
    let entry_block_id = block.id;

    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_block(
                    node_variable_table,
                    type_table,
                    function,
                    loop_entry_block_id,
                    loop_exit_block_id,
                    user_type_struct_table,
                    &stmt_block,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                    assignment_lhs_table,
                );
                block.new_instruction(InstructionKind::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            ASTStatementKind::Let(stmt_let) => {
                let type_id = type_kind_to_type_id(
                    type_table,
                    node_type_table.types.get(&statement.id).unwrap(),
                );
                let variable = function.variable_table.insert(type_id);
                node_variable_table.insert(statement.id, variable);

                if let Some(let_assignment) = &stmt_let.let_assignment {
                    let temporary = codegen_function_expression(
                        &mut block,
                        node_variable_table,
                        type_table,
                        function,
                        user_type_struct_table,
                        &let_assignment.expression,
                        node_function,
                        node_type_table,
                        type_reference_table,
                        symbol_reference_table,
                    );
                    block.new_instruction(InstructionKind::store(
                        Pointer::new(variable, vec![]),
                        temporary,
                        None,
                    ));
                }
            }
            ASTStatementKind::If(stmt_if) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_if(
                    node_variable_table,
                    type_table,
                    function,
                    loop_entry_block_id,
                    loop_exit_block_id,
                    user_type_struct_table,
                    &stmt_if,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                    assignment_lhs_table,
                );
                block.new_instruction(InstructionKind::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            ASTStatementKind::Loop(stmt_loop) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_loop(
                    node_variable_table,
                    type_table,
                    function,
                    user_type_struct_table,
                    &stmt_loop,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                    assignment_lhs_table,
                );
                block.new_instruction(InstructionKind::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            ASTStatementKind::While(stmt_while) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_while(
                    node_variable_table,
                    type_table,
                    function,
                    user_type_struct_table,
                    &stmt_while,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                    assignment_lhs_table,
                );
                block.new_instruction(InstructionKind::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            ASTStatementKind::Break(..) => {
                block.new_instruction(InstructionKind::jump(loop_exit_block_id.unwrap(), vec![]));
                function.block_table.insert(block);

                block = function.new_block(iter_empty());
            }
            ASTStatementKind::Continue(..) => {
                block.new_instruction(InstructionKind::jump(loop_entry_block_id.unwrap(), vec![]));
                function.block_table.insert(block);

                block = function.new_block(iter_empty());
            }
            ASTStatementKind::Return(stmt_return) => {
                let temporary = stmt_return.expression.as_ref().map(|expression| {
                    codegen_function_expression(
                        &mut block,
                        node_variable_table,
                        type_table,
                        function,
                        user_type_struct_table,
                        &expression,
                        node_function,
                        node_type_table,
                        type_reference_table,
                        symbol_reference_table,
                    )
                });
                block.new_instruction(InstructionKind::terminate(temporary));
                function.block_table.insert(block);

                block = function.new_block(iter_empty());
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                let pointer = lhs_expression_to_pointer(
                    function,
                    type_table,
                    user_type_struct_table,
                    node_variable_table,
                    assignment_lhs_table,
                    &stmt_assignment.left,
                );
                let temporary = codegen_function_expression(
                    &mut block,
                    node_variable_table,
                    type_table,
                    function,
                    user_type_struct_table,
                    &stmt_assignment.right,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                );
                block.new_instruction(InstructionKind::store(
                    pointer,
                    temporary,
                    stmt_assignment.operator_kind.map(convert_assignment_op),
                ));
            }
            ASTStatementKind::Row(stmt_row) => {
                codegen_function_expression(
                    &mut block,
                    node_variable_table,
                    type_table,
                    function,
                    user_type_struct_table,
                    &stmt_row.expression,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                );
            }
        }
    }

    (entry_block_id, block)
}

fn codegen_function_stmt_if(
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    loop_entry_block_id: Option<BlockId>,
    loop_exit_block_id: Option<BlockId>,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    ast: &ASTIf,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
    assignment_lhs_table: &AssignmentLhsTable,
) -> (BlockId, BasicBlock) {
    let mut stack = Vec::new();

    let mut block = function.new_block(iter_empty());
    let condition = codegen_function_expression(
        &mut block,
        node_variable_table,
        type_table,
        function,
        user_type_struct_table,
        &ast.expression,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
    );
    let (then_entry_block_id, mut then_exit_block) = codegen_function_stmt_block(
        node_variable_table,
        type_table,
        function,
        loop_entry_block_id,
        loop_exit_block_id,
        user_type_struct_table,
        &ast.body_block,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
        assignment_lhs_table,
    );
    let end_block = function.new_block(iter_empty());
    let branch_instruction = block.new_instruction(InstructionKind::branch(
        condition,
        then_entry_block_id,
        vec![],
        end_block.id,
        vec![],
    ));
    then_exit_block.new_instruction(InstructionKind::jump(end_block.id, vec![]));
    function.block_table.insert(then_exit_block);
    stack.push((
        block,
        condition,
        branch_instruction,
        then_entry_block_id,
        end_block,
    ));

    for single_else_if in &ast.single_else_ifs {
        let mut block = function.new_block(iter_empty());
        let condition = codegen_function_expression(
            &mut block,
            node_variable_table,
            type_table,
            function,
            user_type_struct_table,
            &single_else_if.expression,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
        );
        let (then_entry_block_id, mut then_exit_block) = codegen_function_stmt_block(
            node_variable_table,
            type_table,
            function,
            loop_entry_block_id,
            loop_exit_block_id,
            user_type_struct_table,
            &single_else_if.body_block,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
            assignment_lhs_table,
        );
        let end_block = function.new_block(iter_empty());
        let branch_instruction = block.new_instruction(InstructionKind::branch(
            condition,
            then_entry_block_id,
            vec![],
            end_block.id,
            vec![],
        ));
        then_exit_block.new_instruction(InstructionKind::jump(end_block.id, vec![]));
        function.block_table.insert(then_exit_block);
        stack.push((
            block,
            condition,
            branch_instruction,
            then_entry_block_id,
            end_block,
        ));
    }

    if let Some(single_else) = &ast.single_else {
        let (else_entry_id, mut else_exit_block) = codegen_function_stmt_block(
            node_variable_table,
            type_table,
            function,
            loop_entry_block_id,
            loop_exit_block_id,
            user_type_struct_table,
            &single_else.body_block,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
            assignment_lhs_table,
        );
        let (block, condition, branch_instruction, then_entry_block_id, end_block) =
            stack.last_mut().unwrap();
        block.replace_instruction(
            *branch_instruction,
            InstructionKind::branch(
                *condition,
                *then_entry_block_id,
                vec![],
                else_entry_id,
                vec![],
            ),
        );
        else_exit_block.new_instruction(InstructionKind::jump(end_block.id, vec![]));
        function.block_table.insert(else_exit_block);
    }

    let (mut last_block, _, _, _, mut last_end_block) = stack.pop().unwrap();

    while let Some((mut block, condition, branch_instruction, then_entry_block_id, end_block)) =
        stack.pop()
    {
        block.replace_instruction(
            branch_instruction,
            InstructionKind::branch(
                condition,
                then_entry_block_id,
                vec![],
                last_end_block.id,
                vec![],
            ),
        );
        last_end_block.new_instruction(InstructionKind::jump(end_block.id, vec![]));

        function.block_table.insert(last_block);
        function.block_table.insert(last_end_block);

        last_block = block;
        last_end_block = end_block;
    }

    let entry_id = last_block.id;
    function.block_table.insert(last_block);

    (entry_id, last_end_block)
}

fn codegen_function_stmt_loop(
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    ast: &ASTLoop,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
    assignment_lhs_table: &AssignmentLhsTable,
) -> (BlockId, BasicBlock) {
    let mut entry_block = function.new_block(iter_empty());
    let entry_block_id = entry_block.id;
    let exit_block = function.new_block(iter_empty());

    let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_block(
        node_variable_table,
        type_table,
        function,
        Some(entry_block_id),
        Some(exit_block.id),
        user_type_struct_table,
        &ast.body_block,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
        assignment_lhs_table,
    );
    entry_block.new_instruction(InstructionKind::jump(inner_entry_block_id, vec![]));
    function.block_table.insert(entry_block);

    inner_exit_block.new_instruction(InstructionKind::jump(entry_block_id, vec![]));
    function.block_table.insert(inner_exit_block);

    (entry_block_id, exit_block)
}

fn codegen_function_stmt_while(
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    ast: &ASTWhile,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
    assignment_lhs_table: &AssignmentLhsTable,
) -> (BlockId, BasicBlock) {
    let mut entry_block = function.new_block(iter_empty());
    let condition = codegen_function_expression(
        &mut entry_block,
        node_variable_table,
        type_table,
        function,
        user_type_struct_table,
        &ast.expression,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
    );

    let entry_block_id = entry_block.id;
    let exit_block = function.new_block(iter_empty());

    let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_block(
        node_variable_table,
        type_table,
        function,
        Some(entry_block_id),
        Some(exit_block.id),
        user_type_struct_table,
        &ast.body_block,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
        assignment_lhs_table,
    );
    entry_block.new_instruction(InstructionKind::branch(
        condition,
        inner_entry_block_id,
        vec![],
        exit_block.id,
        vec![],
    ));
    function.block_table.insert(entry_block);

    inner_exit_block.new_instruction(InstructionKind::jump(entry_block_id, vec![]));
    function.block_table.insert(inner_exit_block);

    (entry_block_id, exit_block)
}

fn codegen_function_expression(
    block: &mut BasicBlock,
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    ast: &ASTExpression,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
) -> TemporaryId {
    match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            let left = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                user_type_struct_table,
                &expr_binary.left,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let right = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                user_type_struct_table,
                &expr_binary.right,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = block.new_temporary(type_id);
            block.new_instruction(InstructionKind::assign(
                temporary,
                Expression::new(
                    ExpressionKind::binary(
                        convert_binary_op(expr_binary.operator_kind),
                        left,
                        right,
                    ),
                    type_id,
                ),
            ));
            temporary
        }
        ASTExpressionKind::Unary(expr_unary) => {
            let right = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                user_type_struct_table,
                &expr_unary.right,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = block.new_temporary(type_id);
            block.new_instruction(InstructionKind::assign(
                temporary,
                Expression::new(
                    ExpressionKind::unary(convert_unary_op(expr_unary.operator_kind), right),
                    type_id,
                ),
            ));
            temporary
        }
        ASTExpressionKind::As(expr_as) => {
            let expression = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                user_type_struct_table,
                &expr_as.expression,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let from_type_id = type_kind_to_type_id(
                type_table,
                node_type_table.types.get(&expr_as.expression.id).unwrap(),
            );
            let type_id = typename_to_type_id(type_table, type_reference_table, &expr_as.typename);
            let temporary = block.new_temporary(type_id);
            block.new_instruction(InstructionKind::assign(
                temporary,
                Expression::new(
                    ExpressionKind::convert(expression, from_type_id, type_id),
                    type_id,
                ),
            ));
            temporary
        }
        ASTExpressionKind::Call(expr_call) => {
            let expression = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                user_type_struct_table,
                &expr_call.expression,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let arguments = expr_call
                .arguments
                .iter()
                .map(|argument| {
                    codegen_function_expression(
                        block,
                        node_variable_table,
                        type_table,
                        function,
                        user_type_struct_table,
                        &argument.expression,
                        node_function,
                        node_type_table,
                        type_reference_table,
                        symbol_reference_table,
                    )
                })
                .collect();
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = block.new_temporary(type_id);
            block.new_instruction(InstructionKind::assign(
                temporary,
                Expression::new(ExpressionKind::call(expression, arguments), type_id),
            ));
            temporary
        }
        ASTExpressionKind::Member(expr_member) => {
            let expression = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                user_type_struct_table,
                &expr_member.expression,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let expression_type_id = type_kind_to_type_id(
                type_table,
                node_type_table
                    .types
                    .get(&expr_member.expression.id)
                    .unwrap(),
            );
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = block.new_temporary(type_id);
            let struct_symbol = type_table
                .types
                .get(&expression_type_id)
                .unwrap()
                .as_user_type_struct();
            let index = user_type_struct_table
                .get(&struct_symbol)
                .unwrap()
                .fields
                .iter()
                .position(|field| field.name == expr_member.member.symbol)
                .unwrap();
            block.new_instruction(InstructionKind::extract(expression, vec![index], temporary));
            temporary
        }
        ASTExpressionKind::Paren(expr_paren) => codegen_function_expression(
            block,
            node_variable_table,
            type_table,
            function,
            user_type_struct_table,
            &expr_paren.expression,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
        ),
        ASTExpressionKind::Literal(expr_literal) => {
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = block.new_temporary(type_id);
            block.new_instruction(InstructionKind::assign(
                temporary,
                Expression::new(
                    ExpressionKind::literal(expr_literal.literal.clone()),
                    type_id,
                ),
            ));
            temporary
        }
        ASTExpressionKind::IdReference(expr_id_ref) => {
            let symbol_reference = symbol_reference_table
                .references
                .get(&expr_id_ref.id)
                .unwrap();
            match symbol_reference.kind {
                SymbolNodeKind::Function => {
                    let type_id = type_kind_to_type_id(
                        type_table,
                        node_type_table.types.get(&ast.id).unwrap(),
                    );
                    let temporary = block.new_temporary(type_id);
                    block.new_instruction(InstructionKind::assign(
                        temporary,
                        Expression::new(
                            ExpressionKind::function(symbol_reference.id.symbol),
                            type_id,
                        ),
                    ));
                    temporary
                }
                SymbolNodeKind::Parameter { index } => {
                    let type_id = function.parameters[index].type_id;
                    let variable = function.parameters[index].variable_id;
                    let temporary = block.new_temporary(type_id);
                    block.new_instruction(InstructionKind::load(
                        Pointer::new(variable, vec![]),
                        temporary,
                    ));
                    temporary
                }
                SymbolNodeKind::Variable => {
                    let type_id = type_kind_to_type_id(
                        type_table,
                        node_type_table.types.get(&ast.id).unwrap(),
                    );
                    let variable = node_variable_table[&symbol_reference.node].clone();
                    let temporary = block.new_temporary(type_id);
                    block.new_instruction(InstructionKind::load(
                        Pointer::new(variable, vec![]),
                        temporary,
                    ));
                    temporary
                }
            }
        }
        ASTExpressionKind::StructLiteral(expr_struct_literal) => {
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let struct_symbol = type_table.types[&type_id].as_user_type_struct();
            let user_type_struct = &user_type_struct_table[&struct_symbol];
            let mut field_with_indices = expr_struct_literal
                .fields
                .iter()
                .map(|field| {
                    (
                        user_type_struct
                            .fields
                            .iter()
                            .position(|f| f.name == field.name.symbol)
                            .unwrap(),
                        codegen_function_expression(
                            block,
                            node_variable_table,
                            type_table,
                            function,
                            user_type_struct_table,
                            &field.expression,
                            node_function,
                            node_type_table,
                            type_reference_table,
                            symbol_reference_table,
                        ),
                    )
                })
                .collect::<Vec<_>>();
            field_with_indices.sort_unstable_by_key(|field| field.0);
            let temporary = block.new_temporary(type_id);
            block.new_instruction(InstructionKind::assign(
                temporary,
                Expression::new(
                    ExpressionKind::struct_literal(
                        type_id,
                        field_with_indices
                            .into_iter()
                            .map(|field| field.1)
                            .collect(),
                    ),
                    type_id,
                ),
            ));
            temporary
        }
    }
}

fn lhs_expression_to_pointer(
    function: &Function,
    type_table: &TypeTable,
    user_type_struct_table: &HashMap<Symbol, UserTypeStruct>,
    node_variable_table: &HashMap<NodeId, VariableId>,
    assignment_lhs_table: &AssignmentLhsTable,
    expr: &ASTExpression,
) -> Pointer {
    match &expr.kind {
        ASTExpressionKind::Member(expr_member) => {
            let mut pointer = lhs_expression_to_pointer(
                function,
                type_table,
                user_type_struct_table,
                node_variable_table,
                assignment_lhs_table,
                &expr_member.expression,
            );
            let type_id = function.variable_table.variables[&pointer.variable].type_id;
            let struct_symbol = type_table.types[&type_id].as_user_type_struct();
            let index = user_type_struct_table
                .get(&struct_symbol)
                .unwrap()
                .fields
                .iter()
                .position(|field| field.name == expr_member.member.symbol)
                .unwrap();
            pointer.indices.push(index);
            pointer
        }
        ASTExpressionKind::Paren(expr_paren) => lhs_expression_to_pointer(
            function,
            type_table,
            user_type_struct_table,
            node_variable_table,
            assignment_lhs_table,
            &expr_paren.expression,
        ),
        ASTExpressionKind::IdReference(_) => {
            match assignment_lhs_table.kinds.get(&expr.id).unwrap() {
                AssignmentLhsKind::Parameter { index } => {
                    return Pointer::new(function.parameters[*index].variable_id, vec![]);
                }
                AssignmentLhsKind::Variable { node } => {
                    return Pointer::new(node_variable_table[node], vec![]);
                }
                AssignmentLhsKind::Field { .. } => {
                    unreachable!()
                }
            }
        }
        _ => unreachable!(),
    }
}

fn convert_assignment_op(op: ASTAssignmentOperatorKind) -> AssignmentOperator {
    match op {
        ASTAssignmentOperatorKind::Add => AssignmentOperator::Add,
        ASTAssignmentOperatorKind::Sub => AssignmentOperator::Sub,
        ASTAssignmentOperatorKind::Mul => AssignmentOperator::Mul,
        ASTAssignmentOperatorKind::Div => AssignmentOperator::Div,
        ASTAssignmentOperatorKind::Mod => AssignmentOperator::Mod,
        ASTAssignmentOperatorKind::Pow => AssignmentOperator::Pow,
        ASTAssignmentOperatorKind::Shl => AssignmentOperator::Shl,
        ASTAssignmentOperatorKind::Shr => AssignmentOperator::Shr,
        ASTAssignmentOperatorKind::BitOr => AssignmentOperator::BitOr,
        ASTAssignmentOperatorKind::BitAnd => AssignmentOperator::BitAnd,
        ASTAssignmentOperatorKind::BitXor => AssignmentOperator::BitXor,
        ASTAssignmentOperatorKind::BitNot => AssignmentOperator::BitNot,
    }
}

fn convert_binary_op(op: ASTBinaryOperatorKind) -> BinaryOperator {
    match op {
        ASTBinaryOperatorKind::Eq => BinaryOperator::Eq,
        ASTBinaryOperatorKind::Ne => BinaryOperator::Ne,
        ASTBinaryOperatorKind::Lt => BinaryOperator::Lt,
        ASTBinaryOperatorKind::Gt => BinaryOperator::Gt,
        ASTBinaryOperatorKind::Le => BinaryOperator::Le,
        ASTBinaryOperatorKind::Ge => BinaryOperator::Ge,
        ASTBinaryOperatorKind::LogOr => BinaryOperator::LogOr,
        ASTBinaryOperatorKind::LogAnd => BinaryOperator::LogAnd,
        ASTBinaryOperatorKind::Add => BinaryOperator::Add,
        ASTBinaryOperatorKind::Sub => BinaryOperator::Sub,
        ASTBinaryOperatorKind::Mul => BinaryOperator::Mul,
        ASTBinaryOperatorKind::Div => BinaryOperator::Div,
        ASTBinaryOperatorKind::Mod => BinaryOperator::Mod,
        ASTBinaryOperatorKind::Pow => BinaryOperator::Pow,
        ASTBinaryOperatorKind::Shl => BinaryOperator::Shl,
        ASTBinaryOperatorKind::Shr => BinaryOperator::Shr,
        ASTBinaryOperatorKind::BitOr => BinaryOperator::BitOr,
        ASTBinaryOperatorKind::BitAnd => BinaryOperator::BitAnd,
        ASTBinaryOperatorKind::BitXor => BinaryOperator::BitXor,
    }
}

fn convert_unary_op(op: ASTUnaryOperatorKind) -> UnaryOperator {
    match op {
        ASTUnaryOperatorKind::Plus => UnaryOperator::Plus,
        ASTUnaryOperatorKind::Minus => UnaryOperator::Minus,
        ASTUnaryOperatorKind::BitNot => UnaryOperator::BitNot,
        ASTUnaryOperatorKind::LogNot => UnaryOperator::LogNot,
        ASTUnaryOperatorKind::AddressOf => todo!(),
        ASTUnaryOperatorKind::Dereference => todo!(),
    }
}

fn typename_to_type_id(
    type_table: &mut TypeTable,
    type_reference_table: &TypeReferenceTable,
    typename: &Typename,
) -> TypeId {
    type_kind_to_type_id(
        type_table,
        &type_reference_table
            .references
            .get(&typename.id)
            .unwrap()
            .kind,
    )
}

fn type_kind_to_type_id(type_table: &mut TypeTable, type_kind: &NodeTypeKind) -> TypeId {
    let type_kind = match type_kind {
        NodeTypeKind::Unknown => unreachable!(),
        NodeTypeKind::Empty => TypeKind::Empty,
        NodeTypeKind::Boolean => TypeKind::Boolean,
        NodeTypeKind::Integer => TypeKind::Integer,
        NodeTypeKind::Float => TypeKind::Float,
        NodeTypeKind::String => TypeKind::String,
        NodeTypeKind::Callable {
            parameters,
            return_type,
        } => TypeKind::Callable {
            parameters: parameters
                .iter()
                .map(|type_kind| type_kind_to_type_id(type_table, type_kind))
                .collect(),
            return_type: type_kind_to_type_id(type_table, return_type),
        },
        NodeTypeKind::UserTypeStruct { symbol } => TypeKind::UserTypeStruct { symbol: *symbol },
        NodeTypeKind::Pointer { type_kind } => TypeKind::Pointer {
            type_id: type_kind_to_type_id(type_table, type_kind),
        },
        NodeTypeKind::Reference { type_kind } => TypeKind::Reference {
            type_id: type_kind_to_type_id(type_table, type_kind),
        },
    };
    type_table.insert(type_kind)
}
