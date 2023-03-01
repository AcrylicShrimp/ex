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
pub use variable::*;
pub use variable_id::*;
pub use variable_id_allocator::*;
pub use variable_table::*;

use ex_parser::{
    ASTBinaryOperatorKind, ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTIf,
    ASTProgram, ASTStatementKind, ASTTopLevelKind, ASTUnaryOperatorKind, NodeId, Typename,
};
use ex_resolve_ref::{
    Function as NodeFunction, FunctionTable, SymbolNodeKind, SymbolReferenceTable,
    TypeKind as NodeTypeKind, TypeReferenceTable,
};
use ex_type_checking::TypeTable as NodeTypeTable;
use std::collections::HashMap;

pub fn codegen(
    ast: &ASTProgram,
    function_table: &FunctionTable,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
) -> Program {
    let mut program = Program::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let function = function_table.functions.get(&top_level.id).unwrap();
                let function = codegen_function(
                    &mut program.type_table,
                    ast,
                    function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                );
                program.functions.insert(function.name, function);
            }
        }
    }

    program
}

fn codegen_function(
    type_table: &mut TypeTable,
    ast: &ASTFunction,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
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
        &ast.body_block,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
    );

    function.new_instruction_indirect(
        function.entry_block,
        InstructionKind::jump(inner_entry_block_id),
    );
    function.new_instruction(&mut inner_exit_block, InstructionKind::terminate(None));
    function.exit_block = Some(inner_exit_block.id);
    function.block_table.insert(inner_exit_block);

    function
}

fn codegen_function_stmt_block(
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    ast: &ASTBlock,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
) -> (BlockId, BasicBlock) {
    let mut block = function.new_block();
    let entry_block_id = block.id;

    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(stmt_block) => {
                let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_block(
                    node_variable_table,
                    type_table,
                    function,
                    &stmt_block,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                );
                function.new_instruction(&mut block, InstructionKind::jump(inner_entry_block_id));
                function.block_table.insert(block);

                block = function.new_block();
                function.new_instruction(&mut inner_exit_block, InstructionKind::jump(block.id));
                function.block_table.insert(inner_exit_block);
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
                        &let_assignment.expression,
                        node_function,
                        node_type_table,
                        type_reference_table,
                        symbol_reference_table,
                    );
                    function.new_instruction(
                        &mut block,
                        InstructionKind::Store {
                            variable,
                            temporary,
                        },
                    );
                }
            }
            ASTStatementKind::If(stmt_if) => {
                let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_if(
                    node_variable_table,
                    type_table,
                    function,
                    &stmt_if,
                    node_function,
                    node_type_table,
                    type_reference_table,
                    symbol_reference_table,
                );
                function.new_instruction(&mut block, InstructionKind::jump(inner_entry_block_id));
                function.block_table.insert(block);

                block = function.new_block();
                function.new_instruction(&mut inner_exit_block, InstructionKind::jump(block.id));
                function.block_table.insert(inner_exit_block);
            }
            ASTStatementKind::Return(stmt_return) => {
                let temporary = stmt_return.expression.as_ref().map(|expression| {
                    codegen_function_expression(
                        &mut block,
                        node_variable_table,
                        type_table,
                        function,
                        &expression,
                        node_function,
                        node_type_table,
                        type_reference_table,
                        symbol_reference_table,
                    )
                });
                function.new_instruction(&mut block, InstructionKind::terminate(temporary));
                function.block_table.insert(block);

                block = function.new_block();
            }
            ASTStatementKind::Assignment(stmt_assignment) => {
                // FIXME: Introduce a variable mapping table, so we can easily lookup which variable is being assigned to.
                if let ASTExpressionKind::IdReference(expr_id_ref) = &stmt_assignment.left.kind {
                    if let Some(symbol_reference) =
                        symbol_reference_table.references.get(&expr_id_ref.id)
                    {
                        let variable = match &symbol_reference.kind {
                            SymbolNodeKind::Function => continue,
                            SymbolNodeKind::Parameter { index } => {
                                function.parameters[*index].variable_id
                            }
                            SymbolNodeKind::Variable => node_variable_table[&symbol_reference.node],
                        };
                        let temporary = codegen_function_expression(
                            &mut block,
                            node_variable_table,
                            type_table,
                            function,
                            &stmt_assignment.right,
                            node_function,
                            node_type_table,
                            type_reference_table,
                            symbol_reference_table,
                        );
                        function.new_instruction(
                            &mut block,
                            InstructionKind::Store {
                                variable,
                                temporary,
                            },
                        );
                    }
                }
            }
            ASTStatementKind::Row(stmt_row) => {
                codegen_function_expression(
                    &mut block,
                    node_variable_table,
                    type_table,
                    function,
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
    ast: &ASTIf,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
) -> (BlockId, BasicBlock) {
    let mut stack = Vec::new();

    let mut block = function.new_block();
    let condition = codegen_function_expression(
        &mut block,
        node_variable_table,
        type_table,
        function,
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
        &ast.body_block,
        node_function,
        node_type_table,
        type_reference_table,
        symbol_reference_table,
    );
    let end_block = function.new_block();
    let branch_instruction = function.new_instruction(
        &mut block,
        InstructionKind::branch(condition, then_entry_block_id, end_block.id),
    );
    function.new_instruction(
        &mut then_exit_block,
        InstructionKind::Jump {
            block: end_block.id,
        },
    );
    function.block_table.insert(then_exit_block);
    stack.push((
        block,
        condition,
        branch_instruction,
        then_entry_block_id,
        end_block,
    ));

    for single_else_if in &ast.single_else_ifs {
        let mut block = function.new_block();
        let condition = codegen_function_expression(
            &mut block,
            node_variable_table,
            type_table,
            function,
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
            &single_else_if.body_block,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
        );
        let end_block = function.new_block();
        let branch_instruction = function.new_instruction(
            &mut block,
            InstructionKind::branch(condition, then_entry_block_id, end_block.id),
        );
        function.new_instruction(
            &mut then_exit_block,
            InstructionKind::Jump {
                block: end_block.id,
            },
        );
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
            &single_else.body_block,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
        );
        let (_, condition, branch_instruction, then_entry_block_id, end_block) =
            stack.last_mut().unwrap();
        function.instruction_table.replace(
            *branch_instruction,
            InstructionKind::branch(*condition, *then_entry_block_id, else_entry_id),
        );
        function.new_instruction(
            &mut else_exit_block,
            InstructionKind::Jump {
                block: end_block.id,
            },
        );
        function.block_table.insert(else_exit_block);
    }

    let (mut last_block, _, _, _, mut last_end_block) = stack.pop().unwrap();

    while let Some((block, condition, branch_instruction, then_entry_block_id, end_block)) =
        stack.pop()
    {
        function.instruction_table.replace(
            branch_instruction,
            InstructionKind::branch(condition, then_entry_block_id, last_end_block.id),
        );
        function.new_instruction(&mut last_end_block, InstructionKind::jump(end_block.id));

        function.block_table.insert(last_block);
        function.block_table.insert(last_end_block);

        last_block = block;
        last_end_block = end_block;
    }

    let entry_id = last_block.id;
    function.block_table.insert(last_block);

    (entry_id, last_end_block)
}

fn codegen_function_expression(
    block: &mut BasicBlock,
    node_variable_table: &mut HashMap<NodeId, VariableId>,
    type_table: &mut TypeTable,
    function: &mut Function,
    ast: &ASTExpression,
    node_function: &NodeFunction,
    node_type_table: &NodeTypeTable,
    type_reference_table: &TypeReferenceTable,
    symbol_reference_table: &SymbolReferenceTable,
) -> TemporaryId {
    let temporary = match &ast.kind {
        ASTExpressionKind::Binary(expr_binary) => {
            let left = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
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
                &expr_binary.right,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = function.temporary_table.insert(type_id);
            function.new_instruction(
                block,
                InstructionKind::assign(
                    temporary,
                    Expression::new(
                        ExpressionKind::binary(
                            convert_binary_op(expr_binary.operator_kind),
                            left,
                            right,
                        ),
                        type_id,
                    ),
                ),
            );
            temporary
        }
        ASTExpressionKind::Unary(expr_unary) => {
            let right = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
                &expr_unary.right,
                node_function,
                node_type_table,
                type_reference_table,
                symbol_reference_table,
            );
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = function.temporary_table.insert(type_id);
            function.new_instruction(
                block,
                InstructionKind::assign(
                    temporary,
                    Expression::new(
                        ExpressionKind::unary(convert_unary_op(expr_unary.operator_kind), right),
                        type_id,
                    ),
                ),
            );
            temporary
        }
        ASTExpressionKind::As(expr_as) => {
            let expression = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
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
            let to_type_id =
                typename_to_type_id(type_table, type_reference_table, &expr_as.typename);
            let temporary = function.temporary_table.insert(to_type_id);
            function.new_instruction(
                block,
                InstructionKind::assign(
                    temporary,
                    Expression::new(
                        ExpressionKind::convert(expression, from_type_id, to_type_id),
                        to_type_id,
                    ),
                ),
            );
            temporary
        }
        ASTExpressionKind::Call(expr_call) => {
            let expression = codegen_function_expression(
                block,
                node_variable_table,
                type_table,
                function,
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
            let temporary = function.temporary_table.insert(type_id);
            function.new_instruction(
                block,
                InstructionKind::assign(
                    temporary,
                    Expression::new(ExpressionKind::call(expression, arguments), type_id),
                ),
            );
            temporary
        }
        ASTExpressionKind::Paren(expr_paren) => codegen_function_expression(
            block,
            node_variable_table,
            type_table,
            function,
            &expr_paren.expression,
            node_function,
            node_type_table,
            type_reference_table,
            symbol_reference_table,
        ),
        ASTExpressionKind::Literal(expr_literal) => {
            let type_id =
                type_kind_to_type_id(type_table, node_type_table.types.get(&ast.id).unwrap());
            let temporary = function.temporary_table.insert(type_id);
            function.new_instruction(
                block,
                InstructionKind::assign(
                    temporary,
                    Expression::new(
                        ExpressionKind::literal(expr_literal.literal.clone()),
                        type_id,
                    ),
                ),
            );
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
                    let temporary = function.temporary_table.insert(type_id);
                    function.new_instruction(
                        block,
                        InstructionKind::assign(
                            temporary,
                            Expression::new(
                                ExpressionKind::function(symbol_reference.id.symbol),
                                type_id,
                            ),
                        ),
                    );
                    temporary
                }
                SymbolNodeKind::Parameter { index } => {
                    let type_id = function.parameters[index].type_id;
                    let variable = function.parameters[index].variable_id;
                    let temporary = function.temporary_table.insert(type_id);
                    function.new_instruction(
                        block,
                        InstructionKind::assign(
                            temporary,
                            Expression::new(ExpressionKind::variable(variable), type_id),
                        ),
                    );
                    temporary
                }
                SymbolNodeKind::Variable => {
                    let type_id = type_kind_to_type_id(
                        type_table,
                        node_type_table.types.get(&ast.id).unwrap(),
                    );
                    let variable = node_variable_table[&symbol_reference.node].clone();
                    let temporary = function.temporary_table.insert(type_id);
                    function.new_instruction(
                        block,
                        InstructionKind::assign(
                            temporary,
                            Expression::new(ExpressionKind::variable(variable), type_id),
                        ),
                    );
                    temporary
                }
            }
        }
    };
    block.temporaries.insert(temporary);
    temporary
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
    };
    type_table.insert(type_kind)
}
