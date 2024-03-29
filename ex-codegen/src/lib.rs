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
mod pointer;
mod program;
mod temporary;
mod temporary_id;
mod temporary_id_allocator;
mod temporary_table;
mod terminator;
mod type_id;
mod type_id_allocator;
mod type_id_kind;
mod type_id_table;
mod user_struct;
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
pub use pointer::*;
pub use program::*;
pub use temporary::*;
pub use temporary_id::*;
pub use temporary_id_allocator::*;
pub use temporary_table::*;
pub use terminator::*;
pub use type_id::*;
pub use type_id_allocator::*;
pub use type_id_kind::*;
pub use type_id_table::*;
pub use user_struct::*;
pub use variable::*;
pub use variable_id::*;
pub use variable_id_allocator::*;
pub use variable_table::*;

use ex_parser::NodeId;
use ex_semantic_checking::{
    HIRBinaryOperatorKind, HIRBlock, HIRExpression, HIRExpressionKind, HIRFunction, HIRIf, HIRLoop,
    HIRProgram, HIRStatementKind, HIRUnaryOperatorKind, ReferenceTable, TopLevelTable, TypeKind,
    TypeTable,
};
use std::{collections::HashMap, iter::empty as iter_empty};

pub fn codegen(
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRProgram,
) -> Program {
    let mut program = Program::new();

    for function in &hir.functions {
        let function = codegen_function(
            &mut program.type_id_table,
            top_level_table,
            reference_table,
            type_table,
            function,
        );
        program.functions.insert(function.id, function);
    }

    for user_struct in &hir.structs {
        let user_struct = UserStruct::new(
            user_struct.id,
            user_struct.name.symbol,
            user_struct
                .fields
                .iter()
                .map(|field| {
                    UserStructField::new(
                        field.name.symbol,
                        type_kind_to_type_id(&mut program.type_id_table, &field.type_ref.kind),
                    )
                })
                .collect(),
        );
        program.user_structs.insert(user_struct.id, user_struct);
    }

    program
}

fn codegen_function(
    type_id_table: &mut TypeIdTable,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRFunction,
) -> Function {
    let mut function = Function::new(
        hir.id,
        hir.signature.name.symbol,
        hir.signature
            .params
            .iter()
            .map(|param| type_kind_to_type_id(type_id_table, &param.type_ref.kind))
            .collect(),
        hir.signature
            .return_type
            .as_ref()
            .map(|return_type| type_kind_to_type_id(type_id_table, &return_type.type_ref.kind))
            .unwrap_or_else(|| type_id_table.insert(TypeIdKind::Empty)),
    );
    let mut reverse_variable_table = ReverseVariableTable::new();

    let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_block(
        type_id_table,
        &mut reverse_variable_table,
        &mut function,
        None,
        top_level_table,
        reference_table,
        type_table,
        &hir.body_block,
    );

    function
        .block_table
        .blocks
        .get_mut(&function.entry_block_id)
        .unwrap()
        .set_terminator(Terminator::jump(inner_entry_block_id, vec![]));
    inner_exit_block.set_terminator(Terminator::terminate(None));
    function.block_table.insert(inner_exit_block);

    function
}

#[derive(Debug, Clone, Hash)]
struct LoopContext {
    pub entry_block_id: BlockId,
    pub exit_block_id: BlockId,
}

type ReverseVariableTable = HashMap<NodeId, VariableId>;

// TODO: Add destructor calls, by adding stack count.
fn codegen_function_stmt_block(
    type_id_table: &mut TypeIdTable,
    reverse_variable_table: &mut ReverseVariableTable,
    function: &mut Function,
    loop_context: Option<&LoopContext>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRBlock,
) -> (BlockId, BasicBlock) {
    let mut block = function.new_block(iter_empty());
    let entry_block_id = block.id;

    for statement in &hir.statements {
        match &statement.kind {
            HIRStatementKind::Block(hir) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_block(
                    type_id_table,
                    reverse_variable_table,
                    function,
                    loop_context,
                    top_level_table,
                    reference_table,
                    type_table,
                    hir,
                );
                block.set_terminator(Terminator::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            HIRStatementKind::Let(hir) => {
                let type_id = type_kind_to_type_id(type_id_table, &type_table.types[&statement.id]);
                let variable = function.variable_table.insert(type_id);
                reverse_variable_table.insert(statement.id, variable);

                if let Some(let_assignment) = &hir.let_assignment {
                    let temporary = codegen_function_expression(
                        type_id_table,
                        &mut block,
                        reverse_variable_table,
                        function,
                        top_level_table,
                        type_table,
                        &let_assignment.expression,
                    );
                    block.new_instruction(InstructionKind::store(
                        Pointer::variable(variable),
                        temporary,
                    ));
                }
            }
            HIRStatementKind::If(hir) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_if(
                    type_id_table,
                    reverse_variable_table,
                    function,
                    loop_context,
                    top_level_table,
                    reference_table,
                    type_table,
                    hir,
                );
                block.set_terminator(Terminator::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            HIRStatementKind::Loop(hir) => {
                let (inner_entry_block_id, inner_exit_block) = codegen_function_stmt_loop(
                    type_id_table,
                    reverse_variable_table,
                    function,
                    top_level_table,
                    reference_table,
                    type_table,
                    hir,
                );
                block.set_terminator(Terminator::jump(inner_entry_block_id, vec![]));
                function.block_table.insert(block);

                block = inner_exit_block;
            }
            HIRStatementKind::Break(_) => {
                let loop_context = loop_context.unwrap();
                block.set_terminator(Terminator::jump(loop_context.exit_block_id, vec![]));
                function.block_table.insert(block);

                block = function.new_block(iter_empty());
            }
            HIRStatementKind::Continue(_) => {
                let loop_context = loop_context.unwrap();
                block.set_terminator(Terminator::jump(loop_context.entry_block_id, vec![]));
                function.block_table.insert(block);

                block = function.new_block(iter_empty());
            }
            HIRStatementKind::Return(hir) => {
                match &hir.expression {
                    Some(expression) => {
                        let temporary = codegen_function_expression(
                            type_id_table,
                            &mut block,
                            reverse_variable_table,
                            function,
                            top_level_table,
                            type_table,
                            expression,
                        );
                        block.set_terminator(Terminator::terminate(Some(temporary)));
                    }
                    None => {
                        block.set_terminator(Terminator::terminate(None));
                    }
                }
                function.block_table.insert(block);

                block = function.new_block(iter_empty());
            }
            HIRStatementKind::Assignment(hir) => {
                let pointer = lhs_expression_to_pointer(
                    type_id_table,
                    &mut block,
                    reverse_variable_table,
                    function,
                    top_level_table,
                    type_table,
                    &hir.left,
                );
                let temporary = codegen_function_expression(
                    type_id_table,
                    &mut block,
                    reverse_variable_table,
                    function,
                    top_level_table,
                    type_table,
                    &hir.right,
                );
                block.new_instruction(InstructionKind::store(pointer, temporary));
            }
            HIRStatementKind::Row(hir) => {
                let temporary = codegen_function_expression(
                    type_id_table,
                    &mut block,
                    reverse_variable_table,
                    function,
                    top_level_table,
                    type_table,
                    &hir.expression,
                );
                block.new_instruction(InstructionKind::expression(temporary));
            }
        }
    }

    (entry_block_id, block)
}

fn codegen_function_stmt_if(
    type_id_table: &mut TypeIdTable,
    reverse_variable_table: &mut ReverseVariableTable,
    function: &mut Function,
    loop_context: Option<&LoopContext>,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRIf,
) -> (BlockId, BasicBlock) {
    let mut stack = Vec::new();

    let mut block = function.new_block(iter_empty());
    let condition = codegen_function_expression(
        type_id_table,
        &mut block,
        reverse_variable_table,
        function,
        top_level_table,
        type_table,
        &hir.expression,
    );
    let (then_entry_block_id, mut then_exit_block) = codegen_function_stmt_block(
        type_id_table,
        reverse_variable_table,
        function,
        loop_context,
        top_level_table,
        reference_table,
        type_table,
        &hir.body_block,
    );
    let end_block = function.new_block(iter_empty());
    block.set_terminator(Terminator::branch(
        condition,
        then_entry_block_id,
        vec![],
        end_block.id,
        vec![],
    ));
    then_exit_block.set_terminator(Terminator::jump(end_block.id, vec![]));
    function.block_table.insert(then_exit_block);
    stack.push((block, condition, then_entry_block_id, end_block));

    for hir in &hir.single_else_ifs {
        let mut block = function.new_block(iter_empty());
        let condition = codegen_function_expression(
            type_id_table,
            &mut block,
            reverse_variable_table,
            function,
            top_level_table,
            type_table,
            &hir.expression,
        );
        let (then_entry_block_id, mut then_exit_block) = codegen_function_stmt_block(
            type_id_table,
            reverse_variable_table,
            function,
            loop_context,
            top_level_table,
            reference_table,
            type_table,
            &hir.body_block,
        );
        let end_block = function.new_block(iter_empty());
        block.set_terminator(Terminator::branch(
            condition,
            then_entry_block_id,
            vec![],
            end_block.id,
            vec![],
        ));
        then_exit_block.set_terminator(Terminator::jump(end_block.id, vec![]));
        function.block_table.insert(then_exit_block);
        stack.push((block, condition, then_entry_block_id, end_block));
    }

    if let Some(hir) = &hir.single_else {
        let (else_entry_id, mut else_exit_block) = codegen_function_stmt_block(
            type_id_table,
            reverse_variable_table,
            function,
            loop_context,
            top_level_table,
            reference_table,
            type_table,
            &hir.body_block,
        );
        let (block, condition, then_entry_block_id, end_block) = stack.last_mut().unwrap();
        block.set_terminator(Terminator::branch(
            *condition,
            *then_entry_block_id,
            vec![],
            else_entry_id,
            vec![],
        ));
        else_exit_block.set_terminator(Terminator::jump(end_block.id, vec![]));
        function.block_table.insert(else_exit_block);
    }

    let (mut last_block, _, _, mut last_end_block) = stack.pop().unwrap();

    while let Some((mut block, condition, then_entry_block_id, end_block)) = stack.pop() {
        block.set_terminator(Terminator::branch(
            condition,
            then_entry_block_id,
            vec![],
            last_end_block.id,
            vec![],
        ));
        last_end_block.set_terminator(Terminator::jump(end_block.id, vec![]));

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
    type_id_table: &mut TypeIdTable,
    reverse_variable_table: &mut ReverseVariableTable,
    function: &mut Function,
    top_level_table: &TopLevelTable,
    reference_table: &ReferenceTable,
    type_table: &TypeTable,
    hir: &HIRLoop,
) -> (BlockId, BasicBlock) {
    let mut entry_block = function.new_block(iter_empty());
    let entry_block_id = entry_block.id;
    let exit_block = function.new_block(iter_empty());
    let loop_context = LoopContext {
        entry_block_id,
        exit_block_id: exit_block.id,
    };

    let (inner_entry_block_id, mut inner_exit_block) = codegen_function_stmt_block(
        type_id_table,
        reverse_variable_table,
        function,
        Some(&loop_context),
        top_level_table,
        reference_table,
        type_table,
        &hir.body_block,
    );
    entry_block.set_terminator(Terminator::jump(inner_entry_block_id, vec![]));
    function.block_table.insert(entry_block);

    inner_exit_block.set_terminator(Terminator::jump(entry_block_id, vec![]));
    function.block_table.insert(inner_exit_block);

    (entry_block_id, exit_block)
}

fn codegen_function_expression(
    type_id_table: &mut TypeIdTable,
    block: &mut BasicBlock,
    reverse_variable_table: &ReverseVariableTable,
    function: &Function,
    top_level_table: &TopLevelTable,
    type_table: &TypeTable,
    hir: &HIRExpression,
) -> TemporaryId {
    let type_id = type_kind_to_type_id(type_id_table, &type_table.types[&hir.id]);

    match &hir.kind {
        HIRExpressionKind::Binary(hir) => {
            let left = codegen_function_expression(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                &hir.left,
            );
            let right = codegen_function_expression(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                &hir.right,
            );
            block.new_temporary(
                type_id,
                Expression::new(
                    ExpressionKind::binary(convert_binary_op(hir.operator_kind), left, right),
                    type_id,
                ),
            )
        }
        HIRExpressionKind::Unary(hir) => {
            match hir.operator_kind {
                HIRUnaryOperatorKind::AddressOf => {
                    // TODO: Look into hir.right and see if the given expression is variable or not.
                    // If variable, we can apply ExpressionKind::element_pointer directly.
                    // Otherwise, this means that we're trying to obtain a pointer of something that has no address.
                    // This makes no sense, so we should error out.
                    // TODO: Let `check_lvalues` or similar pass handle this.
                    // TODO: If the given expression is not a variable and is a reference, we should be able to obtain a pointer of it.
                    let right = lhs_expression_to_pointer(
                        type_id_table,
                        block,
                        reverse_variable_table,
                        function,
                        top_level_table,
                        type_table,
                        &hir.right,
                    );
                    block.new_temporary(
                        type_id,
                        Expression::new(ExpressionKind::pointer(right), type_id),
                    )
                }
                HIRUnaryOperatorKind::Dereference => {
                    let right = codegen_function_expression(
                        type_id_table,
                        block,
                        reverse_variable_table,
                        function,
                        top_level_table,
                        type_table,
                        &hir.right,
                    );
                    block.new_temporary(
                        type_id,
                        Expression::new(ExpressionKind::load(Pointer::RawPointer(right)), type_id),
                    )
                }
                _ => {
                    let right = codegen_function_expression(
                        type_id_table,
                        block,
                        reverse_variable_table,
                        function,
                        top_level_table,
                        type_table,
                        &hir.right,
                    );
                    block.new_temporary(
                        type_id,
                        Expression::new(
                            ExpressionKind::unary(convert_unary_op(hir.operator_kind), right),
                            type_id,
                        ),
                    )
                }
            }
        }
        HIRExpressionKind::As(hir) => {
            let expression = codegen_function_expression(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                &hir.expression,
            );
            let from_type_id =
                type_kind_to_type_id(type_id_table, &type_table.types[&hir.expression.id]);
            block.new_temporary(
                type_id,
                Expression::new(
                    ExpressionKind::convert(expression, from_type_id, type_id),
                    type_id,
                ),
            )
        }
        HIRExpressionKind::Call(hir) => {
            let expression = codegen_function_expression(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                &hir.expression,
            );
            let args = hir
                .args
                .iter()
                .map(|arg| {
                    codegen_function_expression(
                        type_id_table,
                        block,
                        reverse_variable_table,
                        function,
                        top_level_table,
                        type_table,
                        arg,
                    )
                })
                .collect();
            block.new_temporary(
                type_id,
                Expression::new(ExpressionKind::call(expression, args), type_id),
            )
        }
        HIRExpressionKind::Member(..) => {
            let pointer = lhs_expression_to_pointer(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                hir,
            );
            block.new_temporary(
                type_id,
                Expression::new(ExpressionKind::load(pointer), type_id),
            )
        }
        HIRExpressionKind::FunctionRef(..) => {
            let pointer = lhs_expression_to_pointer(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                hir,
            );
            block.new_temporary(
                type_id,
                Expression::new(ExpressionKind::pointer(pointer), type_id),
            )
        }
        HIRExpressionKind::ParamRef(..) => {
            let pointer = lhs_expression_to_pointer(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                hir,
            );
            block.new_temporary(
                type_id,
                Expression::new(ExpressionKind::load(pointer), type_id),
            )
        }
        HIRExpressionKind::VariableRef(..) => {
            let pointer = lhs_expression_to_pointer(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                hir,
            );
            block.new_temporary(
                type_id,
                Expression::new(ExpressionKind::load(pointer), type_id),
            )
        }
        HIRExpressionKind::UnknownRef(..) => {
            unreachable!()
        }
        HIRExpressionKind::StructLiteral(hir) => {
            let user_struct = match &hir.type_ref.kind {
                TypeKind::UserStruct { id } => {
                    top_level_table.user_types[id].as_user_struct().unwrap()
                }
                _ => unreachable!(),
            };
            let mut fields_with_indices = hir
                .fields
                .iter()
                .map(|field| {
                    (
                        user_struct.field_names[&field.name.symbol],
                        codegen_function_expression(
                            type_id_table,
                            block,
                            reverse_variable_table,
                            function,
                            top_level_table,
                            type_table,
                            &field.expression,
                        ),
                    )
                })
                .collect::<Vec<_>>();
            fields_with_indices.sort_unstable_by_key(|field| field.0);
            block.new_temporary(
                type_id,
                Expression::new(
                    ExpressionKind::struct_literal(
                        type_id,
                        fields_with_indices
                            .into_iter()
                            .map(|field| field.1)
                            .collect(),
                    ),
                    type_id,
                ),
            )
        }
        HIRExpressionKind::Literal(literal) => block.new_temporary(
            type_id,
            Expression::new(ExpressionKind::literal(literal.clone()), type_id),
        ),
    }
}

fn lhs_expression_to_pointer(
    type_id_table: &mut TypeIdTable,
    block: &mut BasicBlock,
    reverse_variable_table: &ReverseVariableTable,
    function: &Function,
    top_level_table: &TopLevelTable,
    type_table: &TypeTable,
    hir: &HIRExpression,
) -> Pointer {
    match &hir.kind {
        HIRExpressionKind::Binary(..) => unreachable!(),
        HIRExpressionKind::Unary(hir) => match hir.operator_kind {
            HIRUnaryOperatorKind::AddressOf => {
                // We cannot take the address of a temporary.
                unreachable!()
            }
            HIRUnaryOperatorKind::Dereference => {
                // We don't take dereferences of pointers here.
                // Instead, forward the given pointer directly.
                let temporary = codegen_function_expression(
                    type_id_table,
                    block,
                    reverse_variable_table,
                    function,
                    top_level_table,
                    type_table,
                    &hir.right,
                );
                Pointer::raw_pointer(temporary)
            }
            _ => unreachable!(),
        },
        HIRExpressionKind::As(..) => unreachable!(),
        HIRExpressionKind::Call(..) => unreachable!(),
        HIRExpressionKind::Member(hir) => {
            let pointer = lhs_expression_to_pointer(
                type_id_table,
                block,
                reverse_variable_table,
                function,
                top_level_table,
                type_table,
                &hir.expression,
            );
            let user_struct = match type_table.types[&hir.expression.id].unwrap_reference() {
                TypeKind::UserStruct { id } => {
                    top_level_table.user_types[id].as_user_struct().unwrap()
                }
                _ => unreachable!(),
            };
            let index = user_struct.field_names[&hir.member.symbol];
            let type_id = type_kind_to_type_id(type_id_table, &user_struct.fields[index]);
            let temporary = block.new_temporary(
                type_id,
                Expression::new(
                    ExpressionKind::ElementPointer {
                        base: pointer,
                        indices: vec![index],
                    },
                    type_id,
                ),
            );
            Pointer::raw_pointer(temporary)
        }
        HIRExpressionKind::FunctionRef(hir) => Pointer::Function(FunctionId::new(hir.function)),
        HIRExpressionKind::ParamRef(hir) => {
            let variable = function.params[hir.index].variable_id;
            Pointer::variable(variable)
        }
        HIRExpressionKind::VariableRef(hir) => {
            let variable = reverse_variable_table[&hir.statement];
            Pointer::variable(variable)
        }
        HIRExpressionKind::UnknownRef(..) => unreachable!(),
        HIRExpressionKind::StructLiteral(..) => unreachable!(),
        HIRExpressionKind::Literal(..) => unreachable!(),
    }
}

fn convert_binary_op(op: HIRBinaryOperatorKind) -> BinaryOperator {
    match op {
        HIRBinaryOperatorKind::Eq => BinaryOperator::Eq,
        HIRBinaryOperatorKind::Ne => BinaryOperator::Ne,
        HIRBinaryOperatorKind::Lt => BinaryOperator::Lt,
        HIRBinaryOperatorKind::Gt => BinaryOperator::Gt,
        HIRBinaryOperatorKind::Le => BinaryOperator::Le,
        HIRBinaryOperatorKind::Ge => BinaryOperator::Ge,
        HIRBinaryOperatorKind::LogOr => BinaryOperator::LogOr,
        HIRBinaryOperatorKind::LogAnd => BinaryOperator::LogAnd,
        HIRBinaryOperatorKind::Add => BinaryOperator::Add,
        HIRBinaryOperatorKind::Sub => BinaryOperator::Sub,
        HIRBinaryOperatorKind::Mul => BinaryOperator::Mul,
        HIRBinaryOperatorKind::Div => BinaryOperator::Div,
        HIRBinaryOperatorKind::Mod => BinaryOperator::Mod,
        HIRBinaryOperatorKind::Pow => BinaryOperator::Pow,
        HIRBinaryOperatorKind::Shl => BinaryOperator::Shl,
        HIRBinaryOperatorKind::Shr => BinaryOperator::Shr,
        HIRBinaryOperatorKind::BitOr => BinaryOperator::BitOr,
        HIRBinaryOperatorKind::BitAnd => BinaryOperator::BitAnd,
        HIRBinaryOperatorKind::BitXor => BinaryOperator::BitXor,
    }
}

fn convert_unary_op(op: HIRUnaryOperatorKind) -> UnaryOperator {
    match op {
        HIRUnaryOperatorKind::Minus => UnaryOperator::Minus,
        HIRUnaryOperatorKind::BitNot => UnaryOperator::BitNot,
        HIRUnaryOperatorKind::LogNot => UnaryOperator::LogNot,
        HIRUnaryOperatorKind::AddressOf => unreachable!(),
        HIRUnaryOperatorKind::Dereference => unreachable!(),
    }
}

fn type_kind_to_type_id(type_id_table: &mut TypeIdTable, type_kind: &TypeKind) -> TypeId {
    let type_kind = match type_kind {
        TypeKind::Unknown => unreachable!(),
        TypeKind::Empty => TypeIdKind::Empty,
        TypeKind::Bool => TypeIdKind::Bool,
        TypeKind::Int => TypeIdKind::Int,
        TypeKind::Float => TypeIdKind::Float,
        TypeKind::String => TypeIdKind::String,
        TypeKind::Callable {
            params,
            return_type,
        } => TypeIdKind::Callable {
            param_types: params
                .iter()
                .map(|type_kind| type_kind_to_type_id(type_id_table, type_kind))
                .collect(),
            return_type: type_kind_to_type_id(type_id_table, return_type),
        },
        TypeKind::UserStruct { id } => TypeIdKind::UserStruct {
            id: UserStructId::new(*id),
        },
        TypeKind::Pointer { inner } => TypeIdKind::Pointer {
            type_id: type_kind_to_type_id(type_id_table, inner),
        },
        TypeKind::Reference { inner } => TypeIdKind::Reference {
            type_id: type_kind_to_type_id(type_id_table, inner),
        },
    };
    type_id_table.insert(type_kind)
}
