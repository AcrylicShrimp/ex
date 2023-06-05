mod value;

pub use value::*;

use ex_codegen::{
    BinaryOperator, Expression, ExpressionKind, Function, InstructionKind, Pointer, Program,
    TemporaryId, TypeIdKind, UnaryOperator, VariableId,
};
use ex_symbol::Symbol;
use std::collections::HashMap;

pub fn execute(program: &Program) {
    let main_function_symbol = Symbol::from_str("main");
    let main_function_id = program.functions.iter().find_map(|(id, function)| {
        if function.name == main_function_symbol {
            Some(id)
        } else {
            None
        }
    });
    let main_function = match main_function_id {
        Some(main_function_index) => &program.functions[main_function_index],
        None => {
            println!("no main function found");
            return;
        }
    };

    execute_function(program, main_function, vec![]);
}

fn execute_function(program: &Program, function: &Function, args: Vec<Value>) -> Value {
    if function.name.to_str().starts_with("__print_") {
        println!(
            "{}",
            args.iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
        return Value::Empty;
    }

    let mut stack: HashMap<VariableId, Value> =
        HashMap::from_iter(function.variable_table.variables.iter().map(
            |(variable_id, variable)| {
                (
                    *variable_id,
                    Value::empty_from_type_id(program, variable.type_id),
                )
            },
        ));
    let mut temp_stack: HashMap<TemporaryId, Value> = HashMap::new();

    for (index, arg) in args.into_iter().enumerate() {
        stack.insert(function.params[index].variable_id, arg);
    }

    let mut block_id = function.entry_block_id;

    'outer: loop {
        let basic_block = &function.block_table.blocks[&block_id];
        for instruction in &basic_block.instructions {
            let instruction = &basic_block.instruction_table.instructions[instruction];
            match &instruction.kind {
                // InstructionKind::Load { pointer, temporary } => {
                //     if pointer.indices.is_empty() {
                //         temp_stack.insert(*temporary, stack[&pointer.variable].clone());
                //     } else {
                //         let mut variable =
                //             stack.get_mut(&pointer.variable).unwrap().as_struct_mut().1;

                //         for index in &pointer.indices[..pointer.indices.len() - 1] {
                //             variable = variable[*index].as_struct_mut().1;
                //         }

                //         temp_stack.insert(
                //             *temporary,
                //             variable[pointer.indices[pointer.indices.len() - 1]].clone(),
                //         );
                //     }
                // }
                InstructionKind::Store { pointer, temporary } => {
                    match pointer {
                        Pointer::Function(..) => {
                            panic!("cannot store to function pointer")
                        }
                        Pointer::Variable(variable) => {}
                        Pointer::RawPointer(_) => {}
                    }
                    let variable = if pointer.indices.is_empty() {
                        stack.get_mut(&pointer.variable).unwrap()
                    } else {
                        let mut variable =
                            stack.get_mut(&pointer.variable).unwrap().as_struct_mut().1;

                        for index in &pointer.indices[..pointer.indices.len() - 1] {
                            variable = variable[*index].as_struct_mut().1;
                        }

                        &mut variable[pointer.indices[pointer.indices.len() - 1]]
                    };
                    let temporary = &temp_stack[temporary];
                    *variable = temporary.clone();
                }
                // InstructionKind::Assign {
                //     temporary,
                //     expression,
                // } => {
                //     let value = eval_expression(program, &temp_stack, expression);
                //     temp_stack.insert(*temporary, value);
                // }
                // InstructionKind::Extract {
                //     from,
                //     indices,
                //     temporary,
                // } => {
                //     if indices.is_empty() {
                //         temp_stack.insert(*temporary, temp_stack[from].clone());
                //     } else {
                //         let mut value = temp_stack[from].clone();

                //         for index in indices {
                //             value = value.as_struct().1[*index].clone();
                //         }

                //         temp_stack.insert(*temporary, value);
                //     }
                // }
                InstructionKind::Jump { block, args } => {
                    let next_block = &function.block_table.blocks[block];
                    let next_temp_stack = args
                        .iter()
                        .enumerate()
                        .map(|(index, arg)| (next_block.params[index], temp_stack[arg].clone()))
                        .collect();
                    block_id = *block;
                    temp_stack = next_temp_stack;
                    continue 'outer;
                }
                InstructionKind::Branch {
                    condition,
                    then_block,
                    then_args,
                    else_block,
                    else_args,
                } => {
                    let condition = temp_stack[condition].as_bool();
                    let (next_temp_stack, block) = if condition {
                        let next_block = &function.block_table.blocks[then_block];
                        (
                            then_args
                                .iter()
                                .enumerate()
                                .map(|(index, arg)| {
                                    (next_block.params[index], temp_stack[arg].clone())
                                })
                                .collect(),
                            then_block,
                        )
                    } else {
                        let next_block = &function.block_table.blocks[else_block];
                        (
                            else_args
                                .iter()
                                .enumerate()
                                .map(|(index, arg)| {
                                    (next_block.params[index], temp_stack[arg].clone())
                                })
                                .collect(),
                            else_block,
                        )
                    };
                    block_id = *block;
                    temp_stack = next_temp_stack;
                    continue 'outer;
                }
                InstructionKind::Terminate { temporary } => {
                    return temporary
                        .map(|temporary| temp_stack[&temporary].clone())
                        .unwrap_or_else(|| Value::Empty);
                }
            }
        }
    }
}

fn eval_expression(
    program: &Program,
    temp_stack: &HashMap<TemporaryId, Value>,
    expression: &Expression,
) -> Value {
    match &expression.kind {
        ExpressionKind::Binary {
            operator,
            left,
            right,
        } => {
            let left = &temp_stack[left];
            let right = &temp_stack[right];
            match (operator, left, right) {
                (BinaryOperator::Eq, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(left == right)
                }
                (BinaryOperator::Eq, Value::Integer(left), Value::Integer(right)) => {
                    Value::Boolean(left == right)
                }
                (BinaryOperator::Eq, Value::Float(left), Value::Float(right)) => {
                    Value::Boolean(left == right)
                }
                (BinaryOperator::Eq, Value::String(left), Value::String(right)) => {
                    Value::Boolean(left == right)
                }
                (BinaryOperator::Ne, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(left != right)
                }
                (BinaryOperator::Ne, Value::Integer(left), Value::Integer(right)) => {
                    Value::Boolean(left != right)
                }
                (BinaryOperator::Ne, Value::Float(left), Value::Float(right)) => {
                    Value::Boolean(left != right)
                }
                (BinaryOperator::Ne, Value::String(left), Value::String(right)) => {
                    Value::Boolean(left != right)
                }
                (BinaryOperator::Lt, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(left < right)
                }
                (BinaryOperator::Lt, Value::Integer(left), Value::Integer(right)) => {
                    Value::Boolean(left < right)
                }
                (BinaryOperator::Lt, Value::Float(left), Value::Float(right)) => {
                    Value::Boolean(left < right)
                }
                (BinaryOperator::Lt, Value::String(left), Value::String(right)) => {
                    Value::Boolean(left < right)
                }
                (BinaryOperator::Gt, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(left > right)
                }
                (BinaryOperator::Gt, Value::Integer(left), Value::Integer(right)) => {
                    Value::Boolean(left > right)
                }
                (BinaryOperator::Gt, Value::Float(left), Value::Float(right)) => {
                    Value::Boolean(left > right)
                }
                (BinaryOperator::Gt, Value::String(left), Value::String(right)) => {
                    Value::Boolean(left > right)
                }
                (BinaryOperator::Le, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(left <= right)
                }
                (BinaryOperator::Le, Value::Integer(left), Value::Integer(right)) => {
                    Value::Boolean(left <= right)
                }
                (BinaryOperator::Le, Value::Float(left), Value::Float(right)) => {
                    Value::Boolean(left <= right)
                }
                (BinaryOperator::Le, Value::String(left), Value::String(right)) => {
                    Value::Boolean(left <= right)
                }
                (BinaryOperator::Ge, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(left >= right)
                }
                (BinaryOperator::Ge, Value::Integer(left), Value::Integer(right)) => {
                    Value::Boolean(left >= right)
                }
                (BinaryOperator::Ge, Value::Float(left), Value::Float(right)) => {
                    Value::Boolean(left >= right)
                }
                (BinaryOperator::Ge, Value::String(left), Value::String(right)) => {
                    Value::Boolean(left >= right)
                }
                (BinaryOperator::LogOr, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(*left || *right)
                }
                (BinaryOperator::LogAnd, Value::Boolean(left), Value::Boolean(right)) => {
                    Value::Boolean(*left || *right)
                }
                (BinaryOperator::Add, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left + *right)
                }
                (BinaryOperator::Add, Value::Float(left), Value::Float(right)) => {
                    Value::Float(*left + *right)
                }
                (BinaryOperator::Add, Value::String(left), Value::String(right)) => {
                    Value::String(format!("{}{}", left, right))
                }
                (BinaryOperator::Sub, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left - *right)
                }
                (BinaryOperator::Sub, Value::Float(left), Value::Float(right)) => {
                    Value::Float(*left - *right)
                }
                (BinaryOperator::Mul, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left * *right)
                }
                (BinaryOperator::Mul, Value::Float(left), Value::Float(right)) => {
                    Value::Float(*left * *right)
                }
                (BinaryOperator::Mul, Value::String(left), Value::Integer(right)) => {
                    Value::String(left.repeat(*right as usize))
                }
                (BinaryOperator::Mul, Value::Integer(left), Value::String(right)) => {
                    Value::String(right.repeat(*left as usize))
                }
                (BinaryOperator::Div, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left / *right)
                }
                (BinaryOperator::Div, Value::Float(left), Value::Float(right)) => {
                    Value::Float(*left / *right)
                }
                (BinaryOperator::Mod, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left % *right)
                }
                (BinaryOperator::Mod, Value::Float(left), Value::Float(right)) => {
                    Value::Float(*left % *right)
                }
                (BinaryOperator::Pow, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(left.pow(*right as u32))
                }
                (BinaryOperator::Pow, Value::Float(left), Value::Float(right)) => {
                    Value::Float(left.powf(*right))
                }
                (BinaryOperator::Shl, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left << *right)
                }
                (BinaryOperator::Shr, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left >> *right)
                }
                (BinaryOperator::BitOr, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left | *right)
                }
                (BinaryOperator::BitAnd, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left & *right)
                }
                (BinaryOperator::BitXor, Value::Integer(left), Value::Integer(right)) => {
                    Value::Integer(*left ^ *right)
                }
                _ => unreachable!(),
            }
        }
        ExpressionKind::Unary { operator, right } => {
            let right = &temp_stack[right];
            match (operator, right) {
                (UnaryOperator::Minus, Value::Integer(integer)) => Value::Integer(-integer),
                (UnaryOperator::Minus, Value::Float(float)) => Value::Float(-float),
                (UnaryOperator::BitNot, Value::Integer(integer)) => Value::Integer(!integer),
                (UnaryOperator::LogNot, Value::Boolean(boolean)) => Value::Boolean(!boolean),
                _ => unreachable!(),
            }
        }
        ExpressionKind::Convert {
            expression,
            from,
            to,
        } => {
            let value = temp_stack[expression].clone();
            match (
                &program.type_id_table.types[from],
                &program.type_id_table.types[to],
            ) {
                (TypeIdKind::Bool, TypeIdKind::Bool) => value,
                (TypeIdKind::Bool, TypeIdKind::Int) => {
                    Value::Integer(if value.as_bool() { 1 } else { 0 })
                }
                (TypeIdKind::Bool, TypeIdKind::Float) => {
                    Value::Float(if value.as_bool() { 1f64 } else { 0f64 })
                }
                (TypeIdKind::Bool, TypeIdKind::String) => {
                    Value::String(if value.as_bool() { "true" } else { "false" }.to_string())
                }
                (TypeIdKind::Int, TypeIdKind::Bool) => Value::Boolean(value.as_integer() != 0),
                (TypeIdKind::Int, TypeIdKind::Int) => value,
                (TypeIdKind::Int, TypeIdKind::Float) => Value::Float(value.as_integer() as f64),
                (TypeIdKind::Int, TypeIdKind::String) => {
                    Value::String(value.as_integer().to_string())
                }
                (TypeIdKind::Float, TypeIdKind::Bool) => Value::Boolean(value.as_float() != 0f64),
                (TypeIdKind::Float, TypeIdKind::Int) => Value::Integer(value.as_float() as i64),
                (TypeIdKind::Float, TypeIdKind::Float) => value,
                (TypeIdKind::Float, TypeIdKind::String) => {
                    Value::String(value.as_float().to_string())
                }
                (TypeIdKind::String, TypeIdKind::Bool) => {
                    Value::Boolean(value.as_string() != "false")
                }
                (TypeIdKind::String, TypeIdKind::Int) => {
                    Value::Integer(value.as_string().parse().unwrap())
                }
                (TypeIdKind::String, TypeIdKind::Float) => {
                    Value::Float(value.as_string().parse().unwrap())
                }
                (TypeIdKind::String, TypeIdKind::String) => value,
                _ => unreachable!(),
            }
        }
        ExpressionKind::Call { expression, args } => {
            let function = match temp_stack[expression] {
                Value::Callable(function) => function,
                _ => unreachable!(),
            };
            let args = args.iter().map(|arg| temp_stack[arg].clone()).collect();
            execute_function(program, &program.functions[&function], args)
        }
        ExpressionKind::Literal { literal } => {
            match &program.type_id_table.types[&expression.type_id] {
                TypeIdKind::Bool => Value::Boolean(literal.content == Symbol::from_str("true")),
                TypeIdKind::Int => Value::Integer(literal.content.to_str().parse().unwrap()),
                TypeIdKind::Float => Value::Float(literal.content.to_str().parse().unwrap()),
                TypeIdKind::String => Value::String(literal.content.to_str().to_owned()),
                _ => unreachable!(),
            }
        }
        ExpressionKind::StructLiteral {
            struct_type,
            fields,
        } => {
            let fields = fields
                .iter()
                .map(|field| temp_stack[field].clone())
                .collect();
            Value::Struct(*struct_type, fields)
        }
        ExpressionKind::Function { id } => Value::Callable(*id),
    }
}
