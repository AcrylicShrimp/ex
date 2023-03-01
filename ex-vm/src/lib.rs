mod value;

pub use value::*;

use ex_codegen::{
    BinaryOperator, Expression, ExpressionKind, Function, InstructionKind, Program, TypeKind,
    UnaryOperator,
};
use ex_symbol::Symbol;

pub fn execute(program: &Program) {
    let main_function = match program.functions.get(&Symbol::from_str("main")) {
        Some(function) => function,
        None => {
            println!("no main function found");
            return;
        }
    };

    execute_function(program, main_function, vec![]);
}

fn execute_function(program: &Program, function: &Function, arguments: Vec<Value>) -> Value {
    if function.name == Symbol::from_str("print") {
        println!(
            "{}",
            arguments
                .iter()
                .map(|argument| argument.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
        return Value::Empty;
    }

    let mut stack = vec![Value::Empty; function.variable_table.variables.len()];
    let mut temp_stack = vec![Value::Empty; function.temporary_table.temporaries.len()];

    for (index, argument) in arguments.into_iter().enumerate() {
        stack[index] = argument;
    }

    let mut basic_block = function.entry_block;

    'outer: loop {
        for instruction in &function.block_table.blocks[&basic_block].instructions {
            let instruction = &function.instruction_table.instructions[instruction];

            match &instruction.kind {
                InstructionKind::Store {
                    variable,
                    temporary,
                } => {
                    let temporary = &temp_stack[temporary.get() as usize - 1];
                    stack[variable.get() as usize - 1] = temporary.clone();
                }
                InstructionKind::Assign {
                    temporary,
                    expression,
                } => {
                    let value = eval_expression(program, &stack, &temp_stack, expression);
                    temp_stack[temporary.get() as usize - 1] = value;
                }
                InstructionKind::Jump { block } => {
                    basic_block = *block;
                    continue 'outer;
                }
                InstructionKind::Branch {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let condition = temp_stack[condition.get() as usize - 1].as_bool();
                    if condition {
                        basic_block = *then_block;
                    } else {
                        basic_block = *else_block;
                    }
                    continue 'outer;
                }
                InstructionKind::Terminate { temporary } => {
                    return temporary
                        .map(|temporary| temp_stack[temporary.get() as usize - 1].clone())
                        .unwrap_or_else(|| Value::Empty);
                }
            }
        }
    }
}

fn eval_expression(
    program: &Program,
    stack: &Vec<Value>,
    temp_stack: &Vec<Value>,
    expression: &Expression,
) -> Value {
    match &expression.kind {
        ExpressionKind::Binary {
            operator,
            left,
            right,
        } => {
            let left = &temp_stack[left.get() as usize - 1];
            let right = &temp_stack[right.get() as usize - 1];
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
            let right = &temp_stack[right.get() as usize - 1];
            match (operator, right) {
                (UnaryOperator::Plus, Value::Integer(integer)) => Value::Integer(*integer),
                (UnaryOperator::Plus, Value::Float(float)) => Value::Float(*float),
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
            let value = temp_stack[expression.get() as usize - 1].clone();
            match (
                &program.type_table.types[from],
                &program.type_table.types[to],
            ) {
                (TypeKind::Boolean, TypeKind::Boolean) => value,
                (TypeKind::Boolean, TypeKind::Integer) => {
                    Value::Integer(if value.as_bool() { 1 } else { 0 })
                }
                (TypeKind::Boolean, TypeKind::Float) => {
                    Value::Float(if value.as_bool() { 1f64 } else { 0f64 })
                }
                (TypeKind::Boolean, TypeKind::String) => {
                    Value::String(if value.as_bool() { "true" } else { "false" }.to_string())
                }
                (TypeKind::Integer, TypeKind::Boolean) => Value::Boolean(value.as_integer() != 0),
                (TypeKind::Integer, TypeKind::Integer) => value,
                (TypeKind::Integer, TypeKind::Float) => Value::Float(value.as_integer() as f64),
                (TypeKind::Integer, TypeKind::String) => {
                    Value::String(value.as_integer().to_string())
                }
                (TypeKind::Float, TypeKind::Boolean) => Value::Boolean(value.as_float() != 0f64),
                (TypeKind::Float, TypeKind::Integer) => Value::Integer(value.as_float() as i64),
                (TypeKind::Float, TypeKind::Float) => value,
                (TypeKind::Float, TypeKind::String) => Value::String(value.as_float().to_string()),
                (TypeKind::String, TypeKind::Boolean) => {
                    Value::Boolean(value.as_string() != "false")
                }
                (TypeKind::String, TypeKind::Integer) => {
                    Value::Integer(value.as_string().parse().unwrap())
                }
                (TypeKind::String, TypeKind::Float) => {
                    Value::Float(value.as_string().parse().unwrap())
                }
                (TypeKind::String, TypeKind::String) => value,
                _ => unreachable!(),
            }
        }
        ExpressionKind::Call {
            expression,
            arguments,
        } => {
            let function = match temp_stack[expression.get() as usize - 1] {
                Value::Callable(function) => function,
                _ => unreachable!(),
            };
            let arguments = arguments
                .iter()
                .map(|argument| temp_stack[argument.get() as usize - 1].clone())
                .collect();
            execute_function(program, &program.functions[&function], arguments)
        }
        ExpressionKind::Literal { literal } => {
            match &program.type_table.types[&expression.type_id] {
                TypeKind::Boolean => Value::Boolean(literal.content == Symbol::from_str("true")),
                TypeKind::Integer => Value::Integer(literal.content.to_str().parse().unwrap()),
                TypeKind::Float => Value::Float(literal.content.to_str().parse().unwrap()),
                TypeKind::String => Value::String(literal.content.to_str().to_owned()),
                _ => unreachable!(),
            }
        }
        ExpressionKind::Variable { variable } => stack[variable.get() as usize - 1].clone(),
        ExpressionKind::Function { function } => Value::Callable(function.clone()),
        ExpressionKind::Temporary { temporary } => temp_stack[temporary.get() as usize - 1].clone(),
    }
}
