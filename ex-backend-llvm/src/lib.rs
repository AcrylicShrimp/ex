mod binary_op;
mod dispatcher;
mod unary_op;

use binary_op::register_binary_op_dispatchers;
use dispatcher::{AccepterContext, Dispatcher};
use ex_codegen::{
    ExpressionKind, Function, FunctionId, InstructionId, InstructionKind, Pointer, Program,
    TemporaryId, Terminator, TypeId, TypeIdKind,
};
use ex_parser::TokenLiteralKind;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use std::{collections::HashMap, iter::once};
use unary_op::register_unary_op_dispatchers;

pub struct Backend {
    pub context: Context,
}

impl Backend {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    pub fn compile<'ctx>(&'ctx self, program: &'ctx Program) -> Module<'ctx> {
        ir_to_llvm(&self.context, program)
    }

    pub fn optimize<'ctx>(&'ctx self, module: &'ctx Module<'ctx>) {
        let fpm = PassManager::create(module);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.initialize();

        for function in module.get_functions() {
            fpm.run_on(&function);
        }

        fpm.finalize();
    }

    pub fn execute<'ctx>(&'ctx self, module: &'ctx Module<'ctx>, function_name: impl AsRef<str>) {
        type MainFunc = unsafe extern "C" fn();

        let engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let function = unsafe { engine.get_function::<MainFunc>(function_name.as_ref()) }.unwrap();
        unsafe { function.call() };
    }
}

fn ir_to_llvm<'a, 'ctx: 'a>(context: &'ctx Context, program: &'a Program) -> Module<'ctx> {
    let module = context.create_module("module_0");
    let builder = context.create_builder();

    let llvm_function_map =
        HashMap::<_, _>::from_iter(program.functions.iter().map(|(id, function)| {
            (
                *id,
                module.add_function(
                    function.name.to_str(),
                    create_llvm_function_type(
                        context,
                        program,
                        &function
                            .params
                            .iter()
                            .map(|param| param.type_id)
                            .collect::<Vec<_>>(),
                        function.return_type_id,
                    ),
                    None,
                ),
            )
        }));

    let mut dispatcher = Dispatcher::new();
    register_binary_op_dispatchers(&mut dispatcher);
    register_unary_op_dispatchers(&mut dispatcher);

    for (id, function) in &program.functions {
        let llvm_function = llvm_function_map.get(id).unwrap();
        ir_to_llvm_function(
            context,
            &module,
            &builder,
            program,
            function,
            llvm_function.clone(),
            &llvm_function_map,
            &mut dispatcher,
        );
    }

    module
}

fn ir_to_llvm_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    program: &Program,
    function: &Function,
    llvm_function: FunctionValue<'ctx>,
    llvm_function_map: &HashMap<FunctionId, FunctionValue<'ctx>>,
    dispatcher: &mut Dispatcher,
) {
    // let param_types = function
    //     .params
    //     .iter()
    //     .map(|param| param.type_id)
    //     .collect::<Vec<_>>();

    let entry = context.append_basic_block(llvm_function, "entry");
    builder.position_at_end(entry);

    let llvm_variable_map =
        HashMap::<_, _>::from_iter(function.variable_table.variables.iter().map(
            |(id, variable)| {
                (
                    *id,
                    builder.build_alloca(
                        type_id_to_llvm_type(context, program, variable.type_id),
                        &format!("var_{}", id.get()),
                    ),
                )
            },
        ));

    for (index, param) in function.params.iter().enumerate() {
        let llvm_param = llvm_function.get_nth_param(index as u32).unwrap();
        builder.build_store(
            llvm_variable_map.get(&param.variable_id).unwrap().clone(),
            llvm_param,
        );
    }

    let mut predecessor_map = HashMap::<_, Vec<_>>::new();
    let mut successor_map = HashMap::<_, Vec<_>>::new();

    for block in function.block_table.blocks.values() {
        match block.terminator.as_ref().unwrap() {
            Terminator::Jump { block: dst, .. } => {
                predecessor_map.entry(*dst).or_default().push(block.id);
                successor_map.entry(block.id).or_default().push(*dst);
            }
            Terminator::Branch {
                then_block: dst0,
                else_block: dst1,
                ..
            } => {
                predecessor_map.entry(*dst0).or_default().push(block.id);
                predecessor_map.entry(*dst1).or_default().push(block.id);
                successor_map.entry(block.id).or_default().push(*dst0);
                successor_map.entry(block.id).or_default().push(*dst1);
            }
            Terminator::Terminate { .. } => {}
        }
    }

    let llvm_block_map = HashMap::<_, _>::from_iter(function.block_table.blocks.keys().map(|id| {
        (
            *id,
            context.append_basic_block(llvm_function, &format!("block_{}", id.get())),
        )
    }));
    let mut llvm_temporaries_map = HashMap::<_, _>::from_iter(
        function
            .block_table
            .blocks
            .iter()
            .map(|(id, _)| (*id, HashMap::new())),
    );

    for (id, llvm_block) in &llvm_block_map {
        let block = function.block_table.blocks.get(id).unwrap();
        builder.position_at_end(*llvm_block);

        let mut ctx = AccepterContext {
            context,
            module,
            builder,
            program,
            function,
            block,
            llvm_function_map,
            llvm_variable_map: &llvm_variable_map,
            llvm_temporaries_map: &mut llvm_temporaries_map,
        };

        for id in &block.instructions {
            ir_to_llvm_instruction(&mut ctx, dispatcher, *id);
        }
    }

    for (id, llvm_block) in &llvm_block_map {
        let block = function.block_table.blocks.get(id).unwrap();
        builder.position_at_end(*llvm_block);

        let mut ctx = AccepterContext {
            context,
            module,
            builder,
            program,
            function,
            block,
            llvm_function_map,
            llvm_variable_map: &llvm_variable_map,
            llvm_temporaries_map: &mut llvm_temporaries_map,
        };

        // TODO: Add support for phi nodes.
        // It's OK for now because we don't use them in the IR.

        match block.terminator.as_ref().unwrap() {
            Terminator::Jump { block: dst, .. } => {
                builder.build_unconditional_branch(llvm_block_map.get(dst).unwrap().clone());
            }
            Terminator::Branch {
                condition,
                then_block: dst0,
                else_block: dst1,
                ..
            } => {
                let llvm_condition = ir_to_llvm_expression(&mut ctx, dispatcher, *condition);
                builder.build_conditional_branch(
                    llvm_condition.into_int_value(),
                    llvm_block_map.get(dst0).unwrap().clone(),
                    llvm_block_map.get(dst1).unwrap().clone(),
                );
            }
            Terminator::Terminate { temporary } => match temporary {
                Some(temporary) => {
                    let llvm_temporary = ir_to_llvm_expression(&mut ctx, dispatcher, *temporary);
                    builder.build_return(Some(&llvm_temporary));
                }
                None => {
                    builder.build_return(None);
                }
            },
        }
    }

    builder.position_at_end(entry);
    builder.build_unconditional_branch(
        llvm_block_map
            .get(&function.entry_block_id)
            .unwrap()
            .clone(),
    );
}

fn ir_to_llvm_instruction<'a, 'ctx: 'a>(
    ctx: &mut AccepterContext<'a, 'ctx>,
    dispatcher: &mut Dispatcher,
    instruction_id: InstructionId,
) {
    let instruction = ctx
        .block
        .instruction_table
        .instructions
        .get(&instruction_id)
        .unwrap();
    match &instruction.kind {
        InstructionKind::Store { pointer, temporary } => {
            let llvm_temporary = ir_to_llvm_expression(ctx, dispatcher, *temporary);
            let ptr = ptr_to_llvm_ptr(ctx, *pointer);
            ctx.builder.build_store(ptr, llvm_temporary);
        }
        InstructionKind::Expression { temporary } => {
            ir_to_llvm_expression(ctx, dispatcher, *temporary);
        }
    }
}

fn ir_to_llvm_expression<'a, 'ctx: 'a>(
    ctx: &mut AccepterContext<'a, 'ctx>,
    dispatcher: &mut Dispatcher,
    temporary_id: TemporaryId,
) -> BasicValueEnum<'ctx> {
    let temporary = ctx
        .block
        .temporary_table
        .temporaries
        .get(&temporary_id)
        .unwrap();
    let expression = temporary.expression.as_ref().unwrap();
    let llvm_value = match &expression.kind {
        &ExpressionKind::Binary {
            operator,
            left,
            right,
        } => {
            let llvm_left = ir_to_llvm_expression(ctx, dispatcher, left);
            let llvm_right = ir_to_llvm_expression(ctx, dispatcher, right);
            dispatcher.dispatch_binary_op(ctx, operator, (left, llvm_left), (right, llvm_right))
        }
        &ExpressionKind::Unary { operator, right } => {
            let llvm_right = ir_to_llvm_expression(ctx, dispatcher, right);
            dispatcher.dispatch_unary_op(ctx, operator, (right, llvm_right))
        }
        &ExpressionKind::Convert {
            expression,
            from,
            to,
        } => {
            let llvm_expression = ir_to_llvm_expression(ctx, dispatcher, expression);
            dispatcher.dispatch_conversion_op(ctx, (from, llvm_expression), to)
        }
        ExpressionKind::Call { expression, args } => {
            let function_type_id = ctx
                .block
                .temporary_table
                .temporaries
                .get(expression)
                .unwrap()
                .type_id;
            let function_type = if let TypeIdKind::Callable {
                param_types,
                return_type,
            } = ctx
                .program
                .type_id_table
                .types
                .get(&function_type_id)
                .unwrap()
            {
                create_llvm_function_type(ctx.context, ctx.program, param_types, *return_type)
            } else {
                unreachable!()
            };
            // let function_type = program.type_id_table.
            let callee = ir_to_llvm_expression(ctx, dispatcher, *expression);
            let llvm_args = args
                .iter()
                .map(|arg| match ir_to_llvm_expression(ctx, dispatcher, *arg) {
                    BasicValueEnum::ArrayValue(llvm_value) => {
                        BasicMetadataValueEnum::ArrayValue(llvm_value)
                    }
                    BasicValueEnum::IntValue(llvm_value) => {
                        BasicMetadataValueEnum::IntValue(llvm_value)
                    }
                    BasicValueEnum::FloatValue(llvm_value) => {
                        BasicMetadataValueEnum::FloatValue(llvm_value)
                    }
                    BasicValueEnum::PointerValue(llvm_value) => {
                        BasicMetadataValueEnum::PointerValue(llvm_value)
                    }
                    BasicValueEnum::StructValue(llvm_value) => {
                        BasicMetadataValueEnum::StructValue(llvm_value)
                    }
                    BasicValueEnum::VectorValue(llvm_value) => {
                        BasicMetadataValueEnum::VectorValue(llvm_value)
                    }
                })
                .collect::<Vec<_>>();
            ctx.builder
                .build_indirect_call(
                    function_type,
                    callee.into_pointer_value(),
                    &llvm_args,
                    "call",
                )
                .try_as_basic_value()
                .unwrap_left()
        }
        ExpressionKind::StructLiteral {
            struct_type,
            fields,
        } => {
            let struct_type = ctx.program.type_id_table.types.get(struct_type).unwrap();
            let struct_id = if let TypeIdKind::UserStruct { id } = struct_type {
                id
            } else {
                unreachable!()
            };
            let user_struct = ctx.program.user_structs.get(struct_id).unwrap();
            let field_types = user_struct
                .fields
                .iter()
                .map(|field| type_id_to_llvm_type(&ctx.context, &ctx.program, field.type_id))
                .collect::<Vec<_>>();
            let values = fields
                .iter()
                .map(|field| ir_to_llvm_expression(ctx, dispatcher, *field))
                .collect::<Vec<_>>();
            BasicValueEnum::StructValue(
                ctx.context
                    .struct_type(&field_types, false)
                    .const_named_struct(&values),
            )
        }
        ExpressionKind::Literal { literal } => match literal.kind {
            TokenLiteralKind::Bool => BasicValueEnum::IntValue(ctx.context.bool_type().const_int(
                if literal.content.to_str() == "true" {
                    1
                } else {
                    0
                },
                false,
            )),
            TokenLiteralKind::IntegerBinary => {
                BasicValueEnum::IntValue(ctx.context.i64_type().const_int(
                    i64::from_str_radix(&literal.content.to_str()[2..], 2).unwrap() as u64,
                    false,
                ))
            }
            TokenLiteralKind::IntegerOctal => {
                BasicValueEnum::IntValue(ctx.context.i64_type().const_int(
                    i64::from_str_radix(&literal.content.to_str()[2..], 8).unwrap() as u64,
                    false,
                ))
            }
            TokenLiteralKind::IntegerHexadecimal => {
                BasicValueEnum::IntValue(ctx.context.i64_type().const_int(
                    i64::from_str_radix(&literal.content.to_str()[2..], 16).unwrap() as u64,
                    false,
                ))
            }
            TokenLiteralKind::IntegerDecimal => {
                BasicValueEnum::IntValue(ctx.context.i64_type().const_int(
                    i64::from_str_radix(literal.content.to_str(), 10).unwrap() as u64,
                    false,
                ))
            }
            TokenLiteralKind::Float => BasicValueEnum::FloatValue(
                ctx.context
                    .f64_type()
                    .const_float_from_string(literal.content.to_str()),
            ),
            TokenLiteralKind::Character { .. } => {
                BasicValueEnum::IntValue(ctx.context.i64_type().const_int(
                    literal.content.to_str().chars().next().unwrap() as u64,
                    false,
                ))
            }
            TokenLiteralKind::String { terminated } => {
                let content = literal.content.to_str();
                let content = if terminated {
                    &content[1..content.len() - 1]
                } else {
                    &content[1..]
                };

                let array = ctx.context.const_string(content.as_bytes(), false);
                let global = ctx.module.add_global(array.get_type(), None, "string");
                global.set_initializer(&array);

                BasicValueEnum::PointerValue(global.as_pointer_value())
            }
        },
        ExpressionKind::ElementPointer { base, indices } => {
            let ptr_type_id = ptr_to_type_id(ctx, *base);
            let base_type_id = if let TypeIdKind::Pointer { type_id } =
                ctx.program.type_id_table.types.get(&ptr_type_id).unwrap()
            {
                *type_id
            } else {
                ptr_type_id
            };
            let base_type = type_id_to_llvm_type(&ctx.context, &ctx.program, base_type_id);
            let base = ptr_to_llvm_ptr(ctx, *base);
            let indices = once(0)
                .chain(indices.iter().cloned())
                .map(|index| ctx.context.i64_type().const_int(index as u64, false))
                .collect::<Vec<_>>();
            BasicValueEnum::PointerValue(unsafe { base.const_gep(base_type, &indices) })
        }
        ExpressionKind::Pointer { pointer } => {
            BasicValueEnum::PointerValue(ptr_to_llvm_ptr(ctx, *pointer))
        }
        ExpressionKind::Load { pointer } => match *pointer {
            Pointer::Function(..) => unreachable!(),
            Pointer::Variable(variable) => {
                let type_id = ctx
                    .function
                    .variable_table
                    .variables
                    .get(&variable)
                    .unwrap()
                    .type_id;
                let llvm_type = type_id_to_llvm_type(ctx.context, ctx.program, type_id);
                let llvm_variable = ctx.llvm_variable_map.get(&variable).unwrap().clone();
                ctx.builder.build_load(llvm_type, llvm_variable, "load_var")
            }
            Pointer::RawPointer(temporary) => {
                let type_id = ctx
                    .block
                    .temporary_table
                    .temporaries
                    .get(&temporary)
                    .unwrap()
                    .type_id;
                let llvm_type = type_id_to_llvm_type(ctx.context, ctx.program, type_id);
                let llvm_temporary = ir_to_llvm_expression(ctx, dispatcher, temporary);
                ctx.builder
                    .build_load(llvm_type, llvm_temporary.into_pointer_value(), "load_raw")
            }
        },
    };
    ctx.llvm_temporaries_map
        .get_mut(&ctx.block.id)
        .unwrap()
        .insert(temporary_id, llvm_value.clone());
    llvm_value
}

fn ptr_to_type_id<'a, 'ctx>(ctx: &mut AccepterContext<'a, 'ctx>, pointer: Pointer) -> TypeId {
    match pointer {
        Pointer::Function(_) => unreachable!(),
        Pointer::Variable(id) => {
            ctx.function
                .variable_table
                .variables
                .get(&id)
                .unwrap()
                .type_id
        }
        Pointer::RawPointer(id) => {
            ctx.block
                .temporary_table
                .temporaries
                .get(&id)
                .unwrap()
                .type_id
        }
    }
}

fn ptr_to_llvm_ptr<'a, 'ctx>(
    ctx: &mut AccepterContext<'a, 'ctx>,
    pointer: Pointer,
) -> PointerValue<'ctx> {
    match pointer {
        Pointer::Function(id) => ctx
            .llvm_function_map
            .get(&id)
            .unwrap()
            .as_global_value()
            .as_pointer_value(),
        Pointer::Variable(id) => ctx.llvm_variable_map.get(&id).unwrap().clone(),
        Pointer::RawPointer(id) => ctx
            .llvm_temporaries_map
            .get(&ctx.block.id)
            .unwrap()
            .get(&id)
            .unwrap()
            .clone()
            .into_pointer_value(),
    }
}

fn type_id_to_llvm_type<'ctx>(
    context: &'ctx Context,
    program: &Program,
    type_id: TypeId,
) -> BasicTypeEnum<'ctx> {
    match program.type_id_table.types.get(&type_id).unwrap() {
        TypeIdKind::Empty => {
            unreachable!()
        }
        TypeIdKind::Bool => BasicTypeEnum::IntType(context.bool_type()),
        TypeIdKind::Int => BasicTypeEnum::IntType(context.i64_type()),
        TypeIdKind::Float => BasicTypeEnum::FloatType(context.f64_type()),
        TypeIdKind::String => {
            BasicTypeEnum::PointerType(context.i8_type().ptr_type(AddressSpace::default()))
        }
        TypeIdKind::Callable {
            param_types,
            return_type,
        } => {
            // NOTE: We're converting function types into pointer types here.
            // If raw function types are needed, use `create_llvm_function_type` instead.
            BasicTypeEnum::PointerType(
                create_llvm_function_type(context, program, param_types, *return_type)
                    .ptr_type(AddressSpace::default()),
            )
        }
        TypeIdKind::UserStruct { id } => {
            let user_struct = program.user_structs.get(id).unwrap();
            let field_types = user_struct
                .fields
                .iter()
                .map(|field| type_id_to_llvm_type(context, program, field.type_id))
                .collect::<Vec<_>>();
            BasicTypeEnum::StructType(context.struct_type(&field_types, false))
        }
        TypeIdKind::Pointer { type_id } => {
            BasicTypeEnum::PointerType(create_llvm_pointer_type(context, program, *type_id))
        }
        TypeIdKind::Reference { type_id } => {
            BasicTypeEnum::PointerType(create_llvm_pointer_type(context, program, *type_id))
        }
    }
}

fn create_llvm_function_type<'ctx>(
    context: &'ctx Context,
    program: &Program,
    param_types: &[TypeId],
    return_type: TypeId,
) -> FunctionType<'ctx> {
    let param_types = param_types
        .iter()
        .map(
            |type_id| match type_id_to_llvm_type(context, program, *type_id) {
                BasicTypeEnum::ArrayType(llvm_type) => BasicMetadataTypeEnum::ArrayType(llvm_type),
                BasicTypeEnum::FloatType(llvm_type) => BasicMetadataTypeEnum::FloatType(llvm_type),
                BasicTypeEnum::IntType(llvm_type) => BasicMetadataTypeEnum::IntType(llvm_type),
                BasicTypeEnum::PointerType(llvm_type) => {
                    BasicMetadataTypeEnum::PointerType(llvm_type)
                }
                BasicTypeEnum::StructType(llvm_type) => {
                    BasicMetadataTypeEnum::StructType(llvm_type)
                }
                BasicTypeEnum::VectorType(llvm_type) => {
                    BasicMetadataTypeEnum::VectorType(llvm_type)
                }
            },
        )
        .collect::<Vec<_>>();
    match program.type_id_table.types.get(&return_type).unwrap() {
        TypeIdKind::Empty => context.void_type().fn_type(&param_types, false),
        _ => type_id_to_llvm_type(context, program, return_type).fn_type(&param_types, false),
    }
}

fn create_llvm_pointer_type<'ctx>(
    context: &'ctx Context,
    program: &Program,
    type_id: TypeId,
) -> PointerType<'ctx> {
    type_id_to_llvm_type(context, program, type_id).ptr_type(AddressSpace::default())
}
