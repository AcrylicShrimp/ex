mod visitor;

use ex_codegen::{
    BasicBlock, BinaryOperator, BlockId, ExpressionKind, Function, FunctionId, InstructionId,
    InstructionKind, Pointer, Program, TemporaryId, Terminator, TypeId, TypeIdKind, VariableId,
};
use ex_parser::TokenLiteralKind;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::collections::HashMap;

pub struct Backend {
    pub context: Context,
}

impl Backend {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    pub fn compile(&self, program: &Program) -> Module {
        ir_to_llvm(&self.context, program)
    }

    pub fn optimize(&self, module: &Module) {
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

    pub fn execute(&self, module: &Module, function_name: impl AsRef<str>) {
        type MainFunc = unsafe extern "C" fn();

        let engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let function = unsafe { engine.get_function::<MainFunc>(function_name.as_ref()) }.unwrap();
        unsafe { function.call() };
    }
}

fn ir_to_llvm<'ctx>(context: &'ctx Context, program: &Program) -> Module<'ctx> {
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

    for (id, function) in &program.functions {
        let llvm_function = llvm_function_map.get(id).unwrap();
        ir_to_llvm_function(
            context,
            &module,
            &builder,
            program,
            &llvm_function_map,
            function,
            llvm_function.clone(),
        );
    }

    module
}

fn ir_to_llvm_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    program: &Program,
    llvm_function_map: &HashMap<FunctionId, FunctionValue<'ctx>>,
    function: &Function,
    llvm_function: FunctionValue<'ctx>,
) {
    let param_types = function
        .params
        .iter()
        .map(|param| param.type_id)
        .collect::<Vec<_>>();

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
            .map(|(id, block)| (*id, HashMap::new())),
    );

    for (id, llvm_block) in &llvm_block_map {
        let block = function.block_table.blocks.get(id).unwrap();
        builder.position_at_end(*llvm_block);

        for id in &block.instructions {
            ir_to_llvm_instruction(
                context,
                module,
                builder,
                program,
                function,
                block,
                llvm_function_map,
                &llvm_variable_map,
                &mut llvm_temporaries_map,
                *id,
            );
        }
    }

    for (id, llvm_block) in &llvm_block_map {
        let block = function.block_table.blocks.get(id).unwrap();
        builder.position_at_end(*llvm_block);

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
                let llvm_condition = ir_to_llvm_expression(
                    context,
                    module,
                    builder,
                    program,
                    function,
                    block,
                    llvm_function_map,
                    &llvm_variable_map,
                    &mut llvm_temporaries_map,
                    *condition,
                );
                builder.build_conditional_branch(
                    llvm_condition.into_int_value(),
                    llvm_block_map.get(dst0).unwrap().clone(),
                    llvm_block_map.get(dst1).unwrap().clone(),
                );
            }
            Terminator::Terminate { temporary } => match temporary {
                Some(temporary) => {
                    let llvm_temporary = ir_to_llvm_expression(
                        context,
                        module,
                        builder,
                        program,
                        function,
                        block,
                        llvm_function_map,
                        &llvm_variable_map,
                        &mut llvm_temporaries_map,
                        *temporary,
                    );
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

fn ir_to_llvm_instruction<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    program: &Program,
    function: &Function,
    block: &BasicBlock,
    llvm_function_map: &HashMap<FunctionId, FunctionValue<'ctx>>,
    llvm_variable_map: &HashMap<VariableId, PointerValue<'ctx>>,
    llvm_temporaries_map: &mut HashMap<BlockId, HashMap<TemporaryId, BasicValueEnum<'ctx>>>,
    instruction_id: InstructionId,
) {
    let instruction = block
        .instruction_table
        .instructions
        .get(&instruction_id)
        .unwrap();
    match &instruction.kind {
        InstructionKind::Store { pointer, temporary } => {
            let llvm_temporary = ir_to_llvm_expression(
                context,
                module,
                builder,
                program,
                function,
                block,
                llvm_function_map,
                llvm_variable_map,
                llvm_temporaries_map,
                *temporary,
            );
            let ptr = ptr_to_llvm_ptr(
                context,
                module,
                builder,
                program,
                function,
                block,
                llvm_function_map,
                llvm_variable_map,
                llvm_temporaries_map,
                *pointer,
            );
            builder.build_store(ptr, llvm_temporary);
        }
        InstructionKind::Expression { temporary } => {
            ir_to_llvm_expression(
                context,
                module,
                builder,
                program,
                function,
                block,
                llvm_function_map,
                llvm_variable_map,
                llvm_temporaries_map,
                *temporary,
            );
        }
    }
}

fn ir_to_llvm_expression<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    program: &Program,
    function: &Function,
    block: &BasicBlock,
    llvm_function_map: &HashMap<FunctionId, FunctionValue<'ctx>>,
    llvm_variable_map: &HashMap<VariableId, PointerValue<'ctx>>,
    llvm_temporaries_map: &mut HashMap<BlockId, HashMap<TemporaryId, BasicValueEnum<'ctx>>>,
    temporary_id: TemporaryId,
) -> BasicValueEnum<'ctx> {
    let temporary = block
        .temporary_table
        .temporaries
        .get(&temporary_id)
        .unwrap();
    let expression = temporary.expression.as_ref().unwrap();
    let llvm_value = match &expression.kind {
        ExpressionKind::Binary {
            operator,
            left,
            right,
        } => match *operator {
            BinaryOperator::Eq => {
                let left = ir_to_llvm_expression(
                    context,
                    module,
                    builder,
                    program,
                    function,
                    block,
                    llvm_function_map,
                    llvm_variable_map,
                    llvm_temporaries_map,
                    *left,
                );
                let right = ir_to_llvm_expression(
                    context,
                    module,
                    builder,
                    program,
                    function,
                    block,
                    llvm_function_map,
                    llvm_variable_map,
                    llvm_temporaries_map,
                    *right,
                );
                BasicValueEnum::IntValue(builder.build_int_compare::<IntValue<'ctx>>(
                    IntPredicate::EQ,
                    left.into_int_value(),
                    right.into_int_value(),
                    "eq",
                ))
            }
            BinaryOperator::Ne => todo!(),
            BinaryOperator::Lt => todo!(),
            BinaryOperator::Gt => todo!(),
            BinaryOperator::Le => todo!(),
            BinaryOperator::Ge => todo!(),
            BinaryOperator::LogOr => todo!(),
            BinaryOperator::LogAnd => todo!(),
            BinaryOperator::Add => {
                let left = ir_to_llvm_expression(
                    context,
                    module,
                    builder,
                    program,
                    function,
                    block,
                    llvm_function_map,
                    llvm_variable_map,
                    llvm_temporaries_map,
                    *left,
                );
                let right = ir_to_llvm_expression(
                    context,
                    module,
                    builder,
                    program,
                    function,
                    block,
                    llvm_function_map,
                    llvm_variable_map,
                    llvm_temporaries_map,
                    *right,
                );
                BasicValueEnum::IntValue(builder.build_int_add::<IntValue<'ctx>>(
                    left.into_int_value(),
                    right.into_int_value(),
                    "add_int",
                ))
            }
            BinaryOperator::Sub => todo!(),
            BinaryOperator::Mul => todo!(),
            BinaryOperator::Div => todo!(),
            BinaryOperator::Mod => todo!(),
            BinaryOperator::Pow => todo!(),
            BinaryOperator::Shl => todo!(),
            BinaryOperator::Shr => todo!(),
            BinaryOperator::BitOr => todo!(),
            BinaryOperator::BitAnd => todo!(),
            BinaryOperator::BitXor => todo!(),
        },
        ExpressionKind::Unary { operator, right } => todo!(),
        ExpressionKind::Convert {
            expression,
            from,
            to,
        } => todo!(),
        ExpressionKind::Call { expression, args } => {
            let function_type_id = block
                .temporary_table
                .temporaries
                .get(expression)
                .unwrap()
                .type_id;
            let function_type = if let TypeIdKind::Callable {
                param_types,
                return_type,
            } = program.type_id_table.types.get(&function_type_id).unwrap()
            {
                create_llvm_function_type(context, program, param_types, *return_type)
            } else {
                unreachable!()
            };
            // let function_type = program.type_id_table.
            let callee = ir_to_llvm_expression(
                context,
                module,
                builder,
                program,
                function,
                block,
                llvm_function_map,
                llvm_variable_map,
                llvm_temporaries_map,
                *expression,
            );
            let llvm_args = args
                .iter()
                .map(|arg| {
                    match ir_to_llvm_expression(
                        context,
                        module,
                        builder,
                        program,
                        function,
                        block,
                        llvm_function_map,
                        llvm_variable_map,
                        llvm_temporaries_map,
                        *arg,
                    ) {
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
                    }
                })
                .collect::<Vec<_>>();
            builder
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
        } => todo!(),
        ExpressionKind::Literal { literal } => match literal.kind {
            TokenLiteralKind::Bool => todo!(),
            TokenLiteralKind::IntegerBinary => todo!(),
            TokenLiteralKind::IntegerOctal => todo!(),
            TokenLiteralKind::IntegerHexadecimal => todo!(),
            TokenLiteralKind::IntegerDecimal => BasicValueEnum::IntValue(
                context
                    .i64_type()
                    .const_int(literal.content.to_str().parse().unwrap(), false),
            ),
            TokenLiteralKind::Float => todo!(),
            TokenLiteralKind::Character { terminated } => todo!(),
            TokenLiteralKind::String { terminated } => todo!(),
        },
        ExpressionKind::ElementPointer { base, indices } => todo!(),
        ExpressionKind::Pointer { pointer } => BasicValueEnum::PointerValue(ptr_to_llvm_ptr(
            context,
            module,
            builder,
            program,
            function,
            block,
            llvm_function_map,
            llvm_variable_map,
            llvm_temporaries_map,
            *pointer,
        )),
        ExpressionKind::Load { pointer } => match *pointer {
            Pointer::Function(..) => unreachable!(),
            Pointer::Variable(variable) => {
                let type_id = function
                    .variable_table
                    .variables
                    .get(&variable)
                    .unwrap()
                    .type_id;
                let llvm_type = type_id_to_llvm_type(context, program, type_id);
                let llvm_variable = llvm_variable_map.get(&variable).unwrap().clone();
                builder.build_load(llvm_type, llvm_variable, "load_var")
            }
            Pointer::RawPointer(temporary) => {
                let type_id = block
                    .temporary_table
                    .temporaries
                    .get(&temporary)
                    .unwrap()
                    .type_id;
                let llvm_type = type_id_to_llvm_type(context, program, type_id);
                let llvm_temporary = ir_to_llvm_expression(
                    context,
                    module,
                    builder,
                    program,
                    function,
                    block,
                    llvm_function_map,
                    llvm_variable_map,
                    llvm_temporaries_map,
                    temporary,
                );
                builder.build_load(llvm_type, llvm_temporary.into_pointer_value(), "load_raw")
            }
        },
    };
    llvm_temporaries_map
        .get_mut(&block.id)
        .unwrap()
        .insert(temporary_id, llvm_value.clone());
    llvm_value
}

fn ptr_to_llvm_ptr<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    program: &Program,
    function: &Function,
    block: &BasicBlock,
    llvm_function_map: &HashMap<FunctionId, FunctionValue<'ctx>>,
    llvm_variable_map: &HashMap<VariableId, PointerValue<'ctx>>,
    llvm_temporaries_map: &mut HashMap<BlockId, HashMap<TemporaryId, BasicValueEnum<'ctx>>>,
    pointer: Pointer,
) -> PointerValue<'ctx> {
    match pointer {
        Pointer::Function(id) => llvm_function_map
            .get(&id)
            .unwrap()
            .as_global_value()
            .as_pointer_value(),
        Pointer::Variable(id) => llvm_variable_map.get(&id).unwrap().clone(),
        Pointer::RawPointer(id) => llvm_temporaries_map
            .get(&block.id)
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
