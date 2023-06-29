use crate::dispatcher::{AccepterContext, Dispatcher, LLVMBool, LLVMFloat, LLVMInt};
use ex_codegen::TypeIdKind;
use inkwell::values::BasicValueEnum;

pub fn register_conversion_op_dispatchers(dispatcher: &mut Dispatcher) {
    ex_backend_llvm_codegen::register_conversion_op_dispatchers! {
        dispatcher [
            cast_bool_bool,
            cast_bool_int,
            cast_int_int,
            cast_float_float,
        ]
    };
}

fn cast_bool_bool<'a, 'ctx>(
    _ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    _rhs: &TypeIdKind,
) -> BasicValueEnum<'ctx> {
    BasicValueEnum::IntValue(lhs.value)
}

fn cast_bool_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    _rhs: &TypeIdKind,
) -> BasicValueEnum<'ctx> {
    BasicValueEnum::IntValue(ctx.builder.build_int_z_extend(
        lhs.value,
        ctx.context.i64_type(),
        "bool_to_int",
    ))
}

fn cast_int_int<'a, 'ctx>(
    _ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    _rhs: &TypeIdKind,
) -> BasicValueEnum<'ctx> {
    BasicValueEnum::IntValue(lhs.value)
}

fn cast_float_float<'a, 'ctx>(
    _ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    _rhs: &TypeIdKind,
) -> BasicValueEnum<'ctx> {
    BasicValueEnum::FloatValue(lhs.value)
}
