use crate::dispatcher::{AccepterContext, Dispatcher, LLVMBool, LLVMFloat, LLVMInt};
use ex_codegen::UnaryOperator;
use inkwell::values::BasicValueEnum;

pub fn register_unary_op_dispatchers(dispatcher: &mut Dispatcher) {
    ex_backend_llvm_codegen::register_unary_op_dispatchers! {
        dispatcher [
            UnaryOperator::Minus => neg_int,
            UnaryOperator::Minus => neg_float,
            UnaryOperator::BitNot => bit_not_int,
            UnaryOperator::LogNot => log_not_bool,
        ]
    };
}

fn neg_int<'a, 'ctx>(ctx: &AccepterContext<'a, 'ctx>, lhs: LLVMInt<'ctx>) -> BasicValueEnum<'ctx> {
    ctx.builder.build_int_neg(lhs.value, "neg_int").into()
}

fn neg_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder.build_float_neg(lhs.value, "neg_float").into()
}

fn bit_not_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder.build_not(lhs.value, "bit_not_int").into()
}

fn log_not_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder.build_not(lhs.value, "log_not_bool").into()
}
