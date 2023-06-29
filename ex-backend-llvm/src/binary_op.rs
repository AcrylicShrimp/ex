use crate::dispatcher::{AccepterContext, Dispatcher, LLVMBool, LLVMFloat, LLVMInt, LLVMString};
use ex_codegen::BinaryOperator;
use inkwell::{
    intrinsics::Intrinsic,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum},
    FloatPredicate, IntPredicate,
};

pub fn register_binary_op_dispatchers(dispatcher: &mut Dispatcher) {
    ex_backend_llvm_codegen::register_binary_op_dispatchers! {
        dispatcher [
            BinaryOperator::Eq => eq_bool_bool,
            BinaryOperator::Eq => eq_int_int,
            BinaryOperator::Eq => eq_float_float,
            BinaryOperator::Eq => eq_string_string,

            BinaryOperator::Ne => ne_bool_bool,
            BinaryOperator::Ne => ne_int_int,
            BinaryOperator::Ne => ne_float_float,
            BinaryOperator::Ne => ne_string_string,

            BinaryOperator::Lt => lt_bool_bool,
            BinaryOperator::Lt => lt_int_int,
            BinaryOperator::Lt => lt_float_float,
            BinaryOperator::Lt => lt_string_string,

            BinaryOperator::Gt => gt_bool_bool,
            BinaryOperator::Gt => gt_int_int,
            BinaryOperator::Gt => gt_float_float,
            BinaryOperator::Gt => gt_string_string,

            BinaryOperator::Le => le_bool_bool,
            BinaryOperator::Le => le_int_int,
            BinaryOperator::Le => le_float_float,
            BinaryOperator::Le => le_string_string,

            BinaryOperator::Ge => ge_bool_bool,
            BinaryOperator::Ge => ge_int_int,
            BinaryOperator::Ge => ge_float_float,
            BinaryOperator::Ge => ge_string_string,

            BinaryOperator::LogOr => log_or_bool_bool,

            BinaryOperator::LogAnd => log_and_bool_bool,

            BinaryOperator::Add => add_int_int,
            BinaryOperator::Add => add_float_float,

            BinaryOperator::Sub => sub_int_int,
            BinaryOperator::Sub => sub_float_float,

            BinaryOperator::Mul => mul_int_int,
            BinaryOperator::Mul => mul_float_float,

            BinaryOperator::Div => div_int_int,
            BinaryOperator::Div => div_float_float,

            BinaryOperator::Mod => mod_int_int,
            BinaryOperator::Mod => mod_float_float,

            BinaryOperator::Pow => pow_float_float,

            BinaryOperator::Shl => shl_int_int,
            BinaryOperator::Shr => shr_int_int,

            BinaryOperator::BitOr => bit_or_int_int,

            BinaryOperator::BitAnd => bit_and_int_int,

            BinaryOperator::BitXor => bit_xor_int_int,
        ]
    };
}

fn eq_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::EQ, lhs.value, rhs.value, "eq_bool_bool")
        .into()
}

fn eq_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::EQ, lhs.value, rhs.value, "eq_int_int")
        .into()
}

fn eq_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_compare(FloatPredicate::OEQ, lhs.value, rhs.value, "eq_float_float")
        .into()
}

fn eq_string_string<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMString<'ctx>,
    rhs: LLVMString<'ctx>,
) -> BasicValueEnum<'ctx> {
    todo!()
}

fn ne_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::NE, lhs.value, rhs.value, "ne_bool_bool")
        .into()
}

fn ne_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::NE, lhs.value, rhs.value, "ne_int_int")
        .into()
}

fn ne_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_compare(FloatPredicate::ONE, lhs.value, rhs.value, "ne_float_float")
        .into()
}

fn ne_string_string<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMString<'ctx>,
    rhs: LLVMString<'ctx>,
) -> BasicValueEnum<'ctx> {
    todo!()
}

fn lt_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::ULT, lhs.value, rhs.value, "lt_bool_bool")
        .into()
}

fn lt_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::SLT, lhs.value, rhs.value, "lt_int_int")
        .into()
}

fn lt_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_compare(FloatPredicate::OLT, lhs.value, rhs.value, "lt_float_float")
        .into()
}

fn lt_string_string<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMString<'ctx>,
    rhs: LLVMString<'ctx>,
) -> BasicValueEnum<'ctx> {
    todo!()
}

fn gt_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::UGT, lhs.value, rhs.value, "gt_bool_bool")
        .into()
}

fn gt_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::SGT, lhs.value, rhs.value, "gt_int_int")
        .into()
}

fn gt_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_compare(FloatPredicate::OGT, lhs.value, rhs.value, "gt_float_float")
        .into()
}

fn gt_string_string<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMString<'ctx>,
    rhs: LLVMString<'ctx>,
) -> BasicValueEnum<'ctx> {
    todo!()
}

fn le_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::ULE, lhs.value, rhs.value, "le_bool_bool")
        .into()
}

fn le_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::SLE, lhs.value, rhs.value, "le_int_int")
        .into()
}

fn le_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_compare(FloatPredicate::OLE, lhs.value, rhs.value, "le_float_float")
        .into()
}

fn le_string_string<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMString<'ctx>,
    rhs: LLVMString<'ctx>,
) -> BasicValueEnum<'ctx> {
    todo!()
}

fn ge_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::UGE, lhs.value, rhs.value, "ge_bool_bool")
        .into()
}

fn ge_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_compare(IntPredicate::SGE, lhs.value, rhs.value, "ge_int_int")
        .into()
}

fn ge_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_compare(FloatPredicate::OGE, lhs.value, rhs.value, "ge_float_float")
        .into()
}

fn ge_string_string<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMString<'ctx>,
    rhs: LLVMString<'ctx>,
) -> BasicValueEnum<'ctx> {
    todo!()
}

fn log_or_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    // TODO: Remove this operation.
    // We can eliminate this operation by converting it to a branch in IR generation.
    // This can introduce short-circuiting which is not possible with this operation.
    ctx.builder
        .build_or(lhs.value, rhs.value, "log_or_bool_bool")
        .into()
}

fn log_and_bool_bool<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMBool<'ctx>,
    rhs: LLVMBool<'ctx>,
) -> BasicValueEnum<'ctx> {
    // TODO: Remove this operation.
    // We can eliminate this operation by converting it to a branch in IR generation.
    // This can introduce short-circuiting which is not possible with this operation.
    ctx.builder
        .build_and(lhs.value, rhs.value, "log_and_bool_bool")
        .into()
}

fn add_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_add(lhs.value, rhs.value, "add_int_int")
        .into()
}

fn add_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_add(lhs.value, rhs.value, "add_float_float")
        .into()
}

fn sub_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_sub(lhs.value, rhs.value, "sub_int_int")
        .into()
}

fn sub_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_sub(lhs.value, rhs.value, "sub_float_float")
        .into()
}

fn mul_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_mul(lhs.value, rhs.value, "mul_int_int")
        .into()
}

fn mul_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_mul(lhs.value, rhs.value, "mul_float_float")
        .into()
}

fn div_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_signed_div(lhs.value, rhs.value, "div_int_int")
        .into()
}

fn div_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_div(lhs.value, rhs.value, "div_float_float")
        .into()
}

fn mod_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_int_signed_rem(lhs.value, rhs.value, "mod_int_int")
        .into()
}

fn mod_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_float_rem(lhs.value, rhs.value, "mod_float_float")
        .into()
}

fn pow_float_float<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMFloat<'ctx>,
    rhs: LLVMFloat<'ctx>,
) -> BasicValueEnum<'ctx> {
    let pow = Intrinsic::find("llvm.pow").unwrap();
    let pow_function = pow
        .get_declaration(
            &ctx.module,
            &[
                BasicTypeEnum::FloatType(lhs.value.clone().get_type()),
                BasicTypeEnum::FloatType(rhs.value.clone().get_type()),
            ],
        )
        .unwrap();
    ctx.builder
        .build_call(
            pow_function,
            &[
                BasicMetadataValueEnum::FloatValue(lhs.value),
                BasicMetadataValueEnum::FloatValue(rhs.value),
            ],
            "pow_float_float",
        )
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn shl_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_left_shift(lhs.value, rhs.value, "shl_int_int")
        .into()
}

fn shr_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_right_shift(lhs.value, rhs.value, true, "shr_int_int")
        .into()
}

fn bit_or_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_or(lhs.value, rhs.value, "bit_or_int_int")
        .into()
}

fn bit_and_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_and(lhs.value, rhs.value, "bit_and_int_int")
        .into()
}

fn bit_xor_int_int<'a, 'ctx>(
    ctx: &AccepterContext<'a, 'ctx>,
    lhs: LLVMInt<'ctx>,
    rhs: LLVMInt<'ctx>,
) -> BasicValueEnum<'ctx> {
    ctx.builder
        .build_xor(lhs.value, rhs.value, "bit_xor_int_int")
        .into()
}
