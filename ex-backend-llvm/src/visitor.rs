use ex_codegen::{
    BasicBlock, BinaryOperator, BlockId, Function, FunctionId, Program, TemporaryId, TypeIdKind,
    VariableId,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue, StructValue},
};
use std::collections::HashMap;

pub trait TypeFilter {
    fn filter(ty: &TypeIdKind) -> bool;
}

pub trait IntoLLVM<'ctx> {
    type LLVMType;

    fn into_llvm(&'ctx self) -> Self::LLVMType;
    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx>;
}

pub struct BoolVal<'ctx> {
    value: IntValue<'ctx>,
}

impl<'ctx> TypeFilter for BoolVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::Bool)
    }
}

impl<'ctx> IntoLLVM<'ctx> for BoolVal<'ctx> {
    type LLVMType = IntValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::IntValue(self.into_llvm())
    }
}

pub struct IntVal<'ctx> {
    value: IntValue<'ctx>,
}

impl<'ctx> TypeFilter for IntVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::Int)
    }
}

impl<'ctx> IntoLLVM<'ctx> for IntVal<'ctx> {
    type LLVMType = IntValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::IntValue(self.into_llvm())
    }
}

pub struct FloatVal<'ctx> {
    value: FloatValue<'ctx>,
}

impl<'ctx> TypeFilter for FloatVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::Float)
    }
}

impl<'ctx> IntoLLVM<'ctx> for FloatVal<'ctx> {
    type LLVMType = FloatValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::FloatValue(self.into_llvm())
    }
}

pub struct StringVal<'ctx> {
    value: PointerValue<'ctx>, // char *
}

impl<'ctx> TypeFilter for StringVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::String)
    }
}

impl<'ctx> IntoLLVM<'ctx> for StringVal<'ctx> {
    type LLVMType = PointerValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::PointerValue(self.into_llvm())
    }
}

pub struct CallableVal<'ctx> {
    value: PointerValue<'ctx>, // fn *
}

impl<'ctx> TypeFilter for CallableVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::Callable { .. })
    }
}

impl<'ctx> IntoLLVM<'ctx> for CallableVal<'ctx> {
    type LLVMType = PointerValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::PointerValue(self.into_llvm())
    }
}

pub struct UserStructVal<'ctx> {
    value: StructValue<'ctx>,
}

impl<'ctx> TypeFilter for UserStructVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::UserStruct { .. })
    }
}

impl<'ctx> IntoLLVM<'ctx> for UserStructVal<'ctx> {
    type LLVMType = StructValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::StructValue(self.into_llvm())
    }
}

pub struct PointerVal<'ctx> {
    value: PointerValue<'ctx>,
}

impl<'ctx> TypeFilter for PointerVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::Pointer { .. })
    }
}

impl<'ctx> IntoLLVM<'ctx> for PointerVal<'ctx> {
    type LLVMType = PointerValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::PointerValue(self.into_llvm())
    }
}

pub struct ReferenceVal<'ctx> {
    value: PointerValue<'ctx>,
}

impl<'ctx> TypeFilter for ReferenceVal<'ctx> {
    fn filter(ty: &TypeIdKind) -> bool {
        matches!(ty, TypeIdKind::Reference { .. })
    }
}

impl<'ctx> IntoLLVM<'ctx> for ReferenceVal<'ctx> {
    type LLVMType = PointerValue<'ctx>;

    fn into_llvm(&'ctx self) -> Self::LLVMType {
        self.value.clone()
    }

    fn into_llvm_any(&'ctx self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::PointerValue(self.into_llvm())
    }
}

pub struct VisitorContext<'ctx> {
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    builder: &'ctx Builder<'ctx>,
    program: &'ctx Program,
    function: &'ctx Function,
    block: &'ctx BasicBlock,
    llvm_function_map: &'ctx HashMap<FunctionId, FunctionValue<'ctx>>,
    llvm_variable_map: &'ctx HashMap<VariableId, PointerValue<'ctx>>,
    llvm_temporaries_map: &'ctx mut HashMap<BlockId, HashMap<TemporaryId, BasicValueEnum<'ctx>>>,
}

pub trait Visitor {}

pub struct VisitorManager {
    bin_op_visitors: HashMap<BinaryOperator, BinOpVisitor>,
}

pub struct BinOpVisitor {}

fn bin_op_callback<'ctx>(
    ctx: VisitorContext<'ctx>,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
) -> IntValue<'ctx> {
    todo!()
}

// fn test() {
//     let visitor_mgr;

//     visitor_mgr.register_bin_op::<Op, LhsType, RhsType>(bin_op_callback_function);
// }

// fn bin_op_callback_function(ctx: &Context, op: Op, lhs: &Value, rhs: &Value) {
//     // do something
// }
