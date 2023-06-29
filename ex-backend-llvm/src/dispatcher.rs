use ex_codegen::{
    BasicBlock, BinaryOperator, BlockId, Function, FunctionId, Program, TemporaryId, TypeId,
    TypeIdKind, UnaryOperator, VariableId,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue, StructValue},
};
use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeDispatchKind {
    Bool,
    Int,
    Float,
    String,
    Callable,
    UserStruct,
    Pointer,
    Reference,
}

impl<'a> From<&'a TypeIdKind> for TypeDispatchKind {
    fn from(value: &'a TypeIdKind) -> Self {
        match value {
            TypeIdKind::Empty => unreachable!(),
            TypeIdKind::Bool => Self::Bool,
            TypeIdKind::Int => Self::Int,
            TypeIdKind::Float => Self::Float,
            TypeIdKind::String => Self::String,
            TypeIdKind::Callable { .. } => Self::Callable,
            TypeIdKind::UserStruct { .. } => Self::UserStruct,
            TypeIdKind::Pointer { .. } => Self::Pointer,
            TypeIdKind::Reference { .. } => Self::Reference,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BinaryOpDispatchKey {
    pub op: BinaryOperator,
    pub lhs: TypeDispatchKind,
    pub rhs: TypeDispatchKind,
}

impl BinaryOpDispatchKey {
    pub fn new(op: BinaryOperator, lhs: TypeDispatchKind, rhs: TypeDispatchKind) -> Self {
        Self { op, lhs, rhs }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnaryOpDispatchKey {
    pub op: UnaryOperator,
    pub lhs: TypeDispatchKind,
}

impl UnaryOpDispatchKey {
    pub fn new(op: UnaryOperator, lhs: TypeDispatchKind) -> Self {
        Self { op, lhs }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConversionOpDispatchKey {
    pub lhs: TypeDispatchKind,
    pub rhs: TypeDispatchKind,
}

impl ConversionOpDispatchKey {
    pub fn new(lhs: TypeDispatchKind, rhs: TypeDispatchKind) -> Self {
        Self { lhs, rhs }
    }
}

pub struct Dispatcher {
    bin_op_dispatchers: HashMap<BinaryOpDispatchKey, BinaryOpDispatcher>,
    unary_op_dispatchers: HashMap<UnaryOpDispatchKey, UnaryOpDispatcher>,
    conversion_op_dispatchers: HashMap<ConversionOpDispatchKey, ConversionOpDispatcher>,
}

impl Dispatcher {
    pub fn new() -> Self {
        Self {
            bin_op_dispatchers: HashMap::new(),
            unary_op_dispatchers: HashMap::new(),
            conversion_op_dispatchers: HashMap::new(),
        }
    }

    pub fn register_binary_op_dispatcher<L, R>(
        &mut self,
        op: BinaryOperator,
        accepter: impl BinaryOpAccepter<L, R> + 'static,
    ) where
        L: IntoLLVM,
        R: IntoLLVM,
    {
        self.bin_op_dispatchers.insert(
            BinaryOpDispatchKey::new(op, L::DISPATCH_KIND, R::DISPATCH_KIND),
            BinaryOpDispatcher::new(accepter),
        );
    }

    pub fn register_unary_op_dispatcher<L>(
        &mut self,
        op: UnaryOperator,
        accepter: impl UnaryOpAccepter<L> + 'static,
    ) where
        L: IntoLLVM,
    {
        self.unary_op_dispatchers.insert(
            UnaryOpDispatchKey::new(op, L::DISPATCH_KIND),
            UnaryOpDispatcher::new(accepter),
        );
    }

    pub fn register_conversion_op_dispatcher<L, R>(
        &mut self,
        accepter: impl ConversionOpAccepter<L> + 'static,
    ) where
        L: IntoLLVM,
        R: IntoLLVM,
    {
        self.conversion_op_dispatchers.insert(
            ConversionOpDispatchKey::new(L::DISPATCH_KIND, R::DISPATCH_KIND),
            ConversionOpDispatcher::new(accepter),
        );
    }

    pub fn dispatch_binary_op<'a, 'ctx: 'a>(
        &mut self,
        ctx: &AccepterContext<'a, 'ctx>,
        op: BinaryOperator,
        lhs: (TemporaryId, BasicValueEnum<'ctx>),
        rhs: (TemporaryId, BasicValueEnum<'ctx>),
    ) -> BasicValueEnum<'ctx> {
        let lhs_type_id = ctx
            .block
            .temporary_table
            .temporaries
            .get(&lhs.0)
            .unwrap()
            .type_id;
        let lhs_type_dispatch_kind = ctx
            .program
            .type_id_table
            .types
            .get(&lhs_type_id)
            .unwrap()
            .into();
        let rhs_type_id = ctx
            .block
            .temporary_table
            .temporaries
            .get(&rhs.0)
            .unwrap()
            .type_id;
        let rhs_type_dispatch_kind = ctx
            .program
            .type_id_table
            .types
            .get(&rhs_type_id)
            .unwrap()
            .into();
        let dispatch_key =
            BinaryOpDispatchKey::new(op, lhs_type_dispatch_kind, rhs_type_dispatch_kind);

        match self.bin_op_dispatchers.get_mut(&dispatch_key) {
            Some(dispatcher) => dispatcher.dispatch(ctx, lhs.1, rhs.1),
            None => {
                panic!("no binary op dispatcher for {:?}", dispatch_key);
            }
        }
    }

    pub fn dispatch_unary_op<'a, 'ctx: 'a>(
        &mut self,
        ctx: &AccepterContext<'a, 'ctx>,
        op: UnaryOperator,
        lhs: (TemporaryId, BasicValueEnum<'ctx>),
    ) -> BasicValueEnum<'ctx> {
        let lhs_type_id = ctx
            .block
            .temporary_table
            .temporaries
            .get(&lhs.0)
            .unwrap()
            .type_id;
        let lhs_type_dispatch_kind = ctx
            .program
            .type_id_table
            .types
            .get(&lhs_type_id)
            .unwrap()
            .into();
        let dispatch_key = UnaryOpDispatchKey::new(op, lhs_type_dispatch_kind);

        match self.unary_op_dispatchers.get_mut(&dispatch_key) {
            Some(dispatcher) => dispatcher.dispatch(ctx, lhs.1),
            None => {
                panic!("no unary op dispatcher for {:?}", dispatch_key);
            }
        }
    }

    pub fn dispatch_conversion_op<'a, 'ctx: 'a>(
        &mut self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: (TypeId, BasicValueEnum<'ctx>),
        rhs: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let lhs_type_dispatch_kind = ctx.program.type_id_table.types.get(&lhs.0).unwrap().into();
        let rhs_type_kind = ctx.program.type_id_table.types.get(&rhs).unwrap();
        let rhs_type_dispatch_kind = rhs_type_kind.into();
        let dispatch_key =
            ConversionOpDispatchKey::new(lhs_type_dispatch_kind, rhs_type_dispatch_kind);

        match self.conversion_op_dispatchers.get_mut(&dispatch_key) {
            Some(dispatcher) => dispatcher.dispatch(ctx, lhs.1, rhs_type_kind),
            None => {
                panic!("no conversion op dispatcher for {:?}", dispatch_key);
            }
        }
    }
}

pub struct BinaryOpDispatcher {
    dispatcher: Box<
        dyn for<'a, 'ctx> Fn(
            &AccepterContext<'a, 'ctx>,
            BasicValueEnum<'ctx>,
            BasicValueEnum<'ctx>,
        ) -> BasicValueEnum<'ctx>,
    >,
}

impl BinaryOpDispatcher {
    pub fn new<L, R>(accepter: impl BinaryOpAccepter<L, R> + 'static) -> Self
    where
        L: IntoLLVM,
        R: IntoLLVM,
    {
        Self {
            dispatcher: Box::new(
                move |ctx: &AccepterContext, lhs: BasicValueEnum, rhs: BasicValueEnum| {
                    accepter.accept(ctx, lhs, rhs)
                },
            ),
        }
    }

    pub fn dispatch<'a, 'ctx: 'a>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        (self.dispatcher)(ctx, lhs, rhs)
    }
}

pub trait BinaryOpAccepter<L, R>
where
    L: IntoLLVM,
    R: IntoLLVM,
{
    fn accept<'a, 'ctx>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx>;
}

impl<T, L, R> BinaryOpAccepter<L, R> for T
where
    T: for<'a, 'ctx> Fn(
        &AccepterContext<'a, 'ctx>,
        L::Of<'ctx>,
        R::Of<'ctx>,
    ) -> BasicValueEnum<'ctx>,
    L: IntoLLVM,
    R: IntoLLVM,
{
    fn accept<'a, 'ctx>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self(ctx, L::into_llvm(lhs), R::into_llvm(rhs))
    }
}

pub struct UnaryOpDispatcher {
    dispatcher: Box<
        dyn for<'a, 'ctx> Fn(
            &AccepterContext<'a, 'ctx>,
            BasicValueEnum<'ctx>,
        ) -> BasicValueEnum<'ctx>,
    >,
}

impl UnaryOpDispatcher {
    pub fn new<T, L>(accepter: T) -> Self
    where
        T: UnaryOpAccepter<L> + 'static,
        L: IntoLLVM,
    {
        let dispatcher: Box<
            dyn for<'a, 'ctx> Fn(
                &AccepterContext<'a, 'ctx>,
                BasicValueEnum<'ctx>,
            ) -> BasicValueEnum<'ctx>,
        > = Box::new(move |ctx: &AccepterContext, lhs: BasicValueEnum| accepter.accept(ctx, lhs));

        Self { dispatcher }
    }

    pub fn dispatch<'a, 'ctx: 'a>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        (self.dispatcher)(ctx, lhs)
    }
}

pub trait UnaryOpAccepter<L>
where
    L: IntoLLVM,
{
    fn accept<'a, 'ctx>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx>;
}

impl<T, L> UnaryOpAccepter<L> for T
where
    T: for<'a, 'ctx> Fn(&AccepterContext<'a, 'ctx>, L::Of<'ctx>) -> BasicValueEnum<'ctx>,
    L: IntoLLVM,
{
    fn accept<'a, 'ctx>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self(ctx, L::into_llvm(lhs))
    }
}

pub struct ConversionOpDispatcher {
    dispatcher: Box<
        dyn for<'a, 'ctx> Fn(
            &AccepterContext<'a, 'ctx>,
            BasicValueEnum<'ctx>,
            &TypeIdKind,
        ) -> BasicValueEnum<'ctx>,
    >,
}

impl ConversionOpDispatcher {
    pub fn new<T, L>(accepter: T) -> Self
    where
        T: ConversionOpAccepter<L> + 'static,
        L: IntoLLVM,
    {
        let dispatcher: Box<
            dyn for<'a, 'ctx> Fn(
                &AccepterContext<'a, 'ctx>,
                BasicValueEnum<'ctx>,
                &TypeIdKind,
            ) -> BasicValueEnum<'ctx>,
        > = Box::new(
            move |ctx: &AccepterContext, lhs: BasicValueEnum, rhs: &TypeIdKind| {
                accepter.accept(ctx, lhs, rhs)
            },
        );

        Self { dispatcher }
    }

    pub fn dispatch<'a, 'ctx: 'a>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: &TypeIdKind,
    ) -> BasicValueEnum<'ctx> {
        (self.dispatcher)(ctx, lhs, rhs)
    }
}

pub trait ConversionOpAccepter<L>
where
    L: IntoLLVM,
{
    fn accept<'a, 'ctx>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: &TypeIdKind,
    ) -> BasicValueEnum<'ctx>;
}

impl<T, L> ConversionOpAccepter<L> for T
where
    T: for<'a, 'ctx> Fn(
        &AccepterContext<'a, 'ctx>,
        L::Of<'ctx>,
        &TypeIdKind,
    ) -> BasicValueEnum<'ctx>,
    L: IntoLLVM,
{
    fn accept<'a, 'ctx>(
        &self,
        ctx: &AccepterContext<'a, 'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: &TypeIdKind,
    ) -> BasicValueEnum<'ctx> {
        self(ctx, L::into_llvm(lhs), rhs)
    }
}

pub trait IntoLLVM {
    type Of<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx>;
}

pub struct LLVMBool<'ctx> {
    pub value: IntValue<'ctx>,
}

impl IntoLLVM for LLVMBool<'static> {
    type Of<'ctx> = LLVMBool<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::Bool;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMBool {
            value: value.into_int_value(),
        }
    }
}

pub struct LLVMInt<'ctx> {
    pub value: IntValue<'ctx>,
}

impl IntoLLVM for LLVMInt<'static> {
    type Of<'ctx> = LLVMInt<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::Int;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMInt {
            value: value.into_int_value(),
        }
    }
}

pub struct LLVMFloat<'ctx> {
    pub value: FloatValue<'ctx>,
}

impl IntoLLVM for LLVMFloat<'static> {
    type Of<'ctx> = LLVMFloat<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::Float;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMFloat {
            value: value.into_float_value(),
        }
    }
}

pub struct LLVMString<'ctx> {
    pub value: PointerValue<'ctx>, // char *
}

impl IntoLLVM for LLVMString<'static> {
    type Of<'ctx> = LLVMString<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::String;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMString {
            value: value.into_pointer_value(),
        }
    }
}

pub struct LLVMCallable<'ctx> {
    pub value: PointerValue<'ctx>, // fn *
}

impl IntoLLVM for LLVMCallable<'static> {
    type Of<'ctx> = LLVMCallable<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::Callable;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMCallable {
            value: value.into_pointer_value(),
        }
    }
}

pub struct LLVMUserStruct<'ctx> {
    pub value: StructValue<'ctx>,
}

impl IntoLLVM for LLVMUserStruct<'static> {
    type Of<'ctx> = LLVMUserStruct<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::UserStruct;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMUserStruct {
            value: value.into_struct_value(),
        }
    }
}

pub struct LLVMPointer<'ctx> {
    pub value: PointerValue<'ctx>,
}

impl IntoLLVM for LLVMPointer<'static> {
    type Of<'ctx> = LLVMPointer<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::Pointer;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMPointer {
            value: value.into_pointer_value(),
        }
    }
}

pub struct LLVMReference<'ctx> {
    pub value: PointerValue<'ctx>,
}

impl IntoLLVM for LLVMReference<'static> {
    type Of<'ctx> = LLVMReference<'ctx>;

    const DISPATCH_KIND: TypeDispatchKind = TypeDispatchKind::Reference;

    fn into_llvm<'ctx>(value: BasicValueEnum<'ctx>) -> Self::Of<'ctx> {
        LLVMReference {
            value: value.into_pointer_value(),
        }
    }
}

pub struct AccepterContext<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
    pub program: &'a Program,
    pub function: &'a Function,
    pub block: &'a BasicBlock,
    pub llvm_function_map: &'a HashMap<FunctionId, FunctionValue<'ctx>>,
    pub llvm_variable_map: &'a HashMap<VariableId, PointerValue<'ctx>>,
    pub llvm_temporaries_map: &'a mut HashMap<BlockId, HashMap<TemporaryId, BasicValueEnum<'ctx>>>,
}
