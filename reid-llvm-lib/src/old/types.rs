use std::{any::Any, marker::PhantomData, ptr::null_mut};

use llvm_sys::{
    LLVMTypeKind,
    core::*,
    prelude::{LLVMTypeRef, LLVMValueRef},
};

use crate::{BasicBlock, Context, PhiBuilder};

pub trait BasicType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef;

    fn is_type(llvm_type: LLVMTypeRef) -> bool
    where
        Self: Sized;

    unsafe fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized;

    fn function_type(&self, params: Vec<TypeEnum>) -> FunctionType<'ctx> {
        unsafe {
            let mut typerefs: Vec<LLVMTypeRef> = params.iter().map(|b| b.llvm_type()).collect();
            let param_ptr = typerefs.as_mut_ptr();
            let param_len = typerefs.len();
            FunctionType {
                phantom: PhantomData,
                return_type: self.llvm_type(),
                param_types: typerefs,
                type_ref: LLVMFunctionType(self.llvm_type(), param_ptr, param_len as u32, 0),
            }
        }
    }

    fn array_type(&'ctx self, length: u32) -> ArrayType<'ctx>
    where
        Self: Sized,
    {
        ArrayType {
            phantom: PhantomData,
            element_type: self.llvm_type(),
            length,
            type_ref: unsafe { LLVMArrayType(self.llvm_type(), length) },
        }
    }
}

impl<'ctx> PartialEq for &dyn BasicType<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.llvm_type() == other.llvm_type()
    }
}

impl<'ctx> PartialEq<LLVMTypeRef> for &dyn BasicType<'ctx> {
    fn eq(&self, other: &LLVMTypeRef) -> bool {
        self.llvm_type() == *other
    }
}

#[derive(Clone, Copy)]
pub struct IntegerType<'ctx> {
    context: &'ctx Context,
    type_ref: LLVMTypeRef,
}

impl<'ctx> BasicType<'ctx> for IntegerType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }

    unsafe fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized,
    {
        IntegerType {
            context,
            type_ref: llvm_type,
        }
    }

    fn is_type(llvm_type: LLVMTypeRef) -> bool {
        unsafe { LLVMGetTypeKind(llvm_type) == LLVMTypeKind::LLVMIntegerTypeKind }
    }
}

impl<'ctx> IntegerType<'ctx> {
    pub(crate) fn in_context(context: &Context, width: u32) -> IntegerType {
        let type_ref = unsafe {
            match width {
                128 => LLVMInt128TypeInContext(context.context_ref),
                64 => LLVMInt64TypeInContext(context.context_ref),
                32 => LLVMInt32TypeInContext(context.context_ref),
                16 => LLVMInt16TypeInContext(context.context_ref),
                8 => LLVMInt8TypeInContext(context.context_ref),
                1 => LLVMInt1TypeInContext(context.context_ref),
                _ => LLVMIntTypeInContext(context.context_ref, width),
            }
        };
        IntegerType { context, type_ref }
    }

    pub fn from_signed(&self, value: i64) -> IntegerValue<'ctx> {
        self.from_const(value as u64, true)
    }

    pub fn from_unsigned(&self, value: i64) -> IntegerValue<'ctx> {
        self.from_const(value as u64, false)
    }

    fn from_const(&self, value: u64, sign: bool) -> IntegerValue<'ctx> {
        unsafe {
            IntegerValue::from_llvm(LLVMConstInt(
                self.type_ref,
                value,
                match sign {
                    true => 1,
                    false => 0,
                },
            ))
        }
    }
}

#[derive(Clone)]
pub struct FunctionType<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    pub(crate) return_type: LLVMTypeRef,
    pub(crate) param_types: Vec<LLVMTypeRef>,
    type_ref: LLVMTypeRef,
}

impl<'ctx> BasicType<'ctx> for FunctionType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }

    unsafe fn from_llvm(_context: &'ctx Context, fn_type: LLVMTypeRef) -> Self
    where
        Self: Sized,
    {
        unsafe {
            let param_count = LLVMCountParamTypes(fn_type);
            let param_types_ptr: *mut LLVMTypeRef = null_mut();
            LLVMGetParamTypes(fn_type, param_types_ptr);
            let param_types: Vec<LLVMTypeRef> =
                std::slice::from_raw_parts(param_types_ptr, param_count as usize)
                    .iter()
                    .map(|t| *t)
                    .collect();
            FunctionType {
                phantom: PhantomData,
                return_type: LLVMGetReturnType(fn_type),
                param_types,
                type_ref: fn_type,
            }
        }
    }

    fn is_type(llvm_type: LLVMTypeRef) -> bool {
        unsafe { LLVMGetTypeKind(llvm_type) == LLVMTypeKind::LLVMFunctionTypeKind }
    }
}

#[derive(Clone, Copy)]
pub struct ArrayType<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    element_type: LLVMTypeRef,
    length: u32,
    type_ref: LLVMTypeRef,
}

impl<'ctx> BasicType<'ctx> for ArrayType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }

    unsafe fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized,
    {
        unsafe {
            let length = LLVMGetArrayLength(llvm_type);
            todo!()
        }
    }

    fn is_type(llvm_type: LLVMTypeRef) -> bool {
        unsafe { LLVMGetTypeKind(llvm_type) == LLVMTypeKind::LLVMArrayTypeKind }
    }
}

#[derive(Clone)]
pub enum TypeEnum<'ctx> {
    Integer(IntegerType<'ctx>),
    Array(ArrayType<'ctx>),
    Function(FunctionType<'ctx>),
}

impl<'ctx> From<IntegerType<'ctx>> for TypeEnum<'ctx> {
    fn from(int: IntegerType<'ctx>) -> Self {
        TypeEnum::Integer(int)
    }
}

impl<'ctx> From<ArrayType<'ctx>> for TypeEnum<'ctx> {
    fn from(arr: ArrayType<'ctx>) -> Self {
        TypeEnum::Array(arr)
    }
}

impl<'ctx> From<FunctionType<'ctx>> for TypeEnum<'ctx> {
    fn from(func: FunctionType<'ctx>) -> Self {
        TypeEnum::Function(func)
    }
}

impl<'ctx> TypeEnum<'ctx> {
    fn inner_basic(&'ctx self) -> &'ctx dyn BasicType<'ctx> {
        match self {
            TypeEnum::Integer(integer_type) => integer_type,
            TypeEnum::Array(array_type) => array_type,
            TypeEnum::Function(function_type) => function_type,
        }
    }
}

impl<'ctx> BasicType<'ctx> for TypeEnum<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.inner_basic().llvm_type()
    }

    fn is_type(llvm_type: LLVMTypeRef) -> bool
    where
        Self: Sized,
    {
        true
    }

    unsafe fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized,
    {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMIntegerTypeKind => {
                    TypeEnum::Integer(IntegerType::from_llvm(context, llvm_type))
                }
                LLVMTypeKind::LLVMArrayTypeKind => {
                    TypeEnum::Array(ArrayType::from_llvm(context, llvm_type))
                }
                LLVMTypeKind::LLVMFunctionTypeKind => {
                    TypeEnum::Function(FunctionType::from_llvm(context, llvm_type))
                }
                _ => todo!(),
            }
        }
    }
}

pub trait BasicValue<'ctx>: std::fmt::Debug {
    type BaseType: BasicType<'ctx>;
    unsafe fn from_llvm(value: LLVMValueRef) -> Self
    where
        Self: Sized;
    fn llvm_value(&self) -> LLVMValueRef;
    fn llvm_type(&self) -> LLVMTypeRef;
}

#[derive(Clone, Debug)]
pub struct IntegerValue<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    pub(crate) value_ref: LLVMValueRef,
}

impl<'ctx> BasicValue<'ctx> for IntegerValue<'ctx> {
    type BaseType = IntegerType<'ctx>;

    unsafe fn from_llvm(value: LLVMValueRef) -> Self {
        IntegerValue {
            phantom: PhantomData,
            value_ref: value,
        }
    }

    fn llvm_value(&self) -> LLVMValueRef {
        self.value_ref
    }

    fn llvm_type(&self) -> LLVMTypeRef {
        unsafe { LLVMTypeOf(self.value_ref) }
    }
}

#[derive(Clone, Debug)]
pub enum Value<'ctx> {
    Integer(IntegerValue<'ctx>),
}

impl<'ctx> BasicValue<'ctx> for Value<'ctx> {
    type BaseType = TypeEnum<'ctx>;

    unsafe fn from_llvm(value: LLVMValueRef) -> Self
    where
        Self: Sized,
    {
        unsafe {
            use LLVMTypeKind::*;

            let llvm_type = LLVMTypeOf(value);
            let type_kind = LLVMGetTypeKind(llvm_type);
            match type_kind {
                LLVMIntegerTypeKind => Value::Integer(IntegerValue::from_llvm(value)),
                _ => panic!("asd"),
            }
        }
    }

    fn llvm_value(&self) -> LLVMValueRef {
        match self {
            Self::Integer(i) => i.llvm_value(),
        }
    }

    fn llvm_type(&self) -> LLVMTypeRef {
        match self {
            Self::Integer(i) => i.llvm_type(),
        }
    }
}

impl<'ctx> From<IntegerValue<'ctx>> for Value<'ctx> {
    fn from(value: IntegerValue<'ctx>) -> Self {
        Value::Integer(value)
    }
}
