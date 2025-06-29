use std::{any::Any, marker::PhantomData, ptr::null_mut};

use llvm_sys::{
    LLVMTypeKind,
    core::*,
    prelude::{LLVMTypeRef, LLVMValueRef},
};

use crate::Context;

pub trait BasicType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef;
    fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized;

    fn function_type(&'ctx self, params: Vec<&'ctx dyn BasicType>) -> FunctionType<'ctx, Self>
    where
        Self: Sized,
    {
        unsafe {
            let mut typerefs: Vec<LLVMTypeRef> = params.iter().map(|b| b.llvm_type()).collect();
            let param_ptr = typerefs.as_mut_ptr();
            let param_len = typerefs.len();
            FunctionType {
                phantom: PhantomData,
                param_types: typerefs,
                type_ref: LLVMFunctionType(self.llvm_type(), param_ptr, param_len as u32, 0),
            }
        }
    }

    fn array_type(&'ctx self, length: u32) -> ArrayType<'ctx, Self>
    where
        Self: Sized,
    {
        ArrayType {
            element_type: self,
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

pub struct IntegerType<'ctx> {
    context: &'ctx Context,
    type_ref: LLVMTypeRef,
}

impl<'ctx> BasicType<'ctx> for IntegerType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }

    fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized,
    {
        IntegerType {
            context,
            type_ref: llvm_type,
        }
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

    pub fn from_signed(&self, value: i64) -> IntegerValue<'_> {
        self.from_const(value as u64, true)
    }

    pub fn from_unsigned(&self, value: i64) -> IntegerValue<'_> {
        self.from_const(value as u64, false)
    }

    fn from_const(&self, value: u64, sign: bool) -> IntegerValue<'_> {
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

pub struct FunctionType<'ctx, ReturnType: BasicType<'ctx>> {
    phantom: PhantomData<&'ctx ReturnType>,
    pub(crate) param_types: Vec<LLVMTypeRef>,
    type_ref: LLVMTypeRef,
}

impl<'ctx, ReturnType: BasicType<'ctx>> BasicType<'ctx> for FunctionType<'ctx, ReturnType> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }

    fn from_llvm(_context: &'ctx Context, fn_type: LLVMTypeRef) -> Self
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
                param_types,
                type_ref: fn_type,
            }
        }
    }
}

impl<'ctx, ReturnType: BasicType<'ctx>> FunctionType<'ctx, ReturnType> {
    pub fn return_type(&self, context: &'ctx Context) -> ReturnType {
        unsafe {
            let return_type = LLVMGetReturnType(self.type_ref);
            ReturnType::from_llvm(context, return_type)
        }
    }
}

pub struct ArrayType<'ctx, T: BasicType<'ctx>> {
    element_type: &'ctx T,
    length: u32,
    type_ref: LLVMTypeRef,
}

impl<'ctx, T: BasicType<'ctx>> BasicType<'ctx> for ArrayType<'ctx, T> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }

    fn from_llvm(context: &'ctx Context, llvm_type: LLVMTypeRef) -> Self
    where
        Self: Sized,
    {
        unsafe {
            let length = LLVMGetArrayLength(llvm_type);
            todo!()
        }
    }
}

pub trait BasicValue<'ctx> {
    type BaseType: BasicType<'ctx>;
    unsafe fn from_llvm(value: LLVMValueRef) -> Self
    where
        Self: Sized;
    fn llvm_value(&self) -> LLVMValueRef;
    fn llvm_type(&self) -> LLVMTypeRef;
}

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

pub enum Value<'ctx> {
    Integer(IntegerValue<'ctx>),
}

impl<'ctx> Value<'ctx> {
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

    pub fn llvm_value(&self) -> LLVMValueRef {
        match self {
            Self::Integer(i) => i.llvm_value(),
        }
    }

    pub fn llvm_type(&self) -> LLVMTypeRef {
        match self {
            Self::Integer(i) => i.llvm_type(),
        }
    }
}
