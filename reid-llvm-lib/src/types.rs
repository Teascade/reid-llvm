use std::{any::Any, marker::PhantomData};

use llvm_sys::{
    LLVMTypeKind,
    core::*,
    prelude::{LLVMTypeRef, LLVMValueRef},
};

use crate::Context;

pub trait BasicType {
    fn llvm_type(&self) -> LLVMTypeRef;

    fn function_type<'a>(&'a self, params: &'a [&'a dyn BasicType]) -> FunctionType<'a, Self>
    where
        Self: Sized,
    {
        unsafe {
            let mut typerefs: Vec<LLVMTypeRef> = params.iter().map(|b| b.llvm_type()).collect();
            let param_ptr = typerefs.as_mut_ptr();
            let param_len = typerefs.len();
            FunctionType {
                return_type: self,
                param_types: typerefs,
                type_ref: LLVMFunctionType(self.llvm_type(), param_ptr, param_len as u32, 0),
            }
        }
    }

    fn array_type(&self, length: u32) -> ArrayType<Self>
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

impl PartialEq for &dyn BasicType {
    fn eq(&self, other: &Self) -> bool {
        self.llvm_type() == other.llvm_type()
    }
}

impl PartialEq<LLVMTypeRef> for &dyn BasicType {
    fn eq(&self, other: &LLVMTypeRef) -> bool {
        self.llvm_type() == *other
    }
}

pub struct IntegerType<'ctx> {
    context: &'ctx Context,
    type_ref: LLVMTypeRef,
}

impl<'ctx> BasicType for IntegerType<'ctx> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
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

pub struct FunctionType<'ctx, ReturnType: BasicType> {
    pub(crate) return_type: &'ctx ReturnType,
    pub(crate) param_types: Vec<LLVMTypeRef>,
    type_ref: LLVMTypeRef,
}

impl<'ctx, ReturnType: BasicType> BasicType for FunctionType<'ctx, ReturnType> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }
}

impl<'ctx, ReturnType: BasicType> FunctionType<'ctx, ReturnType> {
    pub fn return_type(&self) -> &ReturnType {
        self.return_type
    }
}

pub struct ArrayType<'ctx, T: BasicType> {
    element_type: &'ctx T,
    length: u32,
    type_ref: LLVMTypeRef,
}

impl<'ctx, T: BasicType> BasicType for ArrayType<'ctx, T> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }
}

pub trait BasicValue {
    type BaseType: BasicType;
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

impl<'ctx> BasicValue for IntegerValue<'ctx> {
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
