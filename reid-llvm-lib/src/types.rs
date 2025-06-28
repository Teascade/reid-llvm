use llvm_sys::{
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

pub struct IntegerType<'ctx, const WIDTH: u32, const SIGNED: bool> {
    context: &'ctx Context,
    type_ref: LLVMTypeRef,
}

impl<'ctx, const WIDTH: u32, const SIGNED: bool> BasicType for IntegerType<'ctx, WIDTH, SIGNED> {
    fn llvm_type(&self) -> LLVMTypeRef {
        self.type_ref
    }
}

impl<'ctx, const WIDTH: u32, const SIGNED: bool> IntegerType<'ctx, WIDTH, SIGNED> {
    pub(crate) fn in_context(context: &Context) -> IntegerType<WIDTH, SIGNED> {
        let type_ref = unsafe {
            match WIDTH {
                128 => LLVMInt128TypeInContext(context.context_ref),
                64 => LLVMInt64TypeInContext(context.context_ref),
                32 => LLVMInt32TypeInContext(context.context_ref),
                16 => LLVMInt16TypeInContext(context.context_ref),
                8 => LLVMInt8TypeInContext(context.context_ref),
                1 => LLVMInt1TypeInContext(context.context_ref),
                _ => LLVMIntTypeInContext(context.context_ref, WIDTH),
            }
        };
        IntegerType { context, type_ref }
    }

    pub fn from_const(&self, value: u64) -> OpaqueValue {
        unsafe {
            OpaqueValue {
                basic_type: self,
                value_ref: LLVMConstInt(self.type_ref, value, Self::sign_to_i32()),
            }
        }
    }

    const fn sign_to_i32() -> i32 {
        match SIGNED {
            true => 1,
            false => 0,
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

pub struct OpaqueValue<'ctx> {
    pub(crate) basic_type: &'ctx dyn BasicType,
    pub(crate) value_ref: LLVMValueRef,
}

impl<'ctx> OpaqueValue<'ctx> {
    pub(crate) fn new(
        basic_type: &'ctx dyn BasicType,
        value_ref: LLVMValueRef,
    ) -> OpaqueValue<'ctx> {
        OpaqueValue {
            basic_type,
            value_ref,
        }
    }
}
