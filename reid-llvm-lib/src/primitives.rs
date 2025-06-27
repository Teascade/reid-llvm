use std::marker::PhantomData;

use llvm_sys::{
    core::{
        LLVMConstAdd, LLVMConstInt, LLVMInt1TypeInContext, LLVMInt8Type, LLVMInt8TypeInContext,
        LLVMInt16TypeInContext, LLVMInt32TypeInContext, LLVMIntTypeInContext,
    },
    prelude::{LLVMBool, LLVMTypeRef, LLVMValueRef},
};

use crate::IRContext;

pub trait IRType {
    const SIGNED: LLVMBool;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef;
}

impl IRType for bool {
    const SIGNED: LLVMBool = 0;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef {
        unsafe { LLVMInt1TypeInContext(context.context) }
    }
}

impl IRType for i32 {
    const SIGNED: LLVMBool = 1;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef {
        unsafe { LLVMInt8TypeInContext(context.context) }
    }
}

impl IRType for u32 {
    const SIGNED: LLVMBool = 0;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(context.context) }
    }
}

impl IRType for u16 {
    const SIGNED: LLVMBool = 0;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef {
        unsafe { LLVMInt16TypeInContext(context.context) }
    }
}

pub struct OpaqueIRValue(pub(crate) LLVMTypeRef, pub(crate) LLVMValueRef);

pub struct IRValue<T: IRType>(PhantomData<T>, pub(crate) OpaqueIRValue);

impl<T: IRType> IRValue<T> {
    pub(crate) unsafe fn from_runtime(t: LLVMTypeRef, value: LLVMValueRef) -> IRValue<T> {
        IRValue(PhantomData, OpaqueIRValue(t, value))
    }
}

impl<T: IRType + Into<i64>> IRValue<T> {
    pub fn from_const(context: &IRContext, value: T) -> Self {
        unsafe {
            let t = T::llvm_type(context);
            let value = LLVMConstInt(t, value.into() as u64, T::SIGNED);
            IRValue(PhantomData, OpaqueIRValue(t, value))
        }
    }
}

impl<T: IRType> From<IRValue<T>> for OpaqueIRValue {
    fn from(value: IRValue<T>) -> Self {
        value.1
    }
}
