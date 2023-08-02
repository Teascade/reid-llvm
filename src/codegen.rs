use std::mem;

use llvm_sys::{core::*, prelude::*, LLVMBuilder, LLVMContext, LLVMModule};

use crate::parser::Literal;

macro_rules! cstr {
    ($string:expr) => {
        core::ffi::CStr::from_bytes_with_nul_unchecked(concat!($string, "\0").as_bytes()).as_ptr()
    };
}

#[derive(PartialEq, Eq)]
pub enum ValueType {
    I32,
}

impl ValueType {
    unsafe fn get_llvm_type(&self, codegen: &mut CodeGenerator) -> LLVMTypeRef {
        match *self {
            Self::I32 => LLVMInt32TypeInContext(codegen.context),
        }
    }
}

#[must_use = "value contains raw pointer and must be inserted somewhere"]
pub struct Value(ValueType, LLVMValueRef);

pub struct CodeGenerator {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(cstr!("testmodule"), context);
            let builder = LLVMCreateBuilderInContext(context);

            CodeGenerator {
                context,
                module,
                builder,
            }
        }
    }

    pub fn get_const(&mut self, literal_type: &Literal) -> Value {
        unsafe {
            match *literal_type {
                Literal::I32(v) => Value(
                    ValueType::I32,
                    LLVMConstInt(
                        LLVMInt32TypeInContext(self.context),
                        mem::transmute(v as i64),
                        1,
                    ),
                ),
            }
        }
    }

    pub fn add(&mut self, lhs: Value, rhs: Value) -> Result<Value, ()> {
        unsafe {
            if lhs.0 == rhs.0 {
                Ok(Value(
                    lhs.0,
                    LLVMBuildAdd(self.builder, lhs.1, rhs.1, cstr!("tmpadd")),
                ))
            } else {
                Err(())
            }
        }
    }

    pub fn create_func(&mut self, ret: Value) {
        unsafe {
            let mut argts = [];
            let func_type = LLVMFunctionType(
                ret.0.get_llvm_type(self),
                argts.as_mut_ptr(),
                argts.len() as u32,
                0,
            );

            let anon_func = LLVMAddFunction(self.module, cstr!("_anon_func"), func_type);

            // Create a basic block in the function and set our builder to generate
            // code in it.
            let bb = LLVMAppendBasicBlockInContext(self.context, anon_func, cstr!("entry"));
            LLVMPositionBuilderAtEnd(self.builder, bb);

            // Emit a `ret i64` into the function to return the computed sum.
            LLVMBuildRet(self.builder, ret.1);
        }
    }
}

impl Drop for CodeGenerator {
    fn drop(&mut self) {
        // Clean up. Values created in the context mostly get cleaned up there.
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDumpModule(self.module);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
