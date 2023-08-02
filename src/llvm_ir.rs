use std::ffi::{CStr, CString};
use std::mem;

use llvm_sys::{core::*, prelude::*, LLVMBuilder, LLVMContext, LLVMModule};

use crate::ast::Literal;

macro_rules! cstr {
    ($string:expr) => {
        core::ffi::CStr::from_bytes_with_nul_unchecked(concat!($string, "\0").as_bytes()).as_ptr()
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IRValueType {
    I32,
}

impl IRValueType {
    unsafe fn get_llvm_type(&self, codegen: &mut IRModule) -> LLVMTypeRef {
        match *self {
            Self::I32 => LLVMInt32TypeInContext(codegen.context),
        }
    }
}

#[derive(Clone, Debug)]
#[must_use = "value contains raw pointer and must be inserted somewhere"]
pub struct IRValue(IRValueType, LLVMValueRef);

fn into_cstring<T: Into<String>>(value: T) -> CString {
    let string = value.into();
    unsafe { CString::from_vec_with_nul_unchecked((string + "\0").into_bytes()) }
}

pub struct IRModule {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
}

impl IRModule {
    pub fn new<T: Into<String>>(name: T) -> IRModule {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(into_cstring(name).as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            IRModule {
                context,
                module,
                builder,
            }
        }
    }

    pub fn create_block(&mut self) -> IRBlock {
        IRBlock::create("entry", self)
    }

    pub fn create_func<T: Into<String>>(
        &mut self,
        name: T,
        return_type: IRValueType,
    ) -> IRFunction {
        unsafe {
            let mut argts = [];
            let func_type = LLVMFunctionType(
                return_type.get_llvm_type(self),
                argts.as_mut_ptr(),
                argts.len() as u32,
                0,
            );

            let anon_func = LLVMAddFunction(self.module, into_cstring(name).as_ptr(), func_type);
            IRFunction {
                value: IRValue(return_type, anon_func),
            }
        }
    }

    pub fn print_to_string(&mut self) -> Result<&str, std::str::Utf8Error> {
        unsafe { CStr::from_ptr(LLVMPrintModuleToString(self.module)).to_str() }
    }
}

impl Drop for IRModule {
    fn drop(&mut self) {
        // Clean up. Values created in the context mostly get cleaned up there.
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

pub struct IRFunction {
    value: IRValue,
}

impl IRFunction {
    pub fn add_definition(self, ret: IRValue, block: IRBlock) {
        unsafe {
            LLVMAppendExistingBasicBlock(self.value.1, block.blockref);
            LLVMBuildRet(block.module.builder, ret.1);
        }
    }
}

pub struct IRBlock<'a> {
    module: &'a mut IRModule,
    blockref: LLVMBasicBlockRef,
}

impl<'a> IRBlock<'a> {
    fn create<T: Into<String>>(name: T, codegen: &'a mut IRModule) -> IRBlock<'a> {
        unsafe {
            let blockref =
                LLVMCreateBasicBlockInContext(codegen.context, into_cstring(name).as_ptr());
            LLVMPositionBuilderAtEnd(codegen.builder, blockref);
            IRBlock {
                module: codegen,
                blockref,
            }
        }
    }

    pub fn get_const(&mut self, literal_type: &Literal) -> IRValue {
        unsafe {
            match *literal_type {
                Literal::I32(v) => IRValue(
                    IRValueType::I32,
                    LLVMConstInt(
                        LLVMInt32TypeInContext(self.module.context),
                        mem::transmute(v as i64),
                        1,
                    ),
                ),
            }
        }
    }

    pub fn add(&mut self, lhs: IRValue, rhs: IRValue) -> Result<IRValue, Error> {
        unsafe {
            if lhs.0 == rhs.0 {
                Ok(IRValue(
                    lhs.0,
                    LLVMBuildAdd(self.module.builder, lhs.1, rhs.1, cstr!("tmpadd")),
                ))
            } else {
                Err(Error::TypeMismatch(lhs.0, rhs.0))
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Type Mismatch: {0:?} {1:?}")]
    TypeMismatch(IRValueType, IRValueType),
}
