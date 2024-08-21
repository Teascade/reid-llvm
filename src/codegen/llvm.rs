use std::ffi::{CStr, CString};
use std::mem;

use llvm_sys::{
    core::*, prelude::*, LLVMBasicBlock, LLVMBuilder, LLVMContext, LLVMModule, LLVMType, LLVMValue,
};

use crate::ast;

fn into_cstring<T: Into<String>>(value: T) -> CString {
    let string = value.into();
    unsafe { CString::from_vec_with_nul_unchecked((string + "\0").into_bytes()) }
}

pub struct IRContext {
    context: *mut LLVMContext,
    builder: *mut LLVMBuilder,
}

impl IRContext {
    pub fn new() -> IRContext {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(context);
            IRContext { context, builder }
        }
    }

    pub fn module<'a>(&'a mut self, name: String) -> IRModule<'a> {
        IRModule::new(self, name)
    }
}

impl Drop for IRContext {
    fn drop(&mut self) {
        // Clean up. Values created in the context mostly get cleaned up there.
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.context);
        }
    }
}

pub struct IRModule<'a> {
    context: &'a mut IRContext,
    module: *mut LLVMModule,
}

impl<'a> IRModule<'a> {
    fn new<'b: 'a>(context: &'b mut IRContext, name: String) -> IRModule<'a> {
        unsafe {
            let module =
                LLVMModuleCreateWithNameInContext(into_cstring(name).as_ptr(), context.context);

            IRModule { context, module }
        }
    }

    pub fn print_to_string(&mut self) -> Result<&str, std::str::Utf8Error> {
        unsafe { CStr::from_ptr(LLVMPrintModuleToString(self.module)).to_str() }
    }
}

impl<'a> Drop for IRModule<'a> {
    fn drop(&mut self) {
        // Clean up. Values created in the context mostly get cleaned up there.
        unsafe {
            LLVMDisposeModule(self.module);
        }
    }
}

#[derive(Clone)]
pub enum IRType {
    I32,
}

impl IRType {
    fn in_context(&self, context: &mut IRContext) -> *mut LLVMType {
        use IRType::*;
        unsafe {
            return match self {
                I32 => LLVMInt32TypeInContext(context.context),
            };
        }
    }
}

pub struct IRFunction<'a, 'b> {
    module: &'b mut IRModule<'a>,
    /// The actual function
    value: *mut LLVMValue,
}

impl<'a, 'b> IRFunction<'a, 'b> {
    pub fn new(name: &String, module: &'b mut IRModule<'a>) -> IRFunction<'a, 'b> {
        unsafe {
            // TODO, fix later!

            let return_type = LLVMInt32TypeInContext(module.context.context);

            let mut argts = [];
            let func_type =
                LLVMFunctionType(return_type, argts.as_mut_ptr(), argts.len() as u32, 0);

            let function = LLVMAddFunction(module.module, into_cstring(name).as_ptr(), func_type);

            IRFunction {
                module,
                value: function,
            }
        }
    }
}

pub struct IRBlock<'a, 'b, 'c> {
    function: &'a mut IRFunction<'b, 'c>,
    blockref: *mut LLVMBasicBlock,
}

impl<'a, 'b, 'c> IRBlock<'a, 'b, 'c> {
    pub fn new(function: &'a mut IRFunction<'b, 'c>) -> IRBlock<'a, 'b, 'c> {
        unsafe {
            let blockref = LLVMCreateBasicBlockInContext(
                function.module.context.context,
                into_cstring("entryblock").as_ptr(),
            );
            LLVMPositionBuilderAtEnd(function.module.context.builder, blockref);

            IRBlock { function, blockref }
        }
    }

    pub fn add_return(self, value: Option<IRValue>) {
        unsafe {
            if let Some(value) = value {
                LLVMBuildRet(self.function.module.context.builder, value.ir_value);
            } else {
                LLVMBuildRetVoid(self.function.module.context.builder);
            }
        }
    }
}

impl<'a, 'b, 'c> Drop for IRBlock<'a, 'b, 'c> {
    fn drop(&mut self) {
        unsafe {
            LLVMAppendExistingBasicBlock(self.function.value, self.blockref);
        }
    }
}

#[derive(Clone)]
pub struct IRValue {
    pub ir_type: IRType,
    ir_value: *mut LLVMValue,
}

impl IRValue {
    pub fn from_literal(literal: &ast::Literal, block: &mut IRBlock) -> Self {
        use ast::Literal;
        match literal {
            Literal::I32(v) => {
                let ir_type = IRType::I32;
                unsafe {
                    let ir_value = LLVMConstInt(
                        ir_type.in_context(block.function.module.context),
                        mem::transmute(*v as i64),
                        1,
                    );
                    return IRValue { ir_type, ir_value };
                }
            }
        };
    }
}
