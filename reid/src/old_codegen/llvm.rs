use std::borrow::BorrowMut;
use std::ffi::{CStr, CString};
use std::mem;
use std::ptr::null_mut;

use llvm_sys::analysis::LLVMVerifyModule;
use llvm_sys::transforms::pass_manager_builder::{
    self, LLVMOpaquePassManagerBuilder, LLVMPassManagerBuilderCreate,
    LLVMPassManagerBuilderSetOptLevel,
};
use llvm_sys::{
    core::*, prelude::*, LLVMBasicBlock, LLVMBuilder, LLVMContext, LLVMModule, LLVMType, LLVMValue,
};

use crate::parser;

fn into_cstring<T: Into<String>>(value: T) -> CString {
    let string = value.into();
    unsafe { CString::from_vec_with_nul_unchecked((string + "\0").into_bytes()) }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Type mismatch: {0:?} vs {1:?}")]
    TypeMismatch(IRType, IRType),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IRType {
    I32,
    Boolean,
}

impl IRType {
    fn in_context(&self, context: &IRContext) -> *mut LLVMType {
        use IRType::*;
        unsafe {
            return match self {
                I32 => LLVMInt32TypeInContext(context.context),
                Boolean => LLVMInt1TypeInContext(context.context),
            };
        }
    }
}

#[derive(Clone)]
pub struct IRValue(pub IRType, *mut LLVMValue);

impl IRValue {
    pub fn from_literal(literal: &parser::Literal, module: &IRModule) -> Self {
        use parser::Literal;
        match literal {
            Literal::I32(v) => {
                let ir_type = IRType::I32;
                unsafe {
                    let ir_value = LLVMConstInt(
                        ir_type.in_context(module.context),
                        mem::transmute(*v as i64),
                        1,
                    );
                    return IRValue(ir_type, ir_value);
                }
            }
        };
    }
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
        unsafe {
            LLVMVerifyModule(
                self.module,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                null_mut(),
            );
            CStr::from_ptr(LLVMPrintModuleToString(self.module)).to_str()
        }
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

pub struct IRFunction<'a, 'b> {
    pub module: &'b IRModule<'a>,
    /// The actual function
    value: *mut LLVMValue,
}

impl<'a, 'b> IRFunction<'a, 'b> {
    pub fn new(name: &String, module: &'b IRModule<'a>) -> IRFunction<'a, 'b> {
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
    pub function: &'c IRFunction<'a, 'b>,
    blockref: *mut LLVMBasicBlock,
}

impl<'a, 'b, 'c> IRBlock<'a, 'b, 'c> {
    pub fn new(function: &'c IRFunction<'a, 'b>, name: &CStr) -> IRBlock<'a, 'b, 'c> {
        unsafe {
            let blockref =
                LLVMCreateBasicBlockInContext(function.module.context.context, name.as_ptr());

            IRBlock { function, blockref }
        }
    }

    pub fn add(
        &mut self,
        IRValue(lhs_t, lhs_v): IRValue,
        IRValue(rhs_t, rhs_v): IRValue,
    ) -> Result<IRValue, Error> {
        unsafe {
            LLVMPositionBuilderAtEnd(self.function.module.context.builder, self.blockref);
            if lhs_t == rhs_t {
                Ok(IRValue(
                    lhs_t,
                    LLVMBuildAdd(
                        self.function.module.context.builder,
                        lhs_v,
                        rhs_v,
                        c"tmpadd".as_ptr(),
                    ),
                ))
            } else {
                Err(Error::TypeMismatch(lhs_t, rhs_t))
            }
        }
    }

    pub fn mult(
        &mut self,
        IRValue(lhs_t, lhs_v): IRValue,
        IRValue(rhs_t, rhs_v): IRValue,
    ) -> Result<IRValue, Error> {
        unsafe {
            LLVMPositionBuilderAtEnd(self.function.module.context.builder, self.blockref);
            if lhs_t == rhs_t {
                Ok(IRValue(
                    lhs_t,
                    LLVMBuildMul(
                        self.function.module.context.builder,
                        lhs_v,
                        rhs_v,
                        c"tmpadd".as_ptr(),
                    ),
                ))
            } else {
                Err(Error::TypeMismatch(lhs_t, rhs_t))
            }
        }
    }

    pub fn less_than(
        &mut self,
        IRValue(lhs_t, lhs_v): IRValue,
        IRValue(rhs_t, rhs_v): IRValue,
    ) -> Result<IRValue, Error> {
        unsafe {
            LLVMPositionBuilderAtEnd(self.function.module.context.builder, self.blockref);
            if lhs_t == rhs_t {
                Ok(IRValue(
                    IRType::Boolean,
                    LLVMBuildICmp(
                        self.function.module.context.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntULT,
                        lhs_v,
                        rhs_v,
                        c"IntULT".as_ptr(),
                    ),
                ))
            } else {
                Err(Error::TypeMismatch(lhs_t, rhs_t))
            }
        }
    }

    pub fn add_return(&mut self, value: Option<IRValue>) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.function.module.context.builder, self.blockref);
            if let Some(IRValue(_, value)) = value {
                LLVMBuildRet(self.function.module.context.builder, value);
            } else {
                LLVMBuildRetVoid(self.function.module.context.builder);
            }
        }
    }

    pub fn branch(
        &mut self,
        IRValue(_, condition): IRValue,
        then_block: &mut IRBlock,
        else_block: &mut IRBlock,
    ) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.function.module.context.builder, self.blockref);
            LLVMBuildCondBr(
                self.function.module.context.builder,
                condition,
                then_block.blockref,
                else_block.blockref,
            );
        }
    }

    pub fn move_into(&mut self, block: &mut IRBlock) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.function.module.context.builder, self.blockref);
            LLVMBuildBr(self.function.module.context.builder, block.blockref);
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
