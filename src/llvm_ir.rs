use std::ffi::{CStr, CString};
use std::mem;

use llvm_sys::{core::*, prelude::*, LLVMBuilder, LLVMContext, LLVMModule};

use crate::ast::{FunctionSignature, Literal};

macro_rules! cstr {
    ($string:expr) => {
        core::ffi::CStr::from_bytes_with_nul_unchecked(concat!($string, "\0").as_bytes()).as_ptr()
    };
}

#[derive(Clone, Debug)]
#[must_use = "value contains raw pointer and must be inserted somewhere"]
pub struct IRValue(IRValueType, LLVMValueRef);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IRValueType {
    I32,
}

impl IRValueType {
    unsafe fn get_llvm_type(&self, module: &mut IRModule) -> LLVMTypeRef {
        match *self {
            Self::I32 => LLVMInt32TypeInContext(module.context),
        }
    }
}

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

    pub fn create_block(&mut self, name: &str) -> IRBlock {
        IRBlock::create(name, self)
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

#[derive(Clone, Debug)]
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

    pub fn mul(&mut self, lhs: IRValue, rhs: IRValue) -> Result<IRValue, Error> {
        unsafe {
            if lhs.0 == rhs.0 {
                Ok(IRValue(
                    lhs.0,
                    LLVMBuildMul(self.module.builder, lhs.1, rhs.1, cstr!("tmpadd")),
                ))
            } else {
                Err(Error::TypeMismatch(lhs.0, rhs.0))
            }
        }
    }

    pub fn function_call(&mut self, callee: &FunctionSignature) -> Result<IRValue, Error> {
        unsafe {
            let function = LLVMGetNamedFunction(
                self.module.module,
                into_cstring(callee.name.clone()).as_ptr(),
            );

            let ret_t = LLVMInt32TypeInContext(self.module.context);
            let mut argts = [];
            let mut args = [];

            let fun_t = LLVMFunctionType(ret_t, argts.as_mut_ptr(), argts.len() as u32, 0);

            let call = LLVMBuildCall2(
                self.module.builder,
                fun_t,
                function,
                args.as_mut_ptr(),
                args.len() as u32,
                into_cstring(&callee.name).as_ptr(),
            );

            Ok(IRValue(IRValueType::I32, call))
        }
    }

    pub fn cmp(&self, lhs: IRValue, rhs: IRValue) -> Result<IRValue, Error> {
        // FIXME! Only handles I32 comparisons for now
        unsafe {
            Ok(IRValue(
                IRValueType::I32,
                LLVMBuildICmp(
                    self.module.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                    lhs.1,
                    rhs.1,
                    into_cstring("ifcond").as_ptr(),
                ),
            ))
        }
    }

    pub fn conditional_branch(
        &mut self,
        if_ref: IRValue,
        then_ret: IRValue,
        else_ret: IRValue,
    ) -> Result<IRValue, Error> {
        unsafe {
            dbg!("one");
            let ifcont_blockref =
                LLVMCreateBasicBlockInContext(self.module.context, into_cstring("ifcont").as_ptr());

            dbg!("two");
            let (then_val, then_blockref) = {
                let block_ir = self.module.create_block("then");
                LLVMBuildBr(block_ir.module.builder, ifcont_blockref);
                (then_ret, block_ir.blockref)
            };

            dbg!("three");
            let (else_val, else_blockref) = {
                let block_ir = self.module.create_block("else");
                LLVMBuildBr(block_ir.module.builder, ifcont_blockref);
                (else_ret, block_ir.blockref)
            };

            dbg!("f");
            LLVMPositionBuilderAtEnd(self.module.builder, self.blockref);

            dbg!("s");
            LLVMBuildCondBr(self.module.builder, if_ref.1, then_blockref, else_blockref);

            dbg!("se");
            self.blockref = ifcont_blockref;
            LLVMPositionBuilderAtEnd(self.module.builder, self.blockref);

            let phi = LLVMBuildPhi(
                self.module.builder,
                LLVMInt32TypeInContext(self.module.context),
                into_cstring("iftmp").as_ptr(),
            );
            dbg!("e");
            LLVMAddIncoming(
                phi,
                [then_val.1, else_val.1].as_mut_ptr(),
                [then_blockref, else_blockref].as_mut_ptr(),
                2,
            );

            dbg!("ni");
            Ok(IRValue(IRValueType::I32, phi))
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Type Mismatch: {0:?} {1:?}")]
    TypeMismatch(IRValueType, IRValueType),
}
