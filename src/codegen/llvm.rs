use std::ffi::{CStr, CString};
use std::mem;

use llvm_sys::{core::*, prelude::*, LLVMBuilder, LLVMContext, LLVMModule};

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
    fn new(context: &'a mut IRContext, name: String) -> IRModule<'a> {
        unsafe {
            let module =
                LLVMModuleCreateWithNameInContext(into_cstring(name).as_ptr(), context.context);

            // TODO, fix later!

            let t = LLVMInt32TypeInContext(context.context);

            let mut argts = [];
            let func_type = LLVMFunctionType(t, argts.as_mut_ptr(), argts.len() as u32, 0);

            let anon_func = LLVMAddFunction(module, into_cstring("testfunc").as_ptr(), func_type);

            let blockref =
                LLVMCreateBasicBlockInContext(context.context, into_cstring("entryblock").as_ptr());
            LLVMPositionBuilderAtEnd(context.builder, blockref);

            // What is the last 1 ?
            let val = LLVMConstInt(t, mem::transmute(3 as i64), 1);

            LLVMAppendExistingBasicBlock(anon_func, blockref);
            LLVMBuildRet(context.builder, val);

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
