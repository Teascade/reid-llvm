use std::ffi::{CStr, CString};
use std::mem;

use llvm_sys::{core::*, prelude::*, LLVMBuilder, LLVMContext, LLVMModule, LLVMType, LLVMValue};

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

pub struct IRFunction<'a, 'b> {
    module: &'b mut IRModule<'a>,
    /// Signature of the function
    return_type: *mut LLVMType,
    /// Signature of the function
    func_type: *mut LLVMType,
    /// The actual function
    value: *mut LLVMValue,
}

impl<'a, 'b> IRFunction<'a, 'b> {
    pub fn new(module: &'b mut IRModule<'a>) -> IRFunction<'a, 'b> {
        unsafe {
            // TODO, fix later!

            let return_type = LLVMInt32TypeInContext(module.context.context);

            let mut argts = [];
            let func_type =
                LLVMFunctionType(return_type, argts.as_mut_ptr(), argts.len() as u32, 0);

            let function =
                LLVMAddFunction(module.module, into_cstring("testfunc").as_ptr(), func_type);

            let blockref = LLVMCreateBasicBlockInContext(
                module.context.context,
                into_cstring("entryblock").as_ptr(),
            );
            LLVMPositionBuilderAtEnd(module.context.builder, blockref);

            // What is the last 1 ?
            let return_value = LLVMConstInt(return_type, mem::transmute(3 as i64), 1);

            LLVMAppendExistingBasicBlock(function, blockref);
            LLVMBuildRet(module.context.builder, return_value);

            IRFunction {
                module,
                return_type,
                func_type,
                value: function,
            }
        }
    }
}
