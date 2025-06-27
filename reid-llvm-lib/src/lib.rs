use std::ffi::{CStr, CString};
use std::ptr::null_mut;

use llvm_sys::analysis::LLVMVerifyModule;
use llvm_sys::target::{
    LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
    LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets, LLVMSetModuleDataLayout,
};
use llvm_sys::target_machine::{
    LLVMCodeGenFileType, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
    LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMTargetMachineEmitToFile,
};
use llvm_sys::{
    LLVMBasicBlock, LLVMBuilder, LLVMContext, LLVMModule, LLVMType, LLVMValue, core::*, prelude::*,
};
use primitives::IRType;
use util::{ErrorMessageHolder, from_cstring, into_cstring};

pub use primitives::{IRValue, OpaqueIRValue};

mod primitives;
mod util;

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
    context: &'a IRContext,
    module: *mut LLVMModule,
}

impl<'a> IRModule<'a> {
    pub fn new(context: &'a IRContext, name: &String) -> IRModule<'a> {
        unsafe {
            let module =
                LLVMModuleCreateWithNameInContext(into_cstring(name).as_ptr(), context.context);

            IRModule { context, module }
        }
    }

    pub fn print_to_string(&self) -> Result<&str, String> {
        unsafe {
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            let triple = LLVMGetDefaultTargetTriple();

            let mut target: _ = null_mut();
            let mut err = ErrorMessageHolder::null();
            LLVMGetTargetFromTriple(triple, &mut target, err.borrow_mut());
            println!("{:?}, {:?}", from_cstring(triple), target);
            err.into_result().unwrap();

            let target_machine = LLVMCreateTargetMachine(
                target,
                triple,
                c"generic".as_ptr(),
                c"".as_ptr(),
                llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                llvm_sys::target_machine::LLVMRelocMode::LLVMRelocDefault,
                llvm_sys::target_machine::LLVMCodeModel::LLVMCodeModelDefault,
            );

            let data_layout = LLVMCreateTargetDataLayout(target_machine);
            LLVMSetModuleDataLayout(self.module, data_layout);

            let mut err = ErrorMessageHolder::null();
            LLVMVerifyModule(
                self.module,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                CString::new("hello.asm").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMAssemblyFile,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                CString::new("hello.o").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMObjectFile,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            Ok(CStr::from_ptr(LLVMPrintModuleToString(self.module))
                .to_str()
                .expect("UTF8-err"))
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

pub struct IRFunction<'a> {
    pub name: String,
    pub module: &'a IRModule<'a>,
    pub function_ref: *mut LLVMValue,
    pub function_type: *mut LLVMType,
}

impl<'a> IRFunction<'a> {
    pub fn new(module: &'a IRModule<'a>, name: &String) -> IRFunction<'a> {
        unsafe {
            // TODO, fix later!
            let return_type = LLVMInt8TypeInContext(module.context.context);
            let mut argts = [];
            let func_type =
                LLVMFunctionType(return_type, argts.as_mut_ptr(), argts.len() as u32, 0);

            let function_ref =
                LLVMAddFunction(module.module, into_cstring(name).as_ptr(), func_type);

            let function_type =
                LLVMFunctionType(return_type, argts.as_mut_ptr(), argts.len() as u32, 0);

            IRFunction {
                name: name.clone(),
                module,
                function_ref,
                function_type,
            }
        }
    }
}

pub struct IRBlock<'a> {
    context: &'a IRContext,
    blockref: *mut LLVMBasicBlock,
    inserted: bool,
}

impl<'a> IRBlock<'a> {
    pub fn new(context: &'a IRContext, name: &String) -> IRBlock<'a> {
        unsafe {
            let blockref =
                LLVMCreateBasicBlockInContext(context.context, into_cstring(name).as_ptr());

            IRBlock {
                context,
                blockref,
                inserted: false,
            }
        }
    }

    pub fn call(&self, function: &IRFunction) -> OpaqueIRValue {
        unsafe {
            let builder = self.context.builder;
            LLVMPositionBuilderAtEnd(builder, self.blockref);

            // Add way to check and use parameters
            let mut args = [];

            let value = LLVMBuildCall2(
                builder,
                function.function_type,
                function.function_ref,
                args.as_mut_ptr(),
                args.len() as u32,
                into_cstring(&function.name).as_ptr(),
            );
            OpaqueIRValue(i32::llvm_type(&self.context), value)
        }
    }

    pub fn add(&self, lhs: OpaqueIRValue, rhs: OpaqueIRValue) -> Result<OpaqueIRValue, ()> {
        let OpaqueIRValue(t1, lhs) = lhs;
        let OpaqueIRValue(t2, rhs) = rhs;
        if t1 != t2 {
            Err(())
        } else {
            unsafe {
                let builder = self.context.builder;
                LLVMPositionBuilderAtEnd(builder, self.blockref);
                let value = LLVMBuildAdd(builder, lhs, rhs, c"add".as_ptr());
                Ok(OpaqueIRValue(t1, value))
            }
        }
    }

    pub fn less_than(&self, lhs: OpaqueIRValue, rhs: OpaqueIRValue) -> Result<IRValue<bool>, ()> {
        let OpaqueIRValue(t1, lhs) = lhs;
        let OpaqueIRValue(t2, rhs) = rhs;

        if t1 != t2 {
            Err(())
        } else {
            unsafe {
                let builder = self.context.builder;
                LLVMPositionBuilderAtEnd(builder, self.blockref);
                let value = LLVMBuildICmp(
                    builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                    lhs,
                    rhs,
                    c"asd".as_ptr(),
                );
                Ok(IRValue::from_runtime(bool::llvm_type(&self.context), value))
            }
        }
    }

    pub fn cond_br(
        self,
        function: &IRFunction,
        value: IRValue<bool>,
    ) -> (IRBlock<'a>, IRBlock<'a>) {
        let lhs = IRBlock::new(self.context, &"lhs".to_owned());
        let rhs = IRBlock::new(self.context, &"rhs".to_owned());
        unsafe {
            let builder = self.context.builder;
            LLVMPositionBuilderAtEnd(builder, self.blockref);
            LLVMBuildCondBr(builder, value.1.1, lhs.blockref, rhs.blockref);
            self.append(function);
            (lhs, rhs)
        }
    }

    pub fn ret(self, function: &IRFunction, value: OpaqueIRValue) {
        unsafe {
            let builder = self.context.builder;
            LLVMPositionBuilderAtEnd(builder, self.blockref);
            LLVMBuildRet(builder, value.1);
            self.append(function);
        }
    }

    unsafe fn append(mut self, function: &IRFunction<'a>) {
        unsafe {
            LLVMAppendExistingBasicBlock(function.function_ref, self.blockref);
            self.inserted = true;
        }
    }
}

impl<'a> Drop for IRBlock<'a> {
    fn drop(&mut self) {
        unsafe {
            if !self.inserted {
                LLVMDeleteBasicBlock(self.blockref);
            }
        }
    }
}
