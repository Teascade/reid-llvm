use std::ffi::CString;
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
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMIntPredicate, core::*, prelude::*};
use types::{BasicType, BasicValue, FunctionType, IntegerType, Value};
use util::{ErrorMessageHolder, from_cstring, into_cstring};

pub mod types;
mod util;

pub enum IntPredicate {
    ULT,
    SLT,
}

impl IntPredicate {
    pub fn as_llvm(&self) -> LLVMIntPredicate {
        match *self {
            Self::ULT => LLVMIntPredicate::LLVMIntULT,
            Self::SLT => LLVMIntPredicate::LLVMIntSLT,
        }
    }
}

pub struct Context {
    pub(crate) context_ref: *mut LLVMContext,
    pub(crate) builder_ref: *mut LLVMBuilder,
}

impl Context {
    pub fn new() -> Context {
        unsafe {
            // Set up a context, module and builder in that context.
            let context = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(context);

            Context {
                context_ref: context,
                builder_ref: builder,
            }
        }
    }

    pub fn type_i1<'a>(&'a self) -> IntegerType<'a> {
        IntegerType::in_context(&self, 1)
    }

    pub fn type_i8<'a>(&'a self) -> IntegerType<'a> {
        IntegerType::in_context(&self, 8)
    }

    pub fn type_i16<'a>(&'a self) -> IntegerType<'a> {
        IntegerType::in_context(&self, 16)
    }

    pub fn type_i32<'a>(&'a self) -> IntegerType<'a> {
        IntegerType::in_context(&self, 32)
    }

    pub fn module<T: Into<String>>(&self, name: T) -> Module {
        Module::with_name(self, name.into())
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        // Clean up. Values created in the context mostly get cleaned up there.
        unsafe {
            LLVMDisposeBuilder(self.builder_ref);
            LLVMContextDispose(self.context_ref);
        }
    }
}

pub struct Module<'ctx> {
    context: &'ctx Context,
    module_ref: LLVMModuleRef,
    name: CString,
}

impl<'ctx> Module<'ctx> {
    fn with_name(context: &'ctx Context, name: String) -> Module<'ctx> {
        unsafe {
            let cstring_name = into_cstring(name);
            let module_ref =
                LLVMModuleCreateWithNameInContext(cstring_name.as_ptr(), context.context_ref);
            Module {
                context,
                module_ref,
                name: cstring_name,
            }
        }
    }

    pub fn add_function<ReturnValue: BasicValue>(
        &self,
        fn_type: FunctionType<'ctx, ReturnValue::BaseType>,
        name: &str,
    ) -> Function<'_, ReturnValue> {
        unsafe {
            let name_cstring = into_cstring(name);
            let function_ref =
                LLVMAddFunction(self.module_ref, name_cstring.as_ptr(), fn_type.llvm_type());
            Function {
                module: self,
                fn_type,
                name: name_cstring,
                fn_ref: function_ref,
            }
        }
    }

    pub fn print_to_string(&self) -> Result<String, String> {
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
            LLVMSetModuleDataLayout(self.module_ref, data_layout);

            let mut err = ErrorMessageHolder::null();
            LLVMVerifyModule(
                self.module_ref,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToFile(
                target_machine,
                self.module_ref,
                CString::new("hello.asm").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMAssemblyFile,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToFile(
                target_machine,
                self.module_ref,
                CString::new("hello.o").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMObjectFile,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            Ok(from_cstring(LLVMPrintModuleToString(self.module_ref)).expect("UTF8-err"))
        }
    }
}

impl<'a> Drop for Module<'a> {
    fn drop(&mut self) {
        // Clean up. Values created in the context mostly get cleaned up there.
        unsafe {
            LLVMDisposeModule(self.module_ref);
        }
    }
}

pub struct Function<'ctx, ReturnValue: BasicValue> {
    module: &'ctx Module<'ctx>,
    name: CString,
    fn_type: FunctionType<'ctx, ReturnValue::BaseType>,
    fn_ref: LLVMValueRef,
}

impl<'ctx, ReturnValue: BasicValue> Function<'ctx, ReturnValue> {
    pub fn block<T: Into<String>>(&'ctx self, name: T) -> BasicBlock<'ctx, ReturnValue> {
        BasicBlock::in_function(&self, name.into())
    }
}

pub struct BasicBlock<'ctx, ReturnValue: BasicValue> {
    function: &'ctx Function<'ctx, ReturnValue>,
    builder_ref: LLVMBuilderRef,
    name: CString,
    blockref: LLVMBasicBlockRef,
    inserted: bool,
}

impl<'ctx, ReturnValue: BasicValue> BasicBlock<'ctx, ReturnValue> {
    fn in_function(
        function: &'ctx Function<ReturnValue>,
        name: String,
    ) -> BasicBlock<'ctx, ReturnValue> {
        unsafe {
            let block_name = into_cstring(name);
            let block_ref = LLVMCreateBasicBlockInContext(
                function.module.context.context_ref,
                block_name.as_ptr(),
            );
            BasicBlock {
                function: function,
                builder_ref: function.module.context.builder_ref,
                name: block_name,
                blockref: block_ref,
                inserted: false,
            }
        }
    }

    #[must_use]
    pub fn integer_compare<T: BasicValue>(
        &self,
        lhs: &'ctx T,
        rhs: &'ctx T,
        comparison: &IntPredicate,
        name: &str,
    ) -> Result<T, ()> {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            let value = LLVMBuildICmp(
                self.builder_ref,
                comparison.as_llvm(),
                lhs.llvm_value(),
                rhs.llvm_value(),
                into_cstring(name).as_ptr(),
            );

            Ok(T::from_llvm(value))
        }
    }

    #[must_use]
    pub fn call(
        &self,
        callee: &'ctx Function<'ctx, ReturnValue>,
        params: Vec<Value<'ctx>>,
        name: &str,
    ) -> Result<ReturnValue, ()> {
        if params.len() != callee.fn_type.param_types.len() {
            return Err(()); // TODO invalid amount of parameters
        }
        for (t1, t2) in callee.fn_type.param_types.iter().zip(&params) {
            if t1 != &t2.llvm_type() {
                return Err(()); // TODO wrong types in parameters
            }
        }
        unsafe {
            let mut param_list: Vec<LLVMValueRef> = params.iter().map(|p| p.llvm_value()).collect();
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            let ret_val = LLVMBuildCall2(
                self.builder_ref,
                callee.fn_type.llvm_type(),
                callee.fn_ref,
                param_list.as_mut_ptr(),
                param_list.len() as u32,
                into_cstring(name).as_ptr(),
            );
            Ok(ReturnValue::from_llvm(ret_val))
        }
    }

    #[must_use]
    pub fn add<T: BasicValue>(&self, lhs: &T, rhs: &T, name: &str) -> Result<T, ()> {
        if lhs.llvm_type() != rhs.llvm_type() {
            return Err(()); // TODO error
        }
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            let add_value_ref = LLVMBuildAdd(
                self.builder_ref,
                lhs.llvm_value(),
                rhs.llvm_value(),
                into_cstring(name).as_ptr(),
            );
            Ok(T::from_llvm(add_value_ref))
        }
    }

    #[must_use]
    pub fn br(self, into: BasicBlock<'ctx, ReturnValue>) -> Result<(), ()> {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            LLVMBuildBr(self.builder_ref, into.blockref);
            self.terminate();
            Ok(())
        }
    }

    #[must_use]
    pub fn conditional_br<T: BasicValue>(
        self,
        condition: &T,
        lhs_name: &str,
        rhs_name: &str,
    ) -> Result<(BasicBlock<'ctx, ReturnValue>, BasicBlock<'ctx, ReturnValue>), ()> {
        unsafe {
            let lhs = BasicBlock::in_function(&self.function, lhs_name.into());
            let rhs = BasicBlock::in_function(&self.function, rhs_name.into());

            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            LLVMBuildCondBr(
                self.builder_ref,
                condition.llvm_value(),
                lhs.blockref,
                rhs.blockref,
            );
            self.terminate();
            Ok((lhs, rhs))
        }
    }

    #[must_use]
    pub fn ret(self, return_value: &ReturnValue) -> Result<(), ()> {
        if self.function.fn_type.return_type().llvm_type() != return_value.llvm_type() {
            return Err(());
        }
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            LLVMBuildRet(self.builder_ref, return_value.llvm_value());
            self.terminate();
            Ok(())
        }
    }

    unsafe fn terminate(mut self) {
        unsafe {
            LLVMAppendExistingBasicBlock(self.function.fn_ref, self.blockref);
            self.inserted = true;
        }
    }
}

impl<'ctx, ReturnValue: BasicValue> Drop for BasicBlock<'ctx, ReturnValue> {
    fn drop(&mut self) {
        if !self.inserted {
            unsafe {
                LLVMDeleteBasicBlock(self.blockref);
            }
        }
    }
}
