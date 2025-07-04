use std::ffi::CString;
use std::marker::PhantomData;
use std::net::Incoming;
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
    SLT,
    SGT,

    ULT,
    UGT,
}

impl IntPredicate {
    pub fn as_llvm(&self) -> LLVMIntPredicate {
        match *self {
            Self::SLT => LLVMIntPredicate::LLVMIntSLT,
            Self::SGT => LLVMIntPredicate::LLVMIntSGT,
            Self::ULT => LLVMIntPredicate::LLVMIntULT,
            Self::UGT => LLVMIntPredicate::LLVMIntUGT,
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

    pub fn module(&self, name: &str) -> Module {
        Module::with_name(self, name)
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
    fn with_name(context: &'ctx Context, name: &str) -> Module<'ctx> {
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

    pub fn add_function(&'ctx self, fn_type: FunctionType<'ctx>, name: &str) -> Function<'ctx> {
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
            LLVMSetTarget(self.module_ref, triple);
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

            from_cstring(LLVMPrintModuleToString(self.module_ref)).ok_or("UTF-8 error".to_owned())
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

#[derive(Clone)]
pub struct Function<'ctx> {
    module: &'ctx Module<'ctx>,
    name: CString,
    fn_type: FunctionType<'ctx>,
    fn_ref: LLVMValueRef,
}

impl<'ctx> Function<'ctx> {
    pub fn block<T: Into<String>>(&'ctx self, name: T) -> BasicBlock<'ctx> {
        BasicBlock::in_function(&self, name.into())
    }

    pub fn get_param<T: BasicValue<'ctx>>(
        &'ctx self,
        nth: usize,
        param_type: T::BaseType,
    ) -> Result<T, String> {
        if let Some(actual_type) = self.fn_type.param_types.iter().nth(nth) {
            if param_type.llvm_type() != *actual_type {
                return Err(String::from("Wrong type"));
            }
        } else {
            return Err(String::from("nth too large"));
        }
        unsafe { Ok(T::from_llvm(LLVMGetParam(self.fn_ref, nth as u32))) }
    }
}

pub struct BasicBlock<'ctx> {
    function: &'ctx Function<'ctx>,
    builder_ref: LLVMBuilderRef,
    name: String,
    blockref: LLVMBasicBlockRef,
    inserted: bool,
}

impl<'ctx> BasicBlock<'ctx> {
    fn in_function(function: &'ctx Function<'ctx>, name: String) -> BasicBlock<'ctx> {
        unsafe {
            let block_name = into_cstring(name.clone());
            let block_ref = LLVMCreateBasicBlockInContext(
                function.module.context.context_ref,
                block_name.as_ptr(),
            );
            LLVMAppendExistingBasicBlock(function.fn_ref, block_ref);
            BasicBlock {
                function: function,
                builder_ref: function.module.context.builder_ref,
                name,
                blockref: block_ref,
                inserted: false,
            }
        }
    }

    #[must_use]
    pub fn integer_compare<T: BasicValue<'ctx>>(
        &self,
        lhs: &T,
        rhs: &T,
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
    pub fn call<T: BasicValue<'ctx>>(
        &self,
        callee: &Function<'ctx>,
        params: Vec<Value<'ctx>>,
        name: &str,
    ) -> Result<T, ()> {
        if params.len() != callee.fn_type.param_types.len() {
            return Err(()); // TODO invalid amount of parameters
        }
        for (t1, t2) in callee.fn_type.param_types.iter().zip(&params) {
            if t1 != &t2.llvm_type() {
                return Err(()); // TODO wrong types in parameters
            }
        }
        if !T::BaseType::is_type(callee.fn_type.return_type) {
            return Err(()); // TODO wrong return type
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
            Ok(T::from_llvm(ret_val))
        }
    }

    #[must_use]
    pub fn add<T: BasicValue<'ctx>>(&self, lhs: &T, rhs: &T, name: &str) -> Result<T, ()> {
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
    pub fn sub<T: BasicValue<'ctx>>(&self, lhs: &T, rhs: &T, name: &str) -> Result<T, ()> {
        dbg!(lhs, rhs);
        dbg!(lhs.llvm_type(), rhs.llvm_type());
        if lhs.llvm_type() != rhs.llvm_type() {
            return Err(()); // TODO error
        }
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            let add_value_ref = LLVMBuildSub(
                self.builder_ref,
                lhs.llvm_value(),
                rhs.llvm_value(),
                into_cstring(name).as_ptr(),
            );
            Ok(T::from_llvm(add_value_ref))
        }
    }

    #[must_use]
    pub fn phi<PhiValue: BasicValue<'ctx>>(
        &self,
        phi_type: &PhiValue::BaseType,
        name: &str,
    ) -> Result<PhiBuilder<'ctx, PhiValue>, ()> {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            let phi_node = LLVMBuildPhi(
                self.builder_ref,
                phi_type.llvm_type(),
                into_cstring(name).as_ptr(),
            );
            Ok(PhiBuilder::new(phi_node))
        }
    }

    #[must_use]
    pub fn br(&mut self, into: &BasicBlock<'ctx>) -> Result<(), ()> {
        self.try_insert()?;
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            LLVMBuildBr(self.builder_ref, into.blockref);
            Ok(())
        }
    }

    #[must_use]
    pub fn conditional_br<T: BasicValue<'ctx>>(
        &mut self,
        condition: &T,
        lhs: &BasicBlock<'ctx>,
        rhs: &BasicBlock<'ctx>,
    ) -> Result<(), ()> {
        self.try_insert()?;
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            LLVMBuildCondBr(
                self.builder_ref,
                condition.llvm_value(),
                lhs.blockref,
                rhs.blockref,
            );
            Ok(())
        }
    }

    #[must_use]
    pub fn ret<T: BasicValue<'ctx>>(&mut self, return_value: &T) -> Result<(), ()> {
        if self.function.fn_type.return_type != return_value.llvm_type() {
            return Err(());
        }
        self.try_insert()?;

        unsafe {
            LLVMPositionBuilderAtEnd(self.builder_ref, self.blockref);
            LLVMBuildRet(self.builder_ref, return_value.llvm_value());
            Ok(())
        }
    }

    fn try_insert(&mut self) -> Result<(), ()> {
        if self.inserted {
            return Err(());
        }
        self.inserted = true;
        Ok(())
    }
}

impl<'ctx> Drop for BasicBlock<'ctx> {
    fn drop(&mut self) {
        if !self.inserted {
            unsafe {
                LLVMDeleteBasicBlock(self.blockref);
            }
        }
    }
}

pub struct PhiBuilder<'ctx, PhiValue: BasicValue<'ctx>> {
    phi_node: LLVMValueRef,
    phantom: PhantomData<&'ctx PhiValue>,
}

impl<'ctx, PhiValue: BasicValue<'ctx>> PhiBuilder<'ctx, PhiValue> {
    fn new(phi_node: LLVMValueRef) -> PhiBuilder<'ctx, PhiValue> {
        PhiBuilder {
            phi_node,
            phantom: PhantomData,
        }
    }

    pub fn add_incoming(&self, value: &PhiValue, block: &BasicBlock<'ctx>) -> &Self {
        let mut values = vec![value.llvm_value()];
        let mut blocks = vec![block.blockref];
        unsafe {
            LLVMAddIncoming(
                self.phi_node,
                values.as_mut_ptr(),
                blocks.as_mut_ptr(),
                values.len() as u32,
            );
            self
        }
    }

    pub fn build(&self) -> PhiValue {
        unsafe { PhiValue::from_llvm(self.phi_node) }
    }
}
