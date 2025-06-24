use std::ffi::{CStr, CString, c_char};
use std::marker::PhantomData;
use std::mem;
use std::ptr::{null, null_mut};

use llvm_sys::analysis::LLVMVerifyModule;
use llvm_sys::target::{
    LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
    LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets, LLVM_InitializeNativeAsmParser,
    LLVM_InitializeNativeTarget, LLVMInitializeAMDGPUAsmPrinter, LLVMInitializeX86Target,
    LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC, LLVMSetModuleDataLayout,
};
use llvm_sys::target_machine::{
    LLVMCodeGenFileType, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
    LLVMGetDefaultTargetTriple, LLVMGetFirstTarget, LLVMGetHostCPUFeatures, LLVMGetHostCPUName,
    LLVMGetTargetFromTriple, LLVMTargetMachineEmitToFile, LLVMTargetMachineEmitToMemoryBuffer,
};
use llvm_sys::transforms::pass_manager_builder::{
    LLVMPassManagerBuilderCreate, LLVMPassManagerBuilderPopulateModulePassManager,
    LLVMPassManagerBuilderSetOptLevel,
};
use llvm_sys::{
    LLVMBasicBlock, LLVMBuilder, LLVMContext, LLVMModule, LLVMType, LLVMValue, core::*, prelude::*,
};

fn into_cstring<T: Into<String>>(value: T) -> CString {
    let string = value.into();
    unsafe { CString::from_vec_with_nul_unchecked((string + "\0").into_bytes()) }
}

fn from_cstring(value: *mut c_char) -> Option<String> {
    if value.is_null() {
        None
    } else {
        unsafe { CString::from_raw(value).into_string().ok() }
    }
}

fn cstring_to_err(value: *mut c_char) -> Result<(), String> {
    from_cstring(value)
        .filter(|s| !s.is_empty())
        .map_or(Ok(()), |s| Err(s))
}

pub trait IRType {
    const SIGNED: LLVMBool;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef;
}

impl IRType for bool {
    const SIGNED: LLVMBool = 0;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef {
        unsafe { LLVMInt1TypeInContext(context.context) }
    }
}

impl IRType for i32 {
    const SIGNED: LLVMBool = 1;
    unsafe fn llvm_type(context: &IRContext) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(context.context) }
    }
}

pub struct IRValue<T: IRType>(PhantomData<T>, IROpaqueValue);

impl<T: IRType> IRValue<T> {
    unsafe fn from_runtime(t: LLVMTypeRef, value: LLVMValueRef) -> IRValue<T> {
        IRValue(PhantomData, IROpaqueValue(t, value))
    }
}

impl<T: IRType + Into<i64>> IRValue<T> {
    pub fn from_const(context: &IRContext, value: T) -> Self {
        unsafe {
            let t = T::llvm_type(context);
            let value = LLVMConstInt(t, value.into() as u64, T::SIGNED);
            IRValue(PhantomData, IROpaqueValue(t, value))
        }
    }
}

impl<T: IRType> From<IRValue<T>> for IROpaqueValue {
    fn from(value: IRValue<T>) -> Self {
        value.1
    }
}

pub struct IROpaqueValue(LLVMTypeRef, LLVMValueRef);

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
            // let pmb = LLVMPassManagerBuilderCreate();
            // LLVMPassManagerBuilderSetOptLevel(pmb, 0);
            // let pm = LLVMCreatePassManager();
            // LLVMPassManagerBuilderPopulateModulePassManager(pmb, pm);
            // println!("{}", LLVMRunPassManager(pm, self.module));

            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            let triple = LLVMGetDefaultTargetTriple();

            let mut target: _ = null_mut();
            let mut err: _ = null_mut();
            LLVMGetTargetFromTriple(c"x86_64-unknown-linux-gnu".as_ptr(), &mut target, &mut err);
            println!("{:?}, {:?}", from_cstring(triple), target);
            cstring_to_err(err).unwrap();

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

            let mut err = null_mut();
            LLVMVerifyModule(
                self.module,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                &mut err,
            );
            cstring_to_err(err).unwrap();

            let mut err = null_mut();
            LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                CString::new("hello.asm").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMAssemblyFile,
                &mut err,
            );
            cstring_to_err(err).unwrap();

            let mut err = null_mut();
            LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                CString::new("hello.o").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut err,
            );
            cstring_to_err(err).unwrap();

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
    pub module: &'a IRModule<'a>,
    pub functionref: *mut LLVMValue,
}

impl<'a> IRFunction<'a> {
    pub fn new(module: &'a IRModule<'a>, name: &String) -> IRFunction<'a> {
        unsafe {
            // TODO, fix later!
            let return_type = LLVMInt32TypeInContext(module.context.context);
            let mut argts = [];
            let func_type =
                LLVMFunctionType(return_type, argts.as_mut_ptr(), argts.len() as u32, 0);

            let functionref =
                LLVMAddFunction(module.module, into_cstring(name).as_ptr(), func_type);

            IRFunction {
                module,
                functionref,
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

    pub fn less_than(&self, lhs: IROpaqueValue, rhs: IROpaqueValue) -> Result<IRValue<bool>, ()> {
        let IROpaqueValue(t1, lhs) = lhs;
        let IROpaqueValue(t2, rhs) = rhs;

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

    pub fn ret(self, function: &IRFunction, value: IROpaqueValue) {
        unsafe {
            let builder = self.context.builder;
            LLVMPositionBuilderAtEnd(builder, self.blockref);
            LLVMBuildRet(builder, value.1);
            self.append(function);
        }
    }

    unsafe fn append(mut self, function: &IRFunction<'a>) {
        unsafe {
            LLVMAppendExistingBasicBlock(function.functionref, self.blockref);
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
