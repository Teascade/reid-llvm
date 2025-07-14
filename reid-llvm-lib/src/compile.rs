//! This module contains all of the relevant code that is needed to compile the
//! LLIR ([`Context`]) into LLVM IR. This module is the only one that interfaces
//! with the LLVM API.

use std::{collections::HashMap, ffi::CString, ptr::null_mut};

use llvm_sys::{
    LLVMIntPredicate,
    analysis::LLVMVerifyModule,
    core::*,
    prelude::*,
    target::{
        LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
        LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets, LLVMSetModuleDataLayout,
    },
    target_machine::{
        LLVMCodeGenFileType, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
        LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMTargetMachineEmitToFile,
    },
};

use crate::util::{ErrorMessageHolder, from_cstring, into_cstring};

use super::{
    CmpPredicate, ConstValue, Context, TerminatorKind, Type,
    builder::{
        BlockHolder, BlockValue, Builder, FunctionHolder, FunctionValue, InstructionHolder,
        InstructionValue, ModuleHolder,
    },
};

pub struct LLVMContext {
    context_ref: LLVMContextRef,
    builder_ref: LLVMBuilderRef,
}

impl Context {
    pub fn compile(&self) {
        unsafe {
            let context_ref = LLVMContextCreate();

            let context = LLVMContext {
                context_ref,
                builder_ref: LLVMCreateBuilderInContext(context_ref),
            };

            for holder in self.builder.get_modules().borrow().iter() {
                holder.compile(&context, &self.builder);
            }

            LLVMDisposeBuilder(context.builder_ref);
            LLVMContextDispose(context.context_ref);
        }
    }
}

pub struct LLVMModule<'a> {
    builder: &'a Builder,
    context_ref: LLVMContextRef,
    builder_ref: LLVMBuilderRef,
    #[allow(dead_code)]
    module_ref: LLVMModuleRef,
    functions: HashMap<FunctionValue, LLVMFunction>,
    blocks: HashMap<BlockValue, LLVMBasicBlockRef>,
    values: HashMap<InstructionValue, LLVMValue>,
}

#[derive(Clone, Copy)]
pub struct LLVMFunction {
    type_ref: LLVMTypeRef,
    value_ref: LLVMValueRef,
}

pub struct LLVMValue {
    _ty: Type,
    value_ref: LLVMValueRef,
}

impl ModuleHolder {
    fn compile(&self, context: &LLVMContext, builder: &Builder) {
        unsafe {
            let module_ref = LLVMModuleCreateWithNameInContext(
                into_cstring(&self.data.name).as_ptr(),
                context.context_ref,
            );

            // Compile the contents

            let mut functions = HashMap::new();

            for function in &self.functions {
                functions.insert(
                    function.value,
                    function.compile_signature(context, module_ref),
                );
            }

            let mut module = LLVMModule {
                builder,
                context_ref: context.context_ref,
                builder_ref: context.builder_ref,
                module_ref,
                functions,
                blocks: HashMap::new(),
                values: HashMap::new(),
            };

            for function in &self.functions {
                function.compile(&mut module);
            }

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
            LLVMSetTarget(module_ref, triple);
            LLVMSetModuleDataLayout(module_ref, data_layout);

            let mut err = ErrorMessageHolder::null();
            LLVMVerifyModule(
                module_ref,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToFile(
                target_machine,
                module_ref,
                CString::new("hello.asm").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMAssemblyFile,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToFile(
                target_machine,
                module_ref,
                CString::new("hello.o").unwrap().into_raw(),
                LLVMCodeGenFileType::LLVMObjectFile,
                err.borrow_mut(),
            );
            err.into_result().unwrap();

            let module_str = from_cstring(LLVMPrintModuleToString(module_ref));
            println!("{}", module_str.unwrap());
        }
    }
}

impl FunctionHolder {
    unsafe fn compile_signature(
        &self,
        context: &LLVMContext,
        module_ref: LLVMModuleRef,
    ) -> LLVMFunction {
        unsafe {
            let ret_type = self.data.ret.as_llvm(context.context_ref);
            let mut param_types: Vec<LLVMTypeRef> = self
                .data
                .params
                .iter()
                .map(|t| t.as_llvm(context.context_ref))
                .collect();
            let param_ptr = param_types.as_mut_ptr();
            let param_len = param_types.len();

            let fn_type = LLVMFunctionType(ret_type, param_ptr, param_len as u32, 0);

            let function_ref =
                LLVMAddFunction(module_ref, into_cstring(&self.data.name).as_ptr(), fn_type);

            LLVMFunction {
                type_ref: fn_type,
                value_ref: function_ref,
            }
        }
    }

    unsafe fn compile(&self, module: &mut LLVMModule) {
        unsafe {
            let own_function = *module.functions.get(&self.value).unwrap();

            for block in &self.blocks {
                if block.data.deleted {
                    continue;
                }

                let block_ref = LLVMCreateBasicBlockInContext(
                    module.context_ref,
                    into_cstring(&self.data.name).as_ptr(),
                );
                LLVMAppendExistingBasicBlock(own_function.value_ref, block_ref);
                module.blocks.insert(block.value, block_ref);
            }

            for block in &self.blocks {
                block.compile(module, &own_function);
            }
        }
    }
}

impl BlockHolder {
    unsafe fn compile(&self, module: &mut LLVMModule, function: &LLVMFunction) {
        unsafe {
            if self.data.deleted {
                return;
            }

            let block_ref = *module.blocks.get(&self.value).unwrap();
            LLVMPositionBuilderAtEnd(module.builder_ref, block_ref);

            for instruction in &self.instructions {
                let key = instruction.value;
                let ret = instruction.compile(module, function, block_ref);
                module.values.insert(key, ret);
            }

            self.data
                .terminator
                .clone()
                .expect(&format!(
                    "Block {} does not have a terminator!",
                    self.data.name
                ))
                .compile(module, function, block_ref);
        }
    }
}

impl InstructionHolder {
    unsafe fn compile(
        &self,
        module: &LLVMModule,
        function: &LLVMFunction,
        _block: LLVMBasicBlockRef,
    ) -> LLVMValue {
        let _ty = self.value.get_type(module.builder).unwrap();
        let val = unsafe {
            use super::Instr::*;
            match &self.data.kind {
                Param(nth) => LLVMGetParam(function.value_ref, *nth as u32),
                Constant(val) => val.as_llvm(module.context_ref, module.builder_ref),
                Add(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildAdd(module.builder_ref, lhs_val, rhs_val, c"add".as_ptr())
                }
                Sub(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildSub(module.builder_ref, lhs_val, rhs_val, c"sub".as_ptr())
                }
                Mult(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildMul(module.builder_ref, lhs_val, rhs_val, c"mul".as_ptr())
                }
                And(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildAnd(module.builder_ref, lhs_val, rhs_val, c"and".as_ptr())
                }
                ICmp(pred, lhs, rhs) => {
                    let lhs = module.values.get(&lhs).unwrap();
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildICmp(
                        module.builder_ref,
                        // Signedness from LHS
                        pred.as_llvm_int(lhs._ty.signed()),
                        lhs.value_ref,
                        rhs_val,
                        c"icmp".as_ptr(),
                    )
                }
                FunctionCall(function_value, instruction_values) => {
                    let fun = module.functions.get(&function_value).unwrap();
                    let mut param_list: Vec<LLVMValueRef> = instruction_values
                        .iter()
                        .map(|i| module.values.get(i).unwrap().value_ref)
                        .collect();

                    LLVMBuildCall2(
                        module.builder_ref,
                        fun.type_ref,
                        fun.value_ref,
                        param_list.as_mut_ptr(),
                        param_list.len() as u32,
                        c"call".as_ptr(),
                    )
                }
                Phi(values) => {
                    let mut inc_values = Vec::new();
                    let mut inc_blocks = Vec::new();
                    for item in values {
                        inc_values.push(module.values.get(&item).unwrap().value_ref);
                        inc_blocks.push(*module.blocks.get(&item.0).unwrap());
                    }
                    let phi = LLVMBuildPhi(
                        module.builder_ref,
                        _ty.as_llvm(module.context_ref),
                        c"phi".as_ptr(),
                    );
                    LLVMAddIncoming(
                        phi,
                        inc_values.as_mut_ptr(),
                        inc_blocks.as_mut_ptr(),
                        values.len() as u32,
                    );
                    phi
                }
                Alloca(name, ty) => LLVMBuildAlloca(
                    module.builder_ref,
                    ty.as_llvm(module.context_ref),
                    into_cstring(name).as_ptr(),
                ),
                Load(ptr, ty) => LLVMBuildLoad2(
                    module.builder_ref,
                    ty.as_llvm(module.context_ref),
                    module.values.get(&ptr).unwrap().value_ref,
                    c"load".as_ptr(),
                ),
                Store(ptr, val) => LLVMBuildStore(
                    module.builder_ref,
                    module.values.get(&val).unwrap().value_ref,
                    module.values.get(&ptr).unwrap().value_ref,
                ),
                ArrayAlloca(ty, len) => {
                    let array_len = ConstValue::U16(*len as u16)
                        .as_llvm(module.context_ref, module.builder_ref);
                    LLVMBuildArrayAlloca(
                        module.builder_ref,
                        ty.as_llvm(module.context_ref),
                        array_len,
                        c"array_alloca".as_ptr(),
                    )
                }
                GetElemPtr(arr, indices) => {
                    let t = arr.get_type(module.builder).unwrap();
                    let Type::Ptr(elem_t) = t else { panic!() };

                    let mut indices: Vec<_> = indices
                        .iter()
                        .map(|idx| {
                            ConstValue::U32(*idx).as_llvm(module.context_ref, module.builder_ref)
                        })
                        .collect();

                    LLVMBuildGEP2(
                        module.builder_ref,
                        elem_t.as_llvm(module.context_ref),
                        module.values.get(arr).unwrap().value_ref,
                        indices.as_mut_ptr(),
                        indices.len() as u32,
                        c"array_gep".as_ptr(),
                    )
                }
            }
        };
        LLVMValue {
            _ty,
            value_ref: val,
        }
    }
}

impl TerminatorKind {
    fn compile(
        &self,
        module: &LLVMModule,
        _function: &LLVMFunction,
        _block: LLVMBasicBlockRef,
    ) -> LLVMValue {
        let _ty = self.get_type(module.builder).unwrap();
        let val = unsafe {
            match self {
                TerminatorKind::Ret(val) => {
                    let value = module.values.get(val).unwrap();
                    LLVMBuildRet(module.builder_ref, value.value_ref)
                }
                TerminatorKind::RetVoid => LLVMBuildRetVoid(module.builder_ref),
                TerminatorKind::Br(block_value) => {
                    let dest = *module.blocks.get(block_value).unwrap();
                    LLVMBuildBr(module.builder_ref, dest)
                }
                TerminatorKind::CondBr(cond, then_b, else_b) => {
                    let cond_val = module.values.get(cond).unwrap().value_ref;
                    let then_bb = *module.blocks.get(then_b).unwrap();
                    let else_bb = *module.blocks.get(else_b).unwrap();
                    LLVMBuildCondBr(module.builder_ref, cond_val, then_bb, else_bb)
                }
            }
        };
        LLVMValue {
            _ty,
            value_ref: val,
        }
    }
}

impl CmpPredicate {
    fn as_llvm_int(&self, signed: bool) -> LLVMIntPredicate {
        use CmpPredicate::*;
        use LLVMIntPredicate::*;
        match (self, signed) {
            (LT, true) => LLVMIntSLT,
            (LE, true) => LLVMIntSLE,
            (GT, true) => LLVMIntSGT,
            (GE, true) => LLVMIntSGE,
            (LT, false) => LLVMIntULT,
            (LE, false) => LLVMIntULE,
            (GT, false) => LLVMIntUGT,
            (GE, false) => LLVMIntUGE,
            (EQ, _) => LLVMIntEQ,
            (NE, _) => LLVMIntNE,
        }
    }
}

impl ConstValue {
    fn as_llvm(&self, context: LLVMContextRef, builder: LLVMBuilderRef) -> LLVMValueRef {
        unsafe {
            let t = self.get_type().as_llvm(context);
            match self {
                ConstValue::Bool(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::I8(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::I16(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::I32(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::I64(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::I128(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::U8(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::U16(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::U32(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::U64(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::U128(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValue::String(val) => {
                    LLVMBuildGlobalString(builder, into_cstring(val).as_ptr(), c"string".as_ptr())
                }
            }
        }
    }
}

impl Type {
    fn as_llvm(&self, context: LLVMContextRef) -> LLVMTypeRef {
        use Type::*;
        unsafe {
            match self {
                I8 | U8 => LLVMInt8TypeInContext(context),
                I16 | U16 => LLVMInt16TypeInContext(context),
                I32 | U32 => LLVMInt32TypeInContext(context),
                I64 | U64 => LLVMInt64TypeInContext(context),
                I128 | U128 => LLVMInt128TypeInContext(context),
                Bool => LLVMInt1TypeInContext(context),
                Void => LLVMVoidType(),
                Ptr(ty) => LLVMPointerType(ty.as_llvm(context), 0),
                String(length) => LLVMArrayType(LLVMInt8TypeInContext(context), *length),
            }
        }
    }
}
