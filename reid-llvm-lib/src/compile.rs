//! This module contains all of the relevant code that is needed to compile the
//! LLIR ([`Context`]) into LLVM IR. This module is the only one that interfaces
//! with the LLVM API.

use std::{
    cell::Ref,
    collections::HashMap,
    ptr::{null, null_mut},
};

use llvm_sys::{
    LLVMIntPredicate, LLVMLinkage, LLVMRealPredicate, LLVMValueKind,
    analysis::LLVMVerifyModule,
    core::*,
    debuginfo::*,
    linker::LLVMLinkModules2,
    prelude::*,
    target::{
        LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
        LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets, LLVMSetModuleDataLayout,
    },
    target_machine::{
        LLVMCodeGenFileType, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple,
        LLVMGetTargetFromTriple, LLVMTargetMachineEmitToMemoryBuffer,
    },
};

use crate::{
    CustomTypeKind,
    builder::{ConstantValue, GlobalValue, TypeHolder, TypeValue},
    debug_information::*,
    util::{ErrorMessageHolder, MemoryBufferHolder, from_cstring, into_cstring},
};

use super::{
    CmpPredicate, ConstValueKind, Context, TerminatorKind, Type,
    builder::{
        BlockHolder, BlockValue, Builder, FunctionHolder, FunctionValue, InstructionHolder, InstructionValue,
        ModuleHolder,
    },
};

pub struct LLVMContext {
    context_ref: LLVMContextRef,
    builder_ref: LLVMBuilderRef,
}

impl Drop for LLVMContext {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder_ref);
            LLVMContextDispose(self.context_ref);
        }
    }
}

pub struct CompiledModule {
    module_ref: LLVMModuleRef,
    _context: LLVMContext,
    cpu: String,
    features: String,
}

pub struct CompileOutput {
    pub triple: String,
    pub assembly: String,
    pub obj_buffer: Vec<u8>,
    pub llvm_ir: String,
}

impl CompiledModule {
    pub fn output(&self) -> CompileOutput {
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
            err.into_result().unwrap();

            let target_machine = LLVMCreateTargetMachine(
                target,
                triple,
                into_cstring(self.cpu.clone()).as_ptr(),
                into_cstring(self.features.clone()).as_ptr(),
                llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                llvm_sys::target_machine::LLVMRelocMode::LLVMRelocDefault,
                llvm_sys::target_machine::LLVMCodeModel::LLVMCodeModelDefault,
            );

            let data_layout = LLVMCreateTargetDataLayout(target_machine);
            LLVMSetTarget(self.module_ref, triple);
            LLVMSetModuleDataLayout(self.module_ref, data_layout);

            let mut asm_buffer = MemoryBufferHolder::empty("asm");
            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToMemoryBuffer(
                target_machine,
                self.module_ref,
                LLVMCodeGenFileType::LLVMAssemblyFile,
                err.borrow_mut(),
                &mut asm_buffer.buffer,
            );
            err.into_result().unwrap();

            let mut obj_buffer = MemoryBufferHolder::empty("obj");
            let mut err = ErrorMessageHolder::null();
            LLVMTargetMachineEmitToMemoryBuffer(
                target_machine,
                self.module_ref,
                LLVMCodeGenFileType::LLVMObjectFile,
                err.borrow_mut(),
                &mut obj_buffer.buffer,
            );
            err.into_result().unwrap();

            let llvm_ir =
                from_cstring(LLVMPrintModuleToString(self.module_ref)).expect("Unable to print LLVM IR to string");

            let mut err = ErrorMessageHolder::null();
            LLVMVerifyModule(
                self.module_ref,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                err.borrow_mut(),
            );

            CompileOutput {
                triple: from_cstring(triple).expect("Unable to convert triple from cstring"),
                assembly: asm_buffer
                    .as_string()
                    .expect("Error while converting assembly-buffer to string"),
                obj_buffer: obj_buffer.as_buffer(),
                llvm_ir,
            }
        }
    }
}

impl Context {
    pub fn compile(&self, cpu: Option<String>, features: Vec<String>) -> CompiledModule {
        unsafe {
            let context_ref = LLVMContextCreate();

            let context = LLVMContext {
                context_ref,
                builder_ref: LLVMCreateBuilderInContext(context_ref),
            };

            let module_holders = self.builder.get_modules();

            let main_module = module_holders
                .borrow()
                .iter()
                .find(|m| m.data.name == "main")
                .unwrap_or(module_holders.borrow().first().unwrap())
                .clone();

            let main_module_ref = main_module.compile(&context, &self.builder);

            for holder in module_holders.borrow().iter() {
                if holder.value == main_module.value {
                    continue;
                }
                let module_ref = holder.compile(&context, &self.builder);
                LLVMLinkModules2(main_module_ref, module_ref);
            }

            CompiledModule {
                module_ref: main_module_ref,
                _context: context,
                cpu: cpu.unwrap_or("generic".to_owned()),
                features: features.join(","),
            }
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
    types: HashMap<TypeValue, LLVMTypeRef>,
    constants: HashMap<ConstantValue, LLVMValue>,
    globals: HashMap<GlobalValue, LLVMValueRef>,
    debug: Option<LLVMDebugInformation<'a>>,
}

impl<'a> Drop for LLVMModule<'a> {
    fn drop(&mut self) {
        if let Some(LLVMDebugInformation { builder, .. }) = self.debug {
            unsafe {
                LLVMDisposeDIBuilder(builder);
            }
        }
    }
}

pub struct LLVMDebugInformation<'a> {
    info: &'a DebugInformation,
    builder: LLVMDIBuilderRef,
    file_ref: LLVMMetadataRef,
    scopes: &'a mut HashMap<DebugScopeValue, LLVMMetadataRef>,
    types: &'a mut HashMap<DebugTypeValue, LLVMMetadataRef>,
    metadata: &'a mut HashMap<DebugMetadataValue, LLVMMetadataRef>,
    locations: &'a mut HashMap<DebugLocationValue, LLVMMetadataRef>,
}

#[derive(Clone, Copy)]
pub struct LLVMFunction {
    type_ref: LLVMTypeRef,
    value_ref: LLVMValueRef,
    metadata: Option<LLVMMetadataRef>,
}

pub struct LLVMValue {
    ty: Type,
    value_ref: LLVMValueRef,
}

impl ModuleHolder {
    fn compile(&self, context: &LLVMContext, builder: &Builder) -> LLVMModuleRef {
        unsafe {
            let module_ref =
                LLVMModuleCreateWithNameInContext(into_cstring(&self.data.name).as_ptr(), context.context_ref);

            // Compile the contents

            let mut types = HashMap::new();
            let mut constants = HashMap::new();
            let mut globals = HashMap::new();
            let mut metadata = HashMap::new();
            let mut scopes = HashMap::new();
            let mut locations = HashMap::new();

            let mut debug = if let Some(debug) = &self.debug_information {
                let di_builder = LLVMCreateDIBuilder(module_ref);

                let file_ref = LLVMDIBuilderCreateFile(
                    di_builder,
                    into_cstring(debug.file.name.clone()).as_ptr(),
                    debug.file.name.len(),
                    into_cstring(debug.file.directory.clone()).as_ptr(),
                    debug.file.directory.len(),
                );

                let flags = String::new();

                let compile_unit = LLVMDIBuilderCreateCompileUnit(
                    di_builder,
                    LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
                    file_ref,
                    into_cstring(builder.producer.clone()).as_ptr(),
                    builder.producer.len(),
                    // is optimized
                    0,
                    into_cstring(flags.clone()).as_ptr(),
                    flags.len(),
                    // Runtime version
                    0,
                    // Split filename
                    into_cstring(debug.file.name.clone()).as_ptr(),
                    debug.file.name.len(),
                    LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
                    // The DWOId if this is a split skeleton compile unit.
                    0,
                    // Whether to emit inline debug info.
                    true as i32,
                    // Whether to emit extra debug info for
                    // profile collection.
                    false as i32,
                    // The clang system root (value of -isysroot).
                    c"".as_ptr(),
                    0,
                    // The SDK name. On Darwin, this is the last component of
                    // the sysroot.
                    c"".as_ptr(),
                    0,
                );

                // let scope = debug.get_scopes();
                // scopes.insert(scope.borrow().value.clone(), compile_unit);
                // for scope in &scope.borrow().inner_scopes {
                //     dbg!("hello");
                //     scope.compile_scope(compile_unit, file_ref, &mut scopes, di_builder);
                // }
                // dbg!("after!");

                scopes.insert(DebugScopeValue(Vec::new()), compile_unit);

                let debug = LLVMDebugInformation {
                    builder: di_builder,
                    info: debug,
                    file_ref,
                    types: &mut types,
                    metadata: &mut metadata,
                    scopes: &mut scopes,
                    locations: &mut locations,
                };

                for ty in debug.info.get_types().borrow().iter() {
                    let meta_ref = ty.compile_debug(&debug);
                    debug.types.insert(ty.value.clone(), meta_ref);
                }
                Some(debug)
            } else {
                None
            };

            let mut types = HashMap::new();
            for ty in &self.types {
                types.insert(ty.value, ty.compile_type(context, &types));
            }

            for constant in &self.constants {
                constants.insert(
                    constant.value,
                    LLVMValue {
                        ty: constant.kind.get_type(),
                        value_ref: constant
                            .kind
                            .as_llvm(context.context_ref, context.builder_ref, &constants, &types),
                    },
                );
            }

            for global in &self.globals {
                let initializer = constants.get(&global.initializer).expect("No initializer?");
                let global_value = LLVMAddGlobal(
                    module_ref,
                    initializer.ty.as_llvm(context.context_ref, &types),
                    into_cstring(global.name.clone()).as_ptr(),
                );
                LLVMSetInitializer(global_value, initializer.value_ref);
                globals.insert(global.value, global_value);
            }

            let mut functions = HashMap::new();
            for function in &self.functions {
                let func = function.compile_signature(context, module_ref, &types, &mut debug);
                functions.insert(function.value, func);

                if let (Some(debug), Some(debug_info)) = (&mut debug, &function.debug_info) {
                    let scope_refcell = debug.info.get_scope();
                    let mut scope = scope_refcell.borrow();
                    for i in &debug_info.0 {
                        scope = Ref::map(scope, |v| v.inner_scopes.get_unchecked(*i));
                    }
                    for scope in &scope.inner_scopes {
                        scope.compile_scope(func.metadata.unwrap(), debug.file_ref, debug.scopes, debug.builder);
                    }
                }
            }

            if let Some(debug) = &mut debug {
                for location in debug.info.get_locations().borrow().iter() {
                    let location_ref = location.compile(context, &debug);
                    debug.locations.insert(location.value.clone(), location_ref);
                }

                for meta in debug.info.get_metadatas().borrow().iter() {
                    let meta_ref = meta.compile(&debug);
                    debug.metadata.insert(meta.value.clone(), meta_ref);
                }
            }

            let mut module = LLVMModule {
                builder,
                context_ref: context.context_ref,
                builder_ref: context.builder_ref,
                module_ref,
                functions,
                types,
                blocks: HashMap::new(),
                values: HashMap::new(),
                constants,
                globals,
                debug,
            };

            for function in &self.functions {
                function.compile(&mut module, self.data.is_main);
            }

            if let Some(debug) = &module.debug {
                LLVMDIBuilderFinalize(debug.builder);
            }

            module_ref
        }
    }
}

impl DebugLocationHolder {
    unsafe fn compile(&self, context: &LLVMContext, debug: &LLVMDebugInformation) -> LLVMMetadataRef {
        unsafe {
            LLVMDIBuilderCreateDebugLocation(
                context.context_ref,
                self.location.pos.line,
                self.location.pos.column,
                *debug.scopes.get(&self.scope).unwrap(),
                null_mut(),
            )
        }
    }
}

impl DebugScopeHolder {
    unsafe fn compile_scope(
        &self,
        parent: LLVMMetadataRef,
        file: LLVMMetadataRef,
        map: &mut HashMap<DebugScopeValue, LLVMMetadataRef>,
        di_builder: LLVMDIBuilderRef,
    ) {
        unsafe {
            let scope = match &self.data.kind {
                DebugScopeKind::CodegenContext => panic!(),
                DebugScopeKind::LexicalScope(data) => LLVMDIBuilderCreateLexicalBlock(
                    di_builder,
                    parent,
                    file,
                    data.location.pos.line,
                    data.location.pos.column,
                ),
                DebugScopeKind::Subprogram(_) => panic!(),
            };

            for inner in &self.inner_scopes {
                inner.compile_scope(scope, file, map, di_builder);
            }

            map.insert(self.value.clone(), scope);
        }
    }
}

impl DebugMetadataHolder {
    unsafe fn compile(&self, debug: &LLVMDebugInformation) -> LLVMMetadataRef {
        unsafe {
            match &self.data {
                DebugMetadata::ParamVar(param) => LLVMDIBuilderCreateParameterVariable(
                    debug.builder,
                    *debug.scopes.get(&self.location.scope).unwrap(),
                    into_cstring(param.name.clone()).as_ptr(),
                    param.name.len(),
                    param.arg_idx + 1,
                    debug.file_ref,
                    self.location.pos.line,
                    *debug.types.get(&param.ty).unwrap(),
                    param.always_preserve as i32,
                    param.flags.as_llvm(),
                ),
                DebugMetadata::LocalVar(var) => LLVMDIBuilderCreateAutoVariable(
                    debug.builder,
                    *debug.scopes.get(&self.location.scope).unwrap(),
                    into_cstring(var.name.clone()).as_ptr(),
                    var.name.len(),
                    debug.file_ref,
                    self.location.pos.line,
                    *debug.types.get(&var.ty).unwrap(),
                    var.always_preserve as i32,
                    var.flags.as_llvm(),
                    0,
                ),
                DebugMetadata::VarAssignment => todo!(),
            }
        }
    }
}

impl DebugTypeHolder {
    unsafe fn compile_debug(&self, debug: &LLVMDebugInformation) -> LLVMMetadataRef {
        unsafe {
            match &self.data {
                DebugTypeData::Basic(ty) => LLVMDIBuilderCreateBasicType(
                    debug.builder,
                    into_cstring(ty.name.clone()).as_ptr(),
                    ty.name.len(),
                    ty.size_bits as u64,
                    ty.encoding as u32,
                    ty.flags.as_llvm(),
                ),
                DebugTypeData::Subprogram(subprogram) => {
                    let mut params = subprogram
                        .parameters
                        .iter()
                        .map(|p| *debug.types.get(p).unwrap())
                        .collect::<Vec<_>>();
                    LLVMDIBuilderCreateSubroutineType(
                        debug.builder,
                        debug.file_ref,
                        params.as_mut_ptr(),
                        params.len() as u32,
                        subprogram.flags.as_llvm(),
                    )
                }
                DebugTypeData::Pointer(ptr) => LLVMDIBuilderCreatePointerType(
                    debug.builder,
                    *debug.types.get(&ptr.pointee).unwrap(),
                    ptr.size_bits,
                    0,
                    0,
                    into_cstring(ptr.name.clone()).as_ptr(),
                    ptr.name.len(),
                ),
                DebugTypeData::Array(array) => {
                    let subrange = LLVMDIBuilderGetOrCreateSubrange(debug.builder, 0, array.length as i64);
                    let mut elements = vec![subrange];
                    LLVMDIBuilderCreateArrayType(
                        debug.builder,
                        array.size_bits,
                        0,
                        *debug.types.get(&array.element_type).unwrap(),
                        elements.as_mut_ptr(),
                        elements.len() as u32,
                    )
                }
                DebugTypeData::Struct(st) => {
                    let mut elements = st
                        .fields
                        .iter()
                        .map(|field| {
                            LLVMDIBuilderCreateMemberType(
                                debug.builder,
                                *debug.scopes.get(&st.scope).unwrap(),
                                into_cstring(field.name.clone()).as_ptr(),
                                field.name.len(),
                                debug.file_ref,
                                field.pos.map(|p| p.line).unwrap_or(1),
                                field.size_bits,
                                0,
                                1,
                                field.flags.as_llvm(),
                                *debug.types.get(&field.ty).unwrap(),
                            )
                        })
                        .collect::<Vec<_>>();
                    LLVMDIBuilderCreateStructType(
                        debug.builder,
                        *debug.scopes.get(&st.scope).unwrap(),
                        into_cstring(st.name.clone()).as_ptr(),
                        st.name.len(),
                        debug.file_ref,
                        st.pos.map(|p| p.line).unwrap_or(1),
                        st.size_bits,
                        0,
                        st.flags.as_llvm(),
                        null_mut(), // derived from
                        elements.as_mut_ptr(),
                        elements.len() as u32,
                        0,          // Runtime lang
                        null_mut(), // VTable
                        null(),     // Unique ID
                        0,          // Unique ID len
                    )
                }
            }
        }
    }
}

impl DwarfFlags {
    pub fn as_llvm(&self) -> i32 {
        0
    }
}

impl TypeHolder {
    unsafe fn compile_type(&self, context: &LLVMContext, types: &HashMap<TypeValue, LLVMTypeRef>) -> LLVMTypeRef {
        unsafe {
            match &self.data.kind {
                CustomTypeKind::NamedStruct(named_struct) => {
                    let mut elem_types = Vec::new();
                    for ty in &named_struct.1 {
                        elem_types.push(ty.as_llvm(context.context_ref, types));
                    }
                    let struct_ty =
                        LLVMStructCreateNamed(context.context_ref, into_cstring(named_struct.0.clone()).as_ptr());
                    LLVMStructSetBody(struct_ty, elem_types.as_mut_ptr(), elem_types.len() as u32, 0);
                    struct_ty
                }
            }
        }
    }
}

impl FunctionHolder {
    unsafe fn compile_signature(
        &self,
        context: &LLVMContext,
        module_ref: LLVMModuleRef,
        types: &HashMap<TypeValue, LLVMTypeRef>,
        debug: &mut Option<LLVMDebugInformation>,
    ) -> LLVMFunction {
        unsafe {
            let ret_type = self.data.ret.as_llvm(context.context_ref, types);
            let mut param_types: Vec<LLVMTypeRef> = self
                .data
                .params
                .iter()
                .map(|t| t.as_llvm(context.context_ref, types))
                .collect();
            let param_ptr = param_types.as_mut_ptr();
            let param_len = param_types.len();

            let name = if self.data.flags.is_main {
                c"main"
            } else {
                &into_cstring(&self.data.linkage_name.clone().unwrap_or(self.data.name.clone()))
            };

            let fn_type = LLVMFunctionType(ret_type, param_ptr, param_len as u32, 0);

            let function_ref = LLVMAddFunction(module_ref, name.as_ptr(), fn_type);

            if self.data.flags.inline {
                let attribute = LLVMCreateEnumAttribute(context.context_ref, LLVMEnumAttribute::AlwaysInline as u32, 0);
                LLVMAddAttributeAtIndex(function_ref, 0, attribute);
            }

            let metadata = if let Some(debug) = debug {
                if let Some(scope_value) = &self.debug_info {
                    let scope_data = debug.info.get_scope_data(scope_value).unwrap();

                    let mangled_length_ptr = &mut 0;
                    let mangled_name = LLVMGetValueName2(function_ref, mangled_length_ptr);
                    let mangled_length = *mangled_length_ptr;

                    let subprogram = match scope_data.kind {
                        DebugScopeKind::CodegenContext => panic!(),
                        DebugScopeKind::LexicalScope(_) => panic!(),
                        DebugScopeKind::Subprogram(subprogram) => LLVMDIBuilderCreateFunction(
                            debug.builder,
                            *debug.scopes.get(&scope_data.parent.unwrap()).unwrap(),
                            into_cstring(subprogram.name.clone()).as_ptr(),
                            subprogram.name.clone().len(),
                            mangled_name,
                            mangled_length,
                            debug.file_ref,
                            subprogram.location.pos.line,
                            *debug.types.get(&subprogram.ty).unwrap(),
                            subprogram.opts.is_local as i32,
                            subprogram.opts.is_definition as i32,
                            subprogram.opts.scope_line,
                            subprogram.opts.flags.as_llvm(),
                            subprogram.opts.is_optimized as i32,
                        ),
                    };

                    LLVMSetSubprogram(function_ref, subprogram);
                    debug.scopes.insert(scope_value.clone(), subprogram);
                    Some(subprogram)
                } else {
                    None
                }
            } else {
                None
            };

            LLVMFunction {
                type_ref: fn_type,
                value_ref: function_ref,
                metadata,
            }
        }
    }

    unsafe fn compile(&self, module: &mut LLVMModule, in_main_module: bool) {
        unsafe {
            let own_function = *module.functions.get(&self.value).unwrap();

            if self.data.flags.is_extern {
                LLVMSetLinkage(own_function.value_ref, LLVMLinkage::LLVMExternalLinkage);
                // TODO Use "available internally"  if the other kind of extern
                return;
            }

            if self.data.flags.is_imported {
                if self.data.flags.is_extern {
                    LLVMSetLinkage(own_function.value_ref, LLVMLinkage::LLVMAvailableExternallyLinkage);
                } else {
                    LLVMSetLinkage(own_function.value_ref, LLVMLinkage::LLVMExternalLinkage);
                }
            } else {
                LLVMSetLinkage(own_function.value_ref, LLVMLinkage::LLVMPrivateLinkage);
            }

            if in_main_module && self.data.flags.is_main || self.data.flags.is_pub {
                LLVMSetLinkage(own_function.value_ref, LLVMLinkage::LLVMExternalLinkage);
            }

            for block in &self.blocks {
                if block.data.deleted {
                    continue;
                }

                let block_ref =
                    LLVMCreateBasicBlockInContext(module.context_ref, into_cstring(&block.data.name).as_ptr());
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

            let term_instr = self
                .data
                .terminator
                .clone()
                .expect(&format!("Block {} does not have a terminator!", self.data.name))
                .compile(module, function, block_ref);

            if let Some(location) = &self.data.terminator_location {
                LLVMInstructionSetDebugLoc(
                    term_instr.value_ref,
                    *module.debug.as_ref().unwrap().locations.get(&location).unwrap(),
                );
            }
        }
    }
}

impl InstructionHolder {
    unsafe fn compile(&self, module: &LLVMModule, function: &LLVMFunction, _block: LLVMBasicBlockRef) -> LLVMValue {
        let _ty = self.value.get_type(module.builder).unwrap();
        let name = into_cstring(self.name.clone());
        let val = unsafe {
            use super::Instr::*;
            match &self.data.kind {
                Param(nth) => LLVMGetParam(function.value_ref, *nth as u32),
                Constant(val) => val.as_llvm(module.context_ref, module.builder_ref, &module.constants, &module.types),
                Add(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildAdd(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                FAdd(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildFAdd(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                Sub(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildSub(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                FSub(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildFSub(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                Mul(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildMul(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                FMul(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildFMul(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                UDiv(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildUDiv(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                SDiv(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildSDiv(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                FDiv(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildFDiv(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                URem(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildURem(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                SRem(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildSRem(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                FRem(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildFRem(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                And(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildAnd(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                ICmp(pred, lhs, rhs) => {
                    let lhs = module.values.get(&lhs).unwrap();
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildICmp(
                        module.builder_ref,
                        // Signedness from LHS
                        pred.as_llvm_int(lhs.ty.category().signed()),
                        lhs.value_ref,
                        rhs_val,
                        name.as_ptr(),
                    )
                }
                FCmp(pred, lhs, rhs) => {
                    let lhs = module.values.get(&lhs).unwrap();
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildFCmp(
                        module.builder_ref,
                        pred.as_llvm_unsorted_float(),
                        lhs.value_ref,
                        rhs_val,
                        name.as_ptr(),
                    )
                }
                FunctionCall(function_value, instruction_values) => {
                    let fun = module.functions.get(&function_value).unwrap();
                    let mut param_list: Vec<LLVMValueRef> = instruction_values
                        .iter()
                        .map(|i| module.values.get(i).unwrap().value_ref)
                        .collect();

                    let is_void = module.builder.function_data(&*function_value).ret == Type::Void;
                    if is_void {
                        LLVMContextSetDiscardValueNames(module.context_ref, 1);
                    }
                    let value = LLVMBuildCall2(
                        module.builder_ref,
                        fun.type_ref,
                        fun.value_ref,
                        param_list.as_mut_ptr(),
                        param_list.len() as u32,
                        name.as_ptr(),
                    );
                    if is_void {
                        LLVMContextSetDiscardValueNames(module.context_ref, 0);
                    }
                    value
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
                        _ty.as_llvm(module.context_ref, &module.types),
                        name.as_ptr(),
                    );
                    LLVMAddIncoming(
                        phi,
                        inc_values.as_mut_ptr(),
                        inc_blocks.as_mut_ptr(),
                        values.len() as u32,
                    );
                    phi
                }
                Alloca(ty) => LLVMBuildAlloca(
                    module.builder_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                Load(ptr, ty) => LLVMBuildLoad2(
                    module.builder_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    module.values.get(&ptr).unwrap().value_ref,
                    name.as_ptr(),
                ),
                Store(ptr, val) => {
                    let store = LLVMBuildStore(
                        module.builder_ref,
                        module.values.get(&val).unwrap().value_ref,
                        module.values.get(&ptr).unwrap().value_ref,
                    );
                    store
                }
                ArrayAlloca(ty, len) => LLVMBuildArrayAlloca(
                    module.builder_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    module.values.get(&len).unwrap().value_ref,
                    name.as_ptr(),
                ),
                GetElemPtr(arr, indices) => {
                    let t = arr.get_type(module.builder).unwrap();
                    let Type::Ptr(elem_t) = t else { panic!() };

                    let mut llvm_indices: Vec<_> = indices
                        .iter()
                        .map(|idx_elem| module.values.get(idx_elem).unwrap().value_ref)
                        .collect();

                    LLVMBuildInBoundsGEP2(
                        module.builder_ref,
                        elem_t.as_llvm(module.context_ref, &module.types),
                        module.values.get(arr).unwrap().value_ref,
                        llvm_indices.as_mut_ptr(),
                        llvm_indices.len() as u32,
                        name.as_ptr(),
                    )
                }
                GetStructElemPtr(struct_val, idx) => {
                    let t = struct_val.get_type(module.builder).unwrap();
                    let Type::Ptr(struct_t) = t else { panic!() };

                    LLVMBuildStructGEP2(
                        module.builder_ref,
                        struct_t.as_llvm(module.context_ref, &module.types),
                        module.values.get(struct_val).unwrap().value_ref,
                        *idx,
                        name.as_ptr(),
                    )
                }
                ExtractValue(agg_val, idx) => LLVMBuildExtractValue(
                    module.builder_ref,
                    module.values.get(agg_val).unwrap().value_ref,
                    *idx,
                    name.as_ptr(),
                ),
                Trunc(instr_val, ty) => LLVMBuildTrunc(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                ZExt(instr_val, ty) => LLVMBuildZExt(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                SExt(instr_val, ty) => LLVMBuildSExt(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                FPTrunc(instr_val, ty) => LLVMBuildFPTrunc(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                FPExt(instr_val, ty) => LLVMBuildFPExt(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                FPToUI(instr_val, ty) => LLVMBuildFPToUI(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                FPToSI(instr_val, ty) => LLVMBuildFPToSI(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                UIToFP(instr_val, ty) => LLVMBuildUIToFP(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                SIToFP(instr_val, ty) => LLVMBuildSIToFP(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                PtrToInt(instr_val, ty) => LLVMBuildPtrToInt(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                IntToPtr(instr_val, ty) => LLVMBuildIntToPtr(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                BitCast(instr_val, ty) => LLVMBuildBitCast(
                    module.builder_ref,
                    module.values.get(instr_val).unwrap().value_ref,
                    ty.as_llvm(module.context_ref, &module.types),
                    name.as_ptr(),
                ),
                Or(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildOr(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                XOr(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildXor(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                ShiftRightLogical(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildLShr(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                ShiftRightArithmetic(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildAShr(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                ShiftLeft(lhs, rhs) => {
                    let lhs_val = module.values.get(&lhs).unwrap().value_ref;
                    let rhs_val = module.values.get(&rhs).unwrap().value_ref;
                    LLVMBuildShl(module.builder_ref, lhs_val, rhs_val, name.as_ptr())
                }
                GetGlobal(global_value) => module.globals.get(global_value).unwrap().clone(),
            }
        };
        if let Some(record) = &self.record {
            let debug = module.debug.as_ref().unwrap();

            unsafe {
                let mut addr = Vec::<u64>::new();
                let expr = LLVMDIBuilderCreateExpression(debug.builder, addr.as_mut_ptr(), addr.len());

                let location = LLVMDIBuilderCreateDebugLocation(
                    module.context_ref,
                    record.location.pos.line,
                    record.location.pos.column,
                    *debug.scopes.get(&record.scope).unwrap(),
                    null_mut(),
                );

                match record.kind {
                    DebugRecordKind::Declare(instruction_value) => LLVMDIBuilderInsertDeclareRecordBefore(
                        debug.builder,
                        module.values.get(&instruction_value).unwrap().value_ref,
                        *debug.metadata.get(&record.variable).unwrap(),
                        expr,
                        location,
                        val,
                    ),
                    DebugRecordKind::Value(instruction_value) => LLVMDIBuilderInsertDbgValueRecordBefore(
                        debug.builder,
                        module.values.get(&instruction_value).unwrap().value_ref,
                        *debug.metadata.get(&record.variable).unwrap(),
                        expr,
                        location,
                        val,
                    ),
                };
            }
        }
        if let Some(location) = &self.data.location {
            unsafe {
                match LLVMGetValueKind(val) {
                    LLVMValueKind::LLVMInstructionValueKind
                    | LLVMValueKind::LLVMMemoryDefValueKind
                    | LLVMValueKind::LLVMMemoryUseValueKind
                    | LLVMValueKind::LLVMMemoryPhiValueKind => {
                        LLVMInstructionSetDebugLoc(
                            val,
                            *module.debug.as_ref().unwrap().locations.get(&location).unwrap(),
                        );
                    }
                    _ => {}
                }
            }
        }
        LLVMValue {
            ty: _ty,
            value_ref: val,
        }
    }
}

impl TerminatorKind {
    fn compile(&self, module: &LLVMModule, _function: &LLVMFunction, _block: LLVMBasicBlockRef) -> LLVMValue {
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
            ty: _ty,
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

    fn as_llvm_unsorted_float(&self) -> LLVMRealPredicate {
        use CmpPredicate::*;
        use LLVMRealPredicate::*;
        match self {
            LT => LLVMRealULT,
            LE => LLVMRealULE,
            GT => LLVMRealUGT,
            GE => LLVMRealUGE,
            EQ => LLVMRealUEQ,
            NE => LLVMRealUNE,
        }
    }
}

impl ConstValueKind {
    fn as_llvm(
        &self,
        context: LLVMContextRef,
        builder: LLVMBuilderRef,
        constants: &HashMap<ConstantValue, LLVMValue>,
        types: &HashMap<TypeValue, LLVMTypeRef>,
    ) -> LLVMValueRef {
        unsafe {
            let t = self.get_type().as_llvm(context, &types);
            match self {
                ConstValueKind::Bool(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::I8(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::I16(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::I32(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::I64(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::I128(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::U8(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::U16(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::U32(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::U64(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::U128(val) => LLVMConstInt(t, *val as u64, 1),
                ConstValueKind::Str(val) => {
                    LLVMBuildGlobalString(builder, into_cstring(val).as_ptr(), c"string".as_ptr())
                }
                ConstValueKind::F16(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::F32B(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::F32(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::F64(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::F80(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::F128(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::F128PPC(val) => LLVMConstReal(t, *val as f64),
                ConstValueKind::Array(constant_values, elem_ty) => {
                    let mut values = constant_values
                        .iter()
                        .map(|v| constants.get(v).unwrap().value_ref)
                        .collect::<Vec<_>>();

                    LLVMConstArray2(
                        elem_ty.as_llvm(context, &types),
                        values.as_mut_ptr(),
                        values.len() as u64,
                    )
                }
            }
        }
    }
}

impl Type {
    fn as_llvm(&self, context: LLVMContextRef, typemap: &HashMap<TypeValue, LLVMTypeRef>) -> LLVMTypeRef {
        use Type::*;
        unsafe {
            match self {
                I8 | U8 => LLVMInt8TypeInContext(context),
                I16 | U16 => LLVMInt16TypeInContext(context),
                I32 | U32 => LLVMInt32TypeInContext(context),
                I64 | U64 => LLVMInt64TypeInContext(context),
                I128 | U128 => LLVMInt128TypeInContext(context),
                Bool => LLVMInt1TypeInContext(context),
                Void => LLVMVoidTypeInContext(context),
                Ptr(ty) => LLVMPointerType(ty.as_llvm(context, typemap), 0),
                CustomType(struct_ty) => *typemap.get(struct_ty).unwrap(),
                Array(r#type, len) => LLVMArrayType2(r#type.as_llvm(context, typemap), *len),
                F16 => LLVMHalfTypeInContext(context),
                F32 => LLVMFloatTypeInContext(context),
                F32B => LLVMBFloatTypeInContext(context),
                F64 => LLVMDoubleTypeInContext(context),
                F80 => LLVMX86FP80TypeInContext(context),
                F128 => LLVMFP128TypeInContext(context),
                F128PPC => LLVMPPCFP128TypeInContext(context),
            }
        }
    }
}

pub enum LLVMEnumAttribute {
    AlwaysInline = 3,
}
