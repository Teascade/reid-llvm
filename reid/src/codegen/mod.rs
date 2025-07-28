use std::{cell::RefCell, collections::HashMap, rc::Rc};

use allocator::{Allocator, AllocatorScope};
use intrinsics::*;
use reid_lib::{
    compile::CompiledModule,
    debug_information::{
        DebugFileData, DebugLexicalScope, DebugLocalVariable, DebugLocation, DebugMetadata, DebugRecordKind,
        DebugSubprogramData, DebugSubprogramOptionals, DebugSubprogramType, DebugTypeData, DwarfFlags,
        InstructionDebugRecordData,
    },
    CmpPredicate, ConstValue, Context, CustomTypeKind, Function, FunctionFlags, Instr, Module, NamedStruct,
    TerminatorKind as Term, Type,
};
use scope::*;

use crate::{
    mir::{
        self,
        implement::TypeCategory,
        pass::{AssociatedFunctionKey, BinopKey},
        CustomTypeKey, FunctionCall, FunctionDefinitionKind, FunctionParam, NamedVariableRef, SourceModuleId,
        StructField, StructType, TypeDefinition, TypeDefinitionKind, TypeKind, WhileStatement,
    },
    util::try_all,
};

mod allocator;
pub mod intrinsics;
pub(super) mod scope;
pub(super) mod util;

#[derive(thiserror::Error, Debug, Clone, PartialEq, PartialOrd)]
pub enum ErrorKind {
    #[error("NULL error, should never occur!")]
    Null,
}

/// Context that contains all of the given modules as complete codegenerated
/// LLIR that can then be finally compiled into LLVM IR.
#[derive(Debug)]
pub struct CodegenContext<'ctx> {
    pub(crate) context: &'ctx Context,
}

impl<'ctx> CodegenContext<'ctx> {
    /// Compile contained LLIR into LLVM IR and produce `hello.o` and
    /// `hello.asm`
    pub fn compile(&self, cpu: Option<String>, features: Vec<String>) -> CompiledModule {
        self.context.compile(cpu, features)
    }
}

impl mir::Context {
    /// Compile MIR [`Context`] into [`CodegenContext`] containing LLIR.
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> Result<CodegenContext<'ctx>, ErrorKind> {
        let mut modules = HashMap::new();
        let mut modules_sorted = self.modules.iter().map(|(_, m)| m).collect::<Vec<_>>();
        modules_sorted.sort_by(|m1, m2| m2.module_id.cmp(&m1.module_id));

        for module in &modules_sorted {
            let codegen = module.codegen(context, modules.clone())?;
            modules.insert(module.module_id, codegen);
        }
        Ok(CodegenContext { context })
    }
}

#[derive(Clone)]
struct ModuleCodegen<'ctx> {
    name: String,
    module: Module<'ctx>,
}

impl<'ctx> std::fmt::Debug for ModuleCodegen<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.module.as_printable(), f)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct State {
    should_load: bool,
}

impl State {
    /// Sets should load, returning a new state
    fn load(self, should: bool) -> State {
        State { should_load: should }
    }
}

impl Default for State {
    fn default() -> Self {
        Self { should_load: true }
    }
}

impl mir::Module {
    fn codegen<'ctx>(
        &'ctx self,
        context: &'ctx Context,
        modules: HashMap<SourceModuleId, ModuleCodegen<'ctx>>,
    ) -> Result<ModuleCodegen<'ctx>, ErrorKind> {
        let mut module = context.module(&self.name, self.is_main);
        let tokens = &self.tokens;

        let (debug, compile_unit) = if let Some(path) = &self.path {
            module.create_debug_info(DebugFileData {
                name: path.file_name().unwrap().to_str().unwrap().to_owned(),
                directory: path.parent().unwrap().to_str().unwrap().to_owned(),
            })
        } else {
            module.create_debug_info(DebugFileData {
                name: self.name.clone(),
                directory: String::new(),
            })
        };

        let mut types = HashMap::new();
        let mut type_values = HashMap::new();
        let mut debug_types = HashMap::new();

        macro_rules! insert_debug {
            ($kind:expr) => {
                debug_types.insert(
                    $kind.clone(),
                    $kind.get_debug_type_hard(
                        &compile_unit,
                        &debug,
                        &debug_types,
                        &type_values,
                        &types,
                        self.module_id,
                        &self.tokens,
                        &modules,
                    ),
                )
            };
        }

        insert_debug!(&TypeKind::Bool);
        insert_debug!(&TypeKind::U8);
        insert_debug!(&TypeKind::U16);
        insert_debug!(&TypeKind::U32);
        insert_debug!(&TypeKind::U64);
        insert_debug!(&TypeKind::U128);
        insert_debug!(&TypeKind::I8);
        insert_debug!(&TypeKind::I16);
        insert_debug!(&TypeKind::I32);
        insert_debug!(&TypeKind::I64);
        insert_debug!(&TypeKind::I128);
        insert_debug!(&TypeKind::Void);
        insert_debug!(&TypeKind::Char);

        let mut typedefs = self.typedefs.clone();
        typedefs.sort_by(|a, b| b.source_module.cmp(&a.source_module));

        for typedef in typedefs {
            let type_key = CustomTypeKey(typedef.name.clone(), typedef.source_module);
            let type_value = match &typedef.kind {
                TypeDefinitionKind::Struct(StructType(fields)) => {
                    module.custom_type(CustomTypeKind::NamedStruct(NamedStruct(
                        typedef.name.clone(),
                        fields
                            .iter()
                            // TODO: Reorder custom-type definitions such that
                            // inner types get evaluated first. Otherwise this
                            // will cause a panic!
                            .map(|StructField(_, t, _)| t.get_type(&type_values))
                            .collect(),
                    )))
                }
            };
            types.insert(type_value, typedef.clone());
            type_values.insert(type_key.clone(), type_value);
            insert_debug!(&TypeKind::CustomType(type_key.clone()));
        }

        let mut functions = HashMap::new();

        for function in &self.functions {
            let param_types: Vec<Type> = function
                .parameters
                .iter()
                .map(|FunctionParam { ty, .. }| ty.get_type(&type_values))
                .collect();

            let is_main = self.is_main && function.name == "main";

            let module_prefix = if let Some(module) = function.source {
                if module == self.module_id {
                    format!("reid.{}.", self.name)
                } else {
                    format!("reid.{}.", modules.get(&module).unwrap().name)
                }
            } else {
                format!("reid.intrinsic.")
            };
            let linkage_name = function.linkage_name.clone().unwrap_or(function.name.clone());
            let full_name = format!(
                "{}{}",
                module_prefix,
                function.linkage_name.clone().unwrap_or(function.name.clone())
            );

            let func = match &function.kind {
                mir::FunctionDefinitionKind::Local(_, _) => Some(module.function(
                    &full_name,
                    None,
                    function.return_type.get_type(&type_values),
                    param_types,
                    FunctionFlags {
                        is_pub: function.is_pub || is_main,
                        is_main,
                        is_imported: function.is_imported,
                        ..FunctionFlags::default()
                    },
                )),
                mir::FunctionDefinitionKind::Extern(imported) => Some(module.function(
                    &full_name,
                    if function.source == None {
                        Some(function.linkage_name.clone().unwrap())
                    } else {
                        if !*imported {
                            Some(linkage_name.clone())
                        } else {
                            None
                        }
                    },
                    function.return_type.get_type(&type_values),
                    param_types,
                    FunctionFlags {
                        is_extern: true,
                        is_imported: *imported,
                        ..FunctionFlags::default()
                    },
                )),
                mir::FunctionDefinitionKind::Intrinsic(_) => None,
                mir::FunctionDefinitionKind::Macro(_) => None,
            };

            if let Some(func) = func {
                functions.insert(function.name.clone(), ScopeFunctionKind::UserGenerated(func));
            }
        }

        let mut associated_functions = HashMap::new();

        for (ty, function) in &self.associated_functions {
            let param_types: Vec<Type> = function
                .parameters
                .iter()
                .map(|FunctionParam { ty, .. }| ty.get_type(&type_values))
                .collect();

            let is_main = self.is_main && function.name == "main";
            let module_name = if let Some(module) = function.source {
                if module == self.module_id {
                    format!("reid.{}", self.name)
                } else {
                    format!("reid.{}", modules.get(&module).unwrap().name)
                }
            } else {
                format!("reid.intrinsic")
            };
            let func = match &function.kind {
                mir::FunctionDefinitionKind::Local(_, _) => Some(module.function(
                    &format!("{}.{}.{}", module_name, ty, function.name),
                    None,
                    function.return_type.get_type(&type_values),
                    param_types,
                    FunctionFlags {
                        is_pub: function.is_pub || is_main,
                        is_main,
                        is_imported: function.is_imported,
                        ..FunctionFlags::default()
                    },
                )),
                mir::FunctionDefinitionKind::Extern(imported) => Some(module.function(
                    &function.linkage_name.clone().unwrap_or(function.name.clone()),
                    None,
                    function.return_type.get_type(&type_values),
                    param_types,
                    FunctionFlags {
                        is_extern: true,
                        is_imported: *imported,
                        ..FunctionFlags::default()
                    },
                )),
                mir::FunctionDefinitionKind::Intrinsic(_) => None,
                mir::FunctionDefinitionKind::Macro(_) => None,
            };

            if let Some(func) = func {
                associated_functions.insert(
                    AssociatedFunctionKey(ty.clone(), function.name.clone()),
                    ScopeFunctionKind::UserGenerated(func),
                );
            }
        }

        let mut binops = HashMap::new();
        for binop in &self.binop_defs {
            let binop_fn_name = format!(
                "binop.{}.{:?}.{}.{}",
                binop.lhs.ty, binop.op, binop.rhs.ty, binop.return_type
            );
            binops.insert(
                BinopKey {
                    params: (binop.lhs.ty.clone(), binop.rhs.ty.clone()),
                    operator: binop.op,
                },
                StackBinopDefinition {
                    parameters: (binop.lhs.clone(), binop.rhs.clone()),
                    return_ty: binop.return_type.clone(),
                    kind: match &binop.fn_kind {
                        FunctionDefinitionKind::Local(..) => {
                            let ir_function = module.function(
                                &binop_fn_name,
                                None,
                                binop.return_type.get_type(&type_values),
                                vec![binop.lhs.ty.get_type(&type_values), binop.rhs.ty.get_type(&type_values)],
                                FunctionFlags {
                                    is_pub: binop.exported,
                                    is_imported: binop.exported,
                                    ..Default::default()
                                },
                            );
                            let mut entry = ir_function.block("entry");

                            let allocator = Allocator::from(
                                &binop.fn_kind,
                                &vec![binop.lhs.clone(), binop.rhs.clone()],
                                &mut AllocatorScope {
                                    block: &mut entry,
                                    type_values: &type_values,
                                    mod_id: self.module_id,
                                },
                            );

                            let mut scope = Scope {
                                context,
                                modules: &modules,
                                tokens,
                                module: &module,
                                module_id: self.module_id,
                                function: &ir_function,
                                block: entry,
                                assoc_functions: &associated_functions,
                                functions: &functions,
                                types: &types,
                                type_values: &type_values,
                                stack_values: HashMap::new(),
                                debug: Some(Debug {
                                    info: &debug,
                                    scope: compile_unit.clone(),
                                    types: &debug_types,
                                }),
                                binops: &binops,
                                allocator: Rc::new(RefCell::new(allocator)),
                            };

                            binop
                                .fn_kind
                                .codegen(
                                    binop_fn_name.clone(),
                                    false,
                                    &mut scope,
                                    &vec![binop.lhs.clone(), binop.rhs.clone()],
                                    &binop.return_type,
                                    &ir_function,
                                    match &binop.fn_kind {
                                        FunctionDefinitionKind::Local(_, meta) => {
                                            meta.into_debug(tokens, &compile_unit)
                                        }
                                        FunctionDefinitionKind::Extern(_) => None,
                                        FunctionDefinitionKind::Intrinsic(_) => None,
                                        FunctionDefinitionKind::Macro(_) => None,
                                    },
                                )
                                .unwrap();

                            ScopeFunctionKind::UserGenerated(ir_function)
                        }
                        FunctionDefinitionKind::Extern(imported) => ScopeFunctionKind::UserGenerated(module.function(
                            &binop_fn_name,
                            None,
                            binop.return_type.get_type(&type_values),
                            vec![binop.lhs.ty.get_type(&type_values), binop.rhs.ty.get_type(&type_values)],
                            FunctionFlags {
                                is_extern: true,
                                is_imported: *imported,
                                ..FunctionFlags::default()
                            },
                        )),
                        FunctionDefinitionKind::Intrinsic(intrinsic_function) => {
                            ScopeFunctionKind::Intrinsic(intrinsic_function)
                        }
                        FunctionDefinitionKind::Macro(macro_function) => ScopeFunctionKind::Macro(macro_function),
                    },
                },
            );
        }

        for mir_function in &self.functions {
            if let ScopeFunctionKind::UserGenerated(function) = functions.get(&mir_function.name).unwrap() {
                let mut entry = function.block("entry");

                let allocator = Allocator::from(
                    &mir_function.kind,
                    &mir_function.parameters,
                    &mut AllocatorScope {
                        block: &mut entry,
                        type_values: &type_values,
                        mod_id: self.module_id,
                    },
                );

                let mut scope = Scope {
                    context,
                    modules: &modules,
                    tokens,
                    module: &module,
                    module_id: self.module_id,
                    function,
                    block: entry,
                    assoc_functions: &associated_functions,
                    functions: &functions,
                    types: &types,
                    type_values: &type_values,
                    stack_values: HashMap::new(),
                    debug: Some(Debug {
                        info: &debug,
                        scope: compile_unit.clone(),
                        types: &debug_types,
                    }),
                    binops: &binops,
                    allocator: Rc::new(RefCell::new(allocator)),
                };

                mir_function
                    .kind
                    .codegen(
                        mir_function.name.clone(),
                        mir_function.is_pub,
                        &mut scope,
                        &mir_function.parameters,
                        &mir_function.return_type,
                        &function,
                        match &mir_function.kind {
                            FunctionDefinitionKind::Local(..) => {
                                mir_function.signature().into_debug(tokens, &compile_unit)
                            }
                            FunctionDefinitionKind::Extern(_) => None,
                            FunctionDefinitionKind::Intrinsic(_) => None,
                            FunctionDefinitionKind::Macro(macro_function) => None,
                        },
                    )
                    .unwrap();
            }
        }

        for (ty, mir_function) in &self.associated_functions {
            if let ScopeFunctionKind::UserGenerated(function) = associated_functions
                .get(&AssociatedFunctionKey(ty.clone(), mir_function.name.clone()))
                .unwrap()
            {
                let mut entry = function.block("entry");

                let allocator = Allocator::from(
                    &mir_function.kind,
                    &mir_function.parameters,
                    &mut AllocatorScope {
                        block: &mut entry,
                        type_values: &type_values,
                        mod_id: self.module_id,
                    },
                );

                let mut scope = Scope {
                    context,
                    modules: &modules,
                    tokens,
                    module: &module,
                    module_id: self.module_id,
                    function,
                    block: entry,
                    assoc_functions: &associated_functions,
                    functions: &functions,
                    types: &types,
                    type_values: &type_values,
                    stack_values: HashMap::new(),
                    debug: Some(Debug {
                        info: &debug,
                        scope: compile_unit.clone(),
                        types: &debug_types,
                    }),
                    binops: &binops,
                    allocator: Rc::new(RefCell::new(allocator)),
                };

                mir_function
                    .kind
                    .codegen(
                        mir_function.name.clone(),
                        mir_function.is_pub,
                        &mut scope,
                        &mir_function.parameters,
                        &mir_function.return_type,
                        &function,
                        match &mir_function.kind {
                            FunctionDefinitionKind::Local(..) => {
                                mir_function.signature().into_debug(tokens, &compile_unit)
                            }
                            FunctionDefinitionKind::Extern(_) => None,
                            FunctionDefinitionKind::Intrinsic(_) => None,
                            FunctionDefinitionKind::Macro(_) => None,
                        },
                    )
                    .unwrap();
            }
        }

        Ok(ModuleCodegen {
            name: self.name.clone(),
            module,
        })
    }
}

impl FunctionDefinitionKind {
    fn codegen<'ctx, 'scope>(
        &self,
        name: String,
        is_pub: bool,
        scope: &mut Scope,
        parameters: &Vec<FunctionParam>,
        return_type: &TypeKind,
        ir_function: &Function,
        debug_location: Option<DebugLocation>,
    ) -> Result<(), ErrorKind> {
        match &self {
            mir::FunctionDefinitionKind::Local(block, _) => {
                // Insert debug information
                if let Some(debug) = &scope.debug {
                    if let Some(location) = debug_location {
                        // let debug_scope = debug.inner_scope(&outer_scope, location);

                        let fn_param_ty = &return_type.get_debug_type(&debug, scope);

                        let debug_ty = debug.info.debug_type(DebugTypeData::Subprogram(DebugSubprogramType {
                            parameters: vec![*fn_param_ty],
                            flags: DwarfFlags,
                        }));

                        let subprogram = debug.info.subprogram(
                            debug.scope.clone(),
                            DebugSubprogramData {
                                name: name.clone(),
                                outer_scope: debug.scope.clone(),
                                location,
                                ty: debug_ty,
                                opts: DebugSubprogramOptionals {
                                    is_local: !is_pub,
                                    is_definition: true,
                                    ..DebugSubprogramOptionals::default()
                                },
                            },
                        );

                        ir_function.set_debug(subprogram.clone());
                        scope.debug = Some(Debug {
                            info: debug.info,
                            scope: subprogram.clone(),
                            types: debug.types,
                        });
                    }
                }

                // Compile actual IR part
                for (i, p) in parameters.iter().enumerate() {
                    // Codegen actual parameters
                    let arg_name = format!("arg.{}", p.name);
                    let param = scope
                        .block
                        .build_named(format!("{}.get", arg_name), Instr::Param(i))
                        .unwrap();

                    let alloca = scope.allocate(&p.meta, &p.ty).unwrap();

                    scope
                        .block
                        .build_named(format!("{}.store", arg_name), Instr::Store(alloca, param))
                        .unwrap();
                    scope.stack_values.insert(
                        p.name.clone(),
                        StackValue(
                            StackValueKind::mutable(p.ty.is_mutable(), alloca),
                            TypeKind::CodegenPtr(Box::new(p.ty.clone())),
                        ),
                    );
                }

                let state = State::default();
                if let Some(ret) = block.codegen(scope, &state, false)? {
                    scope.block.terminate(Term::Ret(ret.instr())).unwrap();
                } else {
                    if !scope.block.delete_if_unused().unwrap() {
                        // Add a void return just in case if the block
                        // wasn't unused but didn't have a terminator yet
                        scope.block.terminate(Term::RetVoid).ok();
                    }
                }

                if let Some(debug) = &scope.debug {
                    if let Some(location) = &block.return_meta().into_debug(scope.tokens, &debug.scope) {
                        let location = debug.info.location(&debug.scope, location.clone());
                        scope.block.set_terminator_location(location).unwrap();
                    }
                }
            }
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
            FunctionDefinitionKind::Macro(_) => {}
        };
        Ok(())
    }
}

impl mir::Block {
    fn codegen<'ctx, 'a>(
        &self,
        mut scope: &mut Scope<'ctx, 'a>,
        state: &State,
        create_debug_scope: bool,
    ) -> Result<Option<StackValue>, ErrorKind> {
        let parent_scope = if let Some(debug) = &mut scope.debug {
            let parent_scope = debug.scope.clone();
            if create_debug_scope {
                let location = self.meta.into_debug(scope.tokens, &debug.scope).unwrap();
                let scope = debug.info.lexical_scope(&debug.scope, DebugLexicalScope { location });
                debug.scope = scope;
            }
            Some(parent_scope)
        } else {
            None
        };

        for stmt in &self.statements {
            stmt.codegen(&mut scope, state)?.map(|s| {
                if let Some(debug) = &scope.debug {
                    let location = stmt.1.into_debug(scope.tokens, &debug.scope).unwrap();
                    let loc_val = debug.info.location(&debug.scope, location);
                    s.instr().with_location(&mut scope.block, loc_val);
                }
            });
        }

        let return_value = if let Some((kind, expr)) = &self.return_expression {
            if let Some(expr) = expr {
                let ret = expr.codegen(&mut scope, &mut state.load(true))?;
                match kind {
                    mir::ReturnKind::Hard => {
                        if let Some(ret) = ret {
                            scope.block.terminate(Term::Ret(ret.instr())).unwrap();
                        } else {
                            scope.block.terminate(Term::RetVoid).unwrap();
                        }
                        Ok(None)
                    }
                    mir::ReturnKind::Soft => Ok(ret),
                }
            } else {
                match kind {
                    mir::ReturnKind::Hard => scope.block.terminate(Term::RetVoid).unwrap(),
                    mir::ReturnKind::Soft => {}
                }
                Ok(None)
            }
        } else {
            Ok(None)
        };

        if let Some(parent_scope) = parent_scope {
            scope.debug.as_mut().unwrap().scope = parent_scope;
        }

        return_value
    }
}

impl mir::Statement {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, state: &State) -> Result<Option<StackValue>, ErrorKind> {
        let location = scope.debug.clone().map(|d| {
            let location = self.1.into_debug(scope.tokens, &d.scope).unwrap();
            d.info.location(&d.scope, location)
        });

        match &self.0 {
            mir::StmtKind::Let(NamedVariableRef(ty, name, meta), mutable, expression) => {
                let value = expression.codegen(scope, &state)?.unwrap();

                let alloca = scope
                    .allocate(meta, &value.1)
                    .unwrap()
                    .maybe_location(&mut scope.block, location.clone());

                let store = scope
                    .block
                    .build_named(format!("{}.store", name), Instr::Store(alloca, value.instr()))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                let stack_value = match mutable {
                    true => StackValueKind::Mutable(alloca),
                    false => StackValueKind::Immutable(alloca),
                };

                scope.stack_values.insert(
                    name.clone(),
                    StackValue(stack_value, TypeKind::CodegenPtr(Box::new(value.clone().1))),
                );
                if let Some(debug) = &scope.debug {
                    let location = self.1.into_debug(scope.tokens, &debug.scope).unwrap();
                    let var = debug.info.metadata(
                        &location,
                        DebugMetadata::LocalVar(DebugLocalVariable {
                            name: name.clone(),
                            ty: ty.clone().get_debug_type(debug, scope),
                            always_preserve: true,
                            flags: DwarfFlags,
                        }),
                    );
                    store.add_record(
                        &mut scope.block,
                        InstructionDebugRecordData {
                            variable: var,
                            location,
                            kind: DebugRecordKind::Declare(alloca),
                            scope: debug.scope.clone(),
                        },
                    );
                }
                Ok(None)
            }
            mir::StmtKind::Set(lhs, rhs) => {
                let lhs_value = lhs
                    .codegen(scope, &state.load(false))?
                    .expect("non-returning LHS snuck into codegen!");

                let rhs_value = rhs.codegen(scope, state)?;
                let Some(rhs_value) = rhs_value else {
                    return Ok(None);
                };

                let backing_var = if let Some(var) = lhs.backing_var() {
                    &format!("store.{}", var.1)
                } else {
                    "store"
                };

                match lhs_value.0 {
                    StackValueKind::Immutable(_) => {
                        panic!("Tried to assign to immutable!")
                    }
                    StackValueKind::Mutable(instr) => {
                        scope
                            .block
                            .build_named(backing_var, Instr::Store(instr, rhs_value.instr()))
                            .unwrap()
                            .maybe_location(&mut scope.block, location);
                    }
                    StackValueKind::Literal(_) => {
                        panic!("Tried to assign to a literal!")
                    }
                };

                Ok(None)
            }
            mir::StmtKind::Import(_) => todo!(),
            mir::StmtKind::Expression(expression) => expression.codegen(scope, state),
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                let condition_block = scope.function.block("while.cond");
                let condition_true_block = scope.function.block("while.body");
                let condition_failed_block = scope.function.block("while.end");

                scope.block.terminate(Term::Br(condition_block.value())).unwrap();
                let mut condition_scope = scope.with_block(condition_block);
                let condition_res = condition.codegen(&mut condition_scope, state)?.unwrap();
                let true_instr = condition_scope
                    .block
                    .build(Instr::Constant(ConstValue::Bool(true)))
                    .unwrap();
                let check = condition_scope
                    .block
                    .build(Instr::ICmp(CmpPredicate::EQ, condition_res.instr(), true_instr))
                    .unwrap();

                condition_scope
                    .block
                    .terminate(Term::CondBr(
                        check,
                        condition_true_block.value(),
                        condition_failed_block.value(),
                    ))
                    .unwrap();

                let mut condition_true_scope = scope.with_block(condition_true_block);
                block.codegen(&mut condition_true_scope, state, true)?;

                condition_true_scope
                    .block
                    .terminate(Term::Br(condition_scope.block.value()))
                    // Can hard return inside the condition_true_scope
                    .ok();

                scope.swap_block(condition_failed_block);

                Ok(None)
            }
        }
    }
}

impl mir::Expression {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, state: &State) -> Result<Option<StackValue>, ErrorKind> {
        let location = if let Some(debug) = &scope.debug {
            Some(
                debug
                    .info
                    .location(&debug.scope, self.1.into_debug(scope.tokens, &debug.scope).unwrap()),
            )
        } else {
            None
        };

        let value = match &self.0 {
            mir::ExprKind::Variable(varref) => {
                varref.0.known().expect("variable type unknown");
                let v = scope
                    .stack_values
                    .get(&varref.1)
                    .expect("Variable reference not found?!");
                Some({
                    if state.should_load {
                        if let TypeKind::CodegenPtr(inner) = &v.1 {
                            StackValue(
                                v.0.derive(
                                    scope
                                        .block
                                        .build_named(
                                            format!("{}", varref.1),
                                            Instr::Load(v.0.instr(), inner.get_type(scope.type_values)),
                                        )
                                        .unwrap(),
                                ),
                                *inner.clone(),
                            )
                        } else {
                            panic!("Variable was not a pointer?!?")
                        }
                    } else {
                        v.clone()
                    }
                })
            }
            mir::ExprKind::Literal(lit) => Some(StackValue(
                StackValueKind::Literal(lit.as_const(&mut scope.block)),
                lit.as_type(),
            )),
            mir::ExprKind::BinOp(binop, lhs_exp, rhs_exp, return_ty) => {
                let lhs_val = lhs_exp.codegen(scope, state)?.expect("lhs has no return value");
                let rhs_val = rhs_exp.codegen(scope, state)?.expect("rhs has no return value");
                let lhs = lhs_val.instr();
                let rhs = rhs_val.instr();

                let operation = scope.binops.get(&BinopKey {
                    params: (lhs_val.1.clone(), rhs_val.1.clone()),
                    operator: *binop,
                });

                if let Some(operation) = operation {
                    let a = operation.codegen(lhs_val.clone(), rhs_val.clone(), scope)?;
                    Some(a)
                } else {
                    let lhs_type = lhs_exp.return_type(&Default::default(), scope.module_id).unwrap().1;
                    let instr = match (binop, lhs_type.signed(), lhs_type.category() == TypeCategory::Real) {
                        (mir::BinaryOperator::Add, _, false) => Instr::Add(lhs, rhs),
                        (mir::BinaryOperator::Add, _, true) => Instr::FAdd(lhs, rhs),
                        (mir::BinaryOperator::Minus, _, false) => Instr::Sub(lhs, rhs),
                        (mir::BinaryOperator::Minus, _, true) => Instr::FSub(lhs, rhs),
                        (mir::BinaryOperator::Mult, _, false) => Instr::Mul(lhs, rhs),
                        (mir::BinaryOperator::Mult, _, true) => Instr::FMul(lhs, rhs),
                        (mir::BinaryOperator::And, _, _) => Instr::And(lhs, rhs),
                        (mir::BinaryOperator::Cmp(i), _, false) => Instr::ICmp(i.predicate(), lhs, rhs),
                        (mir::BinaryOperator::Cmp(i), _, true) => Instr::FCmp(i.predicate(), lhs, rhs),
                        (mir::BinaryOperator::Div, false, false) => Instr::UDiv(lhs, rhs),
                        (mir::BinaryOperator::Div, true, false) => Instr::SDiv(lhs, rhs),
                        (mir::BinaryOperator::Div, _, true) => Instr::FDiv(lhs, rhs),
                        (mir::BinaryOperator::Mod, false, false) => {
                            let div = scope
                                .block
                                .build(Instr::UDiv(lhs, rhs))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone());
                            let mul = scope
                                .block
                                .build(Instr::Mul(rhs, div))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone());
                            Instr::Sub(lhs, mul)
                        }
                        (mir::BinaryOperator::Mod, true, false) => {
                            let div = scope
                                .block
                                .build(Instr::SDiv(lhs, rhs))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone());
                            let mul = scope
                                .block
                                .build(Instr::Mul(rhs, div))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone());
                            Instr::Sub(lhs, mul)
                        }
                        (mir::BinaryOperator::Mod, _, true) => {
                            let div = scope
                                .block
                                .build(Instr::FDiv(lhs, rhs))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone());
                            let mul = scope
                                .block
                                .build(Instr::Mul(rhs, div))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone());
                            Instr::Sub(lhs, mul)
                        }
                        (mir::BinaryOperator::Or, true, true) => todo!(),
                        (mir::BinaryOperator::Or, true, false) => todo!(),
                        (mir::BinaryOperator::Or, false, true) => todo!(),
                        (mir::BinaryOperator::Or, false, false) => todo!(),
                        (mir::BinaryOperator::Xor, true, true) => todo!(),
                        (mir::BinaryOperator::Xor, true, false) => todo!(),
                        (mir::BinaryOperator::Xor, false, true) => todo!(),
                        (mir::BinaryOperator::Xor, false, false) => todo!(),
                        (mir::BinaryOperator::BitOr, true, true) => todo!(),
                        (mir::BinaryOperator::BitOr, true, false) => todo!(),
                        (mir::BinaryOperator::BitOr, false, true) => todo!(),
                        (mir::BinaryOperator::BitOr, false, false) => todo!(),
                        (mir::BinaryOperator::BitAnd, true, true) => todo!(),
                        (mir::BinaryOperator::BitAnd, true, false) => todo!(),
                        (mir::BinaryOperator::BitAnd, false, true) => todo!(),
                        (mir::BinaryOperator::BitAnd, false, false) => todo!(),
                        (mir::BinaryOperator::BitshiftRight, true, true) => todo!(),
                        (mir::BinaryOperator::BitshiftRight, true, false) => todo!(),
                        (mir::BinaryOperator::BitshiftRight, false, true) => todo!(),
                        (mir::BinaryOperator::BitshiftRight, false, false) => todo!(),
                        (mir::BinaryOperator::BitshiftLeft, true, true) => todo!(),
                        (mir::BinaryOperator::BitshiftLeft, true, false) => todo!(),
                        (mir::BinaryOperator::BitshiftLeft, false, true) => todo!(),
                        (mir::BinaryOperator::BitshiftLeft, false, false) => todo!(),
                    };
                    Some(StackValue(
                        StackValueKind::Immutable(
                            scope
                                .block
                                .build(instr)
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone()),
                        ),
                        return_ty.clone(),
                    ))
                }
            }
            mir::ExprKind::FunctionCall(call) => codegen_function_call(None, call, scope, state)?,
            mir::ExprKind::If(if_expression) => if_expression.codegen(scope, state)?,
            mir::ExprKind::Block(block) => {
                let inner = scope.function.block("inner");
                scope.block.terminate(Term::Br(inner.value())).unwrap();

                let mut inner_scope = scope.with_block(inner);
                let ret = if let Some(ret) = block.codegen(&mut inner_scope, state, true)? {
                    Some(ret)
                } else {
                    None
                };
                let outer = scope.function.block("outer");
                inner_scope.block.terminate(Term::Br(outer.value())).ok();
                scope.swap_block(outer);
                ret
            }
            mir::ExprKind::Indexed(expression, val_t, idx_expr) => {
                let StackValue(kind, ty) = expression
                    .codegen(scope, &state.load(false))?
                    .expect("array returned none!");
                let idx = idx_expr
                    .codegen(scope, &state.load(true))?
                    .expect("index returned none!")
                    .instr();

                let TypeKind::CodegenPtr(inner) = ty else {
                    panic!();
                };

                let (ptr, contained_ty) = if let TypeKind::UserPtr(further_inner) = *inner.clone() {
                    let loaded = scope
                        .block
                        .build_named("load", Instr::Load(kind.instr(), inner.get_type(scope.type_values)))
                        .unwrap();
                    (
                        scope
                            .block
                            .build_named(format!("gep"), Instr::GetElemPtr(loaded, vec![idx]))
                            .unwrap()
                            .maybe_location(&mut scope.block, location.clone()),
                        *further_inner,
                    )
                } else if let TypeKind::CodegenPtr(further_inner) = *inner.clone() {
                    let TypeKind::Array(_, _) = *further_inner else {
                        panic!();
                    };

                    (
                        scope
                            .block
                            .build_named(format!("array.gep"), Instr::GetElemPtr(kind.instr(), vec![idx]))
                            .unwrap()
                            .maybe_location(&mut scope.block, location.clone()),
                        val_t.clone(),
                    )
                } else {
                    let TypeKind::Array(_, _) = *inner else {
                        panic!();
                    };

                    let first = scope
                        .block
                        .build_named("array.zero", Instr::Constant(ConstValue::U32(0)))
                        .unwrap();
                    (
                        scope
                            .block
                            .build_named(format!("array.gep"), Instr::GetElemPtr(kind.instr(), vec![first, idx]))
                            .unwrap()
                            .maybe_location(&mut scope.block, location.clone()),
                        val_t.clone(),
                    )
                };

                if state.should_load {
                    Some(StackValue(
                        kind.derive(
                            scope
                                .block
                                .build_named("array.load", Instr::Load(ptr, contained_ty.get_type(scope.type_values)))
                                .unwrap()
                                .maybe_location(&mut scope.block, location.clone()),
                        ),
                        contained_ty,
                    ))
                } else {
                    Some(StackValue(
                        kind.derive(ptr),
                        TypeKind::CodegenPtr(Box::new(contained_ty)),
                    ))
                }
            }
            mir::ExprKind::Array(expressions) => {
                let stack_value_list: Vec<_> =
                    try_all(expressions.iter().map(|e| e.codegen(scope, state)).collect::<Vec<_>>())
                        .map_err(|e| e.first().cloned().unwrap())?
                        .into_iter()
                        .map(|v| v.unwrap())
                        .collect();

                let instr_list = stack_value_list.iter().map(|s| s.instr()).collect::<Vec<_>>();

                let elem_ty_kind = stack_value_list
                    .iter()
                    .map(|s| s.1.clone())
                    .next()
                    .unwrap_or(TypeKind::Void);

                let array_ty = Type::Array(
                    Box::new(elem_ty_kind.get_type(scope.type_values)),
                    instr_list.len() as u64,
                );
                let array_name = format!("{}.{}", elem_ty_kind, instr_list.len());
                let load_n = format!("{}.load", array_name);

                let array = scope
                    .allocate(
                        &self.1,
                        &TypeKind::Array(Box::new(elem_ty_kind.clone()), expressions.len() as u64),
                    )
                    .unwrap()
                    .maybe_location(&mut scope.block, location.clone());

                for (index, instr) in instr_list.iter().enumerate() {
                    let gep_n = format!("{}.{}.gep", array_name, index);
                    let store_n = format!("{}.{}.store", array_name, index);

                    let index_expr = scope
                        .block
                        .build_named(index.to_string(), Instr::Constant(ConstValue::U32(index as u32)))
                        .unwrap();
                    let first = scope
                        .block
                        .build_named("zero", Instr::Constant(ConstValue::U32(0)))
                        .unwrap();
                    let ptr = scope
                        .block
                        .build_named(gep_n, Instr::GetElemPtr(array, vec![first, index_expr]))
                        .unwrap()
                        .maybe_location(&mut scope.block, location.clone());
                    scope
                        .block
                        .build_named(store_n, Instr::Store(ptr, *instr))
                        .unwrap()
                        .maybe_location(&mut scope.block, location.clone());
                }

                let array_val = scope
                    .block
                    .build_named(load_n, Instr::Load(array, array_ty))
                    .unwrap()
                    .maybe_location(&mut scope.block, location.clone());

                Some(StackValue(
                    StackValueKind::Literal(array_val),
                    TypeKind::Array(Box::new(elem_ty_kind), instr_list.len() as u64),
                ))
            }
            mir::ExprKind::Accessed(expression, type_kind, field) => {
                let struct_val = expression.codegen(scope, &state.load(false))?.unwrap();

                let TypeKind::CodegenPtr(inner) = &struct_val.1 else {
                    panic!("tried accessing non-pointer");
                };
                let TypeKind::CustomType(key) = *inner.clone() else {
                    panic!("tried accessing non-custom-type");
                };
                let TypeDefinitionKind::Struct(struct_ty) = scope.get_typedef(&key).unwrap().kind.clone();
                let idx = struct_ty.find_index(field).unwrap();

                let gep_n = format!("{}.{}.gep", key.0, field);
                let load_n = format!("{}.{}.load", key.0, field);

                let value = scope
                    .block
                    .build_named(gep_n, Instr::GetStructElemPtr(struct_val.instr(), idx as u32))
                    .unwrap();

                // value.maybe_location(&mut scope.block, location);

                if state.should_load {
                    Some(StackValue(
                        struct_val.0.derive(
                            scope
                                .block
                                .build_named(load_n, Instr::Load(value, type_kind.get_type(scope.type_values)))
                                .unwrap(),
                        ),
                        struct_ty.get_field_ty(&field).unwrap().clone(),
                    ))
                } else {
                    Some(StackValue(
                        struct_val.0.derive(value),
                        TypeKind::CodegenPtr(Box::new(struct_ty.get_field_ty(&field).unwrap().clone())),
                    ))
                }
            }
            mir::ExprKind::Struct(name, items) => {
                let type_key = CustomTypeKey(name.clone(), scope.module_id);
                let ty = Type::CustomType({
                    let Some(a) = scope.type_values.get(&type_key) else {
                        return Ok(None);
                    };
                    *a
                });

                let TypeDefinition {
                    kind: TypeDefinitionKind::Struct(struct_ty),
                    ..
                } = scope.types.get(scope.type_values.get(&type_key).unwrap()).unwrap();

                let indices = struct_ty.0.iter().enumerate();

                let load_n = format!("{}.load", name);

                let struct_ptr = scope
                    .allocate(&self.1, &TypeKind::CustomType(type_key.clone()))
                    .unwrap()
                    .maybe_location(&mut scope.block, location.clone());

                for (field_n, exp) in items {
                    let gep_n = format!("{}.{}.gep", name, field_n);
                    let store_n = format!("{}.{}.store", name, field_n);
                    let i = indices.clone().find(|(_, f)| f.0 == *field_n).unwrap().0;

                    let elem_ptr = scope
                        .block
                        .build_named(gep_n, Instr::GetStructElemPtr(struct_ptr, i as u32))
                        .unwrap()
                        .maybe_location(&mut scope.block, location.clone());
                    if let Some(val) = exp.codegen(scope, state)? {
                        scope
                            .block
                            .build_named(store_n, Instr::Store(elem_ptr, val.instr()))
                            .unwrap()
                            .maybe_location(&mut scope.block, location.clone());
                    }
                }

                let struct_val = scope.block.build_named(load_n, Instr::Load(struct_ptr, ty)).unwrap();

                Some(StackValue(
                    StackValueKind::Literal(struct_val),
                    TypeKind::CustomType(type_key),
                ))
            }
            mir::ExprKind::Borrow(expr, mutable) => {
                let v = expr.codegen(scope, &state.load(false))?.unwrap();

                let TypeKind::CodegenPtr(ptr_inner) = &v.1 else {
                    panic!();
                };

                Some(StackValue(
                    StackValueKind::mutable(*mutable, v.0.instr()),
                    TypeKind::Borrow(Box::new(*ptr_inner.clone()), *mutable),
                ))
            }
            mir::ExprKind::Deref(expr) => {
                let v = expr.codegen(scope, &state.load(false))?.unwrap();

                let TypeKind::CodegenPtr(ptr_inner) = &v.1 else {
                    panic!();
                };

                let var_ptr_instr = scope
                    .block
                    .build_named(
                        format!("{}.deref", expr.backing_var().unwrap().1),
                        Instr::Load(v.0.instr(), ptr_inner.get_type(scope.type_values)),
                    )
                    .unwrap();

                Some({
                    if state.should_load {
                        if let TypeKind::Borrow(inner, _) = *ptr_inner.clone() {
                            StackValue(
                                v.0.derive(
                                    scope
                                        .block
                                        .build_named(
                                            format!("{}.deref.inner", expr.backing_var().unwrap().1),
                                            Instr::Load(var_ptr_instr, inner.get_type(scope.type_values)),
                                        )
                                        .unwrap(),
                                ),
                                *inner.clone(),
                            )
                        } else {
                            panic!("Variable was not a pointer?!?")
                        }
                    } else {
                        let TypeKind::Borrow(borrow_inner, mutable) = *ptr_inner.clone() else {
                            panic!();
                        };
                        StackValue(
                            StackValueKind::mutable(mutable, var_ptr_instr),
                            TypeKind::CodegenPtr(borrow_inner.clone()),
                        )
                    }
                })
            }
            mir::ExprKind::CastTo(expression, type_kind) => {
                let val = expression.codegen(scope, state)?;
                let Some(val) = val else { return Ok(None) };

                if val.1 == *type_kind {
                    Some(val)
                } else {
                    match (&val.1, type_kind) {
                        (TypeKind::CodegenPtr(inner), TypeKind::UserPtr(_)) => match *inner.clone() {
                            TypeKind::UserPtr(_) => Some(StackValue(
                                val.0.derive(
                                    scope
                                        .block
                                        .build(Instr::BitCast(
                                            val.instr(),
                                            Type::Ptr(Box::new(type_kind.get_type(scope.type_values))),
                                        ))
                                        .unwrap(),
                                ),
                                TypeKind::CodegenPtr(Box::new(type_kind.clone())),
                            )),
                            _ => panic!(),
                        },
                        (TypeKind::UserPtr(_), TypeKind::UserPtr(_))
                        | (TypeKind::Char, TypeKind::U8)
                        | (TypeKind::U8, TypeKind::Char)
                        | (TypeKind::U8, TypeKind::I8) => Some(StackValue(
                            val.0.derive(
                                scope
                                    .block
                                    .build(Instr::BitCast(val.instr(), type_kind.get_type(scope.type_values)))
                                    .unwrap(),
                            ),
                            type_kind.clone(),
                        )),
                        _ => {
                            let cast_instr = val
                                .1
                                .get_type(scope.type_values)
                                .cast_instruction(val.instr(), &type_kind.get_type(scope.type_values))
                                .unwrap();

                            Some(StackValue(
                                val.0.derive(scope.block.build(cast_instr).unwrap()),
                                type_kind.clone(),
                            ))
                        }
                    }
                }
            }
            mir::ExprKind::AssociatedFunctionCall(ty, call) => codegen_function_call(Some(ty), call, scope, state)?,
        };
        if let Some(value) = &value {
            value.instr().maybe_location(&mut scope.block, location.clone());
        }
        Ok(value)
    }
}

fn codegen_function_call<'ctx, 'a>(
    associated_type: Option<&TypeKind>,
    call: &FunctionCall,
    scope: &mut Scope<'ctx, 'a>,
    state: &State,
) -> Result<Option<StackValue>, ErrorKind> {
    let ret_type_kind = call.return_type.known().expect("function return type unknown");
    let call_name = if let Some(ty) = &associated_type {
        format!("{}::{}", ty, call.name)
    } else {
        String::from(call.name.clone())
    };

    let ret_type = ret_type_kind.get_type(scope.type_values);

    let params = try_all(
        call.parameters
            .iter()
            .map(|e| e.codegen(scope, state))
            .collect::<Vec<_>>(),
    )
    .map_err(|e| e.first().cloned().unwrap())?
    .into_iter()
    .map(|v| v.unwrap())
    .collect::<Vec<_>>();

    let location = if let Some(debug) = &scope.debug {
        call.meta.into_debug(scope.tokens, &debug.scope)
    } else {
        None
    };

    let val = if let Some(ty) = associated_type {
        // Unwrap type from borrow if it is in a borrow
        let ty = if let TypeKind::Borrow(inner, _) = ty { inner } else { ty };

        let assoc_key = AssociatedFunctionKey(ty.clone(), call.name.clone());
        let intrinsic_def = get_intrinsic_assoc_func(&ty, &call.name);
        let intrinsic = intrinsic_def.map(|func_def| {
            let FunctionDefinitionKind::Intrinsic(intrinsic) = func_def.kind else {
                panic!();
            };
            ScopeFunctionKind::IntrinsicOwned(intrinsic)
        });
        let callee = scope
            .assoc_functions
            .get(&assoc_key)
            .or(intrinsic.as_ref())
            .expect(&format!("Function {} does not exist!", call_name));
        callee
            .codegen(
                &call_name,
                params.as_slice(),
                &call.parameters,
                &call.return_type,
                location,
                scope,
            )
            .unwrap()
    } else {
        let callee = scope
            .functions
            .get(&call.name)
            .expect(&format!("Function {} does not exist!", call_name));

        callee
            .codegen(
                &call_name,
                params.as_slice(),
                &call.parameters,
                &call.return_type,
                location,
                scope,
            )
            .unwrap()
    };

    let ptr = if ret_type_kind != TypeKind::Void {
        let ptr = scope
            .allocator
            .borrow_mut()
            .allocate(&call.meta, &call.return_type)
            .unwrap();
        scope
            .block
            .build_named(format!("{}.store", call_name), Instr::Store(ptr, val.instr()))
            .unwrap();

        Some(ptr)
    } else {
        None
    };

    Ok(if let Some(ptr) = ptr {
        if state.should_load {
            Some(StackValue(
                StackValueKind::Immutable(
                    scope
                        .block
                        .build_named(call.name.clone(), Instr::Load(ptr, ret_type))
                        .unwrap(),
                ),
                ret_type_kind,
            ))
        } else {
            Some(StackValue(
                StackValueKind::Immutable(ptr),
                TypeKind::CodegenPtr(Box::new(ret_type_kind)),
            ))
        }
    } else {
        None
    })
}

impl mir::IfExpression {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, state: &State) -> Result<Option<StackValue>, ErrorKind> {
        let condition = self.0.codegen(scope, state)?.unwrap();

        // Create blocks
        let mut then_b = scope.function.block("then");
        let mut else_b = scope.function.block("else");
        let after_b = scope.function.block("after");

        if let Some(debug) = &scope.debug {
            let before_location = self.0 .1.into_debug(scope.tokens, &debug.scope).unwrap();
            let before_v = debug.info.location(&debug.scope, before_location);
            scope.block.set_terminator_location(before_v).ok();

            let then_location = self.1 .1.into_debug(scope.tokens, &debug.scope).unwrap();
            let then_v = debug.info.location(&debug.scope, then_location.clone());
            then_b.set_terminator_location(then_v).unwrap();

            let else_location = if let Some(else_expr) = self.2.as_ref() {
                else_expr.1.into_debug(scope.tokens, &debug.scope).unwrap()
            } else {
                then_location
            };
            let else_v = debug.info.location(&debug.scope, else_location);
            else_b.set_terminator_location(else_v).unwrap();
        }

        // Store for convenience
        let then_bb = then_b.value();
        let else_bb = else_b.value();
        let after_bb = after_b.value();

        // Generate then-block content
        let mut then_scope = scope.with_block(then_b);
        let then_res = self.1.codegen(&mut then_scope, state)?;
        then_scope.block.terminate(Term::Br(after_bb)).ok();

        let else_res = if let Some(else_expr) = self.2.as_ref() {
            let mut else_scope = scope.with_block(else_b);

            scope
                .block
                .terminate(Term::CondBr(condition.instr(), then_bb, else_bb))
                .unwrap();

            let opt = else_expr.codegen(&mut else_scope, state)?;

            else_scope.block.terminate(Term::Br(after_bb)).ok();

            if let Some(ret) = opt {
                Some(ret)
            } else {
                None
            }
        } else {
            else_b.terminate(Term::Br(after_bb)).unwrap();
            scope
                .block
                .terminate(Term::CondBr(condition.instr(), then_bb, after_bb))
                .unwrap();
            None
        };

        // Swap block to the after-block so that construction can continue correctly
        scope.swap_block(after_b);

        if then_res.is_none() && else_res.is_none() {
            Ok(None)
        } else {
            let mut incoming = Vec::from(then_res.as_slice());
            incoming.extend(else_res.clone());
            let instr = scope
                .block
                .build_named("phi", Instr::Phi(incoming.iter().map(|i| i.instr()).collect()))
                .unwrap();

            use StackValueKind::*;
            let value = match (then_res, else_res) {
                (None, None) => StackValue(StackValueKind::Immutable(instr), TypeKind::Void),
                (Some(val), None) | (None, Some(val)) => StackValue(val.0.derive(instr), val.1),
                (Some(lhs_val), Some(rhs_val)) => match (lhs_val.0, rhs_val.0) {
                    (Immutable(_), Immutable(_))
                    | (Immutable(_), Mutable(_))
                    | (Mutable(_), Immutable(_))
                    | (Immutable(_), Literal(_))
                    | (Literal(_), Immutable(_))
                    | (Mutable(_), Literal(_))
                    | (Literal(_), Mutable(_))
                    | (Literal(_), Literal(_)) => StackValue(Immutable(instr), lhs_val.1),
                    (Mutable(_), Mutable(_)) => StackValue(Mutable(instr), lhs_val.1),
                },
            };
            Ok(Some(value))
        }
    }
}
