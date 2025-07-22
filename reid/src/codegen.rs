use std::{array, collections::HashMap, mem};

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    compile::CompiledModule,
    debug_information::{
        DebugArrayType, DebugBasicType, DebugFieldType, DebugFileData, DebugInformation,
        DebugLocalVariable, DebugLocation, DebugMetadata, DebugParamVariable, DebugPointerType,
        DebugProgramValue, DebugRecordKind, DebugStructType, DebugSubprogramData,
        DebugSubprogramOptionals, DebugSubprogramType, DebugTypeData, DebugTypeValue,
        DwarfEncoding, DwarfFlags, InstructionDebugRecordData,
    },
    Block, CmpPredicate, ConstValue, Context, CustomTypeKind, Function, FunctionFlags, Instr,
    Module, NamedStruct, TerminatorKind as Term, Type,
};

use crate::{
    error_raporting::ErrorModules,
    lexer::{FullToken, Position},
    mir::{
        self, implement::TypeCategory, Metadata, NamedVariableRef, SourceModuleId, StructField,
        StructType, TypeDefinition, TypeDefinitionKind, TypeKey, TypeKind, VagueLiteral,
    },
};

/// Context that contains all of the given modules as complete codegenerated
/// LLIR that can then be finally compiled into LLVM IR.
#[derive(Debug)]
pub struct CodegenContext<'ctx> {
    pub(crate) context: &'ctx Context,
}

impl<'ctx> CodegenContext<'ctx> {
    /// Compile contained LLIR into LLVM IR and produce `hello.o` and
    /// `hello.asm`
    pub fn compile(&self) -> CompiledModule {
        self.context.compile()
    }
}

impl mir::Context {
    /// Compile MIR [`Context`] into [`CodegenContext`] containing LLIR.
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> CodegenContext<'ctx> {
        let mut modules = Vec::new();
        for (_, module) in &self.modules {
            modules.push(module.codegen(context));
        }
        CodegenContext { context }
    }
}

struct ModuleCodegen<'ctx> {
    module: Module<'ctx>,
}

impl<'ctx> std::fmt::Debug for ModuleCodegen<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.module.as_printable(), f)
    }
}

pub struct Scope<'ctx, 'a> {
    context: &'ctx Context,
    tokens: &'ctx Vec<FullToken>,
    module: &'ctx Module<'ctx>,
    module_id: SourceModuleId,
    function: &'ctx StackFunction<'ctx>,
    block: Block<'ctx>,
    types: &'a HashMap<TypeValue, TypeDefinition>,
    type_values: &'a HashMap<TypeKey, TypeValue>,
    functions: &'a HashMap<String, StackFunction<'ctx>>,
    stack_values: HashMap<String, StackValue>,
    debug: Option<Debug<'ctx>>,
}

#[derive(Debug, Clone)]
pub struct Debug<'ctx> {
    info: &'ctx DebugInformation,
    scope: DebugProgramValue,
    types: &'ctx HashMap<TypeKind, DebugTypeValue>,
}

pub struct StackFunction<'ctx> {
    ir: Function<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackValue(StackValueKind, TypeKind);

impl StackValue {
    fn instr(&self) -> InstructionValue {
        self.0.instr()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackValueKind {
    Immutable(InstructionValue),
    Mutable(InstructionValue),
    Literal(InstructionValue),
}

impl StackValueKind {
    fn mutable(mutable: bool, instr: InstructionValue) -> StackValueKind {
        match mutable {
            true => StackValueKind::Mutable(instr),
            false => StackValueKind::Immutable(instr),
        }
    }

    fn instr(&self) -> InstructionValue {
        match &self {
            StackValueKind::Immutable(val) => *val,
            StackValueKind::Mutable(val) => *val,
            StackValueKind::Literal(val) => *val,
        }
    }

    fn derive(&self, instr: InstructionValue) -> StackValueKind {
        match &self {
            StackValueKind::Immutable(_) => StackValueKind::Immutable(instr),
            StackValueKind::Mutable(_) => StackValueKind::Mutable(instr),
            StackValueKind::Literal(_) => StackValueKind::Literal(instr),
        }
    }

    #[allow(dead_code)]
    fn map<F>(&self, lambda: F) -> StackValueKind
    where
        F: FnOnce(InstructionValue) -> InstructionValue,
    {
        self.derive(lambda(self.instr()))
    }
}

impl<'ctx, 'a> Scope<'ctx, 'a> {
    fn with_block(&self, block: Block<'ctx>) -> Scope<'ctx, 'a> {
        Scope {
            block,
            tokens: self.tokens,
            function: self.function,
            context: self.context,
            module: self.module,
            module_id: self.module_id,
            functions: self.functions,
            types: self.types,
            type_values: self.type_values,
            stack_values: self.stack_values.clone(),
            debug: self.debug.clone(),
        }
    }

    /// Takes the block out from this scope, swaps the given block in it's place
    /// and returns the old block.
    fn swap_block(&mut self, block: Block<'ctx>) -> Block<'ctx> {
        let mut old_block = block;
        mem::swap(&mut self.block, &mut old_block);
        old_block
    }

    fn get_typedef(&self, key: &TypeKey) -> Option<&TypeDefinition> {
        self.type_values.get(key).and_then(|v| self.types.get(v))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct State {
    should_load: bool,
}

impl State {
    /// Sets should load, returning a new state
    fn load(self, should: bool) -> State {
        State {
            should_load: should,
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self { should_load: true }
    }
}

impl mir::Module {
    fn codegen<'ctx>(&self, context: &'ctx Context) -> ModuleCodegen<'ctx> {
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
                        compile_unit,
                        &debug,
                        &debug_types,
                        &type_values,
                        &types,
                        tokens,
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

        for typedef in &self.typedefs {
            let type_key = TypeKey(typedef.name.clone(), typedef.source_module);

            let type_value = if typedef.source_module != self.module_id {
                todo!()
            } else {
                match &typedef.kind {
                    TypeDefinitionKind::Struct(StructType(fields)) => {
                        module.custom_type(CustomTypeKind::NamedStruct(NamedStruct(
                            typedef.name.clone(),
                            fields
                                .iter()
                                // TODO: Reorder custom-type definitions such that
                                // inner types get evaluated first. Otherwise this
                                // will cause a panic!
                                .map(|StructField(_, t, _)| t.get_type(&type_values, &types))
                                .collect(),
                        )))
                    }
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
                .map(|(_, p)| p.get_type(&type_values, &types))
                .collect();

            let is_main = self.is_main && function.name == "main";
            let func = match &function.kind {
                mir::FunctionDefinitionKind::Local(_, _) => module.function(
                    &function.name,
                    function.return_type.get_type(&type_values, &types),
                    param_types,
                    FunctionFlags {
                        is_pub: function.is_pub || is_main,
                        is_main,
                        is_imported: function.is_imported,
                        ..FunctionFlags::default()
                    },
                ),
                mir::FunctionDefinitionKind::Extern(imported) => module.function(
                    &function.name,
                    function.return_type.get_type(&type_values, &types),
                    param_types,
                    FunctionFlags {
                        is_extern: true,
                        is_imported: *imported,
                        ..FunctionFlags::default()
                    },
                ),
            };

            functions.insert(function.name.clone(), StackFunction { ir: func });
        }

        for mir_function in &self.functions {
            let function = functions.get(&mir_function.name).unwrap();
            let mut entry = function.ir.block("entry");

            // Insert debug information
            let debug_scope = if let Some(location) =
                mir_function.signature().into_debug(tokens, compile_unit)
            {
                // let debug_scope = debug.inner_scope(&outer_scope, location);

                let fn_param_ty = &mir_function.return_type.get_debug_type_hard(
                    compile_unit,
                    &debug,
                    &debug_types,
                    &type_values,
                    &types,
                    tokens,
                );

                let debug_ty = debug.debug_type(DebugTypeData::Subprogram(DebugSubprogramType {
                    parameters: vec![*fn_param_ty],
                    flags: DwarfFlags,
                }));

                let subprogram = debug.subprogram(DebugSubprogramData {
                    name: mir_function.name.clone(),
                    outer_scope: compile_unit.clone(),
                    location,
                    ty: debug_ty,
                    opts: DebugSubprogramOptionals {
                        is_local: !mir_function.is_pub,
                        is_definition: true,
                        ..DebugSubprogramOptionals::default()
                    },
                });

                function.ir.set_debug(subprogram);

                Some(subprogram)
            } else {
                None
            };

            // Compile actual IR part
            let mut stack_values = HashMap::new();
            for (i, (p_name, p_ty)) in mir_function.parameters.iter().enumerate() {
                // Codegen actual parameters
                let arg_name = format!("arg.{}", p_name);
                let param = entry
                    .build_named(format!("{}.get", arg_name), Instr::Param(i))
                    .unwrap();
                let alloca = entry
                    .build_named(
                        &arg_name,
                        Instr::Alloca(p_ty.get_type(&type_values, &types)),
                    )
                    .unwrap();
                entry
                    .build_named(format!("{}.store", arg_name), Instr::Store(alloca, param))
                    .unwrap();
                stack_values.insert(
                    p_name.clone(),
                    StackValue(
                        StackValueKind::mutable(p_ty.is_mutable(), alloca),
                        TypeKind::CodegenPtr(Box::new(p_ty.clone())),
                    ),
                );

                // Generate debug info
                if let (Some(debug_scope), Some(location)) = (
                    &debug_scope,
                    mir_function.signature().into_debug(tokens, compile_unit),
                ) {
                    debug.metadata(
                        &location,
                        DebugMetadata::ParamVar(DebugParamVariable {
                            name: p_name.clone(),
                            arg_idx: i as u32,
                            ty: p_ty.get_debug_type_hard(
                                *debug_scope,
                                &debug,
                                &debug_types,
                                &type_values,
                                &types,
                                tokens,
                            ),
                            always_preserve: true,
                            flags: DwarfFlags,
                        }),
                    );
                }
            }

            let mut scope = Scope {
                context,
                tokens,
                module: &module,
                module_id: self.module_id,
                function,
                block: entry,
                functions: &functions,
                types: &types,
                type_values: &type_values,
                stack_values,
                debug: debug_scope.and_then(|scope| {
                    Some(Debug {
                        info: &debug,
                        scope,
                        types: &debug_types,
                    })
                }),
            };

            match &mir_function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => {
                    let state = State::default();
                    if let Some(ret) = block.codegen(&mut scope, &state) {
                        scope.block.terminate(Term::Ret(ret.instr())).unwrap();
                    } else {
                        if !scope.block.delete_if_unused().unwrap() {
                            // Add a void return just in case if the block
                            // wasn't unused but didn't have a terminator yet
                            scope.block.terminate(Term::RetVoid).ok();
                        }
                    }

                    if let Some(debug) = scope.debug {
                        let location =
                            &block.return_meta().into_debug(tokens, debug.scope).unwrap();
                        let location = debug.info.location(&debug.scope, *location);
                        scope.block.set_terminator_location(location).unwrap();
                    }
                }
                mir::FunctionDefinitionKind::Extern(_) => {}
            }
        }

        ModuleCodegen { module }
    }
}

impl mir::Block {
    fn codegen<'ctx, 'a>(
        &self,
        mut scope: &mut Scope<'ctx, 'a>,
        state: &State,
    ) -> Option<StackValue> {
        for stmt in &self.statements {
            stmt.codegen(&mut scope, state).map(|s| {
                if let Some(debug) = &scope.debug {
                    let location = stmt.1.into_debug(scope.tokens, debug.scope).unwrap();
                    let loc_val = debug.info.location(&debug.scope, location);
                    s.instr().with_location(&mut scope.block, loc_val);
                }
            });
        }

        if let Some((kind, expr)) = &self.return_expression {
            let ret = expr.codegen(&mut scope, &mut state.load(true));
            match kind {
                mir::ReturnKind::Hard => {
                    scope.block.terminate(Term::Ret(ret?.instr())).unwrap();
                    None
                }
                mir::ReturnKind::Soft => ret,
            }
        } else {
            None
        }
    }
}

impl mir::Statement {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, state: &State) -> Option<StackValue> {
        let location = scope.debug.clone().map(|d| {
            let location = self.1.into_debug(scope.tokens, d.scope).unwrap();
            d.info.location(&d.scope, location)
        });

        match &self.0 {
            mir::StmtKind::Let(NamedVariableRef(ty, name, _), mutable, expression) => {
                let value = expression.codegen(scope, &state).unwrap();

                let alloca = scope
                    .block
                    .build_named(
                        name,
                        Instr::Alloca(value.1.get_type(scope.type_values, scope.types)),
                    )
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                scope
                    .block
                    .build_named(
                        format!("{}.store", name),
                        Instr::Store(alloca, value.instr()),
                    )
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
                    let location = self.1.into_debug(scope.tokens, debug.scope).unwrap();
                    let var = debug.info.metadata(
                        &location,
                        DebugMetadata::LocalVar(DebugLocalVariable {
                            name: name.clone(),
                            ty: ty.clone().get_debug_type(debug, scope),
                            always_preserve: true,
                            flags: DwarfFlags,
                        }),
                    );
                    alloca.add_record(
                        &mut scope.block,
                        InstructionDebugRecordData {
                            variable: var,
                            location,
                            kind: DebugRecordKind::Declare(value.instr()),
                            scope: debug.scope,
                        },
                    );
                }
                None
            }
            mir::StmtKind::Set(lhs, rhs) => {
                let lhs_value = lhs
                    .codegen(scope, &state.load(false))
                    .expect("non-returning LHS snuck into codegen!");

                let rhs_value = rhs.codegen(scope, state)?;

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

                None
            }
            mir::StmtKind::Import(_) => todo!(),
            mir::StmtKind::Expression(expression) => expression.codegen(scope, state),
        }
    }
}

impl mir::Expression {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, state: &State) -> Option<StackValue> {
        let location = if let Some(debug) = &scope.debug {
            Some(debug.info.location(
                &debug.scope,
                self.1.into_debug(scope.tokens, debug.scope).unwrap(),
            ))
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
                                            Instr::Load(
                                                v.0.instr(),
                                                inner.get_type(scope.type_values, scope.types),
                                            ),
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
            mir::ExprKind::BinOp(binop, lhs_exp, rhs_exp) => {
                let lhs = lhs_exp
                    .codegen(scope, state)
                    .expect("lhs has no return value")
                    .instr();
                let rhs = rhs_exp
                    .codegen(scope, state)
                    .expect("rhs has no return value")
                    .instr();
                let lhs_type = lhs_exp
                    .return_type(&Default::default(), scope.module_id)
                    .unwrap()
                    .1;
                let instr = match (
                    binop,
                    lhs_type.signed(),
                    lhs_type.category() == TypeCategory::Real,
                ) {
                    (mir::BinaryOperator::Add, _, false) => Instr::Add(lhs, rhs),
                    (mir::BinaryOperator::Add, _, true) => Instr::FAdd(lhs, rhs),
                    (mir::BinaryOperator::Minus, _, false) => Instr::Sub(lhs, rhs),
                    (mir::BinaryOperator::Minus, _, true) => Instr::FSub(lhs, rhs),
                    (mir::BinaryOperator::Mult, _, false) => Instr::Mul(lhs, rhs),
                    (mir::BinaryOperator::Mult, _, true) => Instr::FMul(lhs, rhs),
                    (mir::BinaryOperator::And, _, _) => Instr::And(lhs, rhs),
                    (mir::BinaryOperator::Cmp(i), _, false) => Instr::ICmp(i.predicate(), lhs, rhs),
                    (mir::BinaryOperator::Cmp(i), _, true) => Instr::FCmp(i.predicate(), lhs, rhs),
                };
                Some(StackValue(
                    StackValueKind::Immutable(scope.block.build(instr).unwrap()),
                    TypeKind::U32,
                ))
            }
            mir::ExprKind::FunctionCall(call) => {
                let ret_type_kind = call
                    .return_type
                    .known()
                    .expect("function return type unknown");

                let ret_type = ret_type_kind.get_type(scope.type_values, scope.types);

                let params = call
                    .parameters
                    .iter()
                    .map(|e| e.codegen(scope, &mut state.load(true)).unwrap().instr())
                    .collect();
                let callee = scope
                    .functions
                    .get(&call.name)
                    .expect("function not found!");

                let val = scope
                    .block
                    .build_named(
                        call.name.clone(),
                        Instr::FunctionCall(callee.ir.value(), params),
                    )
                    .unwrap();

                if let Some(debug) = &scope.debug {
                    let location = call.meta.into_debug(scope.tokens, debug.scope).unwrap();
                    let location_val = debug.info.location(&debug.scope, location);
                    val.with_location(&mut scope.block, location_val);
                }

                let ptr = if ret_type_kind != TypeKind::Void {
                    let ptr = scope
                        .block
                        .build_named(&call.name, Instr::Alloca(ret_type.clone()))
                        .unwrap();
                    scope
                        .block
                        .build_named(format!("{}.store", call.name), Instr::Store(ptr, val))
                        .unwrap();

                    Some(ptr)
                } else {
                    None
                };

                if let Some(ptr) = ptr {
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
                }
            }
            mir::ExprKind::If(if_expression) => if_expression.codegen(scope, state),
            mir::ExprKind::Block(block) => {
                let mut inner_scope = scope.with_block(scope.function.ir.block("inner"));
                if let Some(ret) = block.codegen(&mut inner_scope, state) {
                    inner_scope
                        .block
                        .terminate(Term::Br(scope.block.value()))
                        .unwrap();
                    Some(ret)
                } else {
                    None
                }
            }
            mir::ExprKind::Indexed(expression, val_t, idx_expr) => {
                let StackValue(kind, ty) = expression
                    .codegen(scope, &state.load(false))
                    .expect("array returned none!");
                let idx = idx_expr
                    .codegen(scope, &state.load(true))
                    .expect("index returned none!")
                    .instr();

                let first = scope
                    .block
                    .build_named("array.zero", Instr::Constant(ConstValue::U32(0)))
                    .unwrap();

                let TypeKind::CodegenPtr(inner) = ty else {
                    panic!();
                };

                let (ptr, contained_ty) = if let TypeKind::UserPtr(further_inner) = *inner.clone() {
                    dbg!(&further_inner, &val_t);
                    let loaded = scope
                        .block
                        .build_named(
                            "array.load",
                            Instr::Load(
                                kind.instr(),
                                inner.get_type(scope.type_values, scope.types),
                            ),
                        )
                        .unwrap();
                    (
                        scope
                            .block
                            .build_named(format!("array.gep"), Instr::GetElemPtr(loaded, vec![idx]))
                            .unwrap()
                            .maybe_location(&mut scope.block, location),
                        *further_inner,
                    )
                } else {
                    let TypeKind::Array(_, _) = *inner else {
                        panic!();
                    };
                    (
                        scope
                            .block
                            .build_named(
                                format!("array.gep"),
                                Instr::GetElemPtr(kind.instr(), vec![first, idx]),
                            )
                            .unwrap()
                            .maybe_location(&mut scope.block, location),
                        val_t.clone(),
                    )
                };

                if state.should_load {
                    dbg!(&contained_ty);
                    Some(StackValue(
                        kind.derive(
                            scope
                                .block
                                .build_named(
                                    "array.load",
                                    Instr::Load(
                                        ptr,
                                        contained_ty.get_type(scope.type_values, scope.types),
                                    ),
                                )
                                .unwrap()
                                .maybe_location(&mut scope.block, location),
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
                let stack_value_list = expressions
                    .iter()
                    .map(|e| e.codegen(scope, state).unwrap())
                    .collect::<Vec<_>>();
                let instr_list = stack_value_list
                    .iter()
                    .map(|s| s.instr())
                    .collect::<Vec<_>>();

                let elem_ty_kind = stack_value_list
                    .iter()
                    .map(|s| s.1.clone())
                    .next()
                    .unwrap_or(TypeKind::Void);

                let array_ty = Type::Array(
                    Box::new(elem_ty_kind.get_type(scope.type_values, scope.types)),
                    instr_list.len() as u64,
                );
                let array_name = format!("{}.{}", elem_ty_kind, instr_list.len());
                let load_n = format!("{}.load", array_name);

                let array = scope
                    .block
                    .build_named(&array_name, Instr::Alloca(array_ty.clone()))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                for (index, instr) in instr_list.iter().enumerate() {
                    let gep_n = format!("{}.{}.gep", array_name, index);
                    let store_n = format!("{}.{}.store", array_name, index);

                    let index_expr = scope
                        .block
                        .build_named(
                            index.to_string(),
                            Instr::Constant(ConstValue::U32(index as u32)),
                        )
                        .unwrap();
                    let first = scope
                        .block
                        .build_named("zero", Instr::Constant(ConstValue::U32(0)))
                        .unwrap();
                    let ptr = scope
                        .block
                        .build_named(gep_n, Instr::GetElemPtr(array, vec![first, index_expr]))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                    scope
                        .block
                        .build_named(store_n, Instr::Store(ptr, *instr))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                }

                let array_val = scope
                    .block
                    .build_named(load_n, Instr::Load(array, array_ty))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                Some(StackValue(
                    StackValueKind::Literal(array_val),
                    TypeKind::Array(Box::new(elem_ty_kind), instr_list.len() as u64),
                ))
            }
            mir::ExprKind::Accessed(expression, type_kind, field) => {
                let struct_val = expression.codegen(scope, &state.load(false)).unwrap();

                dbg!(&expression);
                let TypeKind::CodegenPtr(inner) = &struct_val.1 else {
                    panic!("tried accessing non-pointer");
                };
                let TypeKind::CustomType(key) = *inner.clone() else {
                    panic!("tried accessing non-custom-type");
                };
                let TypeDefinitionKind::Struct(struct_ty) =
                    scope.get_typedef(&key).unwrap().kind.clone();
                let idx = struct_ty.find_index(field).unwrap();

                let gep_n = format!("{}.{}.gep", key.0, field);
                let load_n = format!("{}.{}.load", key.0, field);

                let value = scope
                    .block
                    .build_named(
                        gep_n,
                        Instr::GetStructElemPtr(struct_val.instr(), idx as u32),
                    )
                    .unwrap();

                // value.maybe_location(&mut scope.block, location);

                if state.should_load {
                    Some(StackValue(
                        struct_val.0.derive(
                            scope
                                .block
                                .build_named(
                                    load_n,
                                    Instr::Load(
                                        value,
                                        type_kind.get_type(scope.type_values, scope.types),
                                    ),
                                )
                                .unwrap(),
                        ),
                        struct_ty.get_field_ty(&field).unwrap().clone(),
                    ))
                } else {
                    Some(StackValue(
                        struct_val.0.derive(value),
                        TypeKind::CodegenPtr(Box::new(
                            struct_ty.get_field_ty(&field).unwrap().clone(),
                        )),
                    ))
                }
            }
            mir::ExprKind::Struct(name, items) => {
                let type_key = TypeKey(name.clone(), scope.module_id);
                let struct_ty = Type::CustomType(*scope.type_values.get(&type_key)?);

                let load_n = format!("{}.load", name);

                let struct_ptr = scope
                    .block
                    .build_named(name, Instr::Alloca(struct_ty.clone()))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                for (i, (field_n, exp)) in items.iter().enumerate() {
                    let gep_n = format!("{}.{}.gep", name, field_n);
                    let store_n = format!("{}.{}.store", name, field_n);

                    let elem_ptr = scope
                        .block
                        .build_named(gep_n, Instr::GetStructElemPtr(struct_ptr, i as u32))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                    if let Some(val) = exp.codegen(scope, state) {
                        scope
                            .block
                            .build_named(store_n, Instr::Store(elem_ptr, val.instr()))
                            .unwrap()
                            .maybe_location(&mut scope.block, location);
                    }
                }

                let struct_val = scope
                    .block
                    .build_named(load_n, Instr::Load(struct_ptr, struct_ty))
                    .unwrap();

                Some(StackValue(
                    StackValueKind::Literal(struct_val),
                    TypeKind::CustomType(type_key),
                ))
            }
            mir::ExprKind::Borrow(varref, mutable) => {
                varref.0.known().expect("variable type unknown");
                let v = scope
                    .stack_values
                    .get(&varref.1)
                    .expect("Variable reference not found?!");
                Some(StackValue(
                    StackValueKind::mutable(*mutable, v.0.instr()),
                    v.1.clone(),
                ))
            }
            mir::ExprKind::Deref(varref) => {
                varref.0.known().expect("variable type unknown");
                let v = scope
                    .stack_values
                    .get(&varref.1)
                    .expect("Variable reference not found?!");

                let TypeKind::CodegenPtr(ptr_inner) = &v.1 else {
                    panic!();
                };

                let var_ptr_instr = scope
                    .block
                    .build_named(
                        format!("{}.deref", varref.1),
                        Instr::Load(
                            v.0.instr(),
                            ptr_inner.get_type(scope.type_values, scope.types),
                        ),
                    )
                    .unwrap();

                Some({
                    if state.should_load {
                        if let TypeKind::CodegenPtr(inner) = *ptr_inner.clone() {
                            StackValue(
                                v.0.derive(
                                    scope
                                        .block
                                        .build_named(
                                            format!("{}.deref.inner", varref.1),
                                            Instr::Load(
                                                var_ptr_instr,
                                                inner.get_type(scope.type_values, scope.types),
                                            ),
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
                            *borrow_inner.clone(),
                        )
                    }
                })
            }
            mir::ExprKind::CastTo(expression, type_kind) => {
                let val = expression.codegen(scope, state)?;
                if val.1 == *type_kind {
                    Some(val)
                } else if let (TypeKind::UserPtr(_), TypeKind::UserPtr(_)) = (&val.1, type_kind) {
                    Some(val)
                } else {
                    let cast_instr = val
                        .1
                        .get_type(scope.type_values, scope.types)
                        .cast_instruction(
                            val.instr(),
                            &type_kind.get_type(scope.type_values, scope.types),
                        )
                        .unwrap();

                    Some(StackValue(
                        val.0.derive(scope.block.build(cast_instr).unwrap()),
                        type_kind.clone(),
                    ))
                }
            }
        };
        if let Some(value) = &value {
            value.instr().maybe_location(&mut scope.block, location);
        }
        value
    }
}

impl mir::IfExpression {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, state: &State) -> Option<StackValue> {
        let condition = self.0.codegen(scope, state).unwrap();

        // Create blocks
        let mut then_b = scope.function.ir.block("then");
        let mut else_b = scope.function.ir.block("else");
        let after_b = scope.function.ir.block("after");

        if let Some(debug) = &scope.debug {
            let before_location = self.0 .1.into_debug(scope.tokens, debug.scope).unwrap();
            let before_v = debug.info.location(&debug.scope, before_location);
            scope.block.set_terminator_location(before_v).unwrap();

            let then_location = self
                .1
                .return_meta()
                .into_debug(scope.tokens, debug.scope)
                .unwrap();
            let then_v = debug.info.location(&debug.scope, then_location);
            then_b.set_terminator_location(then_v).unwrap();

            let else_location = if let Some(else_block) = &self.2 {
                else_block
                    .return_meta()
                    .into_debug(scope.tokens, debug.scope)
                    .unwrap()
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
        let then_res = self.1.codegen(&mut then_scope, state);
        then_scope.block.terminate(Term::Br(after_bb)).ok();

        let else_res = if let Some(else_block) = &self.2 {
            let mut else_scope = scope.with_block(else_b);

            scope
                .block
                .terminate(Term::CondBr(condition.instr(), then_bb, else_bb))
                .unwrap();

            let opt = else_block.codegen(&mut else_scope, state);

            if let Some(ret) = opt {
                else_scope.block.terminate(Term::Br(after_bb)).ok();
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
            None
        } else {
            let mut incoming = Vec::from(then_res.as_slice());
            incoming.extend(else_res.clone());
            let instr = scope
                .block
                .build_named(
                    "phi",
                    Instr::Phi(incoming.iter().map(|i| i.instr()).collect()),
                )
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
            Some(value)
        }
    }
}
impl mir::CmpOperator {
    fn predicate(&self) -> CmpPredicate {
        match self {
            mir::CmpOperator::LT => CmpPredicate::LT,
            mir::CmpOperator::GT => CmpPredicate::GT,
            mir::CmpOperator::LE => CmpPredicate::LE,
            mir::CmpOperator::GE => CmpPredicate::GE,
            mir::CmpOperator::EQ => CmpPredicate::EQ,
            mir::CmpOperator::NE => CmpPredicate::NE,
        }
    }
}

impl mir::Literal {
    fn as_const(&self, block: &mut Block) -> InstructionValue {
        block
            .build_named(format!("{}", self), self.as_const_kind())
            .unwrap()
    }

    fn as_const_kind(&self) -> Instr {
        Instr::Constant(match self.clone() {
            mir::Literal::I8(val) => ConstValue::I8(val),
            mir::Literal::I16(val) => ConstValue::I16(val),
            mir::Literal::I32(val) => ConstValue::I32(val),
            mir::Literal::I64(val) => ConstValue::I64(val),
            mir::Literal::I128(val) => ConstValue::I128(val),
            mir::Literal::U8(val) => ConstValue::U8(val),
            mir::Literal::U16(val) => ConstValue::U16(val),
            mir::Literal::U32(val) => ConstValue::U32(val),
            mir::Literal::U64(val) => ConstValue::U64(val),
            mir::Literal::U128(val) => ConstValue::U128(val),
            mir::Literal::Bool(val) => ConstValue::Bool(val),
            mir::Literal::String(val) => ConstValue::Str(val.clone()),
            mir::Literal::Vague(VagueLiteral::Number(val)) => ConstValue::I32(val as i32),
            mir::Literal::Vague(VagueLiteral::Decimal(val)) => ConstValue::F32(val as f32),
            mir::Literal::F16(val) => ConstValue::F16(val),
            mir::Literal::F32B(val) => ConstValue::F32B(val),
            mir::Literal::F32(val) => ConstValue::F32(val),
            mir::Literal::F64(val) => ConstValue::F64(val),
            mir::Literal::F80(val) => ConstValue::F80(val),
            mir::Literal::F128(val) => ConstValue::F128(val),
            mir::Literal::F128PPC(val) => ConstValue::F128PPC(val),
            mir::Literal::Char(c) => ConstValue::U8(c as u8),
        })
    }
}

impl TypeKind {
    fn get_type(
        &self,
        type_vals: &HashMap<TypeKey, TypeValue>,
        typedefs: &HashMap<TypeValue, TypeDefinition>,
    ) -> Type {
        match &self {
            TypeKind::I8 => Type::I8,
            TypeKind::I16 => Type::I16,
            TypeKind::I32 => Type::I32,
            TypeKind::I64 => Type::I64,
            TypeKind::I128 => Type::I128,
            TypeKind::U8 => Type::U8,
            TypeKind::U16 => Type::U16,
            TypeKind::U32 => Type::U32,
            TypeKind::U64 => Type::U64,
            TypeKind::U128 => Type::U128,
            TypeKind::Bool => Type::Bool,
            TypeKind::F16 => Type::F16,
            TypeKind::F32B => Type::F32B,
            TypeKind::F32 => Type::F32,
            TypeKind::F64 => Type::F64,
            TypeKind::F128 => Type::F128,
            TypeKind::F80 => Type::F80,
            TypeKind::F128PPC => Type::F128PPC,
            TypeKind::Char => Type::I8,
            TypeKind::Array(elem_t, len) => {
                Type::Array(Box::new(elem_t.get_type(type_vals, typedefs)), *len)
            }
            TypeKind::Void => Type::Void,
            TypeKind::Vague(_) => panic!("Tried to compile a vague type!"),
            TypeKind::CustomType(n) => {
                let type_val = type_vals.get(n).unwrap().clone();
                Type::CustomType(type_val)
            }
            TypeKind::UserPtr(type_kind) => {
                Type::Ptr(Box::new(type_kind.get_type(type_vals, typedefs)))
            }
            TypeKind::CodegenPtr(type_kind) => {
                Type::Ptr(Box::new(type_kind.get_type(type_vals, typedefs)))
            }
            TypeKind::Borrow(type_kind, _) => {
                Type::Ptr(Box::new(type_kind.get_type(type_vals, typedefs)))
            }
        }
    }
}

impl TypeKind {
    fn get_debug_type(&self, debug: &Debug, scope: &Scope) -> DebugTypeValue {
        self.get_debug_type_hard(
            debug.scope,
            debug.info,
            debug.types,
            scope.type_values,
            scope.types,
            scope.tokens,
        )
    }

    fn get_debug_type_hard(
        &self,
        scope: DebugProgramValue,
        debug_info: &DebugInformation,
        debug_types: &HashMap<TypeKind, DebugTypeValue>,
        type_values: &HashMap<TypeKey, TypeValue>,
        types: &HashMap<TypeValue, TypeDefinition>,
        tokens: &Vec<FullToken>,
    ) -> DebugTypeValue {
        if let Some(ty) = debug_types.get(self) {
            return *ty;
        }

        let name = format!("{}", self);

        let data = match self {
            TypeKind::CodegenPtr(inner) | TypeKind::UserPtr(inner) | TypeKind::Borrow(inner, _) => {
                DebugTypeData::Pointer(DebugPointerType {
                    name,
                    pointee: inner.get_debug_type_hard(
                        scope,
                        debug_info,
                        debug_types,
                        type_values,
                        types,
                        tokens,
                    ),
                    size_bits: self.size_of(),
                })
            }
            TypeKind::Array(elem_ty, len) => {
                let elem_ty = elem_ty.clone().get_debug_type_hard(
                    scope,
                    debug_info,
                    debug_types,
                    type_values,
                    types,
                    tokens,
                );
                DebugTypeData::Array(DebugArrayType {
                    size_bits: self.size_of(),
                    align_bits: self.alignment(),
                    element_type: elem_ty,
                    length: *len,
                })
            }
            TypeKind::CustomType(key) => {
                let typedef = types.get(type_values.get(key).unwrap()).unwrap();

                match &typedef.kind {
                    TypeDefinitionKind::Struct(struct_type) => {
                        let mut fields = Vec::new();
                        let mut size_bits = 0;
                        for field in &struct_type.0 {
                            fields.push(DebugFieldType {
                                name: field.0.clone(),
                                location: field.2.into_debug(tokens, scope).unwrap(),
                                size_bits: field.1.size_of(),
                                offset: size_bits,
                                flags: DwarfFlags,
                                ty: field.1.get_debug_type_hard(
                                    scope,
                                    debug_info,
                                    debug_types,
                                    type_values,
                                    types,
                                    tokens,
                                ),
                            });
                            size_bits += field.1.size_of();
                        }
                        {
                            DebugTypeData::Struct(DebugStructType {
                                name: key.0.clone(),
                                location: typedef.meta.into_debug(tokens, scope).unwrap(),
                                size_bits,
                                flags: DwarfFlags,
                                fields,
                            })
                        }
                    }
                }
            }
            _ => DebugTypeData::Basic(DebugBasicType {
                name,
                size_bits: self.size_of(),
                encoding: match self {
                    TypeKind::Bool => DwarfEncoding::Boolean,
                    TypeKind::I8 => DwarfEncoding::SignedChar,
                    TypeKind::U8 => DwarfEncoding::UnsignedChar,
                    TypeKind::I16 | TypeKind::I32 | TypeKind::I64 | TypeKind::I128 => {
                        DwarfEncoding::Signed
                    }
                    TypeKind::U16 | TypeKind::U32 | TypeKind::U64 | TypeKind::U128 => {
                        DwarfEncoding::Unsigned
                    }
                    TypeKind::F16
                    | TypeKind::F32
                    | TypeKind::F32B
                    | TypeKind::F64
                    | TypeKind::F80
                    | TypeKind::F128
                    | TypeKind::F128PPC => DwarfEncoding::Float,
                    TypeKind::Void => DwarfEncoding::Address,
                    TypeKind::Char => DwarfEncoding::UnsignedChar,
                    TypeKind::Array(_, _) => DwarfEncoding::Address,
                    TypeKind::CustomType(_) => DwarfEncoding::Address,
                    _ => panic!("tried fetching debug-type for non-supported type!"),
                },
                flags: DwarfFlags,
            }),
        };
        debug_info.debug_type(data)
    }
}

impl Metadata {
    pub fn into_debug(
        &self,
        tokens: &Vec<FullToken>,
        scope: DebugProgramValue,
    ) -> Option<DebugLocation> {
        if let Some((start, _)) = self.into_positions(tokens) {
            Some(start.debug(scope))
        } else {
            Some(Position(0, 0).debug(scope))
        }
    }
}

impl Position {
    fn debug(self, scope: DebugProgramValue) -> DebugLocation {
        DebugLocation {
            line: self.1,
            column: self.0,
            scope,
        }
    }
}
