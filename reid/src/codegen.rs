use std::{collections::HashMap, mem};

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    compile::CompiledModule,
    debug_information::{
        DebugBasicType, DebugFileData, DebugInformation, DebugLocation, DebugMetadata,
        DebugMetadataValue, DebugParamVariable, DebugProgramValue, DebugScopeValue,
        DebugSubprogramData, DebugSubprogramOptionals, DebugSubprogramTypeData, DebugTypeData,
        DebugTypeValue, DwarfEncoding, DwarfFlags,
    },
    Block, CmpPredicate, ConstValue, Context, CustomTypeKind, Function, FunctionFlags, Instr,
    Module, NamedStruct, TerminatorKind as Term, Type,
};

use crate::{
    error_raporting::ModuleMap,
    lexer::{FullToken, Position},
    mir::{
        self, Metadata, NamedVariableRef, StructField, StructType, TypeDefinitionKind, TypeKind,
        VagueLiteral,
    },
};

/// Context that contains all of the given modules as complete codegenerated
/// LLIR that can then be finally compiled into LLVM IR.
#[derive(Debug)]
pub struct CodegenContext<'ctx> {
    context: &'ctx Context,
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
    pub fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        mod_map: &ModuleMap,
    ) -> CodegenContext<'ctx> {
        let mut modules = Vec::new();
        for module in &self.modules {
            modules.push(
                module.codegen(
                    context,
                    mod_map
                        .module(&module.module_id)
                        .unwrap()
                        .tokens
                        .as_ref()
                        .unwrap(),
                ),
            );
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
    function: &'ctx StackFunction<'ctx>,
    block: Block<'ctx>,
    types: &'a HashMap<TypeValue, TypeDefinitionKind>,
    type_values: &'a HashMap<String, TypeValue>,
    functions: &'a HashMap<String, StackFunction<'ctx>>,
    stack_values: HashMap<String, StackValue>,
    debug: Option<Debug<'ctx>>,
    debug_const_tys: &'a HashMap<TypeKind, DebugTypeValue>,
}

#[derive(Debug, Clone)]
pub struct Debug<'ctx> {
    info: &'ctx DebugInformation,
    scope: DebugProgramValue,
}

pub struct StackFunction<'ctx> {
    ir: Function<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackValue(StackValueKind, Type);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackValueKind {
    Immutable(InstructionValue),
    Mutable(InstructionValue),
    Any(InstructionValue),
}

impl<'ctx, 'a> Scope<'ctx, 'a> {
    fn with_block(&self, block: Block<'ctx>) -> Scope<'ctx, 'a> {
        Scope {
            block,
            tokens: self.tokens,
            function: self.function,
            context: self.context,
            module: self.module,
            functions: self.functions,
            types: self.types,
            type_values: self.type_values,
            stack_values: self.stack_values.clone(),
            debug: self.debug.clone(),
            debug_const_tys: self.debug_const_tys,
        }
    }

    /// Takes the block out from this scope, swaps the given block in it's place
    /// and returns the old block.
    fn swap_block(&mut self, block: Block<'ctx>) -> Block<'ctx> {
        let mut old_block = block;
        mem::swap(&mut self.block, &mut old_block);
        old_block
    }

    fn get_typedef(&self, name: &String) -> Option<&TypeDefinitionKind> {
        self.type_values.get(name).and_then(|v| self.types.get(v))
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
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        tokens: &Vec<FullToken>,
    ) -> ModuleCodegen<'ctx> {
        let mut module = context.module(&self.name, self.is_main);

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
        let mut debug_const_types = HashMap::new();

        debug_const_types.insert(
            TypeKind::U32,
            debug.debug_type(DebugTypeData::Basic(DebugBasicType {
                name: String::from("u32"),
                size_bits: 32,
                encoding: DwarfEncoding::Unsigned,
                flags: DwarfFlags,
            })),
        );

        for typedef in &self.typedefs {
            let type_value = match &typedef.kind {
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
            };
            types.insert(type_value, typedef.kind.clone());
            type_values.insert(typedef.name.clone(), type_value);
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
            let debug_scope = if let Some(location) = mir_function.signature().into_debug(tokens) {
                // let debug_scope = debug.inner_scope(&outer_scope, location);

                let fn_param_ty = debug_const_types.get(&TypeKind::U32).unwrap();

                let debug_ty =
                    debug.debug_type(DebugTypeData::Subprogram(DebugSubprogramTypeData {
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
                stack_values.insert(
                    p_name.clone(),
                    StackValue(
                        StackValueKind::Immutable(entry.build(Instr::Param(i)).unwrap()),
                        p_ty.get_type(&type_values, &types),
                    ),
                );

                // Generate debug info
                if let (Some(debug_scope), Some(location)) =
                    (&debug_scope, mir_function.signature().into_debug(tokens))
                {
                    debug.metadata(
                        &debug_scope,
                        DebugMetadata::ParamVar(DebugParamVariable {
                            name: p_name.clone(),
                            arg_idx: i as u32,
                            location,
                            ty: *debug_const_types.get(&TypeKind::U32).unwrap(),
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
                    })
                }),
                debug_const_tys: &debug_const_types,
            };

            match &mir_function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => {
                    let state = State::default();
                    if let Some(ret) = block.codegen(&mut scope, &state) {
                        scope.block.terminate(Term::Ret(ret)).unwrap();
                    } else {
                        if !scope.block.delete_if_unused().unwrap() {
                            // Add a void return just in case if the block
                            // wasn't unused but didn't have a terminator yet
                            scope.block.terminate(Term::RetVoid).ok();
                        }
                    }

                    if let Some(debug) = scope.debug {
                        let location = &block.return_meta().into_debug(tokens).unwrap();
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
    ) -> Option<InstructionValue> {
        for stmt in &self.statements {
            stmt.codegen(&mut scope, state).map(|s| {
                if let Some(debug) = &scope.debug {
                    let location = stmt.1.into_debug(scope.tokens).unwrap();
                    let loc_val = debug.info.location(&debug.scope, location);
                    s.with_location(&mut scope.block, loc_val);
                }
            });
        }

        if let Some((kind, expr)) = &self.return_expression {
            match kind {
                mir::ReturnKind::Hard => {
                    let ret = expr.codegen(&mut scope, &state)?;
                    scope.block.terminate(Term::Ret(ret)).unwrap();
                    None
                }
                mir::ReturnKind::Soft => expr.codegen(&mut scope, state),
            }
        } else {
            None
        }
    }
}

impl mir::Statement {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        state: &State,
    ) -> Option<InstructionValue> {
        let location = self.1.into_debug(scope.tokens).unwrap();
        let location = scope
            .debug
            .as_ref()
            .map(|d| d.info.location(&d.scope, location));

        match &self.0 {
            mir::StmtKind::Let(NamedVariableRef(ty, name, _), mutable, expression) => {
                let value = expression.codegen(scope, &state).unwrap();
                scope.stack_values.insert(
                    name.clone(),
                    StackValue(
                        match mutable {
                            false => StackValueKind::Immutable(value),
                            true => match ty {
                                // Struct is already allocated at initialization
                                TypeKind::Array(_, _) => StackValueKind::Mutable(value),
                                TypeKind::CustomType(n) => match scope
                                    .types
                                    .get(scope.type_values.get(n).unwrap())
                                    .unwrap()
                                {
                                    // Struct also is allocated at initialization
                                    TypeDefinitionKind::Struct(_) => StackValueKind::Mutable(value),
                                },
                                _ => StackValueKind::Mutable({
                                    let alloca = scope
                                        .block
                                        .build(Instr::Alloca(
                                            name.clone(),
                                            ty.get_type(scope.type_values, scope.types),
                                        ))
                                        .unwrap()
                                        .maybe_location(&mut scope.block, location);
                                    scope
                                        .block
                                        .build(Instr::Store(alloca, value))
                                        .unwrap()
                                        .maybe_location(&mut scope.block, location);
                                    alloca
                                }),
                            },
                        },
                        ty.get_type(scope.type_values, scope.types),
                    ),
                );
                None
            }
            mir::StmtKind::Set(lhs, rhs) => {
                let lhs_value = lhs
                    .codegen(scope, &state.load(false))
                    .expect("non-returning LHS snuck into codegen!");

                let rhs_value = rhs.codegen(scope, state)?;

                Some(
                    scope
                        .block
                        .build(Instr::Store(lhs_value, rhs_value))
                        .unwrap()
                        .maybe_location(&mut scope.block, location),
                )
            }
            mir::StmtKind::Import(_) => todo!(),
            mir::StmtKind::Expression(expression) => expression.codegen(scope, state),
        }
    }
}

impl mir::Expression {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        state: &State,
    ) -> Option<InstructionValue> {
        let location = if let Some(debug) = &scope.debug {
            Some(
                debug
                    .info
                    .location(&debug.scope, self.1.into_debug(scope.tokens).unwrap()),
            )
        } else {
            None
        };

        match &self.0 {
            mir::ExprKind::Variable(varref) => {
                varref.0.known().expect("variable type unknown");
                let v = scope
                    .stack_values
                    .get(&varref.1)
                    .expect("Variable reference not found?!");
                Some(match v.0 {
                    StackValueKind::Immutable(val) => val.clone(),
                    StackValueKind::Mutable(val) => {
                        if state.should_load {
                            match v.1 {
                                // TODO probably wrong ..?
                                Type::Ptr(_) => val,
                                _ => scope.block.build(Instr::Load(val, v.1.clone())).unwrap(),
                            }
                        } else {
                            val
                        }
                    }
                    _ => panic!("Found an unknown-mutable variable!"),
                })
            }
            mir::ExprKind::Literal(lit) => Some(lit.as_const(&mut scope.block)),
            mir::ExprKind::BinOp(binop, lhs_exp, rhs_exp) => {
                lhs_exp
                    .return_type()
                    .expect("No ret type in lhs?")
                    .1
                    .known()
                    .expect("lhs ret type is unknown");
                rhs_exp
                    .return_type()
                    .expect("No ret type in rhs?")
                    .1
                    .known()
                    .expect("rhs ret type is unknown");

                let lhs = lhs_exp
                    .codegen(scope, state)
                    .expect("lhs has no return value");
                let rhs = rhs_exp
                    .codegen(scope, state)
                    .expect("rhs has no return value");
                Some(match binop {
                    mir::BinaryOperator::Add => scope.block.build(Instr::Add(lhs, rhs)).unwrap(),
                    mir::BinaryOperator::Minus => scope.block.build(Instr::Sub(lhs, rhs)).unwrap(),
                    mir::BinaryOperator::Mult => scope.block.build(Instr::Mult(lhs, rhs)).unwrap(),
                    mir::BinaryOperator::And => scope.block.build(Instr::And(lhs, rhs)).unwrap(),
                    mir::BinaryOperator::Cmp(l) => scope
                        .block
                        .build(Instr::ICmp(l.int_predicate(), lhs, rhs))
                        .unwrap(),
                })
            }
            mir::ExprKind::FunctionCall(call) => {
                call.return_type
                    .known()
                    .expect("function return type unknown");

                let params = call
                    .parameters
                    .iter()
                    .map(|e| e.codegen(scope, state).unwrap())
                    .collect();
                let callee = scope
                    .functions
                    .get(&call.name)
                    .expect("function not found!");
                Some(
                    scope
                        .block
                        .build(Instr::FunctionCall(callee.ir.value(), params))
                        .unwrap(),
                )
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
                let array = expression
                    .codegen(scope, &state.load(true))
                    .expect("array returned none!");
                let idx = idx_expr
                    .codegen(scope, &state.load(true))
                    .expect("index returned none!");

                let mut ptr = scope
                    .block
                    .build(Instr::GetElemPtr(array, vec![idx]))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                if state.should_load {
                    ptr = scope
                        .block
                        .build(Instr::Load(
                            ptr,
                            val_t.get_type(scope.type_values, scope.types),
                        ))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                }

                Some(ptr)
            }
            mir::ExprKind::Array(expressions) => {
                let instr_list = expressions
                    .iter()
                    .map(|e| e.codegen(scope, state).unwrap())
                    .collect::<Vec<_>>();
                let instr_t = expressions
                    .iter()
                    .map(|e| e.return_type().unwrap().1)
                    .next()
                    .unwrap_or(TypeKind::Void);

                let array = scope
                    .block
                    .build(Instr::ArrayAlloca(
                        instr_t.get_type(scope.type_values, scope.types),
                        instr_list.len() as u32,
                    ))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                for (index, instr) in instr_list.iter().enumerate() {
                    let index_expr = scope
                        .block
                        .build(Instr::Constant(ConstValue::U32(index as u32)))
                        .unwrap();
                    let ptr = scope
                        .block
                        .build(Instr::GetElemPtr(array, vec![index_expr]))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                    scope
                        .block
                        .build(Instr::Store(ptr, *instr))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                }

                Some(array)
            }
            mir::ExprKind::Accessed(expression, type_kind, field) => {
                let struct_val = expression.codegen(scope, &mut state.load(true)).unwrap();

                let struct_ty = expression
                    .return_type()
                    .map(|r| r.1.known())
                    .unwrap()
                    .unwrap();

                let TypeKind::CustomType(name) = struct_ty.deref_borrow() else {
                    panic!("tried accessing non-custom-type");
                };
                let TypeDefinitionKind::Struct(struct_ty) = scope.get_typedef(&name).unwrap();
                let idx = struct_ty.find_index(field).unwrap();

                dbg!(&scope.context);
                dbg!(&struct_val);
                let mut value = scope
                    .block
                    .build(Instr::GetStructElemPtr(struct_val, idx as u32))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                if state.should_load {
                    value = scope
                        .block
                        .build(Instr::Load(
                            value,
                            type_kind.get_type(scope.type_values, scope.types),
                        ))
                        .unwrap();
                }

                Some(value)
            }
            mir::ExprKind::Struct(name, items) => {
                let struct_ptr = scope
                    .block
                    .build(Instr::Alloca(
                        name.clone(),
                        Type::CustomType(*scope.type_values.get(name)?),
                    ))
                    .unwrap()
                    .maybe_location(&mut scope.block, location);

                for (i, (_, exp)) in items.iter().enumerate() {
                    let elem_ptr = scope
                        .block
                        .build(Instr::GetStructElemPtr(struct_ptr, i as u32))
                        .unwrap()
                        .maybe_location(&mut scope.block, location);
                    if let Some(val) = exp.codegen(scope, state) {
                        scope
                            .block
                            .build(Instr::Store(elem_ptr, val))
                            .unwrap()
                            .maybe_location(&mut scope.block, location);
                    }
                }

                Some(struct_ptr)
            }
        }
        .map(|i| i.maybe_location(&mut scope.block, location))
    }
}

impl mir::IfExpression {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        state: &State,
    ) -> Option<InstructionValue> {
        let condition = self.0.codegen(scope, state).unwrap();

        // Create blocks
        let mut then_b = scope.function.ir.block("then");
        let mut else_b = scope.function.ir.block("else");
        let after_b = scope.function.ir.block("after");

        if let Some(debug) = &scope.debug {
            let before_location = self.0 .1.into_debug(scope.tokens).unwrap();
            let before_v = debug.info.location(&debug.scope, before_location);
            scope.block.set_terminator_location(before_v).unwrap();

            let then_location = self.1.return_meta().into_debug(scope.tokens).unwrap();
            let then_v = debug.info.location(&debug.scope, then_location);
            then_b.set_terminator_location(then_v).unwrap();

            let else_location = if let Some(else_block) = &self.2 {
                else_block.return_meta().into_debug(scope.tokens).unwrap()
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
                .terminate(Term::CondBr(condition, then_bb, else_bb))
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
                .terminate(Term::CondBr(condition, then_bb, after_bb))
                .unwrap();
            None
        };

        // Swap block to the after-block so that construction can continue correctly
        scope.swap_block(after_b);

        if then_res.is_none() && else_res.is_none() {
            None
        } else {
            let mut incoming = Vec::from(then_res.as_slice());
            incoming.extend(else_res);
            Some(scope.block.build(Instr::Phi(incoming)).unwrap())
        }
    }
}
impl mir::CmpOperator {
    fn int_predicate(&self) -> CmpPredicate {
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
        block.build(self.as_const_kind()).unwrap()
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
            mir::Literal::String(val) => ConstValue::StringPtr(val.clone()),
            mir::Literal::Vague(VagueLiteral::Number(val)) => ConstValue::I32(val as i32),
        })
    }
}

impl TypeKind {
    fn get_type(
        &self,
        type_vals: &HashMap<String, TypeValue>,
        typedefs: &HashMap<TypeValue, TypeDefinitionKind>,
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
            TypeKind::StringPtr => Type::Ptr(Box::new(Type::I8)),
            TypeKind::Array(elem_t, _) => Type::Ptr(Box::new(elem_t.get_type(type_vals, typedefs))),
            TypeKind::Void => Type::Void,
            TypeKind::Vague(_) => panic!("Tried to compile a vague type!"),
            TypeKind::CustomType(n) => {
                let type_val = type_vals.get(n).unwrap().clone();
                let custom_t = Type::CustomType(type_val);
                match typedefs.get(&type_val).unwrap() {
                    TypeDefinitionKind::Struct(_) => Type::Ptr(Box::new(custom_t)),
                }
            }
            TypeKind::Borrow(type_kind) => {
                Type::Ptr(Box::new(type_kind.get_type(type_vals, typedefs)))
            }
        }
    }
}

impl Metadata {
    pub fn into_debug(&self, tokens: &Vec<FullToken>) -> Option<DebugLocation> {
        if let Some((start, _)) = self.into_positions(tokens) {
            Some(start.into())
        } else {
            None
        }
    }
}

impl Into<DebugLocation> for Position {
    fn into(self) -> DebugLocation {
        DebugLocation {
            line: self.1,
            column: self.0,
        }
    }
}
