use std::{collections::HashMap, mem};

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    compile::CompiledModule,
    Block, CmpPredicate, ConstValue, Context, CustomTypeKind, Function, FunctionFlags, Instr,
    Module, NamedStruct, TerminatorKind as Term, Type,
};

use crate::mir::{
    self, NamedVariableRef, StructField, StructType, TypeDefinitionKind, TypeKind, VagueLiteral,
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
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> CodegenContext<'ctx> {
        let mut modules = Vec::new();
        for module in &self.modules {
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
    module: &'ctx Module<'ctx>,
    function: &'ctx Function<'ctx>,
    block: Block<'ctx>,
    types: &'a HashMap<TypeValue, TypeDefinitionKind>,
    type_values: &'a HashMap<String, TypeValue>,
    functions: &'a HashMap<String, Function<'ctx>>,
    stack_values: HashMap<String, StackValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackValue(StackValueKind, Type);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackValueKind {
    Immutable(InstructionValue),
    Mutable(InstructionValue),
    Any(InstructionValue),
}

impl StackValueKind {
    unsafe fn get_instr(&self) -> &InstructionValue {
        match self {
            StackValueKind::Immutable(val) => val,
            StackValueKind::Mutable(val) => val,
            StackValueKind::Any(val) => val,
        }
    }

    fn with_instr(&self, instr: InstructionValue) -> StackValueKind {
        match self {
            StackValueKind::Immutable(_) => StackValueKind::Immutable(instr),
            StackValueKind::Mutable(_) => StackValueKind::Mutable(instr),
            StackValueKind::Any(_) => StackValueKind::Any(instr),
        }
    }
}

impl<'ctx, 'a> Scope<'ctx, 'a> {
    fn with_block(&self, block: Block<'ctx>) -> Scope<'ctx, 'a> {
        Scope {
            block,
            function: self.function,
            context: self.context,
            module: self.module,
            functions: self.functions,
            types: self.types,
            type_values: self.type_values,
            stack_values: self.stack_values.clone(),
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone, Copy)]
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

impl mir::Module {
    fn codegen<'ctx>(&self, context: &'ctx Context) -> ModuleCodegen<'ctx> {
        let mut module = context.module(&self.name, self.is_main);

        let mut types = HashMap::new();
        let mut type_values = HashMap::new();

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
            functions.insert(function.name.clone(), func);
        }

        for mir_function in &self.functions {
            let function = functions.get(&mir_function.name).unwrap();
            let mut entry = function.block("entry");

            let mut stack_values = HashMap::new();
            for (i, (p_name, p_ty)) in mir_function.parameters.iter().enumerate() {
                stack_values.insert(
                    p_name.clone(),
                    StackValue(
                        StackValueKind::Immutable(entry.build(Instr::Param(i)).unwrap()),
                        p_ty.get_type(&type_values, &types),
                    ),
                );
            }

            let mut scope = Scope {
                context,
                module: &module,
                function,
                block: entry,
                functions: &functions,
                types: &types,
                type_values: &type_values,
                stack_values,
            };
            match &mir_function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => {
                    let mut state = State::default();
                    if let Some(ret) = block.codegen(&mut scope, &mut state) {
                        scope.block.terminate(Term::Ret(ret)).unwrap();
                    } else {
                        if !scope.block.delete_if_unused().unwrap() {
                            // Add a void return just in case if the block
                            // wasn't unused but didn't have a terminator yet
                            scope.block.terminate(Term::RetVoid).ok();
                        }
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
            stmt.codegen(&mut scope, state);
        }

        if let Some((kind, expr)) = &self.return_expression {
            match kind {
                mir::ReturnKind::Hard => {
                    let ret = expr.codegen(&mut scope, &mut state.load(true))?;
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
        match &self.0 {
            mir::StmtKind::Let(NamedVariableRef(ty, name, _), mutable, expression) => {
                let value = expression.codegen(scope, &state.load(true)).unwrap();
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
                                        .unwrap();
                                    scope.block.build(Instr::Store(alloca, value)).unwrap();
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
                    .codegen(scope, &mut state.load(false))
                    .expect("non-returning LHS snuck into codegen!");

                let rhs_value = rhs.codegen(scope, state)?;

                Some(
                    scope
                        .block
                        .build(Instr::Store(lhs_value, rhs_value))
                        .unwrap(),
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
                        .build(Instr::FunctionCall(callee.value(), params))
                        .unwrap(),
                )
            }
            mir::ExprKind::If(if_expression) => if_expression.codegen(scope, state),
            mir::ExprKind::Block(block) => {
                let mut inner_scope = scope.with_block(scope.function.block("inner"));
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
                    .unwrap();

                if state.should_load {
                    ptr = scope
                        .block
                        .build(Instr::Load(
                            ptr,
                            val_t.get_type(scope.type_values, scope.types),
                        ))
                        .unwrap();
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
                    .unwrap();

                for (index, instr) in instr_list.iter().enumerate() {
                    let index_expr = scope
                        .block
                        .build(Instr::Constant(ConstValue::U32(index as u32)))
                        .unwrap();
                    let ptr = scope
                        .block
                        .build(Instr::GetElemPtr(array, vec![index_expr]))
                        .unwrap();
                    scope.block.build(Instr::Store(ptr, *instr)).unwrap();
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
                    .unwrap();

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
                    .unwrap();

                for (i, (_, exp)) in items.iter().enumerate() {
                    let elem_ptr = scope
                        .block
                        .build(Instr::GetStructElemPtr(struct_ptr, i as u32))
                        .unwrap();
                    if let Some(val) = exp.codegen(scope, state) {
                        scope.block.build(Instr::Store(elem_ptr, val)).unwrap();
                    }
                }

                Some(struct_ptr)
            }
        }
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
        let then_b = scope.function.block("then");
        let mut else_b = scope.function.block("else");
        let after_b = scope.function.block("after");

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
