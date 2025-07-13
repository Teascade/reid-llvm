use std::{collections::HashMap, mem};

use reid_lib::{
    builder::InstructionValue, Block, CmpPredicate, ConstValue, Context, Function, Instr, Module,
    TerminatorKind as Term, Type,
};

use crate::mir::{self, types::ReturnType, NamedVariableRef, TypeKind};

/// Context that contains all of the given modules as complete codegenerated
/// LLIR that can then be finally compiled into LLVM IR.
#[derive(Debug)]
pub struct CodegenContext<'ctx> {
    modules: Vec<ModuleCodegen<'ctx>>,
}

impl<'ctx> CodegenContext<'ctx> {
    /// Compile contained LLIR into LLVM IR and produce `hello.o` and
    /// `hello.asm`
    pub fn compile(&self) {
        for module in &self.modules {
            module.context.compile();
        }
    }
}

impl mir::Context {
    /// Compile MIR [`Context`] into [`CodegenContext`] containing LLIR.
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> CodegenContext<'ctx> {
        let mut modules = Vec::new();
        for module in &self.modules {
            modules.push(module.codegen(context));
        }
        CodegenContext { modules }
    }
}

struct ModuleCodegen<'ctx> {
    pub context: &'ctx Context,
    _module: Module<'ctx>,
}

impl<'ctx> std::fmt::Debug for ModuleCodegen<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.context.fmt(f)
    }
}

impl mir::Module {
    fn codegen<'ctx>(&self, context: &'ctx Context) -> ModuleCodegen<'ctx> {
        let mut module = context.module(&self.name);

        let mut functions = HashMap::new();

        for function in &self.functions {
            let param_types: Vec<Type> = function
                .parameters
                .iter()
                .map(|(_, p)| p.get_type())
                .collect();

            let func = match &function.kind {
                mir::FunctionDefinitionKind::Local(_, _) => {
                    module.function(&function.name, function.return_type.get_type(), param_types)
                }
                mir::FunctionDefinitionKind::Extern => todo!(),
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
                        p_ty.get_type(),
                    ),
                );
            }

            let mut scope = Scope {
                context,
                module: &module,
                function,
                block: entry,
                functions: &functions,
                stack_values,
            };
            match &mir_function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => {
                    if let Some(ret) = block.codegen(&mut scope) {
                        scope.block.terminate(Term::Ret(ret)).unwrap();
                    } else {
                        if !scope.block.delete_if_unused().unwrap() {
                            // Add a void return just in case if the block
                            // wasn't unused but didn't have a terminator yet
                            scope.block.terminate(Term::RetVoid).ok();
                        }
                    }
                }
                mir::FunctionDefinitionKind::Extern => {}
            }
        }

        ModuleCodegen {
            context,
            _module: module,
        }
    }
}

pub struct Scope<'ctx, 'a> {
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    function: &'ctx Function<'ctx>,
    block: Block<'ctx>,
    functions: &'a HashMap<String, Function<'ctx>>,
    stack_values: HashMap<String, StackValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackValue(StackValueKind, Type);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackValueKind {
    Immutable(InstructionValue),
    Mutable(InstructionValue),
}

impl<'ctx, 'a> Scope<'ctx, 'a> {
    fn with_block(&self, block: Block<'ctx>) -> Scope<'ctx, 'a> {
        Scope {
            block,
            function: self.function,
            context: self.context,
            module: self.module,
            functions: self.functions,
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
}

impl mir::Statement {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        match &self.0 {
            mir::StmtKind::Let(NamedVariableRef(ty, name, _), mutable, expression) => {
                let value = expression.codegen(scope).unwrap();
                scope.stack_values.insert(
                    name.clone(),
                    StackValue(
                        match mutable {
                            false => StackValueKind::Immutable(value),
                            true => StackValueKind::Mutable({
                                let alloca = scope
                                    .block
                                    .build(Instr::Alloca(name.clone(), ty.get_type()))
                                    .unwrap();
                                scope.block.build(Instr::Store(alloca, value)).unwrap();
                                alloca
                            }),
                        },
                        ty.get_type(),
                    ),
                );
                None
            }
            mir::StmtKind::Set(var, val) => {
                todo!("Re-think how set needs to work with arrays");
                // if let Some(StackValue(kind, _)) = scope.stack_values.get(&var.1).cloned() {
                //     match kind {
                //         StackValueKind::Immutable(_) => {
                //             panic!("Tried to mutate an immutable variable")
                //         }
                //         StackValueKind::Mutable(ptr) => {
                //             let expression = val.codegen(scope).unwrap();
                //             Some(scope.block.build(Instr::Store(ptr, expression)).unwrap())
                //         }
                //     }
                // } else {
                //     panic!("")
                // }
            }
            // mir::StmtKind::If(if_expression) => if_expression.codegen(scope),
            mir::StmtKind::Import(_) => todo!(),
            mir::StmtKind::Expression(expression) => expression.codegen(scope),
        }
    }
}

impl mir::IfExpression {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        let condition = self.0.codegen(scope).unwrap();

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
        let then_res = self.1.codegen(&mut then_scope);
        then_scope.block.terminate(Term::Br(after_bb)).ok();

        let else_res = if let Some(else_block) = &self.2 {
            let mut else_scope = scope.with_block(else_b);
            scope
                .block
                .terminate(Term::CondBr(condition, then_bb, else_bb))
                .unwrap();

            let opt = else_block.codegen(&mut else_scope);

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

impl mir::Expression {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
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
                        scope.block.build(Instr::Load(val, v.1.clone())).unwrap()
                    }
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

                let lhs = lhs_exp.codegen(scope).expect("lhs has no return value");
                let rhs = rhs_exp.codegen(scope).expect("rhs has no return value");
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
                    .map(|e| e.codegen(scope).unwrap())
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
            mir::ExprKind::If(if_expression) => if_expression.codegen(scope),
            mir::ExprKind::Block(block) => {
                let mut inner_scope = scope.with_block(scope.function.block("inner"));
                if let Some(ret) = block.codegen(&mut inner_scope) {
                    inner_scope
                        .block
                        .terminate(Term::Br(scope.block.value()))
                        .unwrap();
                    Some(ret)
                } else {
                    None
                }
            }
            mir::ExprKind::Index(expression, _) => todo!("codegen for index expression"),
            mir::ExprKind::Array(expressions) => todo!("codegen for array expression"),
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

impl mir::Block {
    fn codegen<'ctx, 'a>(&self, mut scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        for stmt in &self.statements {
            stmt.codegen(&mut scope);
        }

        if let Some((kind, expr)) = &self.return_expression {
            if let Some(ret) = expr.codegen(&mut scope) {
                match kind {
                    mir::ReturnKind::Hard => {
                        scope.block.terminate(Term::Ret(ret)).unwrap();
                        None
                    }
                    mir::ReturnKind::Soft => Some(ret),
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl mir::Literal {
    fn as_const(&self, block: &mut Block) -> InstructionValue {
        block.build(self.as_const_kind()).unwrap()
    }

    fn as_const_kind(&self) -> Instr {
        Instr::Constant(match *self {
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
            mir::Literal::Vague(_) => panic!("Got vague literal!"),
        })
    }
}

impl TypeKind {
    fn get_type(&self) -> Type {
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
            TypeKind::Void => panic!("Void not a supported type"),
            TypeKind::Vague(_) => panic!("Tried to compile a vague type!"),
            TypeKind::Array(_, _) => todo!("codegen for array type"),
        }
    }
}
