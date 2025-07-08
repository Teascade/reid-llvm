use std::{collections::HashMap, mem};

use reid_lib::{
    builder::InstructionValue, Block, ConstValue, Context, Function, InstructionKind, IntPredicate,
    Module, TerminatorKind, Type,
};

use crate::mir::{self, types::ReturnType, TypeKind, VariableReference};

pub struct ModuleCodegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
}

impl mir::Module {
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> ModuleCodegen<'ctx> {
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
            for (i, (p_name, _)) in mir_function.parameters.iter().enumerate() {
                stack_values.insert(
                    p_name.clone(),
                    entry.build(InstructionKind::Param(i)).unwrap(),
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
                        scope.block.terminate(TerminatorKind::Ret(ret)).unwrap();
                    }
                }
                mir::FunctionDefinitionKind::Extern => {}
            }
        }

        ModuleCodegen { context, module }
    }
}

pub struct Scope<'ctx, 'a> {
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    function: &'ctx Function<'ctx>,
    block: Block<'ctx>,
    functions: &'a HashMap<String, Function<'ctx>>,
    stack_values: HashMap<String, InstructionValue>,
}

impl<'ctx, 'a> Scope<'ctx, 'a> {
    pub fn with_block(&self, block: Block<'ctx>) -> Scope<'ctx, 'a> {
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
    pub fn swap_block(&mut self, block: Block<'ctx>) -> Block<'ctx> {
        let mut old_block = block;
        mem::swap(&mut self.block, &mut old_block);
        old_block
    }
}

impl mir::Statement {
    pub fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        match &self.0 {
            mir::StmtKind::Let(VariableReference(_, name, _), expression) => {
                let value = expression.codegen(scope).unwrap();
                scope.stack_values.insert(name.clone(), value);
                None
            }
            // mir::StmtKind::If(if_expression) => if_expression.codegen(scope),
            mir::StmtKind::Import(_) => todo!(),
            mir::StmtKind::Expression(expression) => expression.codegen(scope),
        }
    }
}

impl mir::IfExpression {
    pub fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        let condition = self.0.codegen(scope).unwrap();

        // Create blocks
        let then_bb = scope.function.block("then");
        let after_bb = scope.function.block("after");
        let mut before_bb = scope.swap_block(after_bb);

        let mut then_scope = scope.with_block(then_bb);
        let then_res = self.1.codegen(&mut then_scope);
        then_scope
            .block
            .terminate(TerminatorKind::Branch(scope.block.value()))
            .ok();

        let else_bb = scope.function.block("else");
        let mut else_scope = scope.with_block(else_bb);

        let else_res = if let Some(else_block) = &self.2 {
            before_bb
                .terminate(TerminatorKind::CondBr(
                    condition,
                    then_scope.block.value(),
                    else_scope.block.value(),
                ))
                .unwrap();

            let opt = else_block.codegen(&mut else_scope);

            if let Some(ret) = opt {
                else_scope
                    .block
                    .terminate(TerminatorKind::Branch(scope.block.value()))
                    .ok();
                Some(ret)
            } else {
                None
            }
        } else {
            else_scope
                .block
                .terminate(TerminatorKind::Branch(scope.block.value()))
                .unwrap();
            before_bb
                .terminate(TerminatorKind::CondBr(
                    condition,
                    then_scope.block.value(),
                    scope.block.value(),
                ))
                .unwrap();
            None
        };

        if then_res.is_none() && else_res.is_none() {
            None
        } else {
            let mut inc = Vec::from(then_res.as_slice());
            inc.extend(else_res);

            Some(scope.block.build(InstructionKind::Phi(vec![])).unwrap())
        }
    }
}

impl mir::Expression {
    pub fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        match &self.0 {
            mir::ExprKind::Variable(varref) => {
                varref.0.is_known().expect("variable type unknown");
                let v = scope
                    .stack_values
                    .get(&varref.1)
                    .expect("Variable reference not found?!");
                Some(v.clone())
            }
            mir::ExprKind::Literal(lit) => Some(lit.as_const(&mut scope.block)),
            mir::ExprKind::BinOp(binop, lhs_exp, rhs_exp) => {
                lhs_exp
                    .return_type()
                    .expect("No ret type in lhs?")
                    .is_known()
                    .expect("lhs ret type is unknown");
                rhs_exp
                    .return_type()
                    .expect("No ret type in rhs?")
                    .is_known()
                    .expect("rhs ret type is unknown");

                let lhs = lhs_exp.codegen(scope).expect("lhs has no return value");
                let rhs = rhs_exp.codegen(scope).expect("rhs has no return value");
                Some(match binop {
                    mir::BinaryOperator::Add => {
                        scope.block.build(InstructionKind::Add(lhs, rhs)).unwrap()
                    }
                    mir::BinaryOperator::Minus => {
                        scope.block.build(InstructionKind::Sub(lhs, rhs)).unwrap()
                    }
                    mir::BinaryOperator::Mult => todo!(),
                    mir::BinaryOperator::And => todo!(),
                    mir::BinaryOperator::Logic(l) => scope
                        .block
                        .build(InstructionKind::ICmp(l.int_predicate(), lhs, rhs))
                        .unwrap(),
                })
            }
            mir::ExprKind::FunctionCall(call) => {
                call.return_type
                    .is_known()
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
                        .build(InstructionKind::FunctionCall(callee.value(), params))
                        .unwrap(),
                )
            }
            mir::ExprKind::If(if_expression) => if_expression.codegen(scope),
            mir::ExprKind::Block(block) => {
                let mut inner_scope = scope.with_block(scope.function.block("inner"));
                if let Some(ret) = block.codegen(&mut inner_scope) {
                    inner_scope
                        .block
                        .terminate(TerminatorKind::Branch(scope.block.value()))
                        .unwrap();
                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}

impl mir::LogicOperator {
    fn int_predicate(&self) -> IntPredicate {
        match self {
            mir::LogicOperator::LessThan => IntPredicate::LessThan,
            mir::LogicOperator::GreaterThan => IntPredicate::GreaterThan,
        }
    }
}

impl mir::Block {
    pub fn codegen<'ctx, 'a>(&self, mut scope: &mut Scope<'ctx, 'a>) -> Option<InstructionValue> {
        for stmt in &self.statements {
            stmt.codegen(&mut scope);
        }

        if let Some((kind, expr)) = &self.return_expression {
            let ret = expr.codegen(&mut scope).unwrap();
            match kind {
                mir::ReturnKind::Hard => {
                    scope.block.terminate(TerminatorKind::Ret(ret)).unwrap();
                    None
                }
                mir::ReturnKind::Soft => Some(ret),
            }
        } else {
            None
        }
    }
}

impl mir::Literal {
    pub fn as_const(&self, block: &mut Block) -> InstructionValue {
        block.build(self.as_const_kind()).unwrap()
    }

    pub fn as_const_kind(&self) -> InstructionKind {
        InstructionKind::Constant(match *self {
            mir::Literal::I32(val) => ConstValue::I32(val),
            mir::Literal::I16(val) => ConstValue::I16(val),
            mir::Literal::Vague(_) => panic!("Got vague literal!"),
        })
    }
}

impl TypeKind {
    fn get_type(&self) -> Type {
        match &self {
            TypeKind::I32 => Type::I32,
            TypeKind::I16 => Type::I16,
            TypeKind::Bool => Type::Bool,
            TypeKind::Void => panic!("Void not a supported type"),
            TypeKind::Vague(_) => panic!("Tried to compile a vague type!"),
        }
    }
}
