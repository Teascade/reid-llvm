use std::{collections::HashMap, mem, ops::Deref};

use crate::mir::{self, types::ReturnType, TypeKind, VariableReference};
use reid_lib::{
    types::{BasicType, BasicValue, IntegerValue, TypeEnum, Value},
    BasicBlock, Context, Function, IntPredicate, Module,
};

pub struct ModuleCodegen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
}

impl mir::Module {
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> ModuleCodegen<'ctx> {
        let module = context.module(&self.name);

        let mut functions = HashMap::new();

        for function in &self.functions {
            let ret_type = function.return_type().unwrap().get_type(&context);
            let fn_type = ret_type.function_type(
                function
                    .parameters
                    .iter()
                    .map(|(_, p)| p.get_type(&context))
                    .collect(),
            );

            let func = match &function.kind {
                mir::FunctionDefinitionKind::Local(_, _) => {
                    module.add_function(fn_type, &function.name)
                }
                mir::FunctionDefinitionKind::Extern(_) => todo!(),
            };
            functions.insert(function.name.clone(), func);
        }

        for mir_function in &self.functions {
            let function = functions.get(&mir_function.name).unwrap();

            let mut stack_values = HashMap::new();
            for (i, (p_name, p_type)) in mir_function.parameters.iter().enumerate() {
                stack_values.insert(
                    p_name.clone(),
                    function.get_param(i, p_type.get_type(&context)).unwrap(),
                );
            }

            let mut scope = Scope {
                context,
                module: &module,
                function,
                block: function.block("entry"),
                functions: functions.clone(),
                stack_values,
            };
            match &mir_function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => {
                    if let Some(ret) = block.codegen(&mut scope) {
                        scope.block.ret(&ret).unwrap();
                    }
                }
                mir::FunctionDefinitionKind::Extern(_) => {}
            }
        }

        ModuleCodegen { context, module }
    }
}

pub struct Scope<'ctx> {
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    function: &'ctx Function<'ctx>,
    block: BasicBlock<'ctx>,
    functions: HashMap<String, Function<'ctx>>,
    stack_values: HashMap<String, Value<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn with_block(&self, block: BasicBlock<'ctx>) -> Scope<'ctx> {
        Scope {
            block,
            context: self.context,
            function: self.function,
            module: self.module,
            functions: self.functions.clone(),
            stack_values: self.stack_values.clone(),
        }
    }

    /// Takes the block out from this scope, swaps the given block in it's place
    /// and returns the old block.
    pub fn swap_block(&mut self, block: BasicBlock<'ctx>) -> BasicBlock<'ctx> {
        let mut old_block = block;
        mem::swap(&mut self.block, &mut old_block);
        old_block
    }
}

impl mir::Statement {
    pub fn codegen<'ctx>(&self, scope: &mut Scope<'ctx>) -> Option<Value<'ctx>> {
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
    pub fn codegen<'ctx>(&self, scope: &mut Scope<'ctx>) -> Option<Value<'ctx>> {
        let condition = self.0.codegen(scope).unwrap();

        // Create blocks
        let then_bb = scope.function.block("then");
        let after_bb = scope.function.block("after");
        let mut before_bb = scope.swap_block(after_bb);

        let mut then_scope = scope.with_block(then_bb);
        let then_res = self.1.codegen(&mut then_scope);
        then_scope.block.br(&scope.block).ok();

        let else_bb = scope.function.block("else");
        let mut else_scope = scope.with_block(else_bb);

        let else_opt = if let Some(else_block) = &self.2 {
            before_bb
                .conditional_br(&condition, &then_scope.block, &else_scope.block)
                .unwrap();

            let opt = else_block.codegen(&mut else_scope);

            if let Some(ret) = opt {
                else_scope.block.br(&scope.block).ok();
                Some((else_scope.block, ret))
            } else {
                None
            }
        } else {
            else_scope.block.br(&scope.block).unwrap();
            before_bb
                .conditional_br(&condition, &then_scope.block, &scope.block)
                .unwrap();
            None
        };

        if then_res.is_none() && else_opt.is_none() {
            None
        } else if let Ok(ret_type) = self.1.return_type() {
            let phi = scope
                .block
                .phi(&ret_type.get_type(scope.context), "phi")
                .unwrap();
            if let Some(then_ret) = then_res {
                phi.add_incoming(&then_ret, &then_scope.block);
            }
            if let Some((else_bb, else_ret)) = else_opt {
                phi.add_incoming(&else_ret, &else_bb);
            }

            Some(phi.build())
        } else {
            None
        }
    }
}

impl mir::Expression {
    pub fn codegen<'ctx>(&self, scope: &mut Scope<'ctx>) -> Option<Value<'ctx>> {
        match &self.0 {
            mir::ExprKind::Variable(varref) => {
                let v = scope
                    .stack_values
                    .get(&varref.1)
                    .expect("Variable reference not found?!");
                Some(v.clone())
            }
            mir::ExprKind::Literal(lit) => Some(lit.codegen(scope.context)),
            mir::ExprKind::BinOp(binop, lhs_exp, rhs_exp) => {
                let lhs = lhs_exp.codegen(scope).expect("lhs has no return value");
                let rhs = rhs_exp.codegen(scope).expect("rhs has no return value");
                Some(match binop {
                    mir::BinaryOperator::Add => scope.block.add(&lhs, &rhs, "add").unwrap(),
                    mir::BinaryOperator::Minus => scope.block.sub(&lhs, &rhs, "sub").unwrap(),
                    mir::BinaryOperator::Mult => todo!(),
                    mir::BinaryOperator::And => todo!(),
                    mir::BinaryOperator::Logic(l) => {
                        let ret_type = lhs_exp.return_type().expect("No ret type in lhs?");
                        scope
                            .block
                            .integer_compare(&lhs, &rhs, &l.int_predicate(ret_type.signed()), "cmp")
                            .unwrap()
                    }
                })
            }
            mir::ExprKind::FunctionCall(call) => {
                let params = call
                    .parameters
                    .iter()
                    .map(|e| e.codegen(scope).unwrap())
                    .collect();
                let callee = scope
                    .functions
                    .get(&call.name)
                    .expect("function not found!");
                Some(scope.block.call(callee, params, "call").unwrap())
            }
            mir::ExprKind::If(if_expression) => if_expression.codegen(scope),
            mir::ExprKind::Block(block) => {
                let mut inner_scope = scope.with_block(scope.function.block("inner"));
                if let Some(ret) = block.codegen(&mut inner_scope) {
                    inner_scope.block.br(&scope.block);
                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}

impl mir::LogicOperator {
    fn int_predicate(&self, signed: bool) -> IntPredicate {
        match (self, signed) {
            (mir::LogicOperator::LessThan, true) => IntPredicate::SLT,
            (mir::LogicOperator::GreaterThan, true) => IntPredicate::SGT,
            (mir::LogicOperator::LessThan, false) => IntPredicate::ULT,
            (mir::LogicOperator::GreaterThan, false) => IntPredicate::UGT,
        }
    }
}

impl mir::Block {
    pub fn codegen<'ctx>(&self, mut scope: &mut Scope<'ctx>) -> Option<Value<'ctx>> {
        for stmt in &self.statements {
            stmt.codegen(&mut scope);
        }

        if let Some((kind, expr)) = &self.return_expression {
            let ret = expr.codegen(&mut scope).unwrap();
            match kind {
                mir::ReturnKind::Hard => {
                    scope.block.ret(&ret).unwrap();
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
    pub fn codegen<'ctx>(&self, context: &'ctx Context) -> Value<'ctx> {
        let val: IntegerValue<'ctx> = match *self {
            mir::Literal::I32(val) => context.type_i32().from_signed(val as i64),
            mir::Literal::I16(val) => context.type_i16().from_signed(val as i64),
        };
        Value::Integer(val)
    }
}

impl TypeKind {
    fn get_type<'ctx>(&self, context: &'ctx Context) -> TypeEnum<'ctx> {
        match &self {
            TypeKind::I32 => TypeEnum::Integer(context.type_i32()),
            TypeKind::I16 => TypeEnum::Integer(context.type_i16()),
            TypeKind::Void => panic!("Void not a supported type"),
        }
    }
}
