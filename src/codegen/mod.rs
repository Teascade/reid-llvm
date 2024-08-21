mod llvm;

use std::collections::HashMap;

use llvm::{Error, IRBlock, IRContext, IRFunction, IRModule, IRValue};

use crate::{
    ast::{
        BinaryOperator, Block, BlockLevelStatement, Expression, ExpressionKind, FunctionDefinition,
        LetStatement, ReturnType,
    },
    TopLevelStatement,
};

pub fn form_context() -> IRContext {
    IRContext::new()
}

pub fn from_statements(
    context: &mut IRContext,
    statements: Vec<TopLevelStatement>,
) -> Result<IRModule, Error> {
    let mut module = context.module("testmod".to_owned());
    let mut scope = ScopeData::new();
    for statement in statements {
        statement.codegen(&mut scope, &mut module);
    }

    Ok(module)
}

impl TopLevelStatement {
    fn codegen(&self, scope: &mut ScopeData, module: &mut IRModule) {
        match self {
            Self::FunctionDefinition(func) => func.codegen(scope, module),
            Self::Import(_) => panic!("not implemented"),
        }
    }
}

impl FunctionDefinition {
    fn codegen(&self, scope: &mut ScopeData, module: &mut IRModule) {
        let FunctionDefinition(signature, block, _) = self;
        let mut ir_function = IRFunction::new(&signature.name, module);
        let ir_block = IRBlock::new(&mut ir_function);
        block.codegen(scope.inner(ir_block));
    }
}

impl Block {
    fn codegen(&self, mut scope: Scope) {
        for statement in &self.0 {
            statement.codegen(&mut scope);
        }

        if let Some((_, return_exp)) = &self.1 {
            let value = return_exp.codegen(&mut scope);
            scope.block.add_return(Some(value));
        }
    }
}

impl BlockLevelStatement {
    fn codegen(&self, scope: &mut Scope) {
        use BlockLevelStatement::*;
        match self {
            Expression(exp) | Return(ReturnType::Soft, exp) => {
                exp.codegen(scope);
            }
            Let(LetStatement(name, exp, _)) => {
                let val = exp.codegen(scope);
                scope.data.insert(name, val);
            }
            Return(ReturnType::Hard, _) => panic!("hard returns here should not be possible.."),
            Import(_) => panic!("block level import not supported"),
        }
    }
}

impl Expression {
    fn codegen(&self, scope: &mut Scope) -> IRValue {
        let Expression(kind, _) = self;

        use ExpressionKind::*;
        match kind {
            Literal(lit) => IRValue::from_literal(lit, &mut scope.block),
            VariableName(v) => scope.data.fetch(v),
            Binop(op, lhs, rhs) => {
                let lhs = lhs.codegen(scope);
                let rhs = rhs.codegen(scope);
                use crate::ast::BinaryOperator::*;
                match op {
                    Add => scope.block.add(lhs, rhs).unwrap(),
                    Mult => scope.block.mult(lhs, rhs).unwrap(),
                    _ => panic!("operator not supported: {:?}", op),
                }
            }
            _ => panic!("expression type not supported"),
        }
    }
}

#[derive(Clone)]
struct ScopeData {
    vars: HashMap<String, IRValue>,
}

impl ScopeData {
    fn new() -> ScopeData {
        ScopeData {
            vars: HashMap::new(),
        }
    }

    fn with_block<'a, 'b, 'c>(self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        Scope { data: self, block }
    }

    fn inner<'a, 'b, 'c>(&self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        self.clone().with_block(block)
    }

    fn fetch(&self, name: &String) -> IRValue {
        match self.vars.get(name) {
            Some(val) => val.clone(),
            _ => panic!("No such variable in scope: {}", name),
        }
    }

    fn insert(&mut self, name: &String, value: IRValue) {
        match self.vars.insert(name.clone(), value) {
            Some(_) => panic!("{} was already defined in scope", name),
            _ => {}
        }
    }
}

struct Scope<'a, 'b, 'c> {
    data: ScopeData,
    block: IRBlock<'a, 'b, 'c>,
}

impl<'a, 'b, 'c> Scope<'a, 'b, 'c> {
    fn inner(&self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        self.data.clone().with_block(block)
    }
}
