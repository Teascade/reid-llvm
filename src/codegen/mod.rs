mod llvm;

use std::collections::HashMap;

use llvm::{IRBlock, IRContext, IRFunction, IRModule, IRValue};

use crate::{
    ast::{Block, Expression, ExpressionKind, FunctionDefinition},
    TopLevelStatement,
};

#[derive(thiserror::Error, Debug)]
pub enum Error {}

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
        block.codegen(scope.with_block(ir_block));
    }
}

impl Block {
    fn codegen(&self, mut scope: Scope) {
        if let Some((_, return_exp)) = &self.1 {
            let value = return_exp.codegen(&mut scope);
            scope.block.add_return(Some(value));
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

    fn with_block<'a, 'b, 'c>(&self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        Scope {
            data: self.clone(),
            block,
        }
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
