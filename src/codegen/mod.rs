mod llvm;

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
    for statement in statements {
        statement.codegen(&mut module);
    }

    Ok(module)
}

impl TopLevelStatement {
    fn codegen(&self, module: &mut IRModule) {
        match self {
            Self::FunctionDefinition(func) => func.codegen(module),
            Self::Import(_) => panic!("not implemented"),
        }
    }
}

impl FunctionDefinition {
    fn codegen(&self, module: &mut IRModule) {
        let FunctionDefinition(signature, block, _) = self;
        let mut function = IRFunction::new(&signature.name, module);
        block.codegen(&mut function)
    }
}

impl Block {
    fn codegen(&self, function: &mut IRFunction) {
        let mut block = IRBlock::new(function);

        if let Some((_, return_exp)) = &self.1 {
            let value = return_exp.codegen(&mut block);
            block.add_return(Some(value))
        }
    }
}

impl Expression {
    fn codegen(&self, block: &mut IRBlock) -> IRValue {
        let Expression(kind, _) = self;

        use ExpressionKind::*;
        match kind {
            Literal(lit) => IRValue::from_literal(lit, block),
            _ => panic!("expression type not supported"),
        }
    }
}
