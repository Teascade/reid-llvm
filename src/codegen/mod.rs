mod llvm;

use llvm::{IRBlock, IRContext, IRFunction, IRModule, IRValue};

use crate::{ast::FunctionDefinition, TopLevelStatement};

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
        let mut function = IRFunction::new(module);
        let mut block = IRBlock::new(&mut function);
        let value = IRValue::const_i32(3, &mut block);
        block.add_return(Some(value))
    }
}
