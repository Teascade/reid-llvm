use std::collections::{hash_map, HashMap};

use crate::{
    ast::{BinaryOperator, BlockLevelStatement, Expression, FunctionDefinition, TopLevelStatement},
    llvm_ir::{self, IRBlock, IRModule, IRValue, IRValueType},
};

pub struct Scope<'a> {
    pub block: IRBlock<'a>,
    named_vars: HashMap<String, IRValue>,
}

impl<'a> Scope<'a> {
    pub fn from(block: IRBlock<'a>) -> Self {
        Scope {
            block,
            named_vars: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<&IRValue> {
        self.named_vars.get(name)
    }

    pub fn set(&mut self, name: &str, val: IRValue) -> Result<(), Error> {
        if let hash_map::Entry::Vacant(e) = self.named_vars.entry(name.to_owned()) {
            e.insert(val);
            Ok(())
        } else {
            Err(Error::VariableAlreadyDefined(name.to_owned()))
        }
    }
}

impl TopLevelStatement {
    pub fn codegen(&self, module: &mut IRModule) -> Result<(), Error> {
        match self {
            TopLevelStatement::FunctionDefinition(FunctionDefinition(sig, block)) => {
                let func = module.create_func(&sig.name, IRValueType::I32);
                let mut scope = Scope::from(module.create_block());

                for statement in &block.0 {
                    statement.codegen(&mut scope);
                }

                let value = if let Some(exp) = &block.1 {
                    exp.codegen(&mut scope)?
                } else {
                    panic!("Void-return type function not yet implemented!");
                };
                func.add_definition(value, scope.block);
            }
            TopLevelStatement::Import(_) => {}
        }
        Ok(())
    }
}

impl BlockLevelStatement {
    pub fn codegen(&self, scope: &mut Scope) -> Result<(), Error> {
        match self {
            BlockLevelStatement::Let(let_statement) => {
                let val = let_statement.1.codegen(scope)?;
                scope.set(&let_statement.0, val)?;
                Ok(())
            }
            BlockLevelStatement::Return(_) => panic!("Should never exist!"),
            BlockLevelStatement::Import(_) => Ok(()),
            BlockLevelStatement::Expression(_) => Ok(()),
        }
    }
}

impl Expression {
    pub fn codegen(&self, scope: &mut Scope) -> Result<IRValue, Error> {
        use Expression::*;
        match self {
            Binop(op, lhs, rhs) => match op {
                BinaryOperator::Add => {
                    let lhs = lhs.codegen(scope)?;
                    let rhs = rhs.codegen(scope)?;
                    Ok(scope.block.add(lhs, rhs)?)
                }
                BinaryOperator::Mult => panic!("Not implemented!"),
            },
            BlockExpr(_) => panic!("Not implemented!"),
            FunctionCall(_) => panic!("Not implemented!"),
            VariableName(name) => Ok(scope
                .get(name)
                .cloned()
                .unwrap_or(Err(Error::UndefinedVariable(name.clone()))?)),
            Literal(lit) => Ok(scope.block.get_const(lit)),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Variable already defined {0}")]
    VariableAlreadyDefined(String),
    #[error("Variable not yet defined {0}")]
    UndefinedVariable(String),
    #[error(transparent)]
    Deeper(#[from] llvm_ir::Error),
}
