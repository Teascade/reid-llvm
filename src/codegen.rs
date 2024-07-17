use std::collections::{hash_map, HashMap};

use crate::{
    ast::{
        BinaryOperator, Block, BlockLevelStatement, Expression, FunctionCallExpression,
        FunctionDefinition, FunctionSignature, IfExpression, ReturnType, TopLevelStatement,
    },
    llvm_ir::{self, IRBlock, IRFunction, IRModule, IRValue, IRValueType},
};

#[derive(Clone)]
pub struct ScopeData {
    named_vars: HashMap<String, IRValue>,
    defined_functions: HashMap<String, (FunctionSignature, Option<IRFunction>)>,
}

impl ScopeData {
    pub fn inner<'a, 'b>(&self, block: &'b mut IRBlock<'a>) -> Scope<'a, 'b> {
        Scope {
            block,
            data: self.clone(),
        }
    }

    pub fn var(&self, name: &String) -> Option<&IRValue> {
        self.named_vars.get(name)
    }

    pub fn set_var(&mut self, name: &str, val: IRValue) -> Result<(), Error> {
        if let hash_map::Entry::Vacant(e) = self.named_vars.entry(name.to_owned()) {
            e.insert(val);
            Ok(())
        } else {
            Err(Error::VariableAlreadyDefined(name.to_owned()))
        }
    }

    pub fn function(
        &mut self,
        name: &String,
    ) -> Option<&mut (FunctionSignature, Option<IRFunction>)> {
        self.defined_functions.get_mut(name)
    }

    pub fn set_function_signature(
        &mut self,
        name: &str,
        sig: FunctionSignature,
        ir: IRFunction,
    ) -> Result<(), Error> {
        if let hash_map::Entry::Vacant(e) = self.defined_functions.entry(name.to_owned()) {
            e.insert((sig, Some(ir)));
            Ok(())
        } else {
            Err(Error::VariableAlreadyDefined(name.to_owned()))
        }
    }
}

pub struct Scope<'a, 'b> {
    pub block: &'b mut IRBlock<'a>,
    pub data: ScopeData,
}

impl<'a, 'b> Scope<'a, 'b> {
    pub fn inner<'c>(&'c mut self) -> Scope<'a, 'c> {
        Scope {
            block: self.block,
            data: self.data.clone(),
        }
    }
}

pub fn codegen_from_statements(statements: Vec<TopLevelStatement>) -> Result<IRModule, Error> {
    let mut module = IRModule::new("testmod");

    let mut scope = ScopeData {
        defined_functions: HashMap::new(),
        named_vars: HashMap::new(),
    };

    for statement in &statements {
        match statement {
            TopLevelStatement::FunctionDefinition(FunctionDefinition(sig, _)) => {
                let function = module.create_func(&sig.name, IRValueType::I32);
                scope.set_function_signature(&sig.name.clone(), sig.clone(), function)?;
            }
            TopLevelStatement::Import(_) => {}
        }
    }

    for statement in &statements {
        statement.codegen(&mut module, &mut scope)?;
    }

    Ok(module)
}

impl TopLevelStatement {
    pub fn codegen(&self, module: &mut IRModule, root_data: &mut ScopeData) -> Result<(), Error> {
        match self {
            TopLevelStatement::FunctionDefinition(FunctionDefinition(sig, block)) => {
                if let Some((_, ir)) = root_data.function(&sig.name) {
                    if let Some(ir_function) = ir.take() {
                        let mut ir_block = module.create_block(&sig.name);
                        let mut scope = root_data.inner(&mut ir_block);

                        let (_, value) = match block.codegen(&mut scope)? {
                            Some(v) => v,
                            None => panic!("Void-return type function not yet implemented!"),
                        };

                        ir_function.add_definition(value, ir_block);
                    } else {
                        Err(Error::FunctionAlreadyDefined(sig.name.clone()))?
                    }
                } else {
                    panic!("Function was not declared before it's definition")
                }
            }
            TopLevelStatement::Import(_) => {}
        }
        Ok(())
    }
}

impl Block {
    pub fn codegen(&self, scope: &mut Scope) -> Result<Option<(ReturnType, IRValue)>, Error> {
        for statement in &self.0 {
            statement.codegen(scope)?;
        }

        let value = if let Some((rt, exp)) = &self.1 {
            Some((*rt, exp.codegen(scope)?))
        } else {
            None
        };

        Ok(value)
    }
}

impl BlockLevelStatement {
    pub fn codegen(&self, scope: &mut Scope) -> Result<(), Error> {
        match self {
            BlockLevelStatement::Let(let_statement) => {
                let val = let_statement.1.codegen(scope)?;
                scope.data.set_var(&let_statement.0, val)?;
                Ok(())
            }
            BlockLevelStatement::Return(_) => panic!("Should never happen"),
            BlockLevelStatement::Import(_) => Ok(()), // TODO: To implement
            BlockLevelStatement::Expression(e) => {
                let _value = e.codegen(scope)?;
                Ok(())
            }
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
                BinaryOperator::Mult => {
                    let lhs = lhs.codegen(scope)?;
                    let rhs = rhs.codegen(scope)?;
                    Ok(scope.block.mul(lhs, rhs)?)
                }
                BinaryOperator::LessThan => {
                    let lhs = lhs.codegen(scope)?;
                    let rhs = rhs.codegen(scope)?;
                    Ok(scope.block.cmp(lhs, rhs)?)
                }
                _ => panic!("Other binary operators not supported yet!"),
            },
            BlockExpr(block) => {
                let mut inner = scope.inner();

                Ok(match block.codegen(&mut inner)? {
                    Some((r_type, value)) => match r_type {
                        ReturnType::Soft => value,
                        ReturnType::Hard => {
                            panic!("Hard returns in inner blocks not supported yet")
                        }
                    },
                    None => panic!("Void-return type block not yet implemented!"),
                })
            }
            FunctionCall(fc) => {
                let FunctionCallExpression(name, _) = &**fc;
                if let Some((sig, _)) = scope.data.function(name) {
                    Ok(scope.block.function_call(sig)?)
                } else {
                    Err(Error::UndefinedFunction(name.clone()))?
                }
            }
            VariableName(name) => scope
                .data
                .var(name)
                .cloned()
                .ok_or(Error::UndefinedVariable(name.clone())),
            Literal(lit) => Ok(scope.block.get_const(lit)),
            IfExpr(exp) => Ok(exp.codegen(scope)?),
        }
    }
}

impl IfExpression {
    pub fn codegen(&self, scope: &mut Scope) -> Result<IRValue, Error> {
        let if_cond = self.0.codegen(scope)?;
        let then_block = self.1.codegen(scope)?.unwrap();
        let else_block = self.2.codegen(scope)?.unwrap();
        let cond = scope
            .block
            .conditional_branch(if_cond, then_block.1, else_block.1)?;
        Ok(cond)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Variable '{0}' already defined")]
    VariableAlreadyDefined(String),
    #[error("Variable '{0}' not yet defined")]
    UndefinedVariable(String),
    #[error("Function '{0}' not defined")]
    UndefinedFunction(String),
    #[error("Function '{0}' already defined")]
    FunctionAlreadyDefined(String),
    #[error(transparent)]
    Deeper(#[from] llvm_ir::Error),
}
