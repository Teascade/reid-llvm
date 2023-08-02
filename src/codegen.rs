use std::collections::{hash_map, HashMap};

use crate::{
    llvm_ir::{IRBlock, IRModule, Value, ValueType},
    parser::{
        BinaryOperator, BlockLevelStatement, Expression, FunctionDefinition, Literal,
        TopLevelStatement,
    },
};

pub struct Scope<'a> {
    pub block: IRBlock<'a>,
    named_vars: HashMap<String, Value>,
}

impl<'a> Scope<'a> {
    pub fn from(block: IRBlock<'a>) -> Self {
        Scope {
            block,
            named_vars: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<&Value> {
        self.named_vars.get(name)
    }

    pub fn set(&mut self, name: String, val: Value) -> Result<(), ()> {
        if let hash_map::Entry::Vacant(e) = self.named_vars.entry(name) {
            e.insert(val);
            Ok(())
        } else {
            Err(()) // TODO! Error Handling!
        }
    }
}

pub fn codegen(statements: Vec<TopLevelStatement>) {
    let mut c = IRModule::new();
    for statement in statements {
        match statement {
            TopLevelStatement::FunctionDefinition(FunctionDefinition(sig, block)) => {
                let func = c.create_func(sig.name, ValueType::I32);
                let mut scope = Scope::from(c.create_block());

                for stmt in block.0 {
                    match stmt {
                        BlockLevelStatement::Let(let_statement) => {
                            let value = codegen_exp(&mut scope, let_statement.1);
                            scope.set(let_statement.0, value).unwrap();
                        }
                        BlockLevelStatement::Return(_) => panic!("Should never exist!"),
                        BlockLevelStatement::Import(_) => {}
                        BlockLevelStatement::Expression(_) => {}
                    }
                }

                let value = if let Some(exp) = block.1 {
                    codegen_exp(&mut scope, exp)
                } else {
                    scope.block.get_const(&Literal::I32(0))
                };
                func.add_definition(value, scope.block);
            }
            TopLevelStatement::Import(_) => {}
        }
    }
}

fn codegen_exp(scope: &mut Scope, expression: Expression) -> Value {
    use Expression::*;
    match expression {
        Binop(op, lhs, rhs) => match op {
            BinaryOperator::Add => {
                let lhs = codegen_exp(scope, *lhs);
                let rhs = codegen_exp(scope, *rhs);
                scope.block.add(lhs, rhs).unwrap()
            }
            BinaryOperator::Mult => panic!("Not implemented!"),
        },
        BlockExpr(_) => panic!("Not implemented!"),
        FunctionCall(_) => panic!("Not implemented!"),
        VariableName(name) => scope.get(&name).cloned().unwrap(),
        Literal(lit) => scope.block.get_const(&lit),
    }
}
