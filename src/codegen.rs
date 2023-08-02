use std::collections::HashMap;

use crate::{
    llvm_ir::{IRBlock, IRModule, Value, ValueType},
    parser::{
        BinaryOperator, BlockLevelStatement, Expression, FunctionDefinition, Literal,
        TopLevelStatement,
    },
};

pub fn codegen(statements: Vec<TopLevelStatement>) {
    let mut c = IRModule::new();
    for statement in statements {
        match statement {
            TopLevelStatement::FunctionDefinition(FunctionDefinition(sig, block)) => {
                let mut named_vars: HashMap<String, Value> = HashMap::new();

                let func = c.create_func(sig.name, ValueType::I32);
                let mut c_block = c.create_block();

                for stmt in block.0 {
                    match stmt {
                        BlockLevelStatement::Let(let_statement) => {
                            let value = codegen_exp(&mut c_block, &named_vars, let_statement.1);
                            named_vars.insert(let_statement.0, value);
                        }
                        BlockLevelStatement::Return(_) => panic!("Should never exist!"),
                        BlockLevelStatement::Import(_) => {}
                        BlockLevelStatement::Expression(_) => {}
                    }
                }

                let value = if let Some(exp) = block.1 {
                    codegen_exp(&mut c_block, &named_vars, exp)
                } else {
                    c_block.get_const(&Literal::I32(0))
                };
                func.add_definition(value, c_block);
            }
            TopLevelStatement::Import(_) => {}
        }
    }
}

fn codegen_exp(
    c_block: &mut IRBlock,
    named_vars: &HashMap<String, Value>,
    expression: Expression,
) -> Value {
    use Expression::*;
    match expression {
        Binop(op, lhs, rhs) => match op {
            BinaryOperator::Add => {
                let lhs = codegen_exp(c_block, named_vars, *lhs);
                let rhs = codegen_exp(c_block, named_vars, *rhs);
                c_block.add(lhs, rhs).unwrap()
            }
            BinaryOperator::Mult => panic!("Not implemented!"),
        },
        BlockExpr(_) => panic!("Not implemented!"),
        FunctionCall(_) => panic!("Not implemented!"),
        VariableName(name) => named_vars.get(&name).cloned().unwrap(),
        Literal(lit) => c_block.get_const(&lit),
    }
}
