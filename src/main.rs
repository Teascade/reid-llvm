use std::collections::HashMap;

use codegen::IRBlock;
use parser::Expression;

use crate::{
    codegen::{IRModule, Value},
    lexer::Token,
    parser::{FunctionDefinition, Literal, TopLevelStatement},
    token_stream::TokenStream,
};

pub static EASIEST: &str = include_str!("../reid/easiest.reid");
pub static EASY: &str = include_str!("../reid/easy.reid");
pub static MEDIUM: &str = include_str!("../reid/medium.reid");
pub static HARD: &str = include_str!("../reid/hard.reid");

mod codegen;
mod lexer;
mod parser;
mod token_stream;

// TODO:
// 1. Make it so that TopLevelStatement can only be import or function def
// 2. Make BlockLevelStatement, that has everything TopLevelStatement has now
// 3. Make it so all codegen is done with a Block-struct, that represents a
//    single proper block

fn main() {
    let tokens = lexer::tokenize(EASIEST).unwrap();

    dbg!(&tokens);

    let mut token_stream = TokenStream::from(&tokens);

    let mut statements = Vec::new();

    while !matches!(token_stream.peek().unwrap_or(Token::Eof), Token::Eof) {
        let statement = token_stream.parse::<TopLevelStatement>().unwrap();
        dbg!(&statement);
        statements.push(statement);
    }

    let mut c = IRModule::new();
    for statement in statements {
        match statement {
            TopLevelStatement::FunctionDefinition(FunctionDefinition(sig, block)) => {
                let mut named_vars: HashMap<String, Value> = HashMap::new();

                let func = c.create_func(sig.name, codegen::ValueType::I32);
                let mut c_block = c.create_block();

                for stmt in block.0 {
                    match stmt {
                        parser::BlockLevelStatement::Let(let_statement) => {
                            let value = codegen_exp(&mut c_block, &named_vars, let_statement.1);
                            named_vars.insert(let_statement.0, value);
                        }
                        parser::BlockLevelStatement::Return(_) => panic!("Should never exist!"),
                        parser::BlockLevelStatement::Import(_) => {}
                        parser::BlockLevelStatement::Expression(_) => {}
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
    use parser::Expression::*;
    match expression {
        Binop(op, lhs, rhs) => match op {
            parser::BinaryOperator::Add => {
                let lhs = codegen_exp(c_block, named_vars, *lhs);
                let rhs = codegen_exp(c_block, named_vars, *rhs);
                c_block.add(lhs, rhs).unwrap()
            }
            parser::BinaryOperator::Mult => panic!("Not implemented!"),
        },
        BlockExpr(_) => panic!("Not implemented!"),
        FunctionCall(_) => panic!("Not implemented!"),
        VariableName(name) => named_vars.get(&name).cloned().unwrap(),
        Literal(lit) => c_block.get_const(&lit),
    }
}
