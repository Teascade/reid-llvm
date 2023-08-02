use crate::{lexer::Token, parser::TopLevelStatement, token_stream::TokenStream};

pub static EASIEST: &str = include_str!("../reid/easiest.reid");
pub static EASY: &str = include_str!("../reid/easy.reid");
pub static MEDIUM: &str = include_str!("../reid/medium.reid");
pub static HARD: &str = include_str!("../reid/hard.reid");

mod codegen;
mod lexer;
mod llvm_ir;
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

    codegen::codegen(statements);
}
