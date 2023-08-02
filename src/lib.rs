use crate::{ast::TopLevelStatement, lexer::Token, llvm_ir::IRModule, token_stream::TokenStream};

mod ast;
mod codegen;
mod lexer;
mod llvm_ir;
mod token_stream;

// TODO:
// 1. Make it so that TopLevelStatement can only be import or function def
// 2. Make BlockLevelStatement, that has everything TopLevelStatement has now
// 3. Make it so all codegen is done with a Block-struct, that represents a
//    single proper block

#[derive(thiserror::Error, Debug)]
pub enum ReidError {
    #[error(transparent)]
    LexerError(#[from] lexer::Error),
}

pub fn compile(source: &str) -> Result<String, ReidError> {
    let tokens = lexer::tokenize(source)?;

    dbg!(&tokens);

    let mut token_stream = TokenStream::from(&tokens);

    let mut statements = Vec::new();

    while !matches!(token_stream.peek().unwrap_or(Token::Eof), Token::Eof) {
        let statement = token_stream.parse::<TopLevelStatement>().unwrap();
        dbg!(&statement);
        statements.push(statement);
    }

    let mut module = IRModule::new("testmod");
    for statement in statements {
        statement.codegen(&mut module);
    }
    let text = module.print_to_string().unwrap();
    Ok(text.to_owned())
}
