use reid_lib::Context;

use crate::{ast::TopLevelStatement, lexer::Token, token_stream::TokenStream};

mod ast;
mod codegen;
mod lexer;
pub mod mir;
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
    #[error(transparent)]
    ParserError(#[from] token_stream::Error),
    // #[error(transparent)]
    // CodegenError(#[from] codegen::Error),
}

pub fn compile(source: &str) -> Result<String, ReidError> {
    let tokens = lexer::tokenize(source)?;

    dbg!(&tokens);

    let mut token_stream = TokenStream::from(&tokens);

    let mut statements = Vec::new();

    while !matches!(token_stream.peek().unwrap_or(Token::Eof), Token::Eof) {
        let statement = token_stream.parse::<TopLevelStatement>()?;
        dbg!(&statement);
        statements.push(statement);
    }

    let ast_module = ast::Module {
        name: "test".to_owned(),
        top_level_statements: statements,
    };

    dbg!(&ast_module);
    let mir_module = ast_module.process();

    dbg!(&mir_module);

    let mut context = Context::new();
    let cogegen_module = mir_module.codegen(&mut context);

    Ok(match cogegen_module.module.print_to_string() {
        Ok(v) => v,
        Err(e) => panic!("Err: {:?}", e),
    })
}
