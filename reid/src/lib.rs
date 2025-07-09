use mir::typecheck::TypeCheck;
use reid_lib::Context;

use crate::{ast::TopLevelStatement, lexer::Token, token_stream::TokenStream};

mod ast;
mod codegen;
mod lexer;
pub mod mir;
mod pad_adapter;
mod token_stream;
mod util;

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
    #[error("Errors during typecheck: {0:?}")]
    TypeCheckErrors(Vec<mir::pass::Error<mir::typecheck::ErrorKind>>),
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
        statements.push(statement);
    }

    let ast_module = ast::Module {
        name: "test".to_owned(),
        top_level_statements: statements,
    };

    dbg!(&ast_module);
    let mut mir_context = mir::Context::from(vec![ast_module]);

    let state = mir_context.pass(&mut TypeCheck {});
    dbg!(&state);

    println!("{}", &mir_context);

    if !state.errors.is_empty() {
        return Err(ReidError::TypeCheckErrors(state.errors));
    }

    let mut context = Context::new();
    let codegen_modules = mir_context.codegen(&mut context);

    dbg!(&codegen_modules);
    codegen_modules.compile();

    Ok(String::new())

    // Ok(match cogegen_module.module.print_to_string() {
    //     Ok(v) => v,
    //     Err(e) => panic!("Err: {:?}", e),
    // })
}
