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

// TODO (Missing Relevant Features):
// - Arrays
// - Structs (and custom types as such)
// - Extern functions
// - Strings
// - Loops

#[derive(thiserror::Error, Debug)]
pub enum ReidError {
    #[error(transparent)]
    LexerError(#[from] lexer::Error),
    #[error(transparent)]
    ParserError(#[from] token_stream::Error),
    #[error("Errors during typecheck: {0:?}")]
    TypeCheckErrors(Vec<mir::pass::Error<mir::typecheck::ErrorKind>>),
}

/// Takes in a bit of source code, parses and compiles it and produces `hello.o`
/// and `hello.asm` from it, which can be linked using `ld` to produce an
/// executable file.
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

    let state = mir_context.pass(&mut TypeCheck);
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
}
