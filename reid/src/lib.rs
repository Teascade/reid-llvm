//! Reid is a toy-language compiler I'm working on to learn LLVM (and compiler
//! development).
//!
//! Reid only uses [llvm-sys](https://gitlab.com/taricorp/llvm-sys.rs), which
//! provide very minimal bindings from the LLVM C-API to Rust. `reid_llvm`-crate
//! contains the relevant abstraction to produce a more Rust'y API from that.
//!
//! Much of the syntax in Reid is directly inspired by rust, but mostly it is
//! driven by simplicity.
//!
//! Reid is currently able to (non-exhaustively):
//! - Do basic algebra (e.g. Add, Sub, Mult)
//! - Resolve complex one-liners correctly using PEDMAS (e.g. `5 + 2 * 5 - 5 *
//!   5` is calculated correctly)
//! - Declare and call functions with varying parameters and return types
//! - Perform type-checking and type-inference such that return-types and
//!   parameter types must always match.
//! - Do simple logic-operations (e.g. If/And/Or)
//!
//! An example program of Reid, that calculates the 5th fibonacci number (and
//! uses Rust for highlighting) is:
//! ```reid
//! fn main() -> u16 {
//!     return fibonacci(5);
//! }
//!
//! fn fibonacci(n: u16) -> u16 {
//!     if n <= 2 {
//!         return 1;
//!     }
//!     return fibonacci(n-1) + fibonacci(n-2);
//! }
//! ```
//!
//! Currently missing relevant features (TODOs) are:
//! - ~~Arrays~~ (DONE)
//! - Structs (and custom types as such)
//! - Extern functions
//! - ~~Strings~~ (DONE)
//! - Loops
//! ```

use std::path::PathBuf;

use mir::{
    linker::LinkerPass, typecheck::TypeCheck, typeinference::TypeInference, typerefs::TypeRefs,
};
use reid_lib::Context;

use crate::{ast::TopLevelStatement, lexer::Token, token_stream::TokenStream};

mod ast;
mod codegen;
mod lexer;
pub mod mir;
mod pad_adapter;
mod token_stream;
mod util;

#[derive(thiserror::Error, Debug, Clone)]
pub enum ReidError {
    #[error(transparent)]
    LexerError(#[from] lexer::Error),
    #[error(transparent)]
    ParserError(#[from] token_stream::Error),
    #[error("Errors during typecheck: {0:?}")]
    TypeCheckErrors(Vec<mir::pass::Error<mir::typecheck::ErrorKind>>),
    #[error("Errors during type inference: {0:?}")]
    TypeInferenceErrors(Vec<mir::pass::Error<mir::typecheck::ErrorKind>>),
    #[error("Errors during linking: {0:?}")]
    LinkerErrors(Vec<mir::pass::Error<mir::linker::ErrorKind>>),
}

pub fn compile_module(
    source: &str,
    name: String,
    path: Option<PathBuf>,
    is_main: bool,
) -> Result<mir::Module, ReidError> {
    let tokens = lexer::tokenize(source)?;

    dbg!(&tokens);

    let mut token_stream = TokenStream::from(&tokens);

    let mut statements = Vec::new();

    while !matches!(token_stream.peek().unwrap_or(Token::Eof), Token::Eof) {
        let statement = token_stream.parse::<TopLevelStatement>()?;
        statements.push(statement);
    }

    let ast_module = ast::Module {
        name,
        top_level_statements: statements,
        path,
        is_main,
    };

    Ok(ast_module.process())
}

pub fn perform_all_passes(context: &mut mir::Context) -> Result<(), ReidError> {
    let state = context.pass(&mut LinkerPass);
    #[cfg(debug_assertions)]
    {
        dbg!(&context);
        println!("{}", &context);
    }

    if !state.errors.is_empty() {
        return Err(ReidError::LinkerErrors(state.errors));
    }

    let refs = TypeRefs::default();

    let state = context.pass(&mut TypeInference { refs: &refs });
    #[cfg(debug_assertions)]
    {
        dbg!(&state, &refs);
        dbg!(&context);
        println!("{}", &context);
    }

    if !state.errors.is_empty() {
        return Err(ReidError::TypeInferenceErrors(state.errors));
    }

    let state = context.pass(&mut TypeCheck { refs: &refs });
    #[cfg(debug_assertions)]
    {
        dbg!(&state);
        println!("{}", &context);
    }

    if !state.errors.is_empty() {
        return Err(ReidError::TypeCheckErrors(state.errors));
    }

    Ok(())
}

/// Takes in a bit of source code, parses and compiles it and produces `hello.o`
/// and `hello.asm` from it, which can be linked using `ld` to produce an
/// executable file.
pub fn compile(source: &str, path: PathBuf) -> Result<String, ReidError> {
    let path = path.canonicalize().unwrap();

    let mut mir_context = mir::Context::from(
        vec![compile_module(
            source,
            path.file_name().unwrap().to_str().unwrap().to_owned(),
            Some(path.clone()),
            true,
        )?],
        path.parent().unwrap().to_owned(),
    );

    println!("{}", &mir_context);

    perform_all_passes(&mut mir_context)?;

    let mut context = Context::new();
    let codegen_modules = mir_context.codegen(&mut context);

    dbg!(&codegen_modules);
    let compiled = codegen_modules.compile();
    compiled.output();

    Ok(String::new())
}
