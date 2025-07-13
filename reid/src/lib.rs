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
//! ```rust
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
//!
//! Currently missing relevant features (TODOs) are:
//! - Arrays
//! - Structs (and custom types as such)
//! - Extern functions
//! - Strings
//! - Loops
//! ```

use mir::{scopehints::TypeHints, typecheck::TypeCheck, typeinference::TypeInference};
use reid_lib::Context;

use crate::{ast::TopLevelStatement, lexer::Token, token_stream::TokenStream};

mod ast;
mod codegen;
mod lexer;
pub mod mir;
mod pad_adapter;
mod token_stream;
mod util;

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

    println!("{}", &mir_context);

    let hints = TypeHints::default();

    let state = mir_context.pass(&mut TypeInference { hints: &hints });
    dbg!(&state, &hints);
    println!("{}", &mir_context);

    let state = mir_context.pass(&mut TypeCheck { hints: &hints });
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
