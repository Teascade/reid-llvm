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
//! - ~~Extern functions~~ (DONE)
//! - ~~Strings~~ (DONE)
//! - Loops
//! - Debug Symbols
//! ```

use std::{convert::Infallible, path::PathBuf};

use error_raporting::{ErrorKind as ErrorRapKind, ModuleMap, ReidError};
use lexer::FullToken;
use mir::{
    linker::LinkerPass, typecheck::TypeCheck, typeinference::TypeInference, typerefs::TypeRefs,
    SourceModuleId,
};
use reid_lib::{compile::CompileOutput, Context};

use crate::{ast::TopLevelStatement, lexer::Token, token_stream::TokenStream};

mod ast;
mod codegen;
mod error_raporting;
mod lexer;
pub mod mir;
mod pad_adapter;
mod token_stream;
mod util;

pub fn parse_module<'map, T: Into<String>>(
    source: &str,
    name: T,
    map: &'map mut ModuleMap,
) -> Result<(mir::SourceModuleId, Vec<FullToken>), ReidError> {
    let id = map.add_module(name.into()).unwrap();

    let tokens = ReidError::from_lexer(lexer::tokenize(source), map.clone(), id)?;

    map.set_tokens(id, tokens.clone());

    #[cfg(debug_assertions)]
    dbg!(&tokens);

    Ok((id, tokens))
}

pub fn compile_module<'map>(
    module_id: mir::SourceModuleId,
    tokens: &Vec<FullToken>,
    map: &'map mut ModuleMap,
    path: Option<PathBuf>,
    is_main: bool,
) -> Result<mir::Module, ReidError> {
    let module = map.get_module(&module_id).cloned().unwrap();

    let mut token_stream = TokenStream::from(&tokens);

    let mut statements = Vec::new();

    while !matches!(token_stream.peek().unwrap_or(Token::Eof), Token::Eof) {
        let statement = ReidError::from_parser(
            token_stream.parse::<TopLevelStatement>(),
            map.clone(),
            module_id,
        )?;
        statements.push(statement);
    }

    let ast_module = ast::Module {
        name: module.name,
        top_level_statements: statements,
        path,
        is_main,
    };

    Ok(ast_module.process(module_id))
}

pub fn perform_all_passes<'map>(
    context: &mut mir::Context,
    module_map: &'map mut ModuleMap,
) -> Result<(), ReidError> {
    #[cfg(debug_assertions)]
    dbg!(&context);

    #[cfg(debug_assertions)]
    println!("{}", &context);

    let state = context.pass(&mut LinkerPass { module_map });

    #[cfg(debug_assertions)]
    println!("{}", &context);
    #[cfg(debug_assertions)]
    dbg!(&state);

    if !state.errors.is_empty() {
        return Err(ReidError::from_kind::<()>(
            state.errors.iter().map(|e| e.clone().into()).collect(),
            module_map.clone(),
        ));
    }

    let refs = TypeRefs::default();

    let mut errors = Vec::new();

    let state = context.pass(&mut TypeInference { refs: &refs });

    #[cfg(debug_assertions)]
    dbg!(&refs);
    #[cfg(debug_assertions)]
    println!("{}", &context);
    #[cfg(debug_assertions)]
    dbg!(&state);

    errors.extend(
        state
            .errors
            .iter()
            .map(|e| ErrorRapKind::TypeInferenceError(e.clone()))
            .collect::<Vec<_>>(),
    );

    let state = context.pass(&mut TypeCheck { refs: &refs });

    #[cfg(debug_assertions)]
    println!("{}", &context);
    #[cfg(debug_assertions)]
    dbg!(&state);

    errors.extend(
        state
            .errors
            .iter()
            .map(|e| ErrorRapKind::TypeInferenceError(e.clone()))
            .collect::<Vec<_>>(),
    );

    if !errors.is_empty() {
        return Err(ReidError::from_kind::<()>(errors, module_map.clone()));
    }

    Ok(())
}

/// Takes in a bit of source code, parses and compiles it and produces `hello.o`
/// and `hello.asm` from it, which can be linked using `ld` to produce an
/// executable file.
pub fn compile_and_pass<'map>(
    source: &str,
    path: PathBuf,
    module_map: &'map mut ModuleMap,
) -> Result<CompileOutput, ReidError> {
    let path = path.canonicalize().unwrap();
    let name = path.file_name().unwrap().to_str().unwrap().to_owned();

    let (id, tokens) = parse_module(source, name, module_map).unwrap();
    let module = compile_module(id, &tokens, module_map, Some(path.clone()), true)?;

    let mut mir_context = mir::Context::from(vec![module], path.parent().unwrap().to_owned());

    perform_all_passes(&mut mir_context, module_map)?;

    let mut context = Context::new();
    let codegen_modules = mir_context.codegen(&mut context);

    #[cfg(debug_assertions)]
    dbg!(&codegen_modules);

    let compiled = codegen_modules.compile();
    Ok(compiled.output())
}

pub fn compile_simple(source: &str, path: PathBuf) -> Result<CompileOutput, ReidError> {
    let mut map = ModuleMap::default();
    compile_and_pass(source, path, &mut map)
}
