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
//! Specifications and a bunch of [documentation for the language can be found
//! here](./documentation/).
//!
//! An example of a real whole program (a CPU pathtracer) can be found [in
//! examples/cpu_raytracer.reid](./examples/cpu_raytracer.reid), go have a look!
//!
//! Reid is currently able to (non-exhaustively):
//! - Do basic algebra binary and unary-operations (e.g. Add, Sub, Div, Mult,
//!   And, Not)
//! - Resolve complex one-liners correctly using PEDMAS (e.g. `5 + 2 * 5 - 5 *
//!   5` is calculated correctly)
//! - Handle borrows/derefs, pointers.
//! - Declare and call functions with varying parameters and return types
//! - Perform type-checking and type-inference such that return-types and
//!   parameter types must always match.
//! - Do simple logic-operations (e.g. If/And/Or)
//! - Handle, access, define and initialize structs and arrays.
//! - Define and execute For/While loops
//! - Output detailed debug information
//! - Define extern functions that can be linked to outside modules such as
//!   `libc`.
//! - Define custom binary operations for any two types that hasn't been defined
//!   previously (such as `u16 + u32`).
//!
//!
//! An example program of Reid, that calculates the 5th fibonacci number:
//! ```reid
//! fn main() -> u16 {
//!     return fibonacci(5);
//! }
//! fn fibonacci(n: u16) -> u16 {
//!     if n <= 2 {
//!         return 1;
//!     }
//!     return fibonacci(n-1) + fibonacci(n-2);
//! }
//! ```
//!
//! TODOs still (see README.md for more)
//! - Error handling
//! - Lexing & parsing of whitespace and comments as well
//! - LSP implementation
//! ```

use std::{collections::HashMap, path::PathBuf};

use ast::{
    lexer::{self, FullToken, Token},
    token_stream::TokenStream,
};
use codegen::intrinsics::{form_intrinsic_binops, form_intrinsics};
use error_raporting::{ErrorKind as ErrorRapKind, ErrorModules, ReidError};
use mir::{
    linker::LinkerPass,
    pass::BinopMap,
    typecheck::{typecheck::TypeCheck, typeinference::TypeInference, typerefs::TypeRefs},
};
use reid_lib::{compile::CompileOutput, Context};

use crate::{
    ast::TopLevelStatement,
    mir::macros::{form_macros, MacroModule, MacroPass},
};

pub mod ast;
mod codegen;
pub mod error_raporting;
pub mod ld;
pub mod mir;
mod pad_adapter;
mod util;

pub fn parse_module<'map, T: Into<String>>(
    source: &str,
    name: T,
    map: &'map mut ErrorModules,
) -> Result<(mir::SourceModuleId, Vec<FullToken>), ReidError> {
    let id = map.add_module(name.into()).unwrap();
    map.set_source(id, source.to_owned());

    let tokens = ReidError::from_lexer(lexer::tokenize(source), map.clone(), id)?;

    map.set_tokens(id, tokens.clone());

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#?}", &tokens);

    Ok((id, tokens))
}

pub fn compile_module<'map>(
    module_id: mir::SourceModuleId,
    tokens: Vec<FullToken>,
    map: &'map mut ErrorModules,
    path: Option<PathBuf>,
    is_main: bool,
) -> Result<Result<mir::Module, (ast::Module, ReidError)>, ReidError> {
    let module = map.module(&module_id).cloned().unwrap();

    let mut token_stream = TokenStream::from(&tokens);

    let mut statements = Vec::new();

    while !matches!(token_stream.peek().unwrap_or(Token::Eof), Token::Eof) {
        let statement = ReidError::from_parser(token_stream.parse::<TopLevelStatement>(), map.clone(), module_id)?;
        statements.push(statement);
    }

    let errors = token_stream.errors();

    drop(token_stream);

    let ast_module = ast::Module {
        name: module.name,
        tokens: tokens,
        top_level_statements: statements,
        path,
        is_main,
    };

    if errors.len() > 0 {
        // dbg!(&ast_module);
        return Ok(Err((
            ast_module,
            ReidError::from_kind(
                errors
                    .into_iter()
                    .map(|e| {
                        error_raporting::ErrorKind::from(mir::pass::Error {
                            metadata: mir::Metadata {
                                source_module_id: module_id,
                                range: *e.get_range().unwrap_or(&Default::default()),
                                position: None,
                            },
                            kind: e,
                        })
                    })
                    .collect(),
                map.clone(),
            ),
        )));
    }

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    dbg!(&ast_module);

    Ok(Ok(ast_module.process(module_id)))
}

pub fn perform_all_passes<'map>(
    context: &mut mir::Context,
    module_map: &'map mut ErrorModules,
) -> Result<(), ReidError> {
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    dbg!(&context);

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#}", &context);

    let state = context.pass(&mut LinkerPass {
        module_map,
        is_lib: true,
    })?;

    for module in &mut context.modules {
        for intrinsic in form_intrinsics() {
            module.1.functions.insert(0, intrinsic);
        }
    }

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:-^100}", "LINKER OUTPUT");
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#}", &context);
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    dbg!(&state);

    if !state.errors.is_empty() {
        return Err(ReidError::from_kind(
            state.errors.iter().map(|e| e.clone().into()).collect(),
            module_map.clone(),
        ));
    }

    let mut macro_modules: HashMap<_, MacroModule> = HashMap::new();
    for (k, v) in &context.modules {
        macro_modules.insert(k.clone(), v.into());
    }

    let mut macro_pass = MacroPass {
        macros: form_macros(),
        module_map: macro_modules,
    };
    let state = context.pass(&mut macro_pass)?;

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:-^100}", "MACRO OUTPUT");
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#}", &context);
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    dbg!(&state);

    if !state.errors.is_empty() {
        return Err(ReidError::from_kind(
            state.errors.iter().map(|e| e.clone().into()).collect(),
            module_map.clone(),
        ));
    }

    let mut binops = BinopMap::default();
    for module in &mut context.modules {
        for intrinsic in form_intrinsic_binops() {
            binops
                .set(
                    mir::pass::BinopKey {
                        params: (intrinsic.lhs.ty.clone(), intrinsic.rhs.ty.clone()),
                        operator: intrinsic.op,
                    },
                    mir::pass::ScopeBinopDef {
                        hands: (intrinsic.lhs.ty.clone(), intrinsic.rhs.ty.clone()),
                        operator: intrinsic.op,
                        return_ty: intrinsic.return_type.clone(),
                    },
                )
                .ok();
            module.1.binop_defs.insert(0, intrinsic);
        }
    }

    let mut refs = TypeRefs::with_binops(binops);

    let state = context.pass(&mut TypeInference { refs: &mut refs })?;

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:-^100}", "TYPE INFERRER OUTPUT");
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{}", &refs);
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#}", &context);
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    dbg!(&state);

    if !state.errors.is_empty() {
        return Err(ReidError::from_kind(
            state
                .errors
                .iter()
                .map(|e| ErrorRapKind::TypeInferenceError(e.clone()))
                .collect::<Vec<_>>(),
            module_map.clone(),
        ));
    }

    let state = context.pass(&mut TypeCheck { refs: &refs })?;

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:-^100}", "TYPECHECKER OUTPUT");
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#}", &context);
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    dbg!(&state);

    if !state.errors.is_empty() {
        return Err(ReidError::from_kind(
            state
                .errors
                .iter()
                .map(|e| ErrorRapKind::TypeCheckError(e.clone()))
                .collect::<Vec<_>>(),
            module_map.clone(),
        ));
    }

    #[cfg(feature = "context_debug")]
    dbg!(&context);

    Ok(())
}

/// Takes in a bit of source code, parses and compiles it and produces `hello.o`
/// and `hello.asm` from it, which can be linked using `ld` to produce an
/// executable file.
pub fn compile_and_pass<'map>(
    source: &str,
    path: PathBuf,
    module_map: &'map mut ErrorModules,
    cpu: Option<String>,
    features: Vec<String>,
) -> Result<(CompileOutput, CustomIRs), ReidError> {
    let path = path.canonicalize().unwrap();
    let name = path.file_name().unwrap().to_str().unwrap().to_owned();

    let (id, tokens) = parse_module(source, name, module_map)?;
    let module = compile_module(id, tokens, module_map, Some(path.clone()), true)?.map_err(|(_, e)| e)?;

    let mut mir_context = mir::Context::from(vec![module], path.parent().unwrap().to_owned());

    perform_all_passes(&mut mir_context, module_map)?;

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:-^100}", "FINAL OUTPUT");
    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{:#}", &mir_context);

    let mut context = Context::new(format!("Reid ({})", env!("CARGO_PKG_VERSION")));
    let codegen_modules = match mir_context.codegen(&mut context) {
        Ok(modules) => modules,
        Err(e) => Err(ReidError::from_kind(vec![e.into()], module_map.clone()))?,
    };

    #[cfg(debug_assertions)]
    #[cfg(feature = "log_output")]
    println!("{}", &codegen_modules.context);

    let compiled = codegen_modules.compile(cpu, features);
    Ok((
        compiled.output(),
        CustomIRs {
            llir: format!("{}", codegen_modules.context),
            mir: format!("{}", mir_context),
        },
    ))
}

pub struct CustomIRs {
    pub llir: String,
    pub mir: String,
}

pub fn compile_simple(
    source: &str,
    path: PathBuf,
    cpu: Option<String>,
    features: Vec<String>,
) -> Result<(CompileOutput, CustomIRs), ReidError> {
    let mut map = ErrorModules::default();
    compile_and_pass(source, path, &mut map, cpu, features)
}
