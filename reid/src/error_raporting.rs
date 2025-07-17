use std::{
    collections::HashMap,
    fmt::{Debug, Write},
};

use crate::{
    ast,
    lexer::{self, FullToken},
    mir::{self, pass, Metadata, SourceModuleId},
    token_stream,
};

impl<T: std::error::Error + std::fmt::Display> pass::Error<T> {
    fn fmt_simple(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    #[error("Lexing: {}", .0.kind)]
    LexerError(#[from] mir::pass::Error<lexer::Error>),
    #[error("Parsing: {}", .0.kind)]
    ParserError(#[from] mir::pass::Error<token_stream::Error>),
    #[error("Typechecking: {}", .0.kind)]
    TypeCheckError(#[source] mir::pass::Error<mir::typecheck::ErrorKind>),
    #[error("Type Inference: {}", .0.kind)]
    TypeInferenceError(#[source] mir::pass::Error<mir::typecheck::ErrorKind>),
    #[error("Linking: {}", .0.kind)]
    LinkerError(#[from] mir::pass::Error<mir::linker::ErrorKind>),
}

impl ErrorKind {
    pub fn from_typecheck(err: mir::pass::Error<mir::typecheck::ErrorKind>) -> ErrorKind {
        ErrorKind::TypeCheckError(err)
    }

    pub fn from_typeinference(err: mir::pass::Error<mir::typecheck::ErrorKind>) -> ErrorKind {
        ErrorKind::TypeInferenceError(err)
    }
}

impl ErrorKind {
    fn get_meta(&self) -> Metadata {
        match &self {
            ErrorKind::LexerError(error) => error.metadata,
            ErrorKind::ParserError(error) => error.metadata,
            ErrorKind::TypeCheckError(error) => error.metadata,
            ErrorKind::TypeInferenceError(error) => error.metadata,
            ErrorKind::LinkerError(error) => error.metadata,
        }
    }
}

impl PartialOrd for ErrorKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get_meta()
            .source_module_id
            .partial_cmp(&other.get_meta().source_module_id)
    }
}

impl Ord for ErrorKind {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_meta().cmp(&other.get_meta())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrModule {
    pub name: String,
    pub tokens: Option<Vec<FullToken>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleMap {
    module_map: HashMap<mir::SourceModuleId, ErrModule>,
    module_counter: mir::SourceModuleId,
}

impl ModuleMap {
    pub fn add_module<T: Into<String>>(&mut self, name: T) -> Option<mir::SourceModuleId> {
        let id = self.module_counter.increment();
        self.module_map.insert(
            id,
            ErrModule {
                name: name.into(),
                tokens: None,
            },
        );
        Some(id)
    }

    pub fn set_tokens(&mut self, id: mir::SourceModuleId, tokens: Vec<FullToken>) {
        if let Some(module) = self.module_map.get_mut(&id) {
            module.tokens = Some(tokens);
        }
    }

    pub fn get_module(&self, id: &mir::SourceModuleId) -> Option<&ErrModule> {
        self.module_map.get(id)
    }
}

// impl TryFrom<&mir::Context> for ModuleMap {
//     type Error = ();

//     fn try_from(value: &mir::Context) -> Result<Self, Self::Error> {
//         let mut map = HashMap::new();
//         for module in &value.modules {
//             if let Some(_) = map.insert(
//                 module.module_id,
//                 ErrModule {
//                     name: module.name.clone(),
//                     tokens: Some(module.clone()),
//                 },
//             ) {
//                 return Err(());
//             }
//         }
//         let module_counter = value.modules.iter().map(|m| m.module_id).max().ok_or(())?;
//         Ok(ModuleMap {
//             module_map: map,
//             module_counter,
//         })
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReidError {
    map: ModuleMap,
    errors: Vec<ErrorKind>,
}

impl std::error::Error for ReidError {}

impl std::fmt::Display for ReidError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sorted_errors = self.errors.clone();
        sorted_errors.sort_by(|a, b| a.cmp(&b));
        sorted_errors.dedup();

        let mut curr_module = None;
        for error in sorted_errors {
            let meta = error.get_meta();
            if curr_module != Some(meta.source_module_id) {
                curr_module = Some(meta.source_module_id);
                writeln!(
                    f,
                    "Errors in module {}:",
                    color_err(format!(
                        "{}",
                        self.map
                            .module_map
                            .get(&meta.source_module_id)
                            .unwrap()
                            .name
                    ))?
                )?;
            }
            write!(f, "  {}: ", color_err("Error")?)?;
            writeln!(f, "{}", error)?;
            writeln!(f, "      {}: {}", color_warn("At")?, meta)?;
        }
        Ok(())
    }
}

impl ReidError {
    pub fn from_lexer<U>(
        result: Result<U, lexer::Error>,
        map: ModuleMap,
        module: SourceModuleId,
    ) -> Result<U, ReidError> {
        result.map_err(|error| {
            let pass_err = pass::Error {
                metadata: Metadata {
                    source_module_id: module,
                    range: Default::default(),
                    position: Some(*error.get_position()),
                },
                kind: error,
            };
            ReidError {
                map,
                errors: vec![ErrorKind::LexerError(pass_err)],
            }
        })
    }

    pub fn from_parser<U>(
        result: Result<U, token_stream::Error>,
        map: ModuleMap,
        module: SourceModuleId,
    ) -> Result<U, ReidError> {
        result.map_err(|error| {
            let pass_err = pass::Error {
                metadata: Metadata {
                    source_module_id: module,
                    range: Default::default(),
                    position: error.get_position().copied(),
                },
                kind: error,
            };
            ReidError {
                map,
                errors: vec![ErrorKind::ParserError(pass_err)],
            }
        })
    }

    pub fn from_kind<U>(errors: Vec<ErrorKind>, map: ModuleMap) -> ReidError {
        ReidError { map, errors }
    }
}

fn color_err(elem: impl std::fmt::Display) -> Result<String, std::fmt::Error> {
    let mut text = format!("{}", elem);

    #[cfg(feature = "color")]
    {
        use colored::Colorize;
        text = format!("{}", text.bright_red())
    }

    Ok(text)
}

fn color_warn(elem: impl std::fmt::Display) -> Result<String, std::fmt::Error> {
    let mut text = format!("{}", elem);

    #[cfg(feature = "color")]
    {
        use colored::Colorize;
        text = format!("{}", text.bright_yellow())
    }

    Ok(text)
}
