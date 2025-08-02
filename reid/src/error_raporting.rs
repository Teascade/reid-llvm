use std::{
    collections::HashMap,
    fmt::{Debug, Write},
};

use crate::{
    ast::token_stream::{self, TokenRange},
    codegen,
    lexer::{self, Cursor, FullToken, Position},
    mir::{self, macros, pass, typecheck, Metadata, SourceModuleId},
};

use crate::mir::typecheck::ErrorKind as TypecheckError;

fn label(text: &str) -> &str {
    #[cfg(debug_assertions)]
    {
        return text;
    }
    #[cfg(not(debug_assertions))]
    ""
}

#[derive(thiserror::Error, Debug, Clone, PartialEq)]
pub enum ErrorKind {
    #[error("{}{}", label("(Lexing) "), .0.kind)]
    LexerError(#[from] mir::pass::Error<lexer::Error>),
    #[error("{}{}", label("(Parsing) "), .0.kind)]
    ParserError(#[from] mir::pass::Error<token_stream::Error>),
    #[error("{}{}", label("(TypeCheck) "), .0.kind)]
    TypeCheckError(#[source] mir::pass::Error<typecheck::ErrorKind>),
    #[error("{}{}", label("(TypeInference) "), .0.kind)]
    TypeInferenceError(#[source] mir::pass::Error<TypecheckError>),
    #[error("{}{}", label("(Linker) "), .0.kind)]
    LinkerError(#[from] mir::pass::Error<mir::linker::ErrorKind>),
    #[error("{}{}", label("(Macro) "), .0)]
    MacroError(#[from] mir::pass::Error<macros::ErrorKind>),
    #[error("{}{}", label("(Codegen) "), .0)]
    CodegenError(#[from] codegen::ErrorKind),
}

impl ErrorKind {
    pub fn from_typecheck(err: mir::pass::Error<typecheck::ErrorKind>) -> ErrorKind {
        ErrorKind::TypeCheckError(err)
    }

    pub fn from_typeinference(err: mir::pass::Error<typecheck::ErrorKind>) -> ErrorKind {
        ErrorKind::TypeInferenceError(err)
    }
}

impl ErrorKind {
    pub fn get_meta(&self) -> Metadata {
        match &self {
            ErrorKind::LexerError(error) => error.metadata,
            ErrorKind::ParserError(error) => error.metadata,
            ErrorKind::TypeCheckError(error) => error.metadata,
            ErrorKind::TypeInferenceError(error) => error.metadata,
            ErrorKind::LinkerError(error) => error.metadata,
            ErrorKind::CodegenError(error) => match error {
                codegen::ErrorKind::Null => Default::default(),
            },
            ErrorKind::MacroError(error) => error.metadata,
        }
    }

    pub fn get_type_str(&self) -> &str {
        match self {
            ErrorKind::LexerError(_) => "lexer",
            ErrorKind::ParserError(_) => "parser",
            ErrorKind::TypeCheckError(_) => "typechecker",
            ErrorKind::TypeInferenceError(_) => "type-inferrer",
            ErrorKind::LinkerError(_) => "linker",
            ErrorKind::MacroError(_) => "macro-pass",
            ErrorKind::CodegenError(_) => "codegen",
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorModule {
    pub name: String,
    pub tokens: Option<Vec<FullToken>>,
    pub source: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ErrorModules {
    pub(super) module_map: HashMap<mir::SourceModuleId, ErrorModule>,
    module_counter: mir::SourceModuleId,
}

impl ErrorModules {
    pub fn add_module<T: Into<String>>(&mut self, name: T) -> Option<mir::SourceModuleId> {
        let id = self.module_counter.increment();
        self.module_map.insert(
            id,
            ErrorModule {
                name: name.into(),
                tokens: None,
                source: None,
            },
        );
        Some(id)
    }

    pub fn set_tokens(&mut self, id: mir::SourceModuleId, tokens: Vec<FullToken>) {
        if let Some(module) = self.module_map.get_mut(&id) {
            module.tokens = Some(tokens);
        }
    }

    pub fn set_source(&mut self, id: mir::SourceModuleId, source: String) {
        if let Some(module) = self.module_map.get_mut(&id) {
            module.source = Some(source);
        }
    }

    pub fn module(&self, id: &mir::SourceModuleId) -> Option<&ErrorModule> {
        self.module_map.get(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReidError {
    map: ErrorModules,
    pub errors: Vec<ErrorKind>,
}

impl ReidError {
    pub fn from_lexer<U>(
        result: Result<U, lexer::Error>,
        map: ErrorModules,
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
        map: ErrorModules,
        module: SourceModuleId,
    ) -> Result<U, ReidError> {
        result.map_err(|error| {
            let pass_err = pass::Error {
                metadata: Metadata {
                    source_module_id: module,
                    range: *error.get_range().unwrap_or(&Default::default()),
                    position: None,
                },
                kind: error,
            };
            ReidError {
                map,
                errors: vec![ErrorKind::ParserError(pass_err)],
            }
        })
    }

    pub fn from_kind(errors: Vec<ErrorKind>, map: ErrorModules) -> ReidError {
        ReidError { map, errors }
    }

    pub fn extend(&mut self, other: ReidError) {
        self.errors.extend(other.errors);
    }
}

impl std::error::Error for ReidError {}

impl std::fmt::Display for ReidError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sorted_errors = self.errors.clone();
        sorted_errors.sort_by(|a, b| a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal));
        sorted_errors.dedup();

        let mut curr_module = None;
        for error in sorted_errors {
            let meta = error.get_meta();
            let module = self.map.module(&meta.source_module_id);
            let position = if let Some(module) = module {
                if let Some(tokens) = &module.tokens {
                    meta.range.into_position(tokens).or(meta.position.map(|p| (p, p)))
                } else if let Some(position) = meta.position {
                    Some((position, position))
                } else {
                    None
                }
            } else {
                None
            };

            let module_name = if curr_module != Some(meta.source_module_id) {
                curr_module = Some(meta.source_module_id);
                if let Some(module) = self.map.module_map.get(&meta.source_module_id) {
                    module.name.clone()
                } else {
                    "unknown".to_owned()
                }
            } else {
                "unknown".to_owned()
            };

            writeln!(f, "Errors detected: {}", color_err(format!("{}", module_name))?)?;

            writeln!(f)?;
            write!(f, "  Error: ")?;
            writeln!(f, "{}", color_err(format!("{}", error))?)?;
            write!(
                f,
                "{:>20} {}:{}",
                color_warn("At: ")?,
                module_name,
                position.map(|p| fmt_positions(p)).unwrap_or(String::from("{unknown}")),
            )?;
            if let (Some(position), Some(source)) = (position, &module.and_then(|m| m.source.clone())) {
                writeln!(f, "{}", fmt_lines(source, position, 6)?)?;
            }
        }
        Ok(())
    }
}

impl TokenRange {
    pub fn into_tokens<'v>(&self, tokens: &'v Vec<FullToken>) -> Vec<&'v FullToken> {
        tokens
            .iter()
            .skip(self.start)
            .by_ref()
            .take(self.end + 1 - self.start)
            .collect::<Vec<_>>()
    }

    pub fn into_position<'v>(&self, tokens: &'v Vec<FullToken>) -> Option<(Position, Position)> {
        let tokens = self.into_tokens(tokens);
        get_position(&tokens)
    }
}

fn get_position(tokens: &Vec<&FullToken>) -> Option<(Position, Position)> {
    if let Some(first) = tokens.first() {
        let last = tokens.last().unwrap();
        Some((first.position, last.position.add(last.token.len() as u32)))
    } else {
        None
    }
}

fn into_full_lines<'v>((start, end): (Position, Position)) -> (Position, Position) {
    (Position(0, start.1), Position(u32::MAX, end.1))
}

fn fmt_lines(
    source: &String,
    (highlight_start, highlight_end): (Position, Position),
    ident: usize,
) -> Result<String, std::fmt::Error> {
    let (line_start, line_end) = into_full_lines((highlight_start, highlight_end));
    let mut cursor = Cursor {
        position: Position(0, 1),
        char_stream: source.chars(),
    };

    let mut text = String::new();

    while let Some(c) = cursor.next() {
        if cursor.position.1 > line_end.1 {
            break;
        }
        if cursor.position.1 >= line_start.1 {
            if c == '\n' {
                write!(text, "\n{}", " ".repeat(ident))?;
            } else {
                if cursor.position > highlight_start && cursor.position <= highlight_end {
                    write!(text, "{}", color_highlight(c)?)?;
                } else {
                    text.write_char(c)?;
                }
            }
        }
    }

    Ok(text)
}

fn fmt_positions((start, end): (Position, Position)) -> String {
    if start == end {
        format!("{}:{}", start.1, start.0)
    } else if start.1 == end.1 {
        format!("{}:{} - {}", start.1, start.0, end.0)
    } else {
        format!("{}:{} - {}:{}", start.1, start.0, end.1, end.0)
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

fn color_highlight(elem: impl std::fmt::Display) -> Result<String, std::fmt::Error> {
    let mut text = format!("{}", elem);

    #[cfg(feature = "color")]
    {
        use colored::Colorize;
        text = format!("{}", text.bright_yellow().underline())
    }

    Ok(text)
}
