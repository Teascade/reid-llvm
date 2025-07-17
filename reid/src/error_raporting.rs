use std::{
    collections::HashMap,
    fmt::{Debug, Write},
};

use crate::{
    ast,
    lexer::{self, FullToken, Position},
    mir::{self, pass, Metadata, SourceModuleId},
    token_stream::{self, TokenRange},
};

impl<T: std::error::Error + std::fmt::Display> pass::Error<T> {
    fn fmt_simple(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

fn label(text: &str) -> &str {
    #[cfg(debug_assertions)]
    {
        return text;
    }
    #[cfg(not(debug_assertions))]
    ""
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    #[error("{}{}", label("(Lexing) "), .0.kind)]
    LexerError(#[from] mir::pass::Error<lexer::Error>),
    #[error("{}{}", label("(Parsing) "), .0.kind)]
    ParserError(#[from] mir::pass::Error<token_stream::Error>),
    #[error("{}{}", label("(TypeCheck) "), .0.kind)]
    TypeCheckError(#[source] mir::pass::Error<mir::typecheck::ErrorKind>),
    #[error("{}{}", label("(TypeInference) "), .0.kind)]
    TypeInferenceError(#[source] mir::pass::Error<mir::typecheck::ErrorKind>),
    #[error("{}{}", label("(Linker) "), .0.kind)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReidError {
    map: ModuleMap,
    errors: Vec<ErrorKind>,
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

impl std::error::Error for ReidError {}

impl std::fmt::Display for ReidError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sorted_errors = self.errors.clone();
        sorted_errors.sort_by(|a, b| a.cmp(&b));
        sorted_errors.dedup();

        let mut curr_module = None;
        for error in sorted_errors {
            let meta = error.get_meta();
            let module = self.map.get_module(&meta.source_module_id).unwrap();
            let (position_fmt, line_fmt) = if let Some(tokens) = &module.tokens {
                let range_tokens = meta.range.into_tokens(&tokens);
                let position = get_position(&range_tokens).unwrap();
                let full_lines = get_full_lines(&tokens, position);
                (
                    fmt_positions(get_position(&full_lines).unwrap()),
                    Some(fmt_tokens(&full_lines, &range_tokens)),
                )
            } else if let Some(position) = meta.position {
                (fmt_positions((position, position)), None)
            } else {
                ("unknown".to_owned(), None)
            };

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
            writeln!(f)?;
            write!(f, "  Error: ")?;
            writeln!(f, "{}", color_err(format!("{}", error))?)?;
            writeln!(f, "{:>20}{}", color_warn("At: ")?, position_fmt)?;
            if let Some(line_fmt) = line_fmt {
                writeln!(f, "{:>20}{}", color_warn("")?, line_fmt)?;
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
            .take(self.end - self.start)
            .collect::<Vec<_>>()
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

fn get_full_lines<'v>(
    tokens: &'v Vec<FullToken>,
    (start, end): (Position, Position),
) -> Vec<&'v FullToken> {
    let (first_token_pos, _) = tokens
        .iter()
        .enumerate()
        .find(|(_, token)| token.position.1 == start.1)
        .unwrap();
    tokens
        .iter()
        .skip(first_token_pos)
        .by_ref()
        .take_while(|token| token.position.1 <= end.1)
        .collect::<Vec<_>>()
}

fn fmt_tokens(tokens: &Vec<&FullToken>, highlighted: &Vec<&FullToken>) -> String {
    let mut text = String::new();
    let mut last_likes_space = false;
    for (i, token) in tokens.iter().enumerate() {
        if token.token.needs_space() || (token.token.likes_space() && last_likes_space) {
            text += " ";
        }
        last_likes_space = token.token.likes_space();

        let mut token_fmt = format!("{}", token.token.to_string());
        if highlighted.contains(token) {
            token_fmt = color_underline(token_fmt).unwrap();
        }
        text += &token_fmt;
        if token.token.is_newline() && i > (tokens.len() - 1) {
            text += "\n"
        }
    }

    text
}

fn fmt_positions((start, end): (Position, Position)) -> String {
    if start == end {
        format!("ln {}, col {}", start.1, start.0)
    } else if start.1 == end.1 {
        format!("ln {}, col {}-{}", start.1, start.0, end.0)
    } else {
        format!("{}:{} - {}:{}", start.1, start.0, end.1, end.0)
    }
}

impl lexer::Token {
    fn likes_space(&self) -> bool {
        match self {
            lexer::Token::Identifier(_) => true,
            lexer::Token::DecimalValue(_) => true,
            lexer::Token::StringLit(_) => true,
            lexer::Token::LetKeyword => true,
            lexer::Token::MutKeyword => true,
            lexer::Token::ImportKeyword => true,
            lexer::Token::ReturnKeyword => true,
            lexer::Token::FnKeyword => true,
            lexer::Token::PubKeyword => true,
            lexer::Token::Arrow => true,
            lexer::Token::If => true,
            lexer::Token::Else => true,
            lexer::Token::True => true,
            lexer::Token::False => true,
            lexer::Token::Extern => true,
            lexer::Token::Struct => true,
            lexer::Token::Equals => true,
            _ => false,
        }
    }

    fn needs_space(&self) -> bool {
        match self {
            lexer::Token::LetKeyword => true,
            lexer::Token::MutKeyword => true,
            lexer::Token::ImportKeyword => true,
            lexer::Token::ReturnKeyword => true,
            lexer::Token::FnKeyword => true,
            lexer::Token::PubKeyword => true,
            lexer::Token::Arrow => true,
            lexer::Token::Equals => true,
            _ => false,
        }
    }

    fn is_newline(&self) -> bool {
        match self {
            lexer::Token::Semi => true,
            lexer::Token::BraceOpen => true,
            lexer::Token::BraceClose => true,
            _ => false,
        }
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

fn color_underline(elem: impl std::fmt::Display) -> Result<String, std::fmt::Error> {
    let mut text = format!("{}", elem);

    #[cfg(feature = "color")]
    {
        use colored::Colorize;
        text = format!("{}", text.bright_yellow().underline())
    }

    Ok(text)
}
