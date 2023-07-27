use std::{fmt::Debug, iter::Peekable, str::Chars};

pub static EASIEST: &str = include_str!("../easiest.reid");
// pub static EASY: &str = include_str!("../easy.reid");
// pub static MEDIUM: &str = include_str!("../medium.reid");
// pub static HARD: &str = include_str!("../hard.reid");

static DECIMAL_NUMERICS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

pub fn tokenize<T: Into<String>>(to_tokenize: T) -> Result<Vec<Token>, String> {
    let to_tokenize = to_tokenize.into();
    let mut position = (0, 1);
    let mut cursor = Cursor {
        char_stream: to_tokenize.chars().peekable(),
        position,
    };

    let mut tokens = Vec::new();

    while let Some(character) = &cursor.consume() {
        position.0 += 1;
        if *character == '\n' {
            position.1 += 1;
            position.0 = 0;
        }

        let peek = cursor.peek();

        let variant = match character {
            // Whitespace
            w if w.is_whitespace() => continue,
            // Comments
            '/' if peek == Some(&'/') => {
                while !matches!(&cursor.peek(), Some('\n')) {
                    cursor.consume();
                }
                continue;
            }
            // "words"
            c if c.is_alphabetic() => {
                let mut value = character.to_string();
                while let Some(c) = &cursor.peek() {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    value += &c.to_string();
                    cursor.consume();
                }

                // Check for keywords
                let variant = match value.as_str() {
                    "let" => TokenVariant::LetKeyword,
                    _ => TokenVariant::Identifier(value),
                };
                variant
            }
            // Decimals
            c if DECIMAL_NUMERICS.contains(c) => {
                let mut value = character.to_string();
                while let Some(c) = &cursor.peek() {
                    if !DECIMAL_NUMERICS.contains(c) {
                        break;
                    }
                    value += &c.to_string();
                    cursor.consume();
                }
                TokenVariant::DecimalValue(value)
            }
            // Single character tokens
            '=' => TokenVariant::Equals,
            ';' => TokenVariant::Semicolon,
            // Invalid token
            _ => Err(format!(
                "Unknown token '{}' at {}, {}",
                character, position.0, position.1
            ))?,
        };

        tokens.push(Token { variant, position });
    }

    position.0 += 1;

    tokens.push(Token {
        variant: TokenVariant::Eof,
        position,
    });

    Ok(tokens)
}

pub struct Token {
    variant: TokenVariant,
    position: Position,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?} (Ln {}, Col {})",
            self.variant, self.position.1, self.position.0
        ))
    }
}

pub type Position = (u32, u32);

#[derive(Debug)]
pub enum TokenVariant {
    LetKeyword,
    Semicolon,
    Equals,
    Identifier(String),
    /// Number with at most one decimal point
    DecimalValue(String),
    Eof,
}

pub struct Cursor<'a> {
    pub position: Position,
    char_stream: Peekable<Chars<'a>>,
}

impl<'a> Cursor<'a> {
    fn consume(&mut self) -> Option<char> {
        let next = self.char_stream.next();
        self.position.0 += 1;
        if let Some('\n') = next {
            self.position.1 += 1;
            self.position.0 = 0;
        }
        next
    }

    fn peek(&mut self) -> Option<&char> {
        self.char_stream.peek()
    }
}
