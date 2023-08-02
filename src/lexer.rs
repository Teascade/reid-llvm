use std::{fmt::Debug, iter::Peekable, str::Chars};

static DECIMAL_NUMERICS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    // Values
    Identifier(String),
    /// Number with at most one decimal point
    DecimalValue(String),

    // Keywords
    /// `let`
    LetKeyword,
    /// `import`
    ImportKeyword,
    /// `return`
    ReturnKeyword,
    /// `fn`
    FnKeyword,

    // Symbols
    /// `;`
    Semi,
    /// `=`
    Equals,
    /// `:`
    Colon,
    /// `+`
    Plus,
    /// `*`
    Times,
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    /// `{`
    BraceOpen,
    /// `}`
    BraceClose,
    /// `,`
    Comma,

    Eof,
}

impl Token {
    pub fn get_token_prec(&self) -> i8 {
        match &self {
            Token::Plus => 10,
            Token::Times => 20,
            _ => -1,
        }
    }
}

#[derive(Clone)]
pub struct FullToken {
    pub token: Token,
    pub position: Position,
}

impl Debug for FullToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?} (Ln {}, Col {})",
            self.token, self.position.1, self.position.0
        ))
    }
}

pub type Position = (u32, u32);

const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    pub position: Position,
    char_stream: Chars<'a>,
}

impl<'a> Cursor<'a> {
    fn next(&mut self) -> Option<char> {
        let next = self.char_stream.next();
        self.position.0 += 1;
        if let Some('\n') = next {
            self.position.1 += 1;
            self.position.0 = 0;
        }
        next
    }

    fn first(&mut self) -> Option<char> {
        // `.next()` optimizes better than `.nth(0)`
        self.char_stream.clone().next()
    }

    fn second(&mut self) -> Option<char> {
        // `.next()` optimizes better than `.nth(1)`
        let mut stream = self.char_stream.clone();
        stream.next();
        stream.next()
    }
}

pub fn tokenize<T: Into<String>>(to_tokenize: T) -> Result<Vec<FullToken>, String> {
    let to_tokenize = to_tokenize.into();
    let mut position = (0, 1);
    let mut cursor = Cursor {
        char_stream: to_tokenize.chars(),
        position,
    };

    let mut tokens = Vec::new();

    while let Some(character) = &cursor.next() {
        let variant = match character {
            // Whitespace
            w if w.is_whitespace() => continue,
            // Comments
            '/' if cursor.first() == Some('/') => {
                while !matches!(cursor.first(), Some('\n')) {
                    cursor.next();
                }
                continue;
            }
            // "words"
            c if c.is_alphabetic() => {
                let mut value = character.to_string();
                while let Some(c) = cursor.first() {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    value += &c.to_string();
                    cursor.next();
                }

                // Check for keywords
                let variant = match value.as_str() {
                    "let" => Token::LetKeyword,
                    "import" => Token::ImportKeyword,
                    "return" => Token::ReturnKeyword,
                    "fn" => Token::FnKeyword,
                    _ => Token::Identifier(value),
                };
                variant
            }
            // Decimals
            c if DECIMAL_NUMERICS.contains(c) => {
                let mut value = character.to_string();
                while let Some(c) = cursor.first() {
                    if !DECIMAL_NUMERICS.contains(&c) {
                        break;
                    }
                    value += &c.to_string();
                    cursor.next();
                }
                Token::DecimalValue(value)
            }
            // Single character tokens
            '=' => Token::Equals,
            ';' => Token::Semi,
            ':' => Token::Colon,
            '+' => Token::Plus,
            '*' => Token::Times,
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '{' => Token::BraceOpen,
            '}' => Token::BraceClose,
            ',' => Token::Comma,
            // Invalid token
            _ => Err(format!(
                "Unknown token '{}' at {}, {}",
                character, position.0, position.1
            ))?,
        };

        tokens.push(FullToken {
            token: variant,
            position,
        });
    }

    position.0 += 1;

    tokens.push(FullToken {
        token: Token::Eof,
        position,
    });

    Ok(tokens)
}
