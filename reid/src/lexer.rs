use std::{fmt::Debug, hint::unreachable_unchecked, str::Chars};

static DECIMAL_NUMERICS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord)]
pub enum Token {
    /// Values
    Identifier(String),
    /// Number with at most one decimal point
    DecimalValue(u64),
    /// Some character literal that was surrounded by 'single-quotes'.
    CharLit(String),
    /// Some string literal that was surrounded by "double-quotes".
    StringLit(String),

    // Keywords
    /// `let`
    LetKeyword,
    /// `mut`
    MutKeyword,
    /// `import`
    ImportKeyword,
    /// `return`
    ReturnKeyword,
    /// `fn`
    FnKeyword,
    /// `pub`
    PubKeyword,
    /// `as`
    AsKeyword,
    /// `->`
    Arrow,
    /// `if`
    If,
    /// `else`
    Else,
    /// `true`
    True,
    /// `false`
    False,
    /// `extern`
    Extern,
    /// `struct`
    Struct,

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
    Star,
    /// `-`
    Minus,

    /// `>`
    GreaterThan,
    /// `<`
    LessThan,
    /// `&`
    Et,
    /// `!`
    Exclamation,

    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    /// `{`
    BraceOpen,
    /// `}`
    BraceClose,
    /// `[`
    BracketOpen,
    /// `]`
    BracketClose,
    /// `,`
    Comma,
    /// `.`
    Dot,

    Eof,
}

impl Token {
    pub fn get_token_prec(&self) -> i8 {
        match &self {
            Token::Plus => 10,
            Token::Minus => 10,
            Token::Star => 20,
            _ => -1,
        }
    }
}

impl From<Token> for String {
    fn from(value: Token) -> Self {
        format!("{:?}", value)
    }
}

impl Token {
    pub fn len(&self) -> usize {
        self.to_string().len()
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match &self {
            Token::Identifier(ident) => ident.clone(),
            Token::DecimalValue(val) => val.to_string(),
            Token::CharLit(lit) => format!("\'{}\'", lit),
            Token::StringLit(lit) => format!("\"{}\"", lit),
            Token::LetKeyword => String::from("let"),
            Token::MutKeyword => String::from("mut"),
            Token::ImportKeyword => String::from("import"),
            Token::ReturnKeyword => String::from("return"),
            Token::FnKeyword => String::from("fn"),
            Token::PubKeyword => String::from("pub"),
            Token::Arrow => String::from("=>"),
            Token::If => String::from("if"),
            Token::Else => String::from("else"),
            Token::True => String::from("true"),
            Token::False => String::from("false"),
            Token::Extern => String::from("extern"),
            Token::Struct => String::from("struct"),
            Token::AsKeyword => String::from("as"),
            Token::Semi => String::from(';'),
            Token::Equals => String::from('='),
            Token::Colon => String::from(':'),
            Token::Plus => String::from('+'),
            Token::Star => String::from('*'),
            Token::Minus => String::from('-'),
            Token::GreaterThan => String::from('>'),
            Token::LessThan => String::from('<'),
            Token::Et => String::from('&'),
            Token::Exclamation => String::from('!'),
            Token::ParenOpen => String::from('('),
            Token::ParenClose => String::from(')'),
            Token::BraceOpen => String::from('{'),
            Token::BraceClose => String::from('}'),
            Token::BracketOpen => String::from('['),
            Token::BracketClose => String::from(']'),
            Token::Comma => String::from(','),
            Token::Dot => String::from('.'),
            Token::Eof => String::new(),
        }
    }
}

/// A token with a position
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
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

/// (Column, Line)
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Ord)]
pub struct Position(pub u32, pub u32);

impl Position {
    pub fn add(&self, num: u32) -> Position {
        Position(self.0 + num, self.1)
    }

    pub fn sub(&self, num: u32) -> Position {
        Position(self.0 - num, self.1)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.1.partial_cmp(&other.1) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.0.partial_cmp(&other.0)
    }
}

pub struct Cursor<'a> {
    pub position: Position,
    pub char_stream: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn next(&mut self) -> Option<char> {
        let next = self.char_stream.next();
        if let Some('\n') = next {
            self.position.1 += 1;
            self.position.0 = 0;
        }
        self.position.0 += 1;
        next
    }

    fn first(&mut self) -> Option<char> {
        // `.next()` optimizes better than `.nth(0)`
        self.char_stream.clone().next()
    }

    #[allow(dead_code)] // Is this actually needed?
    fn second(&mut self) -> Option<char> {
        // `.next()` optimizes better than `.nth(1)`
        let mut stream = self.char_stream.clone();
        stream.next();
        stream.next()
    }
}

/// Take source text and produce a list of [`FullToken`]s from it, ie.
/// tokenizing it.
pub fn tokenize<T: Into<String>>(to_tokenize: T) -> Result<Vec<FullToken>, Error> {
    let to_tokenize = to_tokenize.into();
    let mut cursor = Cursor {
        char_stream: to_tokenize.chars(),
        position: Position(0, 1),
    };

    let mut tokens = Vec::new();

    while let Some(character) = &cursor.next() {
        // Save "current" token first character position
        let position = cursor.position.sub(1);

        let variant = match character {
            // Whitespace
            w if w.is_whitespace() => continue,
            // Comments
            '/' if cursor.first() == Some('/') => {
                while !matches!(cursor.first(), Some('\n') | None) {
                    cursor.next();
                }
                continue;
            }
            '\"' | '\'' => {
                let mut value = String::new();
                let mut escape_next = false;
                while cursor.first().is_some()
                    && (cursor.first() != Some(*character) || escape_next)
                {
                    if cursor.first() == Some('\\') && !escape_next {
                        cursor.next(); // Consume backslash and always add next character
                        escape_next = true;
                    } else {
                        let c = &cursor.next().unwrap();
                        if escape_next {
                            value += &escape_char(&c).to_string();
                        } else {
                            value += &c.to_string();
                        }
                        escape_next = false;
                    }
                }
                if cursor.first() == Some(*character) {
                    cursor.next();
                } else {
                    return Err(Error::MissingQuotation(position));
                }
                match character {
                    '\'' => Token::CharLit(value),
                    '\"' => Token::StringLit(value),
                    _ => unreachable!(),
                }
            }
            // "words"
            c if c.is_alphabetic() => {
                let mut value = character.to_string();
                while let Some(c) = cursor.first() {
                    if !(c.is_ascii_alphanumeric() || c == '_') {
                        break;
                    }
                    value += &c.to_string();
                    cursor.next();
                }

                // Check for keywords
                let variant = match value.as_str() {
                    "let" => Token::LetKeyword,
                    "mut" => Token::MutKeyword,
                    "import" => Token::ImportKeyword,
                    "return" => Token::ReturnKeyword,
                    "fn" => Token::FnKeyword,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "true" => Token::True,
                    "false" => Token::False,
                    "extern" => Token::Extern,
                    "pub" => Token::PubKeyword,
                    "struct" => Token::Struct,
                    "as" => Token::AsKeyword,
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
                Token::DecimalValue(value.parse().expect("Decimal not parseable to u64"))
            }
            '-' if cursor.first() == Some('>') => {
                cursor.next(); // Eat `>`
                Token::Arrow
            }
            // Single character tokens
            '=' => Token::Equals,
            ';' => Token::Semi,
            ':' => Token::Colon,
            '+' => Token::Plus,
            '*' => Token::Star,
            '-' => Token::Minus,
            '>' => Token::GreaterThan,
            '<' => Token::LessThan,
            '&' => Token::Et,
            '!' => Token::Exclamation,
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '[' => Token::BracketOpen,
            ']' => Token::BracketClose,
            '{' => Token::BraceOpen,
            '}' => Token::BraceClose,
            ',' => Token::Comma,
            '.' => Token::Dot,
            // Invalid token
            _ => Err(Error::InvalidToken(*character, cursor.position))?,
        };

        tokens.push(FullToken {
            token: variant,
            position,
        });
    }

    tokens.push(FullToken {
        token: Token::Eof,
        position: cursor.position,
    });

    Ok(tokens)
}

fn escape_char(c: &char) -> char {
    match c {
        't' => '\t',
        'n' => '\n',
        'r' => '\r',
        '0' => '\0',
        _ => *c,
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    #[error("Invalid token '{}' ", .0)]
    InvalidToken(char, Position),
    #[error("String literal is never finished!")]
    MissingQuotation(Position),
}

impl Error {
    pub fn get_position(&self) -> &Position {
        match self {
            Error::InvalidToken(_, pos) => pos,
            Error::MissingQuotation(pos) => pos,
        }
    }
}
