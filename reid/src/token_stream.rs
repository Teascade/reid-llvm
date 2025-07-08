use crate::{
    ast::parse::Parse,
    lexer::{FullToken, Position, Token},
};

pub struct TokenStream<'a, 'b> {
    ref_position: Option<&'b mut usize>,
    tokens: &'a [FullToken],
    pub position: usize,
}

impl<'a, 'b> TokenStream<'a, 'b> {
    pub fn from(tokens: &'a [FullToken]) -> Self {
        TokenStream {
            ref_position: None,
            tokens,
            position: 0,
        }
    }

    pub fn expected_err<T: Into<String>>(&mut self, expected: T) -> Result<Error, Error> {
        Ok(Error::Expected(
            expected.into(),
            self.peek().unwrap_or(Token::Eof),
            self.get_next_position()?,
        ))
    }

    pub fn expect(&mut self, token: Token) -> Result<(), Error> {
        if let Some(peeked) = self.peek() {
            if token == peeked {
                self.position += 1;
                Ok(())
            } else {
                Err(self.expected_err(token)?)
            }
        } else {
            Err(self.expected_err(token)?)
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        let value = if self.tokens.len() < self.position {
            None
        } else {
            Some(self.tokens[self.position].token.clone())
        };
        self.position += 1;
        value
    }

    pub fn peek(&mut self) -> Option<Token> {
        if self.tokens.len() < self.position {
            None
        } else {
            Some(self.tokens[self.position].token.clone())
        }
    }

    /// Parse the next value of trait Parse. If the parse succeeded, the related
    /// tokens are consumed, otherwise token stream does not advance.
    ///
    /// Parsetime-error is returned on failure.
    pub fn parse<T: Parse + std::fmt::Debug>(&mut self) -> Result<T, Error> {
        let (res, pos) = self.parse_meta()?;
        self.position = pos;
        Ok(res)
    }

    /// Parse the next item with Parse-trait (Same as [`TokenStream::parse`])
    /// without consuming the related tokens, essentially only peeking.
    pub fn parse_peek<T: Parse + std::fmt::Debug>(&mut self) -> Result<T, Error> {
        self.parse_meta().map(|(res, _)| res)
    }

    /// Parse the next item with Parse-trait, also mapping it with the given
    /// function. The token-stream is only consumed, if the inner function
    /// retuns an Ok.
    #[allow(dead_code)]
    pub fn parse_map<T: Parse + std::fmt::Debug, F, O>(&mut self, inner: F) -> Result<O, Error>
    where
        F: Fn(T) -> Result<O, Error>,
    {
        let (res, pos) = self.parse_meta::<T>()?;
        match inner(res) {
            Ok(mapped) => {
                self.position = pos;
                Ok(mapped)
            }
            Err(e) => Err(e),
        }
    }

    /// Parses the item with Parse if the condition specified by the
    /// lambda-function is passed. Errors returned from this should not be
    /// passed to the end-user.
    pub fn parse_if<T: Parse + std::fmt::Debug, F>(&mut self, inner: F) -> Result<T, Error>
    where
        F: Fn(&T) -> bool,
    {
        let (res, pos) = self.parse_meta::<T>()?;
        if inner(&res) {
            self.position = pos;
            Ok(res)
        } else {
            Err(Error::IfFailed)
        }
    }

    /// Parse the next item with Parse-trait. If successful, returning the
    /// parsed item and the new position of the TokenStream. Failing, returning
    /// parse-error.
    ///
    /// Used for [`TokenStream::parse`] and [`TokenStream::parse_peek`]
    fn parse_meta<T: Parse + std::fmt::Debug>(&mut self) -> Result<(T, usize), Error> {
        let mut ref_pos = self.position;

        let position = self.position;
        let clone = TokenStream {
            ref_position: Some(&mut ref_pos),
            tokens: self.tokens,
            position,
        };

        match T::parse(clone) {
            Ok(res) => {
                dbg!(&res);
                let new_pos = ref_pos.max(self.position);
                Ok((res, new_pos))
            }
            Err(e) => Err(e),
        }
    }

    fn get_next_position(&self) -> Result<Position, Error> {
        if self.tokens.is_empty() {
            Err(Error::FileEmpty)
        } else {
            let token_idx = self.position.min(self.tokens.len() - 1);
            Ok(self.tokens[token_idx].position)
        }
    }

    pub fn get_range(&self) -> Option<TokenRange> {
        self.ref_position.as_ref().map(|ref_pos| TokenRange {
            start: **ref_pos,
            end: self.position,
        })
    }
}

impl Drop for TokenStream<'_, '_> {
    fn drop(&mut self) {
        if let Some(ref_pos) = &mut self.ref_position {
            **ref_pos = self.position;
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct TokenRange {
    pub start: usize,
    pub end: usize,
}

impl std::fmt::Debug for TokenRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tokens[{} - {}]", self.start, self.end)
    }
}

impl std::ops::Add for TokenRange {
    type Output = TokenRange;

    fn add(self, rhs: Self) -> Self::Output {
        TokenRange {
            start: self.start.min(rhs.start),
            end: self.end.min(rhs.end),
        }
    }
}

impl std::iter::Sum for TokenRange {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        let mut start = iter.next().unwrap_or(Default::default());
        for item in iter {
            start = start + item;
        }
        start
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Expected {} at Ln {}, Col {}, got {:?}", .0, (.2).1, (.2).0, .1)]
    Expected(String, Token, Position),
    #[error("Source file contains no tokens")]
    FileEmpty,
    /// Only use this error in situations where the error never ends up for the end-user!
    #[error("Undefined error, should only be used in situations where the error is not emitted!")]
    Undefined,
    /// Condition failed for the parse-if
    #[error("Condition failed for parse-if. Should never be returned to end-user.")]
    IfFailed,
}
