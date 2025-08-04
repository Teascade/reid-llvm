//! Contains relevant code for parsing tokens received from
//! Lexing/Tokenizing-stage.

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::parse::Parse,
    lexer::{FullToken, Token},
};

/// Utility struct that is able to parse [`FullToken`]s while being
/// failure-resistance in that it can backtrack easily, and is able to keep
/// track of parsed Token-ranges easily.
pub struct TokenStream<'a, 'b> {
    ref_position: Option<&'b mut usize>,
    tokens: &'a [FullToken],
    errors: Rc<RefCell<Vec<Error>>>,
    pub position: usize,
}

impl<'a, 'b> TokenStream<'a, 'b> {
    pub fn from(tokens: &'a [FullToken]) -> Self {
        TokenStream {
            ref_position: None,
            tokens,
            errors: Rc::new(RefCell::new(Vec::new())),
            position: 0,
        }
    }

    /// Returns expected-error for the next token in-line. Useful in conjunction
    /// with [`TokenStream::peek`]
    pub fn expected_err<T: Into<String>>(&mut self, expected: T) -> Result<Error, Error> {
        let next_token = self.previous().unwrap_or(Token::Eof);
        Ok(Error::Expected(
            expected.into(),
            next_token,
            TokenRange {
                start: self.position - 1,
                end: self.position - 1,
            },
        ))
    }

    /// Returns expected-error for the next token in-line. Useful in conjunction
    /// with [`TokenStream::peek`]
    pub fn expected_err_nonfatal<T: Into<String>>(&mut self, expected: T) {
        let err = match self.expected_err(expected) {
            Ok(e) => e,
            Err(e) => e,
        };
        self.errors.borrow_mut().push(err);
    }

    /// Returns expected-error for the previous token that was already consumed.
    /// Useful in conjunction with [`TokenStream::next`]
    pub fn expecting_err<T: Into<String>>(&mut self, expected: T) -> Result<Error, Error> {
        let next_token = self.peek().unwrap_or(Token::Eof);
        let pos = self.next_token(self.position).0;
        Ok(Error::Expected(
            expected.into(),
            next_token,
            TokenRange { start: pos, end: pos },
        ))
    }

    /// Returns expected-error for the previous token that was already consumed.
    /// Useful in conjunction with [`TokenStream::next`]
    pub fn expecting_err_nonfatal<T: Into<String>>(&mut self, expected: T) {
        let err = match self.expecting_err(expected) {
            Ok(e) => e,
            Err(e) => e,
        };
        self.errors.borrow_mut().push(err);
    }

    pub fn expect(&mut self, token: Token) -> Result<(), Error> {
        if let (pos, Some(peeked)) = self.next_token(self.position) {
            if token == peeked.token {
                self.position = pos + 1;
                Ok(())
            } else {
                Err(self.expecting_err(token)?)
            }
        } else {
            Err(self.expecting_err(token)?)
        }
    }

    pub fn expect_nonfatal(&mut self, token: Token) -> Result<(), ()> {
        if let (pos, Some(peeked)) = self.next_token(self.position) {
            if token == peeked.token {
                self.position = pos + 1;
                Ok(())
            } else {
                self.expecting_err_nonfatal(token);
                Err(())
            }
        } else {
            self.expecting_err_nonfatal(token);
            Err(())
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        let (position, token) = self.next_token(self.position);
        self.position = position + 1;
        token.map(|t| t.token.clone())
    }

    pub fn previous(&mut self) -> Option<Token> {
        let (_, token) = self.previous_token(self.position);
        token.map(|t| t.token.clone())
    }

    pub fn peek(&mut self) -> Option<Token> {
        let (_, token) = self.next_token(self.position);
        token.map(|t| t.token.clone())
    }

    pub fn peek2(&mut self) -> Option<Token> {
        let (pos2, _) = self.next_token(self.position);
        let (_, token) = self.next_token(pos2 + 1);
        token.map(|t| t.token.clone())
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
            errors: self.errors.clone(),
            position,
        };

        match T::parse(clone) {
            Ok(res) => {
                let new_pos = ref_pos.max(self.position);
                Ok((res, new_pos))
            }
            Err(e) => Err(e),
        }
    }

    pub fn parse_with<T, U>(&mut self, fun: T) -> U
    where
        T: FnOnce(TokenStream) -> U,
    {
        let mut ref_pos = self.position;

        let position = self.position;
        let clone = TokenStream {
            ref_position: Some(&mut ref_pos),
            tokens: self.tokens,
            errors: self.errors.clone(),
            position,
        };

        fun(clone)
    }

    pub fn get_range(&self) -> Option<TokenRange> {
        self.ref_position.as_ref().map(|ref_pos| TokenRange {
            start: **ref_pos,
            end: self.position,
        })
    }

    /// Gets range from the previous position to the current. Useful when using
    /// with [`TokenStream::next`]
    pub fn get_range_prev(&self) -> Option<TokenRange> {
        self.ref_position.as_ref().map(|ref_pos| TokenRange {
            start: **ref_pos,
            end: self.previous_token(self.position).0,
        })
    }

    /// Gets range of the previous token only.
    pub fn get_range_prev_curr(&self) -> Option<TokenRange> {
        Some(TokenRange {
            start: self.previous_token(self.position).0,
            end: self.previous_token(self.position).0,
        })
    }

    fn previous_token(&self, mut from: usize) -> (usize, Option<&'a FullToken>) {
        from -= 1;
        while let Some(token) = self.tokens.get(from) {
            if matches!(token.token, Token::Whitespace(_) | Token::Comment(_)) {
                from -= 1;
            } else {
                break;
            }
        }
        (from, self.tokens.get(from))
    }

    fn next_token(&self, mut from: usize) -> (usize, Option<&'a FullToken>) {
        while let Some(token) = self.tokens.get(from) {
            if matches!(token.token, Token::Whitespace(_) | Token::Comment(_)) {
                from += 1;
            } else {
                break;
            }
        }
        (from, self.tokens.get(from))
    }

    pub fn errors(&self) -> Vec<Error> {
        self.errors.borrow().clone().clone()
    }

    pub fn next_is_whitespace(&self) -> bool {
        if let Some(token) = self.tokens.get(self.position) {
            if let Token::Whitespace(_) = token.token {
                true
            } else {
                false
            }
        } else {
            true
        }
    }
}

impl Drop for TokenStream<'_, '_> {
    fn drop(&mut self) {
        if let Some(ref_pos) = &mut self.ref_position {
            **ref_pos = self.position;
        }
    }
}

/// Index-range that can be used with the original array of [`FullToken`]s to
/// retrieve the precise location of a failure.
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
            start: self.start.min(rhs.start).min(rhs.end),
            end: self.end.max(rhs.end).max(rhs.start),
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

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    #[error("Expected {} got \"{:?}\"", .0, .1)]
    Expected(String, Token, TokenRange),
    #[error("Source file contains no tokens")]
    FileEmpty,
    /// Only use this error in situations where the error never ends up for the end-user!
    #[error("Undefined error, should only be used in situations where the error is not emitted!")]
    Undefined,
    /// Condition failed for the parse-if
    #[error("Condition failed for parse-if. Should never be returned to end-user.")]
    IfFailed,
}

impl Error {
    pub fn get_range(&self) -> Option<&TokenRange> {
        match self {
            Error::Expected(_, _, pos) => Some(pos),
            Error::FileEmpty => None,
            Error::Undefined => None,
            Error::IfFailed => None,
        }
    }
}
