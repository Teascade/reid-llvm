use crate::{
    ast::Parse,
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

    pub fn parse<T: Parse>(&mut self) -> Result<T, Error> {
        let mut ref_pos = self.position;

        let position = self.position;
        let clone = TokenStream {
            ref_position: Some(&mut ref_pos),
            tokens: self.tokens,
            position,
        };

        match T::parse(clone) {
            Ok(res) => {
                self.position = ref_pos.max(self.position);
                Ok(res)
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
}

impl Drop for TokenStream<'_, '_> {
    fn drop(&mut self) {
        if let Some(ref_pos) = &mut self.ref_position {
            **ref_pos = self.position;
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Expected {} at Ln {}, Col {}, got {:?}", .0, (.2).1, (.2).0, .1)]
    Expected(String, Token, Position),
    #[error("Source file contains no tokens")]
    FileEmpty,
}
