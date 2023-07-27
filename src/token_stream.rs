use crate::{
    lexer::{FullToken, Token},
    parser::Parse,
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

    pub fn expect(&mut self, token: Token) -> Result<(), ()> {
        if let Some(peeked) = self.peek() {
            if token == peeked {
                self.position += 1;
                Ok(())
            } else {
                Err(())
            }
        } else {
            Err(())
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

    pub fn parse<T: Parse>(&mut self) -> Result<T, ()> {
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
}

impl Drop for TokenStream<'_, '_> {
    fn drop(&mut self) {
        if let Some(ref_pos) = &mut self.ref_position {
            **ref_pos = self.position;
        }
    }
}
