use crate::{lexer::Token, token_stream::TokenStream};

pub trait Parseable
where
    Self: std::marker::Sized,
{
    fn parse(stream: TokenStream) -> Result<Self, ()>;
}

#[derive(Debug)]
pub enum Expression {
    VariableName(String),
    ContantI32(i32),
}

impl Parseable for Expression {
    fn parse(mut stream: TokenStream) -> Result<Expression, ()> {
        if let Some(token) = stream.next() {
            Ok(match &token {
                Token::Identifier(v) => Expression::VariableName(v.clone()),
                Token::DecimalValue(v) => Expression::ContantI32(v.parse().unwrap()),
                _ => Err(())?,
            })
        } else {
            Err(())
        }
    }
}

#[derive(Debug)]
pub struct LetStatement(String, Expression);

impl Parseable for LetStatement {
    fn parse(mut stream: TokenStream) -> Result<LetStatement, ()> {
        stream.expect(Token::LetKeyword)?;

        if let Some(Token::Identifier(variable)) = stream.next() {
            stream.expect(Token::Equals)?;

            let expression = stream.parse()?;
            stream.expect(Token::Semicolon)?;
            Ok(LetStatement(variable, expression))
        } else {
            Err(())
        }
    }
}
