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
pub enum TopLevelStatement {
    Let(LetStatement),
    Import(ImportStatement),
}

impl Parseable for TopLevelStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, ()> {
        Ok(match stream.peek() {
            Some(Token::LetKeyword) => TopLevelStatement::Let(stream.parse()?),
            Some(Token::ImportKeyword) => TopLevelStatement::Import(stream.parse()?),
            _ => Err(())?,
        })
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

#[derive(Debug)]
pub struct ImportStatement(Vec<String>);

impl Parseable for ImportStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, ()> {
        stream.expect(Token::ImportKeyword)?;

        let mut import_list = Vec::new();

        if let Some(Token::Identifier(name)) = stream.next() {
            import_list.push(name);
            while stream.expect(Token::Colon).is_ok() && stream.expect(Token::Colon).is_ok() {
                if let Some(Token::Identifier(name)) = stream.next() {
                    import_list.push(name);
                } else {
                    Err(())?
                }
            }
        } else {
            Err(())?
        }

        stream.expect(Token::Semicolon)?;

        Ok(ImportStatement(import_list))
    }
}
