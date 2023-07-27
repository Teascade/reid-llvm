use crate::{lexer::Token, token_stream::TokenStream};

pub trait Parseable
where
    Self: std::marker::Sized,
{
    fn parse(stream: TokenStream) -> Result<Self, ()>;
}

#[derive(Debug, Clone)]
pub enum Expression {
    VariableName(String),
    ContantI32(i32),
    BinopAdd(Box<Expression>, Box<Expression>),
    BinopMult(Box<Expression>, Box<Expression>),
}

impl Parseable for Expression {
    fn parse(mut stream: TokenStream) -> Result<Expression, ()> {
        let lhs = parse_primary_expression(&mut stream)?;
        parse_binop_rhs(&mut stream, lhs, 0)
    }
}

fn parse_primary_expression(stream: &mut TokenStream) -> Result<Expression, ()> {
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

/// This algorithm seems somewhat like magic to me. I understand it if I read
/// carefully, but it is difficult to read every single time.
///
/// Reference for how the algorithm is formed:
/// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl02.html#binary-expression-parsing
fn parse_binop_rhs(
    stream: &mut TokenStream,
    mut lhs: Expression,
    expr_prec: i8,
) -> Result<Expression, ()> {
    while let Some(token) = stream.peek() {
        let curr_token_prec = token.get_token_prec();

        if curr_token_prec < expr_prec {
            break; // Just return lhs
        } else {
            // token has to be an operator
            stream.next(); // Eat token

            let mut rhs = parse_primary_expression(stream)?;
            if let Some(next_op) = stream.peek() {
                let next_prec = next_op.get_token_prec();
                if curr_token_prec < next_prec {
                    // Operator on the right of rhs has more precedence, turn
                    // rhs into lhs for new binop
                    rhs = parse_binop_rhs(stream, rhs, curr_token_prec + 1)?;
                }
            }

            lhs = match &token {
                Token::Plus => Expression::BinopAdd(Box::new(lhs), Box::new(rhs)),
                Token::Times => Expression::BinopMult(Box::new(lhs), Box::new(rhs)),
                _ => Err(())?,
            };
        }
    }

    Ok(lhs)
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
