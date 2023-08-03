use crate::{
    lexer::Token,
    token_stream::{Error, TokenStream},
};

pub trait Parse
where
    Self: std::marker::Sized,
{
    fn parse(stream: TokenStream) -> Result<Self, Error>;
}

#[derive(Debug, Clone)]
pub enum Literal {
    I32(i32),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Mult,
}

#[derive(Debug, Clone)]
pub enum Expression {
    VariableName(String),
    Literal(Literal),
    Binop(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(Box<FunctionCallExpression>),
    BlockExpr(Box<Block>),
}

impl Parse for Expression {
    fn parse(mut stream: TokenStream) -> Result<Expression, Error> {
        let lhs = parse_primary_expression(&mut stream)?;
        parse_binop_rhs(&mut stream, lhs, 0)
    }
}

fn parse_primary_expression(stream: &mut TokenStream) -> Result<Expression, Error> {
    if let Ok(exp) = stream.parse() {
        Ok(Expression::FunctionCall(Box::new(exp)))
    } else if let Ok(block) = stream.parse() {
        Ok(Expression::BlockExpr(Box::new(block)))
    } else if let Some(token) = stream.next() {
        Ok(match &token {
            Token::Identifier(v) => Expression::VariableName(v.clone()),
            Token::DecimalValue(v) => Expression::Literal(Literal::I32(v.parse().unwrap())),
            Token::ParenOpen => {
                let exp = stream.parse()?;
                stream.expect(Token::ParenClose)?;
                exp
            }
            _ => Err(stream.expected_err("identifier, constant or parentheses")?)?,
        })
    } else {
        Err(stream.expected_err("expression")?)?
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
) -> Result<Expression, Error> {
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

            use BinaryOperator::*;

            lhs = match &token {
                Token::Plus => Expression::Binop(Add, Box::new(lhs), Box::new(rhs)),
                Token::Times => Expression::Binop(Mult, Box::new(lhs), Box::new(rhs)),
                _ => Err(stream.expected_err("+ or *")?)?,
            };
        }
    }

    Ok(lhs)
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression(pub String, pub Vec<Expression>);

impl Parse for FunctionCallExpression {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        if let Some(Token::Identifier(name)) = stream.next() {
            stream.expect(Token::ParenOpen)?;

            let mut args = Vec::new();

            if let Ok(exp) = stream.parse() {
                args.push(exp);

                while stream.expect(Token::Comma).is_ok() {
                    args.push(stream.parse()?);
                }
            }

            stream.expect(Token::ParenClose)?;

            Ok(FunctionCallExpression(name, args))
        } else {
            Err(stream.expected_err("identifier")?)
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement(pub String, pub Expression);

impl Parse for LetStatement {
    fn parse(mut stream: TokenStream) -> Result<LetStatement, Error> {
        stream.expect(Token::LetKeyword)?;

        if let Some(Token::Identifier(variable)) = stream.next() {
            stream.expect(Token::Equals)?;

            let expression = stream.parse()?;
            stream.expect(Token::Semi)?;
            Ok(LetStatement(variable, expression))
        } else {
            Err(stream.expected_err("identifier")?)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportStatement(Vec<String>);

impl Parse for ImportStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::ImportKeyword)?;

        let mut import_list = Vec::new();

        if let Some(Token::Identifier(name)) = stream.next() {
            import_list.push(name);
            while stream.expect(Token::Colon).is_ok() && stream.expect(Token::Colon).is_ok() {
                if let Some(Token::Identifier(name)) = stream.next() {
                    import_list.push(name);
                } else {
                    Err(stream.expected_err("identifier")?)?
                }
            }
        } else {
            Err(stream.expected_err("identifier")?)?
        }

        stream.expect(Token::Semi)?;

        Ok(ImportStatement(import_list))
    }
}

#[derive(Debug)]
pub struct FunctionDefinition(pub FunctionSignature, pub Block);

impl Parse for FunctionDefinition {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::FnKeyword)?;
        Ok(FunctionDefinition(stream.parse()?, stream.parse()?))
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
}

impl Parse for FunctionSignature {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        if let Some(Token::Identifier(name)) = stream.next() {
            stream.expect(Token::ParenOpen)?;
            stream.expect(Token::ParenClose)?;
            Ok(FunctionSignature { name })
        } else {
            Err(stream.expected_err("identifier")?)?
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ReturnType {
    Soft,
    Hard,
}

#[derive(Debug, Clone)]
pub struct Block(
    pub Vec<BlockLevelStatement>,
    pub Option<(ReturnType, Expression)>,
);

impl Parse for Block {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let mut statements = Vec::new();
        let mut return_stmt = None;
        stream.expect(Token::BraceOpen)?;

        while !matches!(stream.peek(), Some(Token::BraceClose)) {
            if let Some((r_type, e)) = return_stmt.take() {
                println!("Oh no, does this statement lack ;");
                dbg!(r_type, e);
            }
            let statement = stream.parse()?;
            if let BlockLevelStatement::Return((r_type, e)) = &statement {
                match r_type {
                    ReturnType::Hard => {
                        return_stmt = Some((*r_type, e.clone()));
                        break; // Return has to be the last statement
                               // TODO: Make a mechanism that "can" parse even after this
                    }
                    ReturnType::Soft => {
                        return_stmt = Some((*r_type, e.clone()));
                        continue; // In theory possible to have lines after a soft return
                    }
                };
            }
            statements.push(statement);
        }
        stream.expect(Token::BraceClose)?;
        Ok(Block(statements, return_stmt))
    }
}

#[derive(Debug, Clone)]
pub enum BlockLevelStatement {
    Let(LetStatement),
    Import(ImportStatement),
    Expression(Expression),
    Return((ReturnType, Expression)),
}

impl Parse for BlockLevelStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        use BlockLevelStatement as Stmt;
        Ok(match stream.peek() {
            Some(Token::LetKeyword) => Stmt::Let(stream.parse()?),
            Some(Token::ImportKeyword) => Stmt::Import(stream.parse()?),
            Some(Token::ReturnKeyword) => {
                stream.next();
                let exp = stream.parse()?;
                stream.expect(Token::Semi)?;
                Stmt::Return((ReturnType::Hard, exp))
            }
            _ => {
                if let Ok(e) = stream.parse() {
                    if stream.expect(Token::Semi).is_ok() {
                        Stmt::Expression(e)
                    } else {
                        Stmt::Return((ReturnType::Soft, e))
                    }
                } else {
                    Err(stream.expected_err("expression")?)?
                }
            }
        })
    }
}

#[derive(Debug)]
pub enum TopLevelStatement {
    Import(ImportStatement),
    FunctionDefinition(FunctionDefinition),
}

impl Parse for TopLevelStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        use TopLevelStatement as Stmt;
        Ok(match stream.peek() {
            Some(Token::ImportKeyword) => Stmt::Import(stream.parse()?),
            Some(Token::FnKeyword) => Stmt::FunctionDefinition(stream.parse()?),
            _ => Err(stream.expected_err("import or fn")?)?,
        })
    }
}
