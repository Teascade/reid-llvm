use crate::ast::*;
use crate::{
    lexer::Token,
    token_stream::{Error, TokenRange, TokenStream},
};

pub trait Parse
where
    Self: std::marker::Sized,
{
    fn parse(stream: TokenStream) -> Result<Self, Error>;
}

impl Parse for Type {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let kind = if let Some(Token::Identifier(ident)) = stream.next() {
            Ok(match &*ident {
                "i32" => TypeKind::I32,
                _ => panic!("asd"),
            })
        } else {
            Err(stream.expected_err("type identifier")?)
        }?;

        Ok(Type(kind, stream.get_range().unwrap()))
    }
}

impl Parse for Expression {
    fn parse(mut stream: TokenStream) -> Result<Expression, Error> {
        let lhs = stream.parse::<PrimaryExpression>()?.0;
        parse_binop_rhs(&mut stream, lhs, None)
    }
}

#[derive(Debug)]
pub struct PrimaryExpression(Expression);

impl Parse for PrimaryExpression {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        use ExpressionKind as Kind;

        let expr = if let Ok(exp) = stream.parse() {
            Expression(
                Kind::FunctionCall(Box::new(exp)),
                stream.get_range().unwrap(),
            )
        } else if let Ok(block) = stream.parse() {
            Expression(
                Kind::BlockExpr(Box::new(block)),
                stream.get_range().unwrap(),
            )
        } else if let Ok(ifexpr) = stream.parse() {
            Expression(Kind::IfExpr(Box::new(ifexpr)), stream.get_range().unwrap())
        } else if let Some(token) = stream.next() {
            match &token {
                Token::Identifier(v) => {
                    Expression(Kind::VariableName(v.clone()), stream.get_range().unwrap())
                }
                Token::DecimalValue(v) => Expression(
                    Kind::Literal(Literal::I32(v.parse().unwrap())),
                    stream.get_range().unwrap(),
                ),
                Token::ParenOpen => {
                    let exp = stream.parse()?;
                    stream.expect(Token::ParenClose)?;
                    exp
                }
                _ => Err(stream.expected_err("identifier, constant or parentheses")?)?,
            }
        } else {
            Err(stream.expected_err("expression")?)?
        };
        Ok(PrimaryExpression(expr))
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
    mut prev_operator: Option<BinaryOperator>,
) -> Result<Expression, Error> {
    // Expression precedence = LHS precedence so far.
    let expr_precedence = if let Some(op) = prev_operator.take() {
        op.get_precedence()
    } else {
        0
    };

    while let Ok(op) =
        // If next operator precedence is lower than expression precedence, we
        // need to climb back up the recursion.
        stream.parse_if::<BinaryOperator, _>(|b| b.get_precedence() >= expr_precedence)
    {
        let curr_token_prec = op.get_precedence();
        let mut rhs = stream.parse::<PrimaryExpression>()?.0;

        if let Ok(next_op) = stream.parse_peek::<BinaryOperator>() {
            let next_prec = next_op.get_precedence();
            if curr_token_prec < next_prec {
                // Operator on the right of rhs has more precedence, turn
                // rhs into lhs for new binop
                rhs = parse_binop_rhs(stream, rhs, Some(op))?;
            } else {
                let _ = prev_operator.insert(next_op);
            }
        }

        lhs = Expression(
            ExpressionKind::Binop(op, Box::new(lhs), Box::new(rhs)),
            stream.get_range().unwrap(),
        );
    }

    Ok(lhs)
}

impl Parse for BinaryOperator {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        Ok(match (stream.next(), stream.peek()) {
            (Some(Token::Et), Some(Token::Et)) => {
                stream.next();
                BinaryOperator::And
            }
            (Some(Token::LessThan), _) => BinaryOperator::LessThan,

            (Some(Token::Plus), _) => BinaryOperator::Add,
            (Some(Token::Minus), _) => BinaryOperator::Minus,
            (Some(Token::Times), _) => BinaryOperator::Mult,
            (_, _) => Err(stream.expected_err("expected operator")?)?,
        })
    }
}

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

            Ok(FunctionCallExpression(
                name,
                args,
                stream.get_range().unwrap(),
            ))
        } else {
            Err(stream.expected_err("identifier")?)
        }
    }
}

impl Parse for IfExpression {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::If)?;
        Ok(IfExpression(
            stream.parse()?,
            stream.parse()?,
            None,
            stream.get_range().unwrap(),
        ))
    }
}

impl Parse for LetStatement {
    fn parse(mut stream: TokenStream) -> Result<LetStatement, Error> {
        stream.expect(Token::LetKeyword)?;

        if let Some(Token::Identifier(variable)) = stream.next() {
            stream.expect(Token::Equals)?;

            let expression = stream.parse()?;
            stream.expect(Token::Semi)?;
            Ok(LetStatement(
                variable,
                expression,
                stream.get_range().unwrap(),
            ))
        } else {
            Err(stream.expected_err("identifier")?)
        }
    }
}

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

        Ok(ImportStatement(import_list, stream.get_range().unwrap()))
    }
}

impl Parse for FunctionDefinition {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::FnKeyword)?;
        Ok(FunctionDefinition(
            stream.parse()?,
            stream.parse()?,
            stream.get_range().unwrap(),
        ))
    }
}

impl Parse for FunctionSignature {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        if let Some(Token::Identifier(name)) = stream.next() {
            stream.expect(Token::ParenOpen)?;
            let mut args = Vec::new();

            while let Some(Token::Identifier(arg_name)) = stream.peek() {
                stream.next();
                stream.expect(Token::Colon)?;
                args.push((arg_name, stream.parse()?));
            }

            stream.expect(Token::ParenClose)?;

            let mut return_type = None;
            if stream.expect(Token::Arrow).is_ok() {
                return_type = Some(stream.parse()?);
            }

            Ok(FunctionSignature {
                name,
                args,
                return_type,
                range: stream.get_range().unwrap(),
            })
        } else {
            Err(stream.expected_err("identifier")?)?
        }
    }
}

impl Parse for Block {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let mut statements = Vec::new();
        let mut return_stmt = None;
        stream.expect(Token::BraceOpen)?;

        while !matches!(stream.peek(), Some(Token::BraceClose)) {
            if let Some((r_type, e)) = return_stmt.take() {
                // Special list of expressions that are simply not warned about,
                // if semicolon is missing.
                if !matches!(e, Expression(ExpressionKind::IfExpr(_), _)) {
                    dbg!(r_type, &e);
                    println!("Oh no, does this statement lack ;");
                }

                statements.push(BlockLevelStatement::Expression(e));
            }
            let statement = stream.parse()?;
            if let BlockLevelStatement::Return(r_type, e) = &statement {
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
        Ok(Block(statements, return_stmt, stream.get_range().unwrap()))
    }
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
                Stmt::Return(ReturnType::Hard, exp)
            }
            _ => {
                if let Ok(e) = stream.parse() {
                    if stream.expect(Token::Semi).is_ok() {
                        Stmt::Expression(e)
                    } else {
                        Stmt::Return(ReturnType::Soft, e)
                    }
                } else {
                    Err(stream.expected_err("expression")?)?
                }
            }
        })
    }
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
