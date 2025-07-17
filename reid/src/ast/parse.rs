use crate::ast::*;
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

impl Parse for Type {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let kind = if let Some(Token::BracketOpen) = stream.peek() {
            stream.expect(Token::BracketOpen)?;
            let inner = stream.parse::<Type>()?;
            stream.expect(Token::Semi)?;
            let length = if let Some(Token::DecimalValue(length)) = stream.next() {
                length
            } else {
                return Err(stream.expected_err("array length (number)")?);
            };
            stream.expect(Token::BracketClose)?;
            TypeKind::Array(Box::new(inner.0), length)
        } else {
            if let Some(Token::Identifier(ident)) = stream.next() {
                match &*ident {
                    "bool" => TypeKind::Bool,
                    "i8" => TypeKind::I8,
                    "i16" => TypeKind::I16,
                    "i32" => TypeKind::I32,
                    "i64" => TypeKind::I64,
                    "i128" => TypeKind::I128,
                    "u8" => TypeKind::U8,
                    "u16" => TypeKind::U16,
                    "u32" => TypeKind::U32,
                    "u64" => TypeKind::U64,
                    "u128" => TypeKind::U128,
                    "string" => TypeKind::String,
                    _ => Err(stream.expected_err("known type identifier")?)?,
                }
            } else {
                return Err(stream.expected_err("type identifier")?)?;
            }
        };

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

        let mut expr = if let Ok(exp) = stream.parse() {
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
        } else if let (Some(Token::Identifier(_)), Some(Token::BraceOpen)) =
            (stream.peek(), stream.peek2())
        {
            Expression(
                Kind::StructExpression(stream.parse()?),
                stream.get_range().unwrap(),
            )
        } else if let Some(token) = stream.next() {
            match &token {
                Token::Identifier(v) => {
                    if let Some(Token::BraceOpen) = stream.peek() {
                        Expression(
                            Kind::StructExpression(stream.parse()?),
                            stream.get_range().unwrap(),
                        )
                    } else {
                        Expression(Kind::VariableName(v.clone()), stream.get_range().unwrap())
                    }
                }
                Token::DecimalValue(v) => Expression(
                    Kind::Literal(Literal::Number(*v)),
                    stream.get_range().unwrap(),
                ),
                Token::StringLit(v) => Expression(
                    Kind::Literal(Literal::String(v.clone())),
                    stream.get_range().unwrap(),
                ),
                Token::True => Expression(
                    Kind::Literal(Literal::Bool(true)),
                    stream.get_range().unwrap(),
                ),
                Token::False => Expression(
                    Kind::Literal(Literal::Bool(false)),
                    stream.get_range().unwrap(),
                ),
                Token::ParenOpen => {
                    let exp = stream.parse()?;
                    stream.expect(Token::ParenClose)?;
                    exp
                }
                Token::BracketOpen => {
                    let mut expressions = Vec::new();
                    if let Ok(exp) = stream.parse() {
                        expressions.push(exp);
                        while let Some(Token::Comma) = stream.peek() {
                            stream.next(); // Consume comma
                            expressions.push(stream.parse()?);
                        }
                    }
                    stream.expect(Token::BracketClose)?;
                    Expression(Kind::Array(expressions), stream.get_range().unwrap())
                }
                _ => Err(stream.expected_err("expression")?)?,
            }
        } else {
            Err(stream.expected_err("expression")?)?
        };

        while let Ok(index) = stream.parse::<ValueIndex>() {
            match index {
                ValueIndex::Array(ArrayValueIndex(idx_expr)) => {
                    expr = Expression(
                        ExpressionKind::Indexed(Box::new(expr), Box::new(idx_expr)),
                        stream.get_range().unwrap(),
                    );
                }
                ValueIndex::Struct(StructValueIndex(name)) => {
                    expr = Expression(
                        ExpressionKind::Accessed(Box::new(expr), name),
                        stream.get_range().unwrap(),
                    );
                }
            }
        }

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
            (Some(Token::LessThan), Some(Token::Equals)) => {
                stream.next();
                BinaryOperator::LE
            }
            (Some(Token::GreaterThan), Some(Token::Equals)) => {
                stream.next();
                BinaryOperator::GE
            }
            (Some(Token::Equals), Some(Token::Equals)) => {
                stream.next();
                BinaryOperator::EQ
            }
            (Some(Token::Exclamation), Some(Token::Equals)) => {
                stream.next();
                BinaryOperator::NE
            }

            (Some(Token::LessThan), _) => BinaryOperator::LT,
            (Some(Token::GreaterThan), _) => BinaryOperator::GT,

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
        let cond = stream.parse()?;
        let then_b = stream.parse()?;
        let else_b = if let Ok(_) = stream.expect(Token::Else) {
            Some(stream.parse()?)
        } else {
            None
        };
        Ok(IfExpression(
            cond,
            then_b,
            else_b,
            stream.get_range().unwrap(),
        ))
    }
}

impl Parse for LetStatement {
    fn parse(mut stream: TokenStream) -> Result<LetStatement, Error> {
        stream.expect(Token::LetKeyword)?;
        let mutability = stream.expect(Token::MutKeyword).is_ok();

        if let Some(Token::Identifier(variable)) = stream.next() {
            stream.expect(Token::Equals)?;

            let expression = stream.parse()?;
            stream.expect(Token::Semi)?;
            Ok(LetStatement(
                variable,
                None, // TODO add possibility to name type
                mutability,
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
        let is_pub = if let Some(Token::PubKeyword) = stream.peek() {
            stream.next(); // Consume pub
            true
        } else {
            false
        };

        stream.expect(Token::FnKeyword)?;
        Ok(FunctionDefinition(
            stream.parse()?,
            is_pub,
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
            if let Some((_, e)) = return_stmt.take() {
                // Special list of expressions that are simply not warned about,
                // if semicolon is missing.
                if !matches!(e, Expression(ExpressionKind::IfExpr(_), _)) {
                    // In theory could ignore the missing semicolon..
                    return Err(stream.expected_err("expected semicolon to complete statement")?);
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

impl Parse for StructExpression {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let Some(Token::Identifier(name)) = stream.next() else {
            return Err(stream.expected_err("struct identifier")?);
        };
        stream.expect(Token::BraceOpen)?;
        let named_list = stream.parse::<NamedFieldList<Expression>>()?;
        let fields = named_list.0.into_iter().map(|f| (f.0, f.1)).collect();

        stream.expect(Token::BraceClose)?;

        Ok(StructExpression { name, fields })
    }
}

#[derive(Debug)]
pub struct NamedFieldList<T: Parse + std::fmt::Debug>(Vec<NamedField<T>>);

impl<T: Parse + std::fmt::Debug> Parse for NamedFieldList<T> {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let mut fields = Vec::new();
        while let Ok(field) = stream.parse() {
            fields.push(field);
            match stream.peek() {
                Some(Token::Comma) => {
                    stream.next();
                } // Consume comma
                Some(Token::BraceClose) => break,
                Some(_) | None => Err(stream.expecting_err("another field or closing brace")?)?,
            }
        }
        Ok(NamedFieldList(fields))
    }
}

#[derive(Debug)]
pub struct NamedField<T: Parse + std::fmt::Debug>(String, T, TokenRange);

impl<T: Parse + std::fmt::Debug> Parse for NamedField<T> {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let Some(Token::Identifier(field_name)) = stream.next() else {
            return Err(stream.expected_err("type name")?);
        };
        stream.expect(Token::Colon)?;
        let value = stream.parse()?;

        Ok(NamedField(field_name, value, stream.get_range().unwrap()))
    }
}

#[derive(Debug, Clone)]
pub enum ValueIndex {
    Array(ArrayValueIndex),
    Struct(StructValueIndex),
}

impl Parse for ValueIndex {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        match stream.peek() {
            Some(Token::BracketOpen) => Ok(ValueIndex::Array(stream.parse()?)),
            Some(Token::Dot) => Ok(ValueIndex::Struct(stream.parse()?)),
            _ => Err(stream.expecting_err("value or struct index")?),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayValueIndex(Expression);

impl Parse for ArrayValueIndex {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::BracketOpen)?;
        let expr = stream.parse()?;
        stream.expect(Token::BracketClose)?;
        Ok(ArrayValueIndex(expr))
    }
}

#[derive(Debug, Clone)]
pub struct StructValueIndex(String);

impl Parse for StructValueIndex {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::Dot)?;
        if let Some(Token::Identifier(name)) = stream.next() {
            Ok(StructValueIndex(name))
        } else {
            return Err(stream.expected_err("struct index (number)")?);
        }
    }
}

impl Parse for BlockLevelStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        use BlockLevelStatement as Stmt;
        Ok(match stream.peek() {
            Some(Token::LetKeyword) => Stmt::Let(stream.parse()?),
            Some(Token::ImportKeyword) => Stmt::Import {
                _i: stream.parse()?,
            },
            Some(Token::ReturnKeyword) => {
                stream.next();
                let exp = stream.parse()?;
                stream.expect(Token::Semi)?;
                Stmt::Return(ReturnType::Hard, exp)
            }
            _ => {
                if let Ok(SetStatement(ident, expr, range)) = stream.parse() {
                    Stmt::Set(ident, expr, range)
                } else {
                    if let Ok(e) = stream.parse() {
                        if stream.expect(Token::Semi).is_ok() {
                            Stmt::Expression(e)
                        } else {
                            Stmt::Return(ReturnType::Soft, e)
                        }
                    } else {
                        Err(stream.expecting_err("expression")?)?
                    }
                }
            }
        })
    }
}

#[derive(Debug)]
pub struct SetStatement(Expression, Expression, TokenRange);

impl Parse for SetStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let var_ref = stream.parse()?;
        stream.expect(Token::Equals)?;
        let expr = stream.parse()?;
        stream.expect(Token::Semi)?;
        Ok(SetStatement(var_ref, expr, stream.get_range().unwrap()))
    }
}

#[derive(Debug)]
pub struct StructDefinition(String, Vec<StructDefinitionField>, TokenRange);

impl Parse for StructDefinition {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::Struct)?;

        let Some(Token::Identifier(name)) = stream.next() else {
            return Err(stream.expected_err("identifier")?);
        };

        stream.expect(Token::BraceOpen)?;
        let named_fields = stream.parse::<NamedFieldList<Type>>()?;
        let fields = named_fields
            .0
            .into_iter()
            .map(|f| StructDefinitionField {
                name: f.0,
                ty: f.1,
                range: f.2,
            })
            .collect();
        stream.expect(Token::BraceClose)?;
        Ok(StructDefinition(name, fields, stream.get_range().unwrap()))
    }
}

impl Parse for TopLevelStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        use TopLevelStatement as Stmt;
        Ok(match stream.peek() {
            Some(Token::ImportKeyword) => Stmt::Import(stream.parse()?),
            Some(Token::Extern) => {
                stream.next(); // Consume Extern
                stream.expect(Token::FnKeyword)?;
                let extern_fn = Stmt::ExternFunction(stream.parse()?);
                stream.expect(Token::Semi)?;
                extern_fn
            }
            Some(Token::FnKeyword) | Some(Token::PubKeyword) => {
                Stmt::FunctionDefinition(stream.parse()?)
            }
            Some(Token::Struct) => {
                let StructDefinition(name, fields, range) = stream.parse::<StructDefinition>()?;
                Stmt::TypeDefinition(TypeDefinition {
                    name,
                    kind: TypeDefinitionKind::Struct(fields),
                    range,
                })
            }
            _ => Err(stream.expecting_err("import or fn")?)?,
        })
    }
}
