use crate::ast::*;
use crate::lexer::Token;

use super::token_stream::{Error, TokenStream};

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
            TypeKind::Array(
                Box::new(inner.0),
                length.parse().expect("Array length not parseable as u64!"),
            )
        } else if let Some(Token::Et) = stream.peek() {
            stream.expect(Token::Et)?;
            let mutable = if let Some(Token::MutKeyword) = stream.peek() {
                stream.next();
                true
            } else {
                false
            };

            let inner = stream.parse::<Type>()?;
            TypeKind::Borrow(Box::new(inner.0), mutable)
        } else if let Some(Token::Star) = stream.peek() {
            stream.expect(Token::Star)?;
            let inner = stream.parse::<Type>()?;
            TypeKind::Ptr(Box::new(inner.0))
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
                    "f16" => TypeKind::F16,
                    "f32b" => TypeKind::F32B,
                    "f32" => TypeKind::F32,
                    "f64" => TypeKind::F64,
                    "f80" => TypeKind::F80,
                    "f128" => TypeKind::F128,
                    "f128ppc" => TypeKind::F128PPC,
                    "char" => TypeKind::Char,
                    _ => TypeKind::Custom(ident),
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

impl Parse for UnaryOperator {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        if let Some(token) = stream.next() {
            match token {
                Token::Plus => Ok(UnaryOperator::Plus),
                Token::Minus => Ok(UnaryOperator::Minus),
                Token::Exclamation => Ok(UnaryOperator::Not),
                _ => Err(stream.expected_err("unary operator")?),
            }
        } else {
            Err(stream.expected_err("unary operator")?)
        }
    }
}

fn specific_float_lit(value: f64, stream: &mut TokenStream) -> Result<Expression, Error> {
    use ExpressionKind as Kind;

    let lit = match stream
        .parse_if::<Type, _>(|t| match t.0 {
            TypeKind::F16 => true,
            TypeKind::F32B => true,
            TypeKind::F32 => true,
            TypeKind::F64 => true,
            TypeKind::F128 => true,
            TypeKind::F80 => true,
            TypeKind::F128PPC => true,
            _ => false,
        })?
        .0
    {
        TypeKind::F16 => SpecificLiteral::F16(value as f32),
        TypeKind::F32 => SpecificLiteral::F32(value as f32),
        TypeKind::F32B => SpecificLiteral::F32B(value as f32),
        TypeKind::F64 => SpecificLiteral::F64(value as f64),
        TypeKind::F128 => SpecificLiteral::F128(value as f64),
        TypeKind::F80 => SpecificLiteral::F80(value as f64),
        TypeKind::F128PPC => SpecificLiteral::F128PPC(value as f64),
        _ => Err(stream.expected_err("float-compatible type")?)?,
    };
    Ok(Expression(
        Kind::Literal(Literal::Specific(lit)),
        stream.get_range().unwrap(),
    ))
}

fn specific_int_lit(value: u128, stream: &mut TokenStream) -> Result<Expression, Error> {
    use ExpressionKind as Kind;

    let lit = match stream
        .parse_if::<Type, _>(|t| match t.0 {
            TypeKind::Char => false,
            TypeKind::Array(_, _) => false,
            TypeKind::Custom(_) => false,
            TypeKind::Borrow(_, _) => false,
            TypeKind::Ptr(_) => false,
            TypeKind::Bool => false,
            _ => true,
        })?
        .0
    {
        TypeKind::I8 => SpecificLiteral::I8(value as i8),
        TypeKind::I16 => SpecificLiteral::I16(value as i16),
        TypeKind::I32 => SpecificLiteral::I32(value as i32),
        TypeKind::I64 => SpecificLiteral::I64(value as i64),
        TypeKind::I128 => SpecificLiteral::I128(value as i128),
        TypeKind::U8 => SpecificLiteral::U8(value as u8),
        TypeKind::U16 => SpecificLiteral::U16(value as u16),
        TypeKind::U32 => SpecificLiteral::U32(value as u32),
        TypeKind::U64 => SpecificLiteral::U64(value as u64),
        TypeKind::U128 => SpecificLiteral::U128(value as u128),
        TypeKind::F16 => SpecificLiteral::F16(value as f32),
        TypeKind::F32 => SpecificLiteral::F32(value as f32),
        TypeKind::F32B => SpecificLiteral::F32B(value as f32),
        TypeKind::F64 => SpecificLiteral::F64(value as f64),
        TypeKind::F128 => SpecificLiteral::F128(value as f64),
        TypeKind::F80 => SpecificLiteral::F80(value as f64),
        TypeKind::F128PPC => SpecificLiteral::F128PPC(value as f64),
        _ => return Err(stream.expected_err("integer-compatible type")?),
    };
    Ok(Expression(
        Kind::Literal(Literal::Specific(lit)),
        stream.get_range().unwrap(),
    ))
}

#[derive(Debug)]
pub struct AssociatedFunctionCall(Type, FunctionCallExpression);

impl Parse for AssociatedFunctionCall {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let ty = stream.parse()?;
        stream.expect(Token::Colon)?;
        stream.expect(Token::Colon)?;
        Ok(AssociatedFunctionCall(ty, stream.parse()?))
    }
}

fn apply_inner<T>(expr: PrimaryExpression, fun: T) -> Expression
where
    T: FnOnce(PrimaryExpression) -> Expression,
{
    match &expr.0 .0 {
        ExpressionKind::Indexed(value_expr, index_expr) => Expression(
            ExpressionKind::Indexed(
                Box::new(apply_inner(PrimaryExpression(*value_expr.clone()), fun)),
                index_expr.clone(),
            ),
            expr.0 .1,
        ),
        ExpressionKind::Accessed(value_expr, index_name) => Expression(
            ExpressionKind::Accessed(
                Box::new(apply_inner(PrimaryExpression(*value_expr.clone()), fun)),
                index_name.clone(),
            ),
            expr.0 .1,
        ),
        ExpressionKind::AccessCall(value_expr, fn_call) => Expression(
            ExpressionKind::AccessCall(
                Box::new(apply_inner(PrimaryExpression(*value_expr.clone()), fun)),
                fn_call.clone(),
            ),
            expr.0 .1,
        ),
        _ => fun(expr),
    }
}

#[derive(Debug)]
pub struct PrimaryExpression(Expression);

impl Parse for PrimaryExpression {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        use ExpressionKind as Kind;

        let mut expr = if let Ok(exp) = stream.parse() {
            Expression(Kind::FunctionCall(Box::new(exp)), stream.get_range().unwrap())
        } else if let Ok(block) = stream.parse() {
            Expression(Kind::BlockExpr(Box::new(block)), stream.get_range().unwrap())
        } else if let Some(Token::If) = stream.peek() {
            Expression(Kind::IfExpr(Box::new(stream.parse()?)), stream.get_range().unwrap())
        } else if let (Some(Token::Et), Some(Token::MutKeyword)) = (stream.peek(), stream.peek2()) {
            stream.next(); // Consume Et
            stream.next(); // Consume mut
            Expression(
                Kind::Borrow(Box::new(stream.parse()?), true),
                stream.get_range().unwrap(),
            )
        } else if let Some(Token::Et) = stream.peek() {
            stream.next(); // Consume Et
            Expression(
                Kind::Borrow(Box::new(stream.parse()?), false),
                stream.get_range().unwrap(),
            )
        } else if let Some(Token::Star) = stream.peek() {
            stream.next(); // Consume Et
            apply_inner(stream.parse()?, |e| {
                Expression(Kind::Deref(Box::new(e.0)), stream.get_range().unwrap())
            })
        } else if let Ok(unary) = stream.parse() {
            Expression(
                Kind::UnaryOperation(unary, Box::new(stream.parse()?)),
                stream.get_range().unwrap(),
            )
        } else if let Ok(assoc_function) = stream.parse::<AssociatedFunctionCall>() {
            Expression(
                Kind::AssociatedFunctionCall(assoc_function.0, Box::new(assoc_function.1)),
                stream.get_range().unwrap(),
            )
        } else if let Some(token) = stream.peek() {
            match &token {
                Token::Identifier(v) => {
                    if let Ok(struct_expr) = stream.parse::<StructExpression>() {
                        let range = struct_expr.range.clone();
                        Expression(Kind::StructExpression(struct_expr), range)
                    } else {
                        stream.next(); // Consume ident
                        Expression(Kind::VariableName(v.clone()), stream.get_range().unwrap())
                    }
                }
                Token::BinaryValue(v) => {
                    stream.next(); // Consume binary
                    let value = u128::from_str_radix(&v, 2).expect("Value is not parseable as u128!");
                    if let Ok(expr) = specific_int_lit(value, &mut stream) {
                        expr
                    } else {
                        Expression(Kind::Literal(Literal::Integer(value)), stream.get_range().unwrap())
                    }
                }
                Token::OctalValue(v) => {
                    stream.next(); // Consume octal
                    let value = u128::from_str_radix(&v, 8).expect("Value is not parseable as u128!");
                    if let Ok(expr) = specific_int_lit(value, &mut stream) {
                        expr
                    } else {
                        Expression(Kind::Literal(Literal::Integer(value)), stream.get_range().unwrap())
                    }
                }
                Token::HexadecimalValue(v) => {
                    stream.next(); // Consume hexadecimal

                    let value = u128::from_str_radix(&v, 16).expect("Value is not parseable as u128!");
                    if let Ok(expr) = specific_int_lit(value, &mut stream) {
                        expr
                    } else {
                        Expression(Kind::Literal(Literal::Integer(value)), stream.get_range().unwrap())
                    }
                }
                Token::DecimalValue(v) => {
                    stream.next(); // Consume decimal
                    if let (Some(Token::Dot), Some(Token::DecimalValue(fractional))) = (stream.peek(), stream.peek2()) {
                        stream.next(); // Consume dot
                        stream.next(); // Consume fractional

                        let value = format!("{}.{}", v, fractional)
                            .parse()
                            .expect("Decimal is not parseable as f64!");

                        if let Ok(expr) = specific_float_lit(value, &mut stream) {
                            expr
                        } else {
                            Expression(Kind::Literal(Literal::Decimal(value)), stream.get_range().unwrap())
                        }
                    } else {
                        let value = u128::from_str_radix(&v, 10).expect("Value is not parseable as u128!");
                        if let Ok(expr) = specific_int_lit(value, &mut stream) {
                            expr
                        } else {
                            Expression(Kind::Literal(Literal::Integer(value)), stream.get_range().unwrap())
                        }
                    }
                }
                Token::StringLit(v) => {
                    stream.next(); // Consume
                    Expression(Kind::Literal(Literal::String(v.clone())), stream.get_range().unwrap())
                }
                Token::CharLit(v) => {
                    stream.next(); // Consume
                    let chars = v.as_bytes();
                    if chars.len() == 0 {
                        stream.expected_err("char to not be empty")?;
                    } else if chars.len() > 0 {
                        stream.expected_err("char to only have one char inside it")?;
                    }

                    Expression(
                        Kind::Literal(Literal::Char(v.chars().next().unwrap())),
                        stream.get_range().unwrap(),
                    )
                }
                Token::True => {
                    stream.next(); // Consume
                    Expression(Kind::Literal(Literal::Bool(true)), stream.get_range().unwrap())
                }
                Token::False => {
                    stream.next(); // Consume
                    Expression(Kind::Literal(Literal::Bool(false)), stream.get_range().unwrap())
                }
                Token::ParenOpen => {
                    stream.next(); // Consume
                    let exp = stream.parse()?;
                    stream.expect(Token::ParenClose)?;
                    exp
                }
                Token::BracketOpen => {
                    stream.next(); // Consume
                    if let Ok(exp) = stream.parse() {
                        if let Some(Token::Semi) = stream.peek() {
                            stream.next(); // Consume colon
                            let Some(Token::DecimalValue(val)) = stream.next() else {
                                return Err(stream.expecting_err("decimal value describing array length")?);
                            };
                            stream.expect(Token::BracketClose)?;
                            Expression(
                                Kind::ArrayShort(
                                    Box::new(exp),
                                    u64::from_str_radix(&val, 10)
                                        .expect("Unable to parse array length to 64-bit decimal value"),
                                ),
                                stream.get_range().unwrap(),
                            )
                        } else {
                            let mut expressions = Vec::new();
                            expressions.push(exp);
                            while let Some(Token::Comma) = stream.peek() {
                                stream.next(); // Consume comma
                                expressions.push(stream.parse()?);
                            }
                            stream.expect(Token::BracketClose)?;
                            Expression(Kind::Array(expressions), stream.get_range().unwrap())
                        }
                    } else {
                        stream.expect(Token::BraceClose)?;
                        Expression(Kind::Array(Vec::new()), stream.get_range().unwrap())
                    }
                }
                _ => Err(stream.expecting_err("expression")?)?,
            }
        } else {
            Err(stream.expecting_err("expression")?)?
        };

        while let Ok(index) = stream.parse::<ValueIndex>() {
            match index {
                ValueIndex::Array(ArrayValueIndex(idx_expr)) => {
                    expr = Expression(
                        ExpressionKind::Indexed(Box::new(expr), Box::new(idx_expr)),
                        stream.get_range().unwrap(),
                    );
                }
                ValueIndex::Dot(val) => match val {
                    DotIndexKind::StructValueIndex(name) => {
                        expr = Expression(
                            ExpressionKind::Accessed(Box::new(expr), name),
                            stream.get_range().unwrap(),
                        );
                    }
                    DotIndexKind::FunctionCall(function_call_expression) => {
                        expr = Expression(
                            ExpressionKind::AccessCall(Box::new(expr), Box::new(function_call_expression)),
                            stream.get_range().unwrap(),
                        );
                    }
                },
            }
        }

        while let Ok(as_cast) = stream.parse::<AsCast>() {
            expr = Expression(
                ExpressionKind::CastTo(Box::new(expr), as_cast.0),
                stream.get_range().unwrap(),
            );
        }

        Ok(PrimaryExpression(expr))
    }
}

#[derive(Debug)]
pub struct AsCast(Type);

impl Parse for AsCast {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::AsKeyword)?;
        Ok(AsCast(stream.parse()?))
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
            stream.get_range_prev().unwrap(),
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
            (Some(Token::Star), _) => BinaryOperator::Mult,
            (Some(Token::Slash), _) => BinaryOperator::Div,
            (Some(Token::Percent), _) => BinaryOperator::Mod,
            (_, _) => Err(stream.expected_err("expected operator")?)?,
        })
    }
}

impl Parse for FunctionCallExpression {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        if let Some(Token::Identifier(name)) = stream.next() {
            let args = stream.parse::<FunctionArgs>()?;
            Ok(FunctionCallExpression(name, args.0, stream.get_range().unwrap()))
        } else {
            Err(stream.expected_err("identifier")?)
        }
    }
}

#[derive(Debug)]
pub struct FunctionArgs(Vec<Expression>);

impl Parse for FunctionArgs {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::ParenOpen)?;

        let mut params = Vec::new();
        if let Ok(exp) = stream.parse() {
            params.push(exp);

            while stream.expect(Token::Comma).is_ok() {
                params.push(stream.parse()?);
            }
        }
        stream.expect(Token::ParenClose)?;

        Ok(FunctionArgs(params))
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
        Ok(IfExpression(cond, then_b, else_b, stream.get_range().unwrap()))
    }
}

impl Parse for LetStatement {
    fn parse(mut stream: TokenStream) -> Result<LetStatement, Error> {
        stream.expect(Token::LetKeyword)?;
        let mutability = stream.expect(Token::MutKeyword).is_ok();

        if let Some(Token::Identifier(variable)) = stream.next() {
            let ty = if let Some(Token::Colon) = stream.peek() {
                stream.next(); // Consume colon
                Some(stream.parse()?)
            } else {
                None
            };

            let range = stream.get_range_prev().unwrap();
            stream.expect(Token::Equals)?;

            let expression = stream.parse()?;
            stream.expect(Token::Semi)?;
            Ok(LetStatement {
                name: variable,
                ty,
                mutable: mutability,
                value: expression,
                name_range: range,
            })
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

#[derive(Debug)]
struct FunctionParam(String, Type);

impl Parse for FunctionParam {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let Some(Token::Identifier(arg_name)) = stream.next() else {
            return Err(stream.expected_err("parameter name")?);
        };
        stream.expect(Token::Colon)?;
        Ok(FunctionParam(arg_name, stream.parse()?))
    }
}

#[derive(Debug)]
pub struct SelfParam(SelfKind);

pub enum SelfParamKind {
    Owned,
    Borrow,
    BorrowMut,
}

impl Parse for SelfParam {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        let kind = if let Some(Token::Et) = stream.peek() {
            stream.next();
            if let Some(Token::MutKeyword) = stream.peek() {
                stream.next();
                SelfParamKind::BorrowMut
            } else {
                SelfParamKind::Borrow
            }
        } else {
            SelfParamKind::Owned
        };

        let Some(Token::Identifier(name)) = stream.next() else {
            return Err(stream.expected_err("parameter name")?);
        };
        if name == "self" {
            match kind {
                SelfParamKind::BorrowMut => Ok(SelfParam(SelfKind::MutBorrow(TypeKind::Unknown))),
                SelfParamKind::Borrow => Ok(SelfParam(SelfKind::Borrow(TypeKind::Unknown))),
                SelfParamKind::Owned => Ok(SelfParam(SelfKind::Owned(TypeKind::Unknown))),
            }
        } else {
            Err(stream.expected_err("self parameter")?)
        }
    }
}

impl Parse for FunctionSignature {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        if let Some(Token::Identifier(name)) = stream.next() {
            stream.expect(Token::ParenOpen)?;
            let mut params = Vec::new();

            let self_kind = stream.parse::<SelfParam>().map(|s| s.0).unwrap_or(SelfKind::None);

            if let Ok(param) = stream.parse::<FunctionParam>() {
                params.push((param.0, param.1));
                while let Some(Token::Comma) = stream.peek() {
                    stream.next();
                    let param = stream.parse::<FunctionParam>()?;
                    params.push((param.0, param.1));
                }
            }

            stream.expect(Token::ParenClose)?;

            let mut return_type = None;
            if stream.expect(Token::Arrow).is_ok() {
                return_type = Some(stream.parse()?);
            }

            Ok(FunctionSignature {
                name,
                params,
                self_kind,
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
            if let Some((_, Some(e))) = return_stmt.take() {
                // Special list of expressions that are simply not warned about,
                // if semicolon is missing.
                if !matches!(e, Expression(ExpressionKind::IfExpr(_), _)) {
                    // In theory could ignore the missing semicolon..
                    return Err(stream.expected_err("semicolon to complete statement")?);
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
        Ok(Block(statements, return_stmt, stream.get_range_prev().unwrap()))
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

        Ok(StructExpression {
            name,
            fields,
            range: stream.get_range().unwrap(),
        })
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
    Dot(DotIndexKind),
}

impl Parse for ValueIndex {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        match stream.peek() {
            Some(Token::BracketOpen) => Ok(ValueIndex::Array(stream.parse()?)),
            Some(Token::Dot) => Ok(ValueIndex::Dot(stream.parse()?)),
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
pub enum DotIndexKind {
    StructValueIndex(String),
    FunctionCall(FunctionCallExpression),
}

impl Parse for DotIndexKind {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::Dot)?;
        if let Some(Token::Identifier(name)) = stream.next() {
            if let Ok(args) = stream.parse::<FunctionArgs>() {
                Ok(Self::FunctionCall(FunctionCallExpression(
                    name,
                    args.0,
                    stream.get_range_prev().unwrap(),
                )))
            } else {
                Ok(Self::StructValueIndex(name))
            }
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
            Some(Token::ImportKeyword) => Stmt::Import { _i: stream.parse()? },
            Some(Token::ReturnKeyword) => {
                stream.next();
                let exp = stream.parse().ok();
                stream.expect(Token::Semi)?;
                Stmt::Return(ReturnType::Hard, exp)
            }
            Some(Token::For) => {
                let for_stmt = stream.parse::<ForStatement>()?;
                Stmt::ForLoop(for_stmt.0, for_stmt.1, for_stmt.2, for_stmt.3, for_stmt.4)
            }
            Some(Token::While) => {
                let while_stmt = stream.parse::<WhileStatement>()?;
                Stmt::WhileLoop(while_stmt.0, while_stmt.1)
            }
            _ => {
                if let Ok(SetStatement(ident, expr, range)) = stream.parse() {
                    Stmt::Set(ident, expr, range)
                } else {
                    let e = stream.parse()?;
                    if stream.expect(Token::Semi).is_ok() {
                        Stmt::Expression(e)
                    } else {
                        Stmt::Return(ReturnType::Soft, Some(e))
                    }
                }
            }
        })
    }
}

#[derive(Debug)]
pub struct ForStatement(String, TokenRange, Expression, Expression, Block);

#[derive(Debug)]
pub struct WhileStatement(pub Expression, pub Block);

impl Parse for ForStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::For)?;
        let Some(Token::Identifier(idx)) = stream.next() else {
            return Err(stream.expected_err("loop counter")?);
        };
        let start_range = stream.get_range().unwrap();
        stream.expect(Token::In)?;
        let start = stream.parse()?;
        stream.expect(Token::Dot)?;
        stream.expect(Token::Dot)?;
        let end = stream.parse()?;

        Ok(ForStatement(idx, start_range, start, end, stream.parse()?))
    }
}

impl Parse for WhileStatement {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::While)?;

        Ok(WhileStatement(stream.parse()?, stream.parse()?))
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
            Some(Token::FnKeyword) | Some(Token::PubKeyword) => Stmt::FunctionDefinition(stream.parse()?),
            Some(Token::Struct) => {
                let StructDefinition(name, fields, range) = stream.parse::<StructDefinition>()?;
                Stmt::TypeDefinition(TypeDefinition {
                    name,
                    kind: TypeDefinitionKind::Struct(fields),
                    range,
                })
            }
            Some(Token::Impl) => match stream.peek2() {
                Some(Token::Binop) => Stmt::BinopDefinition(stream.parse()?),
                Some(_) => {
                    let AssociatedFunctionBlock(ty, functions) = stream.parse::<AssociatedFunctionBlock>()?;
                    Stmt::AssociatedFunction(ty, functions)
                }
                _ => Err(stream.expecting_err("binop or associated function block")?)?,
            },
            _ => Err(stream.expecting_err("import or fn")?)?,
        })
    }
}

impl Parse for BinopDefinition {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::Impl)?;
        stream.expect(Token::Binop)?;

        stream.expect(Token::ParenOpen)?;
        let Some(Token::Identifier(lhs_name)) = stream.next() else {
            return Err(stream.expected_err("lhs name")?);
        };
        stream.expect(Token::Colon)?;
        let lhs_type = stream.parse()?;
        stream.expect(Token::ParenClose)?;

        let operator = stream.parse()?;

        stream.expect(Token::ParenOpen)?;
        let Some(Token::Identifier(rhs_name)) = stream.next() else {
            return Err(stream.expected_err("rhs name")?);
        };
        stream.expect(Token::Colon)?;
        let rhs_type = stream.parse()?;
        stream.expect(Token::ParenClose)?;

        let signature_range = stream.get_range().unwrap();

        stream.expect(Token::Arrow)?;

        Ok(BinopDefinition {
            lhs: (lhs_name, lhs_type),
            op: operator,
            rhs: (rhs_name, rhs_type),
            return_ty: stream.parse()?,
            block: stream.parse()?,
            signature_range,
        })
    }
}

#[derive(Debug)]
pub struct AssociatedFunctionBlock(Type, Vec<FunctionDefinition>);

impl Parse for AssociatedFunctionBlock {
    fn parse(mut stream: TokenStream) -> Result<Self, Error> {
        stream.expect(Token::Impl)?;
        let ty = stream.parse::<Type>()?;
        stream.expect(Token::BraceOpen)?;
        let mut functions = Vec::new();
        while let Some(Token::FnKeyword) = stream.peek() {
            let mut fun: FunctionDefinition = stream.parse()?;
            fun.0.self_kind = match fun.0.self_kind {
                SelfKind::Owned(_) => SelfKind::Owned(ty.0.clone()),
                SelfKind::Borrow(_) => SelfKind::Borrow(ty.0.clone()),
                SelfKind::MutBorrow(_) => SelfKind::MutBorrow(ty.0.clone()),
                SelfKind::None => SelfKind::None,
            };
            functions.push(fun);
        }

        stream.expect(Token::BraceClose)?;
        Ok(AssociatedFunctionBlock(ty, functions))
    }
}
