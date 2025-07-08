use crate::token_stream::TokenRange;

pub mod parse;
pub mod process;

#[derive(Debug, Clone, Copy)]
pub struct Type(pub TypeKind, pub TokenRange);

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
    I32,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(u64),
}

#[derive(Debug, Clone)]
pub struct Expression(pub ExpressionKind, pub TokenRange);

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    VariableName(String),
    Literal(Literal),
    Binop(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(Box<FunctionCallExpression>),
    BlockExpr(Box<Block>),
    IfExpr(Box<IfExpression>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Minus,
    Mult,

    And,
    LessThan,
}

impl BinaryOperator {
    pub fn get_precedence(&self) -> i8 {
        use BinaryOperator::*;
        match &self {
            Add => 10,
            Minus => 10,
            Mult => 20,
            And => 100,
            LessThan => 100,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression(pub String, pub Vec<Expression>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct IfExpression(pub Expression, pub Block, pub Option<Block>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct LetStatement(pub String, pub Option<Type>, pub Expression, pub TokenRange);

#[derive(Debug, Clone)]
pub struct ImportStatement(Vec<String>, pub TokenRange);

#[derive(Debug)]
pub struct FunctionDefinition(pub FunctionSignature, pub Block, pub TokenRange);

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub range: TokenRange,
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
    pub TokenRange,
);

#[derive(Debug, Clone)]
pub enum BlockLevelStatement {
    Let(LetStatement),
    Import(ImportStatement),
    Expression(Expression),
    Return(ReturnType, Expression),
}

#[derive(Debug)]
pub enum TopLevelStatement {
    Import(ImportStatement),
    FunctionDefinition(FunctionDefinition),
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub top_level_statements: Vec<TopLevelStatement>,
}
