/// In this module are defined structs that are used for performing passes on
/// Reid. It contains a simplified version of Reid which must already be
/// type-checked beforehand.
use std::collections::HashMap;

use types::*;

use crate::token_stream::TokenRange;

pub mod types;

#[derive(Clone, Copy)]
pub enum TypeKind {
    I32,
    I16,
}

impl TypeKind {
    pub fn signed(&self) -> bool {
        match self {
            _ => true,
        }
    }
}

#[derive(Clone, Copy)]
pub enum Literal {
    I32(i32),
    I16(i16),
}

impl Literal {
    fn as_type(self: &Literal) -> TypeKind {
        match self {
            Literal::I32(_) => TypeKind::I32,
            Literal::I16(_) => TypeKind::I16,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Minus,
    Mult,
    And,
    Logic(LogicOperator),
}

#[derive(Debug, Clone, Copy)]
pub enum LogicOperator {
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone, Copy)]
pub enum ReturnKind {
    HardReturn,
    SoftReturn,
}

pub struct VariableReference(pub TypeKind, pub String, pub TokenRange);

pub struct Import(pub String, pub TokenRange);

pub enum ExpressionKind {
    Variable(VariableReference),
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(FunctionCall),
    If(IfExpression),
    Block(Block),
}

pub struct Expression(pub ExpressionKind, pub TokenRange);

/// Condition, Then, Else
pub struct IfExpression(pub Box<Expression>, pub Block, pub Option<Block>);

pub struct FunctionCall {
    pub name: String,
    pub return_type: TypeKind,
    pub parameters: Vec<Expression>,
}

pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<(String, TypeKind)>,
    pub kind: FunctionDefinitionKind,
}

pub enum FunctionDefinitionKind {
    /// Actual definition block and surrounding signature range
    Local(Block, TokenRange),
    /// Return Type
    Extern(TypeKind),
}

pub struct Block {
    /// List of non-returning statements
    pub statements: Vec<Statement>,
    pub return_expression: Option<(ReturnKind, Box<Expression>)>,
    pub range: TokenRange,
}

pub struct Statement(pub StatementKind, pub TokenRange);

pub enum StatementKind {
    /// Variable name+type, evaluation
    Let(VariableReference, Expression),
    If(IfExpression),
    Import(Import),
    Expression(Expression),
}

pub struct Module {
    pub name: String,
    pub imports: Vec<Import>,
    pub functions: Vec<FunctionDefinition>,
}
