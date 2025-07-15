//! This is the module that contains relevant code to parsing Reid, that is to
//! say transforming a Vec of FullTokens into a loose parsed AST that can be
//! used for unwrapping syntax sugar, and then be transformed into Reid MIR.
use std::path::PathBuf;

use crate::token_stream::TokenRange;

pub mod parse;
pub mod process;

#[derive(Debug, Clone)]
pub struct Type(pub TypeKind, pub TokenRange);

#[derive(Debug, Clone)]
pub enum TypeKind {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    String,
    Array(Box<TypeKind>, u64),
    Custom(String),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(u64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub struct Expression(pub ExpressionKind, pub TokenRange);

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    VariableName(String),
    Literal(Literal),
    Array(Vec<Expression>),
    Index(Box<Expression>, u64),
    Binop(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(Box<FunctionCallExpression>),
    BlockExpr(Box<Block>),
    IfExpr(Box<IfExpression>),
    StructInit(StructInit),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Minus,
    Mult,

    And,
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

impl BinaryOperator {
    pub fn get_precedence(&self) -> i8 {
        use BinaryOperator::*;
        match &self {
            Add => 10,
            Minus => 10,
            Mult => 20,
            And => 100,
            LT => 100,
            LE => 100,
            GT => 100,
            GE => 100,
            EQ => 100,
            NE => 100,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression(
    pub String,
    pub Vec<Expression>,
    #[allow(dead_code)] pub TokenRange,
);

#[derive(Debug, Clone)]
pub struct IfExpression(
    pub Expression,
    pub Block,
    pub Option<Block>,
    #[allow(dead_code)] pub TokenRange,
);

#[derive(Debug, Clone)]
pub struct LetStatement(
    pub String,
    pub Option<Type>,
    /// Mutability
    pub bool,
    pub Expression,
    pub TokenRange,
);

#[derive(Debug, Clone)]
pub struct ImportStatement(pub Vec<String>, pub TokenRange);

#[derive(Debug)]
pub struct FunctionDefinition(pub FunctionSignature, pub bool, pub Block, pub TokenRange);

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    #[allow(dead_code)]
    pub range: TokenRange,
}

#[derive(Debug, Clone, Copy)]
pub enum ReturnType {
    Soft,
    Hard,
}

#[derive(Debug, Clone)]
pub struct StructInit {
    name: String,
    fields: Vec<(String, Expression)>,
}

#[derive(Debug, Clone)]
pub struct Block(
    pub Vec<BlockLevelStatement>,
    pub Option<(ReturnType, Expression)>,
    pub TokenRange,
);

#[derive(Debug, Clone)]
pub struct VariableReference(pub VariableReferenceKind, pub TokenRange);

#[derive(Debug, Clone)]
pub enum VariableReferenceKind {
    Name(String, TokenRange),
    Index(Box<VariableReference>, u64),
}

#[derive(Debug, Clone)]
pub enum BlockLevelStatement {
    Let(LetStatement),
    /// Try to set a variable to a specified expression value
    Set(VariableReference, Expression, TokenRange),
    Import {
        _i: ImportStatement,
    },
    Expression(Expression),
    Return(ReturnType, Expression),
}

#[derive(Debug)]
pub struct TypeDefinition {
    name: String,
    kind: TypeDefinitionKind,
    range: TokenRange,
}

#[derive(Debug)]
pub enum TypeDefinitionKind {
    Struct(Vec<StructDefinitionField>),
}

#[derive(Debug)]
pub struct StructDefinitionField {
    name: String,
    ty: Type,
    range: TokenRange,
}

#[derive(Debug)]
pub enum TopLevelStatement {
    Import(ImportStatement),
    ExternFunction(FunctionSignature),
    FunctionDefinition(FunctionDefinition),
    TypeDefinition(TypeDefinition),
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub top_level_statements: Vec<TopLevelStatement>,
    pub path: Option<PathBuf>,
    pub is_main: bool,
}
