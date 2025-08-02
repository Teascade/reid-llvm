//! This is the module that contains relevant code to parsing Reid, that is to
//! say transforming a Vec of FullTokens into a loose parsed AST that can be
//! used for unwrapping syntax sugar, and then be transformed into Reid MIR.
use std::{fs::Metadata, path::PathBuf};

use token_stream::TokenRange;

use crate::lexer::FullToken;

pub mod lexer;
pub mod parse;
pub mod process;
pub mod token_stream;

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
    F16,
    F32B,
    F32,
    F64,
    F128,
    F80,
    F128PPC,
    Char,
    Array(Box<TypeKind>, u64),
    Custom(String),
    Borrow(Box<TypeKind>, bool),
    Ptr(Box<TypeKind>),
    Unknown,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(u128),
    Decimal(f64),
    Bool(bool),
    String(String),
    Char(char),
    Specific(SpecificLiteral),
}

#[derive(Debug, Clone)]
pub enum SpecificLiteral {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F16(f32),
    F32(f32),
    F32B(f32),
    F64(f64),
    F80(f64),
    F128(f64),
    F128PPC(f64),
}

#[derive(Debug, Clone)]
pub struct Expression(pub ExpressionKind, pub TokenRange);

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    VariableName(String),
    Borrow(Box<Expression>, bool),
    Deref(Box<Expression>),
    Literal(Literal),
    Array(Vec<Expression>),
    ArrayShort(Box<Expression>, u64),
    /// Array-indexed, e.g. <expr>[<expr>]
    Indexed(Box<Expression>, Box<Expression>),
    /// Struct-accessed, e.g. <expr>.<expr>
    Accessed(Box<Expression>, String, TokenRange),
    /// Associated function call, but with a shorthand
    AccessCall(Box<Expression>, Box<FunctionCallExpression>),
    Binop(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(Box<FunctionCallExpression>),
    AssociatedFunctionCall(Type, Box<FunctionCallExpression>),
    BlockExpr(Box<Block>),
    IfExpr(Box<IfExpression>),
    StructExpression(StructExpression),
    UnaryOperation(UnaryOperator, Box<Expression>),
    CastTo(Box<Expression>, Type),
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Minus,
    Mult,
    Div,
    Mod,

    BitshiftRight,
    BitshiftLeft,

    And,
    Or,
    Xor,
    BitAnd,
    BitOr,
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

impl BinaryOperator {
    pub fn get_precedence(&self) -> u8 {
        use BinaryOperator::*;
        match &self {
            Minus => 5,
            Add => 10,
            Mult => 15,
            Div => 20,
            Mod => 20,
            BitAnd => 90,
            BitOr => 90,
            BitshiftLeft => 100,
            BitshiftRight => 100,
            And => 150,
            Or => 150,
            Xor => 150,
            LT => 150,
            LE => 150,
            GT => 150,
            GE => 150,
            EQ => 150,
            NE => 150,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression {
    pub name: String,
    pub params: Vec<Expression>,
    pub range: TokenRange,
    pub is_macro: bool,
}

#[derive(Debug, Clone)]
pub struct IfExpression(
    pub Expression,
    pub Expression,
    pub Option<Expression>,
    #[allow(dead_code)] pub TokenRange,
);

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: String,
    pub ty: Option<Type>,
    pub mutable: bool,
    pub value: Expression,
    pub name_range: TokenRange,
}

#[derive(Debug, Clone)]
pub struct ImportStatement(pub Vec<(String, TokenRange)>, pub TokenRange);

#[derive(Debug)]
pub struct FunctionDefinition(pub FunctionSignature, pub bool, pub Block, pub TokenRange);

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub self_kind: SelfKind,
    pub params: Vec<(String, Type, TokenRange)>,
    pub return_type: Option<Type>,
    #[allow(dead_code)]
    pub range: TokenRange,
}

#[derive(Debug, Clone)]
pub enum SelfKind {
    Owned(Type),
    Borrow(Type),
    MutBorrow(Type),
    None,
}

#[derive(Debug, Clone, Copy)]
pub enum ReturnType {
    Soft,
    Hard,
}

#[derive(Debug, Clone)]
pub struct StructExpression {
    name: String,
    fields: Vec<(String, Expression, TokenRange)>,
    range: TokenRange,
}

#[derive(Debug, Clone)]
pub struct Block(
    pub Vec<BlockLevelStatement>,
    pub Option<(ReturnType, Option<Expression>)>,
    pub TokenRange,
);

#[derive(Debug, Clone)]
pub enum BlockLevelStatement {
    Let(LetStatement),
    /// Try to set a variable to a specified expression value
    Set(Expression, Expression, TokenRange),
    Import {
        _i: ImportStatement,
    },
    Expression(Expression),
    Return(ReturnType, Option<Expression>),
    ForLoop(String, TokenRange, Expression, Expression, Block),
    WhileLoop(Expression, Block),
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    name: String,
    kind: TypeDefinitionKind,
    range: TokenRange,
}

#[derive(Debug, Clone)]
pub enum TypeDefinitionKind {
    Struct(Vec<StructDefinitionField>),
}

#[derive(Debug, Clone)]
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
    BinopDefinition(BinopDefinition),
    AssociatedFunction(Type, Vec<FunctionDefinition>),
}

#[derive(Debug)]
pub struct BinopDefinition {
    pub lhs: (String, Type),
    pub op: BinaryOperator,
    pub rhs: (String, Type),
    pub return_ty: Type,
    pub block: Block,
    pub signature_range: TokenRange,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub tokens: Vec<FullToken>,
    pub top_level_statements: Vec<TopLevelStatement>,
    pub path: Option<PathBuf>,
    pub is_main: bool,
}
