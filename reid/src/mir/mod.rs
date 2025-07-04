/// In this module are defined structs that are used for performing passes on
/// Reid. It contains a simplified version of Reid which must already be
/// type-checked beforehand.
use crate::token_stream::TokenRange;

pub mod types;

#[derive(Debug, Clone, Copy)]
pub struct Metadata {
    pub range: TokenRange,
}

impl From<TokenRange> for Metadata {
    fn from(value: TokenRange) -> Self {
        Metadata { range: value }
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Metadata {
            range: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    I32,
    I16,
    Void,
}

impl TypeKind {
    pub fn signed(&self) -> bool {
        match self {
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    I32(i32),
    I16(i16),
}

impl Literal {
    pub fn as_type(self: &Literal) -> TypeKind {
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
    Hard,
    Soft,
}

#[derive(Debug)]
pub struct VariableReference(pub TypeKind, pub String, pub Metadata);

#[derive(Debug)]
pub struct Import(pub String, pub Metadata);

#[derive(Debug)]
pub enum ExprKind {
    Variable(VariableReference),
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(FunctionCall),
    If(IfExpression),
    Block(Block),
}

#[derive(Debug)]
pub struct Expression(pub ExprKind, pub Metadata);

/// Condition, Then, Else
#[derive(Debug)]
pub struct IfExpression(pub Box<Expression>, pub Block, pub Option<Block>);

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub return_type: TypeKind,
    pub parameters: Vec<Expression>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<(String, TypeKind)>,
    pub kind: FunctionDefinitionKind,
}

#[derive(Debug)]
pub enum FunctionDefinitionKind {
    /// Actual definition block and surrounding signature range
    Local(Block, Metadata),
    /// Return Type
    Extern(TypeKind),
}

#[derive(Debug)]
pub struct Block {
    /// List of non-returning statements
    pub statements: Vec<Statement>,
    pub return_expression: Option<(ReturnKind, Box<Expression>)>,
    pub meta: Metadata,
}

#[derive(Debug)]
pub struct Statement(pub StmtKind, pub Metadata);

#[derive(Debug)]
pub enum StmtKind {
    /// Variable name+type, evaluation
    Let(VariableReference, Expression),
    Import(Import),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub imports: Vec<Import>,
    pub functions: Vec<FunctionDefinition>,
}
