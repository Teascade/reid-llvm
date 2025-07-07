/// In this module are defined structs that are used for performing passes on
/// Reid. It contains a simplified version of Reid which must already be
/// type-checked beforehand.
use crate::token_stream::TokenRange;

pub mod typecheck;
pub mod types;

#[derive(Default, Debug, Clone, Copy)]
pub struct Metadata {
    pub range: TokenRange,
}

impl std::fmt::Display for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.range)
    }
}

impl std::ops::Add for Metadata {
    type Output = Metadata;

    fn add(self, rhs: Self) -> Self::Output {
        Metadata {
            range: self.range + rhs.range,
        }
    }
}

impl From<TokenRange> for Metadata {
    fn from(value: TokenRange) -> Self {
        Metadata { range: value }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum TypeKind {
    #[error("i32")]
    I32,
    #[error("i16")]
    I16,
    #[error("bool")]
    Bool,
    #[error("void")]
    Void,
    #[error(transparent)]
    Vague(#[from] VagueType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum VagueType {
    #[error("Unknown")]
    Unknown,
    #[error("Number")]
    Number,
}

impl TypeKind {
    pub fn is_known(&self) -> Result<TypeKind, VagueType> {
        if let TypeKind::Vague(vague) = self {
            Err(*vague)
        } else {
            Ok(*self)
        }
    }
}

impl TypeKind {
    pub fn signed(&self) -> bool {
        match self {
            TypeKind::Void => false,
            TypeKind::Vague(_) => false,
            _ => true,
        }
    }

    pub fn is_maths(&self) -> bool {
        use TypeKind::*;
        match &self {
            I32 => true,
            I16 => true,
            Bool => true,
            Vague(_) => false,
            Void => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    I32(i32),
    I16(i16),
    Vague(VagueLiteral),
}

#[derive(Debug, Clone, Copy)]
pub enum VagueLiteral {
    Number(u64),
}

impl Literal {
    pub fn as_type(self: &Literal) -> TypeKind {
        match self {
            Literal::I32(_) => TypeKind::I32,
            Literal::I16(_) => TypeKind::I16,
            Literal::Vague(VagueLiteral::Number(_)) => TypeKind::Vague(VagueType::Number),
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
    pub return_type: TypeKind,
    pub parameters: Vec<(String, TypeKind)>,
    pub kind: FunctionDefinitionKind,
}

#[derive(Debug)]
pub enum FunctionDefinitionKind {
    /// Actual definition block and surrounding signature range
    Local(Block, Metadata),
    Extern,
}

impl FunctionDefinition {
    fn block_meta(&self) -> Metadata {
        match &self.kind {
            FunctionDefinitionKind::Local(block, _) => block.meta,
            FunctionDefinitionKind::Extern => Metadata::default(),
        }
    }

    fn signature(&self) -> Metadata {
        match &self.kind {
            FunctionDefinitionKind::Local(_, metadata) => *metadata,
            FunctionDefinitionKind::Extern => Metadata::default(),
        }
    }
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
