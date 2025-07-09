/// In this module are defined structs that are used for performing passes on
/// Reid. It contains a simplified version of Reid which can be e.g. typechecked.
use crate::token_stream::TokenRange;

mod display;
pub mod pass;
pub mod typecheck;
pub mod types;

#[derive(Debug, Default, Clone, Copy)]
pub struct Metadata {
    pub range: TokenRange,
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
    #[error("bool")]
    Bool,
    #[error("i8")]
    I8,
    #[error("i16")]
    I16,
    #[error("i32")]
    I32,
    #[error("i64")]
    I64,
    #[error("i128")]
    I128,
    #[error("u8")]
    U8,
    #[error("u16")]
    U16,
    #[error("u32")]
    U32,
    #[error("u64")]
    U64,
    #[error("u128")]
    U128,
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
            TypeKind::Bool => false,
            TypeKind::I8 => false,
            TypeKind::I16 => false,
            TypeKind::I32 => false,
            TypeKind::I64 => false,
            TypeKind::I128 => false,
            TypeKind::U8 => false,
            TypeKind::U16 => false,
            TypeKind::U32 => false,
            TypeKind::U64 => false,
            TypeKind::U128 => false,
        }
    }

    pub fn is_maths(&self) -> bool {
        use TypeKind::*;
        match &self {
            I8 => true,
            I16 => true,
            I32 => true,
            I64 => true,
            I128 => true,
            U8 => true,
            U16 => true,
            U32 => true,
            U64 => true,
            U128 => true,
            Bool => true,
            Vague(_) => false,
            Void => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
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
    Bool(bool),
    Vague(VagueLiteral),
}

#[derive(Debug, Clone, Copy)]
pub enum VagueLiteral {
    Number(u64),
}

impl Literal {
    pub fn as_type(self: &Literal) -> TypeKind {
        match self {
            Literal::I8(_) => TypeKind::I8,
            Literal::I16(_) => TypeKind::I16,
            Literal::I32(_) => TypeKind::I32,
            Literal::I64(_) => TypeKind::I64,
            Literal::I128(_) => TypeKind::I128,
            Literal::U8(_) => TypeKind::U8,
            Literal::U16(_) => TypeKind::U16,
            Literal::U32(_) => TypeKind::U32,
            Literal::U64(_) => TypeKind::U64,
            Literal::U128(_) => TypeKind::U128,
            Literal::Bool(_) => TypeKind::Bool,
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
    Cmp(CmpOperator),
}

/// Specifically the operators that LLVM likes to take in as "icmp" parameters
#[derive(Debug, Clone, Copy)]
pub enum CmpOperator {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
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

#[derive(Debug)]
pub struct Context {
    pub modules: Vec<Module>,
}
