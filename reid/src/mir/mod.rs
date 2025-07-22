//! In this module are defined structs that are used for performing passes on
//! Reid. It contains a simplified version of Reid which can be e.g.
//! typechecked.

use std::{collections::HashMap, path::PathBuf};

use crate::{
    lexer::{FullToken, Position},
    token_stream::TokenRange,
};

mod fmt;
pub mod implement;
pub mod linker;
pub mod pass;
pub mod typecheck;
pub mod typeinference;
pub mod typerefs;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Hash)]
pub struct SourceModuleId(pub u32);

impl SourceModuleId {
    pub fn increment(&mut self) -> SourceModuleId {
        self.0 += 1;
        SourceModuleId(self.0)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Metadata {
    pub source_module_id: SourceModuleId,
    pub range: TokenRange,
    pub position: Option<Position>,
}

impl Metadata {
    pub fn complete_overlap(&self, other: &Metadata) -> bool {
        (self.range.start >= other.range.start && self.range.end <= other.range.end)
            || (other.range.start >= self.range.start && other.range.end <= self.range.end)
    }

    pub fn into_positions(&self, tokens: &Vec<FullToken>) -> Option<(Position, Position)> {
        let mut iter = tokens
            .iter()
            .skip(self.range.start)
            .take(self.range.end - self.range.start);
        if let Some(first) = iter.next() {
            let last = iter.last().unwrap_or(first);
            Some((first.position, last.position.add(last.token.len() as u32)))
        } else {
            None
        }
    }
}

impl std::ops::Add for Metadata {
    type Output = Metadata;

    fn add(self, rhs: Self) -> Self::Output {
        assert!(self.source_module_id == rhs.source_module_id);
        Metadata {
            range: self.range + rhs.range,
            source_module_id: self.source_module_id,
            position: None,
        }
    }
}

impl TokenRange {
    pub fn as_meta(self, module: SourceModuleId) -> Metadata {
        Metadata {
            range: self,
            source_module_id: module,
            position: None,
        }
    }
}

#[derive(Hash, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CustomTypeKey(pub String, pub SourceModuleId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    Void,
    F16,
    F32B,
    F32,
    F64,
    F128,
    F80,
    F128PPC,
    Char,
    Array(Box<TypeKind>, u64),
    CustomType(CustomTypeKey),
    Borrow(Box<TypeKind>, bool),
    UserPtr(Box<TypeKind>),
    CodegenPtr(Box<TypeKind>),
    Vague(VagueType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VagueType {
    Unknown,
    Integer,
    Decimal,
    TypeRef(usize),
}

#[derive(Clone, Debug)]
pub struct StructType(pub Vec<StructField>);

#[derive(Clone, Debug)]
pub struct StructField(pub String, pub TypeKind, pub Metadata);

impl StructType {
    pub fn find_index(&self, name: &String) -> Option<u32> {
        self.0
            .iter()
            .enumerate()
            .find(|(_, StructField(n, _, _))| n == name)
            .map(|(i, _)| i as u32)
    }
}

impl TypeKind {
    pub fn known(&self) -> Result<TypeKind, VagueType> {
        if let TypeKind::Vague(vague) = self {
            Err(*vague)
        } else {
            Ok(self.clone())
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    F16(f32),
    F32B(f32),
    F32(f32),
    F64(f64),
    F80(f64),
    F128(f64),
    F128PPC(f64),
    Bool(bool),
    String(String),
    Char(char),
    Vague(VagueLiteral),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum VagueLiteral {
    Number(u64),
    Decimal(f64),
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
            Literal::String(_) => TypeKind::UserPtr(Box::new(TypeKind::Char)),
            Literal::Vague(VagueLiteral::Number(_)) => TypeKind::Vague(VagueType::Integer),
            Literal::Vague(VagueLiteral::Decimal(_)) => TypeKind::Vague(VagueType::Decimal),
            Literal::F16(_) => TypeKind::F16,
            Literal::F32B(_) => TypeKind::F32B,
            Literal::F32(_) => TypeKind::F32,
            Literal::F64(_) => TypeKind::F64,
            Literal::F80(_) => TypeKind::F80,
            Literal::F128(_) => TypeKind::F128,
            Literal::F128PPC(_) => TypeKind::F128PPC,
            Literal::Char(_) => TypeKind::Char,
        }
    }
}

impl VagueLiteral {
    pub fn as_type(self: &VagueLiteral) -> VagueType {
        match self {
            VagueLiteral::Number(_) => VagueType::Integer,
            VagueLiteral::Decimal(_) => VagueType::Decimal,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Minus,
    Mult,
    And,
    Cmp(CmpOperator),
}

/// Specifically the operators that LLVM likes to take in as "icmp" parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CmpOperator {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReturnKind {
    Hard,
    Soft,
}

#[derive(Debug)]
pub struct NamedVariableRef(pub TypeKind, pub String, pub Metadata);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Import(pub Vec<String>, pub Metadata);

#[derive(Debug)]
pub enum ExprKind {
    Variable(NamedVariableRef),
    Indexed(Box<Expression>, TypeKind, Box<Expression>),
    Accessed(Box<Expression>, TypeKind, String),
    Array(Vec<Expression>),
    Struct(String, Vec<(String, Expression)>),
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    FunctionCall(FunctionCall),
    If(IfExpression),
    Block(Block),
    Borrow(NamedVariableRef, bool),
    Deref(NamedVariableRef),
    CastTo(Box<Expression>, TypeKind),
}

#[derive(Debug)]
pub struct Expression(pub ExprKind, pub Metadata);

/// Condition, Then, Else
#[derive(Debug)]
pub struct IfExpression(
    pub Box<Expression>,
    pub Box<Expression>,
    pub Box<Option<Expression>>,
);

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub return_type: TypeKind,
    pub parameters: Vec<Expression>,
    pub meta: Metadata,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    /// Whether this function is visible to outside modules
    pub is_pub: bool,
    /// Whether this module is from an external module, and has been imported
    pub is_imported: bool,
    pub return_type: TypeKind,
    pub parameters: Vec<(String, TypeKind)>,
    pub kind: FunctionDefinitionKind,
}

#[derive(Debug)]
pub enum FunctionDefinitionKind {
    /// Actual definition block and surrounding signature range
    Local(Block, Metadata),
    /// True = imported from other module, False = Is user defined extern
    Extern(bool),
}

impl FunctionDefinition {
    pub fn block_meta(&self) -> Metadata {
        match &self.kind {
            FunctionDefinitionKind::Local(block, _) => block.meta.clone(),
            FunctionDefinitionKind::Extern(_) => Metadata::default(),
        }
    }

    pub fn signature(&self) -> Metadata {
        match &self.kind {
            FunctionDefinitionKind::Local(_, metadata) => metadata.clone(),
            FunctionDefinitionKind::Extern(_) => Metadata::default(),
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
    /// Variable name++mutability+type, evaluation
    Let(NamedVariableRef, bool, Expression),
    Set(Expression, Expression),
    Import(Import),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: String,
    pub kind: TypeDefinitionKind,
    pub meta: Metadata,
    pub source_module: SourceModuleId,
}

#[derive(Debug, Clone)]
pub enum TypeDefinitionKind {
    Struct(StructType),
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub module_id: SourceModuleId,
    pub imports: Vec<Import>,
    pub functions: Vec<FunctionDefinition>,
    pub typedefs: Vec<TypeDefinition>,
    pub path: Option<PathBuf>,
    pub tokens: Vec<FullToken>,
    pub is_main: bool,
}

pub type ModuleMap = HashMap<SourceModuleId, Module>;

#[derive(Debug)]
pub struct Context {
    pub modules: ModuleMap,
    pub base: PathBuf,
}
