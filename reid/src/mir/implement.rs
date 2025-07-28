use crate::util::maybe;

use super::{typecheck::typerefs::TypeRefs, *};

#[derive(Debug, Clone)]
pub enum ReturnTypeOther {
    Import(Metadata),
    Let(Metadata),
    Set(Metadata),
    EmptyBlock(Metadata),
    NoBlockReturn(Metadata),
    IndexingNonArray(Metadata),
    DerefNonBorrow(Metadata),
    Loop,
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, PartialOrd)]
pub enum NumValueError {
    #[error("Cannot divide by zero")]
    DivideZero,
}

enum BlockReturn<'b> {
    Early(&'b Statement),
    Normal(ReturnKind, &'b Option<Box<Expression>>),
}

impl TypeKind {
    pub fn signed(&self) -> bool {
        match self {
            TypeKind::Bool => false,
            TypeKind::I8 => true,
            TypeKind::I16 => true,
            TypeKind::I32 => true,
            TypeKind::I64 => true,
            TypeKind::I128 => true,
            TypeKind::U8 => false,
            TypeKind::U16 => false,
            TypeKind::U32 => false,
            TypeKind::U64 => false,
            TypeKind::U128 => false,
            TypeKind::Void => false,
            TypeKind::Char => false,
            TypeKind::Array(..) => false,
            TypeKind::CustomType(..) => false,
            TypeKind::CodegenPtr(..) => false,
            TypeKind::Vague(..) => false,
            TypeKind::Borrow(..) => false,
            TypeKind::UserPtr(..) => false,
            TypeKind::F16 => true,
            TypeKind::F32B => true,
            TypeKind::F32 => true,
            TypeKind::F64 => true,
            TypeKind::F128 => true,
            TypeKind::F80 => true,
            TypeKind::F128PPC => true,
        }
    }

    pub fn size_of(&self) -> u64 {
        match self {
            TypeKind::Bool => 1,
            TypeKind::I8 => 8,
            TypeKind::U8 => 8,
            TypeKind::I16 => 16,
            TypeKind::U16 => 16,
            TypeKind::I32 => 32,
            TypeKind::U32 => 32,
            TypeKind::I64 => 64,
            TypeKind::U64 => 64,
            TypeKind::I128 => 128,
            TypeKind::U128 => 128,
            TypeKind::Void => 0,
            TypeKind::Char => 8,
            TypeKind::Array(type_kind, len) => type_kind.size_of() * (*len as u64),
            TypeKind::CustomType(..) => 32,
            TypeKind::CodegenPtr(_) => 64,
            TypeKind::Vague(_) => panic!("Tried to sizeof a vague type!"),
            TypeKind::Borrow(..) => 64,
            TypeKind::UserPtr(_) => 64,
            TypeKind::F16 => 16,
            TypeKind::F32B => 16,
            TypeKind::F32 => 32,
            TypeKind::F64 => 64,
            TypeKind::F128 => 128,
            TypeKind::F80 => 80,
            TypeKind::F128PPC => 128,
        }
    }

    pub fn alignment(&self) -> u32 {
        match self {
            TypeKind::Bool => 1,
            TypeKind::I8 => 8,
            TypeKind::U8 => 8,
            TypeKind::I16 => 16,
            TypeKind::U16 => 16,
            TypeKind::I32 => 32,
            TypeKind::U32 => 32,
            TypeKind::I64 => 64,
            TypeKind::U64 => 64,
            TypeKind::I128 => 128,
            TypeKind::U128 => 128,
            TypeKind::Void => 0,
            TypeKind::Char => 8,
            TypeKind::Array(type_kind, _) => type_kind.alignment(),
            TypeKind::CustomType(..) => 32,
            TypeKind::CodegenPtr(_) => 64,
            TypeKind::Vague(_) => panic!("Tried to sizeof a vague type!"),
            TypeKind::Borrow(_, _) => 64,
            TypeKind::UserPtr(_) => 64,
            TypeKind::F16 => 16,
            TypeKind::F32B => 16,
            TypeKind::F32 => 32,
            TypeKind::F64 => 64,
            TypeKind::F128 => 128,
            TypeKind::F80 => 80,
            TypeKind::F128PPC => 128,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            TypeKind::Borrow(_, false) => false,
            _ => true,
        }
    }

    pub fn category(&self) -> TypeCategory {
        match self {
            TypeKind::I8
            | TypeKind::I16
            | TypeKind::I32
            | TypeKind::I64
            | TypeKind::I128
            | TypeKind::U8
            | TypeKind::U16
            | TypeKind::U32
            | TypeKind::U64
            | TypeKind::U128
            | TypeKind::Char => TypeCategory::Integer,
            TypeKind::F16
            | TypeKind::F32B
            | TypeKind::F32
            | TypeKind::F64
            | TypeKind::F128
            | TypeKind::F80
            | TypeKind::F128PPC => TypeCategory::Real,
            TypeKind::Void => TypeCategory::Other,
            TypeKind::Bool => TypeCategory::Bool,
            TypeKind::Array(_, _) => TypeCategory::Other,
            TypeKind::CustomType(..) => TypeCategory::Other,
            TypeKind::Borrow(_, _) => TypeCategory::Other,
            TypeKind::UserPtr(_) => TypeCategory::Other,
            TypeKind::CodegenPtr(_) => TypeCategory::Other,
            TypeKind::Vague(vague_type) => match vague_type {
                VagueType::Unknown => TypeCategory::Other,
                VagueType::Integer => TypeCategory::Integer,
                VagueType::Decimal => TypeCategory::Real,
                VagueType::TypeRef(_) => TypeCategory::TypeRef,
            },
        }
    }

    pub fn try_collapse_two(
        (lhs1, rhs1): (&TypeKind, &TypeKind),
        (lhs2, rhs2): (&TypeKind, &TypeKind),
    ) -> Option<(TypeKind, TypeKind)> {
        if let (Ok(lhs), Ok(rhs)) = (lhs1.narrow_into(&lhs2), rhs1.narrow_into(&rhs2)) {
            Some((lhs, rhs))
        } else if let (Ok(lhs), Ok(rhs)) = (lhs1.narrow_into(&rhs2), rhs1.narrow_into(&lhs2)) {
            Some((rhs, lhs))
        } else {
            None
        }
    }

    pub fn unroll_borrow(&self) -> TypeKind {
        match self {
            TypeKind::Borrow(type_kind, mut1) => match *type_kind.clone() {
                TypeKind::Borrow(type_kind, mut2) => match (mut1, mut2) {
                    (false, false) => TypeKind::Borrow(Box::new(*type_kind.clone()), false),
                    _ => TypeKind::Borrow(Box::new(*type_kind.clone()), true),
                },
                _ => self.clone(),
            },
            _ => self.clone(),
        }
    }
}

impl BinaryOperator {
    pub fn is_commutative(&self) -> bool {
        match self {
            BinaryOperator::Add => true,
            BinaryOperator::Minus => false,
            BinaryOperator::Mult => true,
            BinaryOperator::Div => false,
            BinaryOperator::Mod => false,
            BinaryOperator::And => true,
            BinaryOperator::Cmp(cmp_operator) => match cmp_operator {
                CmpOperator::LT => false,
                CmpOperator::LE => false,
                CmpOperator::GT => false,
                CmpOperator::GE => false,
                CmpOperator::EQ => true,
                CmpOperator::NE => true,
            },
            BinaryOperator::Or => true,
            BinaryOperator::Xor => true,
            BinaryOperator::BitOr => true,
            BinaryOperator::BitAnd => true,
            BinaryOperator::BitshiftRight => false,
            BinaryOperator::BitshiftLeft => false,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeCategory {
    Integer,
    Real,
    Bool,
    Other,
    TypeRef,
}

impl TypeCategory {
    pub fn is_simple_maths(&self) -> bool {
        match self {
            TypeCategory::Integer => true,
            TypeCategory::Real => true,
            TypeCategory::Other => false,
            TypeCategory::TypeRef => false,
            TypeCategory::Bool => true,
        }
    }
}

impl StructType {
    pub fn get_field_ty(&self, name: &String) -> Option<&TypeKind> {
        self.0
            .iter()
            .find(|StructField(n, _, _)| n == name)
            .map(|StructField(_, ty, _)| ty)
    }

    pub fn get_field_ty_mut(&mut self, name: &String) -> Option<&mut TypeKind> {
        self.0
            .iter_mut()
            .find(|StructField(n, _, _)| n == name)
            .map(|StructField(_, ty, _)| ty)
    }
}

impl Block {
    fn return_expr(&self) -> Result<BlockReturn, ReturnTypeOther> {
        let mut early_return = None;

        for statement in &self.statements {
            let ret = statement.return_type(&Default::default(), SourceModuleId(0));
            if let Ok((ReturnKind::Hard, _)) = ret {
                early_return = Some(statement);
            }
        }

        if let Some(s) = early_return {
            return Ok(BlockReturn::Early(s));
        }

        self.return_expression
            .as_ref()
            .map(|(r, e)| BlockReturn::Normal(*r, e))
            .ok_or(ReturnTypeOther::NoBlockReturn(self.meta))
    }

    pub fn return_meta(&self) -> Metadata {
        self.return_expression
            .as_ref()
            .map(|e| e.1.as_ref().map(|e| e.1).unwrap_or(Metadata::default()))
            .or(self.statements.last().map(|s| s.1))
            .unwrap_or(self.meta)
    }

    pub fn return_type(
        &self,
        refs: &TypeRefs,
        mod_id: SourceModuleId,
    ) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        let mut early_return = None;

        for statement in &self.statements {
            let ret = statement.return_type(refs, mod_id);
            if let Ok((ReturnKind::Hard, _)) = ret {
                early_return = early_return.or(ret.ok());
            }
        }

        if let Some((ReturnKind::Hard, ret_ty)) = early_return {
            return Ok((ReturnKind::Hard, ret_ty));
        }

        self.return_expression
            .as_ref()
            .ok_or(ReturnTypeOther::NoBlockReturn(self.meta))
            .and_then(|(kind, stmt)| {
                Ok((
                    *kind,
                    stmt.as_ref()
                        .and_then(|s| s.return_type(refs, mod_id).ok())
                        .map(|s| s.1)
                        .unwrap_or(TypeKind::Void),
                ))
            })
    }

    pub fn backing_var(&self) -> Option<&NamedVariableRef> {
        match self.return_expr().ok()? {
            BlockReturn::Early(statement) => statement.backing_var(),
            BlockReturn::Normal(kind, expr) => {
                if let Some(expr) = expr {
                    if kind == ReturnKind::Soft {
                        expr.backing_var()
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
}

impl Statement {
    pub fn return_type(
        &self,
        refs: &TypeRefs,
        mod_id: SourceModuleId,
    ) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use StmtKind::*;
        match &self.0 {
            Let(var, _, expr) => if_hard(
                expr.return_type(refs, mod_id)?,
                Err(ReturnTypeOther::Let(var.2 + expr.1)),
            ),
            Set(lhs, rhs) => if_hard(rhs.return_type(refs, mod_id)?, Err(ReturnTypeOther::Set(lhs.1 + rhs.1))),
            Import(_) => todo!(),
            Expression(expression) => expression.return_type(refs, mod_id),
            While(_) => Err(ReturnTypeOther::Loop),
        }
    }

    pub fn backing_var(&self) -> Option<&NamedVariableRef> {
        match &self.0 {
            StmtKind::Let(_, _, _) => None,
            StmtKind::Set(_, _) => None,
            StmtKind::Import(_) => None,
            StmtKind::Expression(expr) => expr.backing_var(),
            StmtKind::While(_) => None,
        }
    }
}

impl Expression {
    pub fn return_type(
        &self,
        refs: &TypeRefs,
        mod_id: SourceModuleId,
    ) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use ExprKind::*;
        match &self.0 {
            Literal(lit) => Ok((ReturnKind::Soft, lit.as_type())),
            Variable(var) => var.return_type(),
            BinOp(_, then_e, else_e, return_ty) => {
                let then_r = then_e.return_type(refs, mod_id)?;
                let else_r = else_e.return_type(refs, mod_id)?;

                Ok(match (then_r.0, else_r.0) {
                    (ReturnKind::Hard, ReturnKind::Hard) => (ReturnKind::Hard, return_ty.clone()),
                    _ => (ReturnKind::Soft, return_ty.clone()),
                })
            }
            Block(block) => block.return_type(refs, mod_id),
            FunctionCall(fcall) => fcall.return_type(),
            If(expr) => expr.return_type(refs, mod_id),
            Indexed(expression, _, _) => {
                let expr_type = expression.return_type(refs, mod_id)?;
                if let TypeKind::Array(elem_ty, _) = expr_type.1.resolve_weak(refs) {
                    Ok((ReturnKind::Soft, *elem_ty))
                } else if let TypeKind::UserPtr(_) = expr_type.1.resolve_weak(refs) {
                    Ok((ReturnKind::Soft, expr_type.1))
                } else {
                    Err(ReturnTypeOther::IndexingNonArray(expression.1))
                }
            }
            Array(expressions) => {
                let first = expressions
                    .iter()
                    .next()
                    .map(|e| e.return_type(refs, mod_id))
                    .unwrap_or(Ok((ReturnKind::Soft, TypeKind::Void)))?;
                Ok((
                    ReturnKind::Soft,
                    TypeKind::Array(Box::new(first.1), expressions.len() as u64),
                ))
            }
            Accessed(_, type_kind, _) => Ok((ReturnKind::Soft, type_kind.clone())),
            Struct(name, _) => Ok((
                ReturnKind::Soft,
                TypeKind::CustomType(CustomTypeKey(name.clone(), mod_id)),
            )),
            Borrow(expr, mutable) => {
                let ret_type = expr.return_type(refs, mod_id)?;
                Ok((ret_type.0, TypeKind::Borrow(Box::new(ret_type.1), *mutable)))
            }
            Deref(expr) => {
                let (kind, ret_type) = expr.return_type(refs, mod_id)?;
                match ret_type.resolve_weak(refs) {
                    TypeKind::Borrow(type_kind, _) => Ok((kind, *type_kind)),
                    _ => Err(ReturnTypeOther::DerefNonBorrow(expr.1)),
                }
            }
            CastTo(expr, type_kind) => match expr.return_type(refs, mod_id) {
                Ok(ret_type) => match ret_type {
                    (ReturnKind::Hard, ty) => Ok((ReturnKind::Hard, ty)),
                    _ => Ok((ReturnKind::Soft, type_kind.clone())),
                },
                Err(_) => Ok((ReturnKind::Soft, type_kind.clone())),
            },
            AssociatedFunctionCall(_, fcall) => fcall.return_type(),
            GlobalRef(_, type_kind) => Ok((ReturnKind::Soft, TypeKind::UserPtr(Box::new(type_kind.clone())))),
        }
    }

    pub fn backing_var(&self) -> Option<&NamedVariableRef> {
        match &self.0 {
            ExprKind::Variable(var_ref) => Some(var_ref),
            ExprKind::Indexed(lhs, _, _) => lhs.backing_var(),
            ExprKind::Accessed(lhs, _, _) => lhs.backing_var(),
            ExprKind::Borrow(expr, _) => expr.backing_var(),
            ExprKind::Deref(expr) => expr.backing_var(),
            ExprKind::Block(block) => block.backing_var(),
            ExprKind::Array(_) => None,
            ExprKind::Struct(_, _) => None,
            ExprKind::Literal(_) => None,
            ExprKind::BinOp(_, _, _, _) => None,
            ExprKind::FunctionCall(_) => None,
            ExprKind::If(_) => None,
            ExprKind::CastTo(expression, _) => expression.backing_var(),
            ExprKind::AssociatedFunctionCall(..) => None,
            ExprKind::GlobalRef(..) => None,
        }
    }

    pub fn num_value(&self) -> Result<Option<i128>, NumValueError> {
        Ok(match &self.0 {
            ExprKind::Variable(_) => None,
            ExprKind::Indexed(..) => None,
            ExprKind::Accessed(..) => None,
            ExprKind::Array(_) => None,
            ExprKind::Struct(..) => None,
            ExprKind::Literal(literal) => literal.num_value(),
            ExprKind::BinOp(op, lhs, rhs, _) => match op {
                BinaryOperator::Add => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a + b),
                BinaryOperator::Minus => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a - b),
                BinaryOperator::Mult => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a * b),
                BinaryOperator::And => None,
                BinaryOperator::Cmp(_) => None,
                BinaryOperator::Div => {
                    let rhs_value = rhs.num_value()?;
                    if rhs_value == Some(0) {
                        Err(NumValueError::DivideZero)?
                    }
                    maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a / b)
                }
                BinaryOperator::Mod => {
                    let rhs_value = rhs.num_value()?;
                    if rhs_value == Some(0) {
                        Err(NumValueError::DivideZero)?
                    }
                    maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a % b)
                }
                BinaryOperator::Or => None,
                BinaryOperator::Xor => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a ^ b),
                BinaryOperator::BitOr => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a | b),
                BinaryOperator::BitAnd => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a & b),
                BinaryOperator::BitshiftRight => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a >> b),
                BinaryOperator::BitshiftLeft => maybe(lhs.num_value()?, rhs.num_value()?, |a, b| a << b),
            },
            ExprKind::FunctionCall(..) => None,
            ExprKind::If(_) => None,
            ExprKind::Block(_) => None,
            ExprKind::Borrow(_, _) => None,
            ExprKind::Deref(_) => None,
            ExprKind::CastTo(expression, _) => expression.num_value()?,
            ExprKind::AssociatedFunctionCall(..) => None,
            ExprKind::GlobalRef(..) => None,
        })
    }
}

impl IfExpression {
    pub fn return_type(
        &self,
        refs: &TypeRefs,
        mod_id: SourceModuleId,
    ) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        let then_r = self.1.return_type(refs, mod_id)?;
        if let Some(else_e) = self.2.as_ref() {
            let else_r = else_e.return_type(refs, mod_id)?;

            let kind = if then_r.0 == ReturnKind::Hard && else_r.0 == ReturnKind::Hard {
                ReturnKind::Hard
            } else {
                ReturnKind::Soft
            };
            Ok((kind, then_r.1))
        } else {
            Ok((ReturnKind::Soft, then_r.1))
        }
    }
}

impl NamedVariableRef {
    pub fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        Ok((ReturnKind::Soft, self.0.clone()))
    }
}

impl FunctionCall {
    pub fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        Ok((ReturnKind::Soft, self.return_type.clone()))
    }
}

fn if_hard<TErr>(
    return_type: (ReturnKind, TypeKind),
    default: Result<(ReturnKind, TypeKind), TErr>,
) -> Result<(ReturnKind, TypeKind), TErr> {
    if let (ReturnKind::Hard, _) = return_type {
        Ok(return_type)
    } else {
        default
    }
}

pub fn pick_return<T>(lhs: (ReturnKind, T), rhs: (ReturnKind, T)) -> (ReturnKind, T) {
    use ReturnKind::*;
    match (lhs.0, rhs.0) {
        (Hard, Hard) => (Hard, lhs.1),
        (Hard, Soft) => (Soft, rhs.1),
        (Soft, Hard) => (Soft, lhs.1),
        (_, _) => (Soft, lhs.1),
    }
}

impl Literal {
    pub fn num_value(&self) -> Option<i128> {
        match self {
            Literal::I8(val) => Some(*val as i128),
            Literal::I16(val) => Some(*val as i128),
            Literal::I32(val) => Some(*val as i128),
            Literal::I64(val) => Some(*val as i128),
            Literal::I128(val) => Some(*val as i128),
            Literal::U8(val) => Some(*val as i128),
            Literal::U16(val) => Some(*val as i128),
            Literal::U32(val) => Some(*val as i128),
            Literal::U64(val) => Some(*val as i128),
            Literal::U128(val) => Some(*val as i128),
            Literal::Bool(_) => None,
            Literal::String(_) => None,
            Literal::Vague(VagueLiteral::Number(val)) => Some(*val as i128),
            Literal::Vague(VagueLiteral::Decimal(_)) => None,
            Literal::F16(_) => None,
            Literal::F32B(_) => None,
            Literal::F32(_) => None,
            Literal::F64(_) => None,
            Literal::F80(_) => None,
            Literal::F128(_) => None,
            Literal::F128PPC(_) => None,
            Literal::Char(_) => None,
        }
    }
}

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq, PartialOrd, Ord)]
pub enum EqualsIssue {
    #[error("Function is already defined locally at {:?}", (.0).range)]
    ExistsLocally(Metadata),
    #[error("Equals")]
    Equals,
    #[error("Function {0} is already declared locally at {:?}", (.1).range)]
    AlreadyExtern(String, Metadata),
    #[error("Function {0} is already imported from another module")]
    ConflictWithImport(String),
    #[error("Function is defined as an intrinsic")]
    ExistsAsIntrinsic,
}

impl FunctionDefinition {
    pub fn equals_as_imported(&self, other: &FunctionDefinition) -> Result<(), EqualsIssue> {
        match &self.kind {
            FunctionDefinitionKind::Local(_, metadata) => Err(EqualsIssue::ExistsLocally(*metadata)),
            FunctionDefinitionKind::Extern(imported) => {
                if *imported {
                    Err(EqualsIssue::ConflictWithImport(self.name.clone()))
                } else {
                    if self.is_pub == other.is_pub
                        && self.name == other.name
                        && self.parameters == other.parameters
                        && self.return_type == other.return_type
                    {
                        Ok(())
                    } else {
                        Err(EqualsIssue::AlreadyExtern(self.name.clone(), self.signature()))
                    }
                }
            }
            FunctionDefinitionKind::Intrinsic(_) => Err(EqualsIssue::ExistsAsIntrinsic),
        }
    }
}
