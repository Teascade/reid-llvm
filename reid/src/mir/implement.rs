use super::{typecheck::ErrorKind, typerefs::TypeRefs, VagueType as Vague, *};

#[derive(Debug, Clone)]
pub enum ReturnTypeOther {
    Import(Metadata),
    Let(Metadata),
    Set(Metadata),
    EmptyBlock(Metadata),
    NoBlockReturn(Metadata),
    IndexingNonArray(Metadata),
    DerefNonBorrow(Metadata),
}

impl TypeKind {
    pub fn collapse_into(&self, other: &TypeKind) -> Result<TypeKind, ErrorKind> {
        if self == other {
            return Ok(self.clone());
        }

        match (self, other) {
            (TypeKind::Vague(Vague::Integer), other) | (other, TypeKind::Vague(Vague::Integer)) => {
                match other {
                    TypeKind::Vague(Vague::Unknown) => Ok(TypeKind::Vague(Vague::Integer)),
                    TypeKind::Vague(Vague::Integer) => Ok(TypeKind::Vague(Vague::Integer)),
                    TypeKind::I8
                    | TypeKind::I16
                    | TypeKind::I32
                    | TypeKind::I64
                    | TypeKind::I128
                    | TypeKind::U8
                    | TypeKind::U16
                    | TypeKind::U32
                    | TypeKind::U64
                    | TypeKind::U128 => Ok(other.clone()),
                    _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
                }
            }
            (TypeKind::Vague(Vague::Decimal), other) | (other, TypeKind::Vague(Vague::Decimal)) => {
                match other {
                    TypeKind::Vague(Vague::Unknown) => Ok(TypeKind::Vague(Vague::Decimal)),
                    TypeKind::Vague(Vague::Decimal) => Ok(TypeKind::Vague(Vague::Decimal)),
                    TypeKind::F16
                    | TypeKind::F32B
                    | TypeKind::F32
                    | TypeKind::F64
                    | TypeKind::F80
                    | TypeKind::F128
                    | TypeKind::F128PPC => Ok(other.clone()),
                    _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
                }
            }
            (TypeKind::Vague(Vague::Unknown), other) | (other, TypeKind::Vague(Vague::Unknown)) => {
                Ok(other.clone())
            }
            (TypeKind::Borrow(val1, mut1), TypeKind::Borrow(val2, mut2)) => {
                // Extracted to give priority for other collapse-error
                let collapsed = val1.collapse_into(val2)?;
                if mut1 == mut2 {
                    Ok(TypeKind::Borrow(Box::new(collapsed), *mut1 && *mut2))
                } else {
                    Err(ErrorKind::TypesDifferMutability(
                        self.clone(),
                        other.clone(),
                    ))
                }
            }
            (TypeKind::UserPtr(val1), TypeKind::UserPtr(val2)) => {
                Ok(TypeKind::UserPtr(Box::new(val1.collapse_into(val2)?)))
            }
            _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
        }
    }

    pub fn cast_into(&self, other: &TypeKind) -> Result<TypeKind, ErrorKind> {
        if let Ok(collapsed) = self.collapse_into(other) {
            Ok(collapsed)
        } else {
            let self_cat = self.category();
            let other_cat = other.category();
            match (self, other) {
                (TypeKind::UserPtr(_), TypeKind::UserPtr(_)) => Ok(other.clone()),
                (TypeKind::Str, TypeKind::U8) => Ok(other.clone()),
                (TypeKind::U8, TypeKind::Str) => Ok(other.clone()),
                _ => match (&self_cat, &other_cat) {
                    (TypeCategory::Integer, TypeCategory::Integer) => Ok(other.clone()),
                    (TypeCategory::Integer, TypeCategory::Real) => Ok(other.clone()),
                    (TypeCategory::Real, TypeCategory::Integer) => Ok(other.clone()),
                    (TypeCategory::Real, TypeCategory::Real) => Ok(other.clone()),
                    _ => Err(ErrorKind::NotCastableTo(self.clone(), other.clone())),
                },
            }
        }
    }

    /// Return the type that is the result of a binary operator between two
    /// values of this type
    pub fn binop_type(&self, op: &BinaryOperator) -> TypeKind {
        // TODO make some type of mechanism that allows to binop two values of
        // differing types..
        // TODO Return None for arrays later
        match op {
            BinaryOperator::Add => self.clone(),
            BinaryOperator::Minus => self.clone(),
            BinaryOperator::Mult => self.clone(),
            BinaryOperator::And => TypeKind::Bool,
            BinaryOperator::Cmp(_) => TypeKind::Bool,
        }
    }

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
            TypeKind::Str => false,
            TypeKind::Array(_, _) => false,
            TypeKind::CustomType(_) => false,
            TypeKind::CodegenPtr(_) => false,
            TypeKind::Vague(_) => false,
            TypeKind::Borrow(_, _) => false,
            TypeKind::UserPtr(_) => false,
            TypeKind::F16 => true,
            TypeKind::F32B => true,
            TypeKind::F32 => true,
            TypeKind::F64 => true,
            TypeKind::F128 => true,
            TypeKind::F80 => true,
            TypeKind::F128PPC => true,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
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
            TypeKind::Void => false,
            TypeKind::Str => false,
            TypeKind::Array(_, _) => false,
            TypeKind::CustomType(_) => false,
            TypeKind::CodegenPtr(_) => false,
            TypeKind::Vague(_) => false,
            TypeKind::Borrow(_, _) => false,
            TypeKind::UserPtr(_) => false,
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
            TypeKind::Str => 8,
            TypeKind::Array(type_kind, len) => type_kind.size_of() * len,
            TypeKind::CustomType(_) => 32,
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
            TypeKind::Str => 8,
            TypeKind::Array(type_kind, _) => type_kind.alignment(),
            TypeKind::CustomType(_) => 32,
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
            | TypeKind::Str => TypeCategory::Integer,
            TypeKind::F16
            | TypeKind::F32B
            | TypeKind::F32
            | TypeKind::F64
            | TypeKind::F128
            | TypeKind::F80
            | TypeKind::F128PPC => TypeCategory::Real,
            TypeKind::Void => TypeCategory::Other,
            TypeKind::Bool => TypeCategory::Other,
            TypeKind::Array(_, _) => TypeCategory::Other,
            TypeKind::CustomType(_) => TypeCategory::Other,
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
}

pub enum TypeCategory {
    Integer,
    Real,
    Other,
    TypeRef,
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

enum BlockReturn<'b> {
    Early(&'b Statement),
    Normal(ReturnKind, &'b Expression),
}

impl Block {
    fn return_expr(&self) -> Result<BlockReturn, ReturnTypeOther> {
        let mut early_return = None;

        for statement in &self.statements {
            let ret = statement.return_type(&Default::default());
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
            .map(|e| e.1 .1)
            .or(self.statements.last().map(|s| s.1))
            .unwrap_or(self.meta)
    }

    pub fn return_type(&self, refs: &TypeRefs) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        let mut early_return = None;

        for statement in &self.statements {
            let ret = statement.return_type(refs);
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
            .and_then(|(kind, stmt)| Ok((*kind, stmt.return_type(refs)?.1)))
    }

    pub fn backing_var(&self) -> Option<&NamedVariableRef> {
        match self.return_expr().ok()? {
            BlockReturn::Early(statement) => statement.backing_var(),
            BlockReturn::Normal(kind, expr) => {
                if kind == ReturnKind::Soft {
                    expr.backing_var()
                } else {
                    None
                }
            }
        }
    }
}

impl Statement {
    pub fn return_type(&self, refs: &TypeRefs) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use StmtKind::*;
        match &self.0 {
            Let(var, _, expr) => if_hard(
                expr.return_type(refs)?,
                Err(ReturnTypeOther::Let(var.2 + expr.1)),
            ),
            Set(lhs, rhs) => if_hard(
                rhs.return_type(refs)?,
                Err(ReturnTypeOther::Set(lhs.1 + rhs.1)),
            ),
            Import(_) => todo!(),
            Expression(expression) => expression.return_type(refs),
        }
    }

    pub fn backing_var(&self) -> Option<&NamedVariableRef> {
        match &self.0 {
            StmtKind::Let(_, _, _) => None,
            StmtKind::Set(_, _) => None,
            StmtKind::Import(_) => None,
            StmtKind::Expression(expr) => expr.backing_var(),
        }
    }
}

impl Expression {
    pub fn return_type(&self, refs: &TypeRefs) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use ExprKind::*;
        match &self.0 {
            Literal(lit) => Ok((ReturnKind::Soft, lit.as_type())),
            Variable(var) => var.return_type(),
            BinOp(_, then_e, else_e) => {
                let then_r = then_e.return_type(refs)?;
                let else_r = else_e.return_type(refs)?;

                Ok(pick_return(then_r, else_r))
            }
            Block(block) => block.return_type(refs),
            FunctionCall(fcall) => fcall.return_type(),
            If(expr) => expr.return_type(refs),
            Indexed(expression, _, _) => {
                let expr_type = expression.return_type(refs)?;
                if let TypeKind::Array(elem_ty, _) = expr_type.1.resolve_weak(refs) {
                    Ok((ReturnKind::Soft, *elem_ty))
                } else {
                    Err(ReturnTypeOther::IndexingNonArray(expression.1))
                }
            }
            Array(expressions) => {
                let first = expressions
                    .iter()
                    .next()
                    .map(|e| e.return_type(refs))
                    .unwrap_or(Ok((ReturnKind::Soft, TypeKind::Void)))?;
                Ok((
                    ReturnKind::Soft,
                    TypeKind::Array(Box::new(first.1), expressions.len() as u64),
                ))
            }
            Accessed(_, type_kind, _) => Ok((ReturnKind::Soft, type_kind.clone())),
            Struct(name, _) => Ok((ReturnKind::Soft, TypeKind::CustomType(name.clone()))),
            Borrow(var, mutable) => {
                let ret_type = var.return_type()?;
                Ok((ret_type.0, TypeKind::Borrow(Box::new(ret_type.1), *mutable)))
            }
            Deref(var) => {
                let (kind, ret_type) = var.return_type()?;
                match ret_type.resolve_weak(refs) {
                    TypeKind::Borrow(type_kind, _) => Ok((kind, *type_kind)),
                    _ => Err(ReturnTypeOther::DerefNonBorrow(var.2)),
                }
            }
            CastTo(expr, type_kind) => match expr.return_type(refs) {
                Ok(ret_type) => match ret_type {
                    (ReturnKind::Hard, ty) => Ok((ReturnKind::Hard, ty)),
                    _ => Ok((ReturnKind::Soft, type_kind.clone())),
                },
                Err(_) => Ok((ReturnKind::Soft, type_kind.clone())),
            },
        }
    }

    pub fn backing_var(&self) -> Option<&NamedVariableRef> {
        match &self.0 {
            ExprKind::Variable(var_ref) => Some(var_ref),
            ExprKind::Indexed(lhs, _, _) => lhs.backing_var(),
            ExprKind::Accessed(lhs, _, _) => lhs.backing_var(),
            ExprKind::Borrow(var, _) => Some(var),
            ExprKind::Deref(var) => Some(var),
            ExprKind::Block(block) => block.backing_var(),
            ExprKind::Array(_) => None,
            ExprKind::Struct(_, _) => None,
            ExprKind::Literal(_) => None,
            ExprKind::BinOp(_, _, _) => None,
            ExprKind::FunctionCall(_) => None,
            ExprKind::If(_) => None,
            ExprKind::CastTo(expression, _) => expression.backing_var(),
        }
    }

    pub fn is_zero(&self) -> Option<bool> {
        Some(self.num_value()? == 0)
    }

    pub fn num_value(&self) -> Option<i128> {
        match &self.0 {
            ExprKind::Variable(_) => None,
            ExprKind::Indexed(..) => None,
            ExprKind::Accessed(..) => None,
            ExprKind::Array(_) => None,
            ExprKind::Struct(..) => None,
            ExprKind::Literal(literal) => literal.num_value(),
            ExprKind::BinOp(op, lhs, rhs) => match op {
                BinaryOperator::Add => Some(lhs.num_value()? + rhs.num_value()?),
                BinaryOperator::Minus => Some(lhs.num_value()? - rhs.num_value()?),
                BinaryOperator::Mult => Some(lhs.num_value()? * rhs.num_value()?),
                BinaryOperator::And => None,
                BinaryOperator::Cmp(_) => None,
            },
            ExprKind::FunctionCall(..) => None,
            ExprKind::If(_) => None,
            ExprKind::Block(_) => None,
            ExprKind::Borrow(_, _) => None,
            ExprKind::Deref(_) => None,
            ExprKind::CastTo(expression, _) => expression.num_value(),
        }
    }
}

impl IfExpression {
    pub fn return_type(&self, refs: &TypeRefs) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        let then_r = self.1.return_type(refs)?;
        if let Some(else_b) = &self.2 {
            let else_r = else_b.return_type(refs)?;

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

impl TypeKind {
    /// Assert that a type is already known and not vague. Return said type or
    /// error.
    pub fn assert_known(&self) -> Result<TypeKind, ErrorKind> {
        self.known().map_err(ErrorKind::TypeIsVague)
    }

    /// Try to collapse a type on itself producing a default type if one exists,
    /// Error if not.
    pub fn or_default(&self) -> Result<TypeKind, ErrorKind> {
        match self {
            TypeKind::Vague(vague_type) => match &vague_type {
                Vague::Unknown => Err(ErrorKind::TypeIsVague(*vague_type)),
                Vague::Integer => Ok(TypeKind::I32),
                Vague::TypeRef(_) => panic!("Hinted default!"),
                VagueType::Decimal => Ok(TypeKind::F32),
            },
            _ => Ok(self.clone()),
        }
    }

    pub fn resolve_weak(&self, refs: &TypeRefs) -> TypeKind {
        match self {
            TypeKind::Vague(Vague::TypeRef(idx)) => refs.retrieve_type(*idx).unwrap(),
            _ => self.clone(),
        }
    }

    pub fn resolve_ref(&self, refs: &TypeRefs) -> TypeKind {
        let resolved = self.resolve_weak(refs);
        match resolved {
            TypeKind::Array(t, len) => TypeKind::Array(Box::new(t.resolve_ref(refs)), len),
            TypeKind::Borrow(inner, mutable) => {
                TypeKind::Borrow(Box::new(inner.resolve_ref(refs)), mutable)
            }
            _ => resolved,
        }
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
}

impl FunctionDefinition {
    pub fn equals_as_imported(&self, other: &FunctionDefinition) -> Result<(), EqualsIssue> {
        match &self.kind {
            FunctionDefinitionKind::Local(_, metadata) => {
                Err(EqualsIssue::ExistsLocally(*metadata))
            }
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
                        Err(EqualsIssue::AlreadyExtern(
                            self.name.clone(),
                            self.signature(),
                        ))
                    }
                }
            }
        }
    }
}
