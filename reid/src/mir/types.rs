use super::{
    typecheck::ErrorKind,
    typerefs::{ScopeTypeRefs, TypeRef, TypeRefs},
    VagueType as Vague, *,
};

#[derive(Debug, Clone)]
pub enum ReturnTypeOther {
    Import(Metadata),
    Let(Metadata),
    Set(Metadata),
    EmptyBlock(Metadata),
    NoBlockReturn(Metadata),
    IndexingNonArray(Metadata),
}

impl TypeKind {
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

pub trait ReturnType {
    /// Return the return type of this node
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther>;
}

impl ReturnType for Block {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        let mut early_return = None;

        for statement in &self.statements {
            let ret = statement.return_type();
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
            .and_then(|(kind, stmt)| Ok((*kind, stmt.return_type()?.1)))
    }
}

impl ReturnType for Statement {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use StmtKind::*;
        match &self.0 {
            Let(var, _, expr) => if_hard(
                expr.return_type()?,
                Err(ReturnTypeOther::Let(var.2 + expr.1)),
            ),
            Set(var, expr) => if_hard(
                expr.return_type()?,
                Err(ReturnTypeOther::Set(var.meta + expr.1)),
            ),
            Import(_) => todo!(),
            Expression(expression) => expression.return_type(),
        }
    }
}

impl ReturnType for Expression {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use ExprKind::*;
        match &self.0 {
            Literal(lit) => Ok((ReturnKind::Soft, lit.as_type())),
            Variable(var) => var.return_type(),
            BinOp(_, then_e, else_e) => {
                let then_r = then_e.return_type()?;
                let else_r = else_e.return_type()?;

                Ok(pick_return(then_r, else_r))
            }
            Block(block) => block.return_type(),
            FunctionCall(fcall) => fcall.return_type(),
            If(expr) => expr.return_type(),
            Indexed(expression, _, _) => {
                let expr_type = expression.return_type()?;
                if let (_, TypeKind::Array(elem_ty, _)) = expr_type {
                    Ok((ReturnKind::Soft, *elem_ty))
                } else {
                    Err(ReturnTypeOther::IndexingNonArray(expression.1))
                }
            }
            Array(expressions) => {
                let first = expressions
                    .iter()
                    .next()
                    .map(|e| e.return_type())
                    .unwrap_or(Ok((ReturnKind::Soft, TypeKind::Void)))?;
                Ok((
                    ReturnKind::Soft,
                    TypeKind::Array(Box::new(first.1), expressions.len() as u64),
                ))
            }
            Accessed(_, type_kind, _) => Ok((ReturnKind::Soft, type_kind.clone())),
            Struct(name, _) => Ok((ReturnKind::Soft, TypeKind::CustomType(name.clone()))),
        }
    }
}

impl ReturnType for IfExpression {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        let then_r = self.1.return_type()?;
        if let Some(else_b) = &self.2 {
            let else_r = else_b.return_type()?;

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

impl ReturnType for NamedVariableRef {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        Ok((ReturnKind::Soft, self.0.clone()))
    }
}

impl ReturnType for FunctionCall {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
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
                Vague::Number => Ok(TypeKind::I32),
                Vague::TypeRef(_) => panic!("Hinted default!"),
            },
            _ => Ok(self.clone()),
        }
    }

    pub fn resolve_ref(&self, refs: &TypeRefs) -> TypeKind {
        let resolved = match self {
            TypeKind::Vague(Vague::TypeRef(idx)) => refs.retrieve_type(*idx).unwrap(),
            _ => self.clone(),
        };
        match resolved {
            TypeKind::Array(t, len) => TypeKind::Array(Box::new(t.resolve_ref(refs)), len),
            _ => resolved,
        }
    }
}

impl IndexedVariableReference {
    pub fn get_name(&self) -> String {
        match &self.kind {
            IndexedVariableReferenceKind::Named(NamedVariableRef(_, name, _)) => name.clone(),
            IndexedVariableReferenceKind::ArrayIndex(inner, idx) => {
                format!("{}[{}]", inner.get_name(), idx)
            }
            IndexedVariableReferenceKind::StructIndex(inner, name) => {
                format!("{}.{}", inner.get_name(), name)
            }
        }
    }

    /// Retrieve the indexed type that this variable reference is pointing to
    pub fn retrieve_type(&self, scope: &pass::Scope) -> Result<TypeKind, ErrorKind> {
        match &self.kind {
            IndexedVariableReferenceKind::Named(NamedVariableRef(ty, _, _)) => Ok(ty.clone()),
            IndexedVariableReferenceKind::ArrayIndex(inner, _) => {
                let inner_ty = inner.retrieve_type(scope)?;
                match inner_ty {
                    TypeKind::Array(type_kind, _) => Ok(*type_kind),
                    _ => Err(ErrorKind::TriedIndexingNonArray(inner_ty)),
                }
            }
            IndexedVariableReferenceKind::StructIndex(inner, field_name) => {
                let inner_ty = inner.retrieve_type(scope)?;
                match inner_ty {
                    TypeKind::CustomType(struct_name) => {
                        let struct_ty = scope
                            .get_struct_type(&struct_name)
                            .ok_or(ErrorKind::NoSuchType(struct_name.clone()))?;
                        struct_ty
                            .get_field_ty(&field_name)
                            .ok_or(ErrorKind::NoSuchField(field_name.clone()))
                            .cloned()
                    }
                    _ => Err(ErrorKind::TriedAccessingNonStruct(inner_ty)),
                }
            }
        }
    }

    pub fn into_typeref<'s>(&mut self, typerefs: &'s ScopeTypeRefs) -> Option<(bool, TypeRef<'s>)> {
        match &mut self.kind {
            IndexedVariableReferenceKind::Named(NamedVariableRef(ty, name, _)) => {
                let t = typerefs.find_var(name)?;
                *ty = t.1.as_type();
                Some(t)
            }
            IndexedVariableReferenceKind::ArrayIndex(inner, _) => inner.into_typeref(typerefs),
            IndexedVariableReferenceKind::StructIndex(inner, _) => inner.into_typeref(typerefs),
        }
    }

    pub fn resolve_ref<'s>(&mut self, typerefs: &'s TypeRefs) -> Result<TypeKind, ErrorKind> {
        match &mut self.kind {
            IndexedVariableReferenceKind::Named(NamedVariableRef(ty, _, _)) => {
                *ty = ty.resolve_ref(typerefs);
                Ok(ty.clone())
            }
            IndexedVariableReferenceKind::ArrayIndex(inner, _) => inner.resolve_ref(typerefs),
            IndexedVariableReferenceKind::StructIndex(inner, _) => inner.resolve_ref(typerefs),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
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
