use crate::util::try_all;

use super::*;

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
            ArrayIndex(expression, _, _) => {
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
            StructIndex(expression, type_kind, _) => todo!("todo return type for struct index"),
            Struct(name, items) => {
                let f_types = try_all(items.iter().map(|e| e.1.return_type()).collect())
                    .map_err(|e| unsafe { e.get_unchecked(0).clone() })?
                    .iter()
                    .map(|r| r.1.clone())
                    .collect();
                Ok((
                    ReturnKind::Soft,
                    TypeKind::CustomType(name.clone(), CustomTypeKind::Struct(f_types)),
                ))
            }
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

    pub fn update_type(&mut self, new_ty: &TypeKind) {
        match &mut self.kind {
            IndexedVariableReferenceKind::Named(NamedVariableRef(ty, _, _)) => {
                *ty = new_ty.clone();
            }
            IndexedVariableReferenceKind::ArrayIndex(inner, _) => inner.update_type(new_ty),
            IndexedVariableReferenceKind::StructIndex(inner, _) => inner.update_type(new_ty),
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
