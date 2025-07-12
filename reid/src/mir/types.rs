use crate::mir::typecheck::Collapsable;

use super::*;

#[derive(Debug, Clone)]
pub enum ReturnTypeOther {
    Import(Metadata),
    Let(Metadata),
    Set(Metadata),
    EmptyBlock(Metadata),
    NoBlockReturn(Metadata),
}

pub trait ReturnType {
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

        // TODO should actually probably prune all instructions after this one
        // as to not cause problems in codegen later (when unable to delete the
        // block)
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
                Err(ReturnTypeOther::Set(var.2 + expr.1)),
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

impl ReturnType for VariableReference {
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
