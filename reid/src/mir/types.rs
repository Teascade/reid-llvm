use super::*;

#[derive(Debug, Clone)]
pub enum ReturnTypeOther {
    Import(TokenRange),
    Let(TokenRange),
    EmptyBlock(TokenRange),
    NoBlockReturn(TokenRange),
}

pub trait ReturnType {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther>;
}

impl ReturnType for Block {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        self.return_expression
            .as_ref()
            .ok_or(ReturnTypeOther::NoBlockReturn(self.meta.range))
            .and_then(|(kind, stmt)| Ok((*kind, stmt.return_type()?.1)))
    }
}

impl ReturnType for Statement {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        use StmtKind::*;
        match &self.0 {
            Expression(e) => e.return_type(),
            Import(_) => Err(ReturnTypeOther::Import(self.1.range)),
            Let(_, _) => Err(ReturnTypeOther::Let(self.1.range)),
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
                let else_e = else_e.return_type()?;
                let kind = if then_r.0 == ReturnKind::Hard && else_e.0 == ReturnKind::Hard {
                    ReturnKind::Hard
                } else {
                    ReturnKind::Hard
                };
                Ok((kind, then_r.1))
            }
            Block(block) => block.return_type(),
            FunctionCall(fcall) => fcall.return_type(),
            If(expr) => expr.return_type(),
        }
    }
}

impl ReturnType for IfExpression {
    fn return_type(&self) -> Result<(ReturnKind, TypeKind), ReturnTypeOther> {
        self.1.return_type()
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
