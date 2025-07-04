use super::*;

#[derive(Debug, Clone)]
pub enum ReturnTypeOther {
    Import(TokenRange),
    Let(TokenRange),
    EmptyBlock(TokenRange),
    NoBlockReturn(TokenRange),
}

pub trait ReturnType {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther>;
}

impl ReturnType for Block {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        self.return_expression
            .as_ref()
            .ok_or(ReturnTypeOther::NoBlockReturn(self.range.clone()))
            .and_then(|(_, stmt)| stmt.return_type())
    }
}

impl ReturnType for Statement {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        use StatementKind::*;
        match &self.0 {
            Expression(e) => e.return_type(),
            If(e) => e.return_type(),
            Import(_) => Err(ReturnTypeOther::Import(self.1)),
            Let(_, _) => Err(ReturnTypeOther::Let(self.1)),
        }
    }
}

impl ReturnType for Expression {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        use ExpressionKind::*;
        match &self.0 {
            Literal(lit) => Ok(lit.as_type()),
            Variable(var) => var.return_type(),
            BinOp(_, expr, _) => expr.return_type(),
            Block(block) => block.return_type(),
            FunctionCall(fcall) => fcall.return_type(),
            If(expr) => expr.return_type(),
        }
    }
}

impl ReturnType for IfExpression {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        self.1.return_type()
    }
}

impl ReturnType for VariableReference {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        Ok(self.0)
    }
}

impl ReturnType for FunctionCall {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        Ok(self.return_type)
    }
}

impl ReturnType for FunctionDefinition {
    fn return_type(&self) -> Result<TypeKind, ReturnTypeOther> {
        match &self.kind {
            FunctionDefinitionKind::Local(block, _) => block.return_type(),
            FunctionDefinitionKind::Extern(type_kind) => Ok(*type_kind),
        }
    }
}
