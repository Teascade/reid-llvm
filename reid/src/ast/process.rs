use crate::{
    ast::{self},
    mir::{self, StmtKind, VariableReference},
};

impl mir::Context {
    pub fn from(modules: Vec<ast::Module>) -> mir::Context {
        mir::Context {
            modules: modules.iter().map(|m| m.process()).collect(),
        }
    }
}

impl ast::Module {
    fn process(&self) -> mir::Module {
        let mut imports = Vec::new();
        let mut functions = Vec::new();

        use ast::TopLevelStatement::*;
        for stmt in &self.top_level_statements {
            match stmt {
                Import(import) => {
                    for name in &import.0 {
                        imports.push(mir::Import(name.clone(), import.1.into()));
                    }
                }
                FunctionDefinition(ast::FunctionDefinition(signature, block, range)) => {
                    let def = mir::FunctionDefinition {
                        name: signature.name.clone(),
                        return_type: signature
                            .return_type
                            .map(|r| r.0.into())
                            .unwrap_or(mir::TypeKind::Void),
                        parameters: signature
                            .args
                            .iter()
                            .cloned()
                            .map(|p| (p.0, p.1.into()))
                            .collect(),
                        kind: mir::FunctionDefinitionKind::Local(block.into_mir(), (*range).into()),
                    };
                    functions.push(def);
                }
            }
        }

        // TODO do something with state here

        mir::Module {
            name: self.name.clone(),
            imports,
            functions,
        }
    }
}

impl ast::Block {
    pub fn into_mir(&self) -> mir::Block {
        let mut mir_statements = Vec::new();

        for statement in &self.0 {
            let (kind, range) = match statement {
                ast::BlockLevelStatement::Let(s_let) => (
                    mir::StmtKind::Let(
                        mir::VariableReference(
                            s_let
                                .1
                                .map(|t| t.0.into())
                                .unwrap_or(mir::TypeKind::Vague(mir::VagueType::Unknown)),
                            s_let.0.clone(),
                            s_let.4.into(),
                        ),
                        s_let.2,
                        s_let.3.process(),
                    ),
                    s_let.4,
                ),
                ast::BlockLevelStatement::Set(name, expression, range) => (
                    StmtKind::Set(
                        VariableReference(
                            mir::TypeKind::Vague(mir::VagueType::Unknown),
                            name.clone(),
                            (*range).into(),
                        ),
                        expression.process(),
                    ),
                    *range,
                ),
                ast::BlockLevelStatement::Import { _i } => todo!(),
                ast::BlockLevelStatement::Expression(e) => (StmtKind::Expression(e.process()), e.1),
                ast::BlockLevelStatement::Return(_, e) => (StmtKind::Expression(e.process()), e.1),
            };

            mir_statements.push(mir::Statement(kind, range.into()));
        }

        let return_expression = if let Some(r) = &self.1 {
            Some((r.0.into(), Box::new(r.1.process())))
        } else {
            None
        };

        mir::Block {
            statements: mir_statements,
            return_expression,
            meta: self.2.into(),
        }
    }
}

impl From<ast::ReturnType> for mir::ReturnKind {
    fn from(value: ast::ReturnType) -> Self {
        match value {
            ast::ReturnType::Soft => mir::ReturnKind::Soft,
            ast::ReturnType::Hard => mir::ReturnKind::Hard,
        }
    }
}

impl ast::Expression {
    fn process(&self) -> mir::Expression {
        let kind = match &self.0 {
            ast::ExpressionKind::VariableName(name) => mir::ExprKind::Variable(VariableReference(
                mir::TypeKind::Vague(mir::VagueType::Unknown),
                name.clone(),
                self.1.into(),
            )),
            ast::ExpressionKind::Literal(literal) => mir::ExprKind::Literal(literal.mir()),
            ast::ExpressionKind::Binop(binary_operator, lhs, rhs) => mir::ExprKind::BinOp(
                binary_operator.mir(),
                Box::new(lhs.process()),
                Box::new(rhs.process()),
            ),
            ast::ExpressionKind::FunctionCall(fn_call_expr) => {
                mir::ExprKind::FunctionCall(mir::FunctionCall {
                    name: fn_call_expr.0.clone(),
                    return_type: mir::TypeKind::Vague(mir::VagueType::Unknown),
                    parameters: fn_call_expr.1.iter().map(|e| e.process()).collect(),
                })
            }
            ast::ExpressionKind::BlockExpr(block) => mir::ExprKind::Block(block.into_mir()),
            ast::ExpressionKind::IfExpr(if_expression) => {
                let cond = if_expression.0.process();
                let then_block = if_expression.1.into_mir();
                let else_block = if let Some(el) = &if_expression.2 {
                    Some(el.into_mir())
                } else {
                    None
                };
                mir::ExprKind::If(mir::IfExpression(Box::new(cond), then_block, else_block))
            }
        };

        mir::Expression(kind, self.1.into())
    }
}

impl ast::BinaryOperator {
    fn mir(&self) -> mir::BinaryOperator {
        match self {
            ast::BinaryOperator::Add => mir::BinaryOperator::Add,
            ast::BinaryOperator::Minus => mir::BinaryOperator::Minus,
            ast::BinaryOperator::Mult => mir::BinaryOperator::Mult,
            ast::BinaryOperator::And => mir::BinaryOperator::And,
            ast::BinaryOperator::LT => mir::BinaryOperator::Cmp(mir::CmpOperator::LT),
            ast::BinaryOperator::LE => mir::BinaryOperator::Cmp(mir::CmpOperator::LE),
            ast::BinaryOperator::GT => mir::BinaryOperator::Cmp(mir::CmpOperator::GT),
            ast::BinaryOperator::GE => mir::BinaryOperator::Cmp(mir::CmpOperator::GE),
            ast::BinaryOperator::EQ => mir::BinaryOperator::Cmp(mir::CmpOperator::EQ),
            ast::BinaryOperator::NE => mir::BinaryOperator::Cmp(mir::CmpOperator::NE),
        }
    }
}

impl ast::Literal {
    fn mir(&self) -> mir::Literal {
        match *self {
            ast::Literal::Number(v) => mir::Literal::Vague(mir::VagueLiteral::Number(v)),
            ast::Literal::Bool(v) => mir::Literal::Bool(v),
        }
    }
}

impl From<ast::TypeKind> for mir::TypeKind {
    fn from(value: ast::TypeKind) -> Self {
        match value {
            ast::TypeKind::Bool => mir::TypeKind::Bool,
            ast::TypeKind::I8 => mir::TypeKind::I8,
            ast::TypeKind::I16 => mir::TypeKind::I16,
            ast::TypeKind::I32 => mir::TypeKind::I32,
            ast::TypeKind::I64 => mir::TypeKind::I64,
            ast::TypeKind::I128 => mir::TypeKind::I128,
            ast::TypeKind::U8 => mir::TypeKind::U8,
            ast::TypeKind::U16 => mir::TypeKind::U16,
            ast::TypeKind::U32 => mir::TypeKind::U32,
            ast::TypeKind::U64 => mir::TypeKind::U64,
            ast::TypeKind::U128 => mir::TypeKind::U128,
        }
    }
}

impl From<ast::Type> for mir::TypeKind {
    fn from(value: ast::Type) -> Self {
        value.0.into()
    }
}
