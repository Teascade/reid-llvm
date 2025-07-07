use std::collections::HashMap;

use crate::{
    ast::{self, TypeKind},
    mir::{self, StmtKind, VariableReference},
};

#[derive(Clone)]
pub enum InferredType {
    FromVariable(String),
    FunctionReturn(String),
    Static(mir::TypeKind),
    OneOf(Vec<InferredType>),
    Void,
    Unknown,
}

impl InferredType {
    fn collapse(&self) -> mir::TypeKind {
        match self {
            InferredType::FromVariable(_) => mir::TypeKind::Vague(mir::VagueType::Unknown),
            InferredType::FunctionReturn(_) => mir::TypeKind::Vague(mir::VagueType::Unknown),
            InferredType::Static(type_kind) => type_kind.clone(),
            InferredType::OneOf(inferred_types) => {
                let list: Vec<mir::TypeKind> =
                    inferred_types.iter().map(|t| t.collapse()).collect();
                if let Some(first) = list.first() {
                    if list.iter().all(|i| i == first) {
                        first.clone().into()
                    } else {
                        mir::TypeKind::Vague(mir::VagueType::Unknown)
                    }
                } else {
                    mir::TypeKind::Void
                }
            }
            InferredType::Void => mir::TypeKind::Void,
            InferredType::Unknown => mir::TypeKind::Vague(mir::VagueType::Unknown),
        }
    }
}

pub struct VirtualStorage<T> {
    storage: HashMap<String, Vec<T>>,
}

impl<T> VirtualStorage<T> {
    fn set(&mut self, name: String, value: T) {
        if let Some(list) = self.storage.get_mut(&name) {
            list.push(value);
        } else {
            self.storage.insert(name, vec![value]);
        };
    }

    fn get(&self, name: &String) -> Option<&Vec<T>> {
        self.storage.get(name)
    }
}

impl<T> Default for VirtualStorage<T> {
    fn default() -> Self {
        Self {
            storage: Default::default(),
        }
    }
}

impl ast::Module {
    pub fn process(&self) -> mir::Module {
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
                ast::BlockLevelStatement::Let(s_let) => {
                    let t = s_let.1.infer_return_type().collapse();
                    let inferred = InferredType::Static(t.clone());

                    (
                        mir::StmtKind::Let(
                            mir::VariableReference(t, s_let.0.clone(), s_let.2.into()),
                            s_let.1.process(),
                        ),
                        s_let.2,
                    )
                }
                ast::BlockLevelStatement::Import(_) => todo!(),
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

    fn infer_return_type(&self) -> InferredType {
        self.1
            .as_ref()
            .map(|(_, expr)| expr.infer_return_type())
            .unwrap_or(InferredType::Void)
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

    fn infer_return_type(&self) -> InferredType {
        use ast::ExpressionKind::*;
        match &self.0 {
            VariableName(name) => InferredType::FromVariable(name.clone()),
            Literal(lit) => InferredType::Static(lit.mir().as_type()),
            Binop(_, lhs, rhs) => {
                InferredType::OneOf(vec![lhs.infer_return_type(), rhs.infer_return_type()])
            }
            FunctionCall(fncall) => InferredType::FunctionReturn(fncall.0.clone()),
            BlockExpr(block) => block.infer_return_type(),
            IfExpr(exp) => {
                let mut types = vec![exp.1.infer_return_type()];
                if let Some(e) = &exp.2 {
                    types.push(e.infer_return_type())
                }
                InferredType::OneOf(types)
            }
        }
    }
}

impl ast::BinaryOperator {
    fn mir(&self) -> mir::BinaryOperator {
        match self {
            ast::BinaryOperator::Add => mir::BinaryOperator::Add,
            ast::BinaryOperator::Minus => mir::BinaryOperator::Minus,
            ast::BinaryOperator::Mult => mir::BinaryOperator::Mult,
            ast::BinaryOperator::And => mir::BinaryOperator::And,
            ast::BinaryOperator::LessThan => {
                mir::BinaryOperator::Logic(mir::LogicOperator::LessThan)
            }
        }
    }
}

impl ast::Literal {
    fn mir(&self) -> mir::Literal {
        match *self {
            ast::Literal::Number(v) => mir::Literal::Vague(mir::VagueLiteral::Number(v)),
        }
    }
}

impl From<ast::TypeKind> for mir::TypeKind {
    fn from(value: ast::TypeKind) -> Self {
        match value {
            ast::TypeKind::I32 => mir::TypeKind::I32,
        }
    }
}

impl From<ast::Type> for mir::TypeKind {
    fn from(value: ast::Type) -> Self {
        value.0.into()
    }
}
