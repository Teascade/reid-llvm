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
}

impl InferredType {
    fn collapse(&self, scope: &VirtualScope) -> mir::TypeKind {
        match self {
            InferredType::FromVariable(name) => {
                if let Some(inferred) = scope.get_var(name) {
                    inferred.collapse(scope)
                } else {
                    mir::TypeKind::Vague(mir::VagueType::Unknown)
                }
            }
            InferredType::FunctionReturn(name) => {
                if let Some(type_kind) = scope.get_return_type(name) {
                    type_kind.clone()
                } else {
                    mir::TypeKind::Vague(mir::VagueType::Unknown)
                }
            }
            InferredType::Static(type_kind) => type_kind.clone(),
            InferredType::OneOf(inferred_types) => {
                let list: Vec<mir::TypeKind> =
                    inferred_types.iter().map(|t| t.collapse(scope)).collect();
                if let Some(first) = list.first() {
                    if list.iter().all(|i| i == first) {
                        first.clone().into()
                    } else {
                        // IntoMIRError::ConflictingType(self.get_range())
                        mir::TypeKind::Void
                    }
                } else {
                    mir::TypeKind::Void
                }
            }
            InferredType::Void => mir::TypeKind::Void,
        }
    }
}

pub struct VirtualVariable {
    name: String,
    inferred: InferredType,
}

pub struct VirtualFunctionSignature {
    name: String,
    return_type: mir::TypeKind,
    parameter_types: Vec<mir::TypeKind>,
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

pub struct VirtualScope {
    variables: VirtualStorage<VirtualVariable>,
    functions: VirtualStorage<VirtualFunctionSignature>,
}

impl VirtualScope {
    pub fn set_var(&mut self, variable: VirtualVariable) {
        self.variables.set(variable.name.clone(), variable);
    }

    pub fn set_fun(&mut self, function: VirtualFunctionSignature) {
        self.functions.set(function.name.clone(), function)
    }

    pub fn get_var(&self, name: &String) -> Option<InferredType> {
        self.variables.get(name).and_then(|v| {
            if v.len() > 1 {
                Some(InferredType::OneOf(
                    v.iter().map(|v| v.inferred.clone()).collect(),
                ))
            } else if let Some(v) = v.first() {
                Some(v.inferred.clone())
            } else {
                None
            }
        })
    }

    pub fn get_return_type(&self, name: &String) -> Option<mir::TypeKind> {
        self.functions.get(name).and_then(|v| {
            if v.len() > 1 {
                Some(mir::TypeKind::Vague(mir::VagueType::Unknown))
            } else if let Some(v) = v.first() {
                Some(v.return_type.clone())
            } else {
                None
            }
        })
    }
}

impl Default for VirtualScope {
    fn default() -> Self {
        Self {
            variables: Default::default(),
            functions: Default::default(),
        }
    }
}

impl ast::Module {
    pub fn process(&self) -> mir::Module {
        let mut scope = VirtualScope::default();

        for stmt in &self.top_level_statements {
            match stmt {
                FunctionDefinition(ast::FunctionDefinition(signature, _, _)) => {
                    scope.set_fun(VirtualFunctionSignature {
                        name: signature.name.clone(),
                        return_type: signature.return_type.into(),
                        parameter_types: signature.args.iter().map(|p| p.1.into()).collect(),
                    });
                }
                _ => {}
            }
        }

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
                    for (name, ptype) in &signature.args {
                        scope.set_var(VirtualVariable {
                            name: name.clone(),
                            inferred: InferredType::Static((*ptype).into()),
                        });
                    }

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
                        kind: mir::FunctionDefinitionKind::Local(
                            block.into_mir(&mut scope),
                            (*range).into(),
                        ),
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
    pub fn into_mir(&self, scope: &mut VirtualScope) -> mir::Block {
        let mut mir_statements = Vec::new();

        for statement in &self.0 {
            let (kind, range) = match statement {
                ast::BlockLevelStatement::Let(s_let) => {
                    let t = s_let.1.infer_return_type().collapse(scope);
                    let inferred = InferredType::Static(t.clone());
                    scope.set_var(VirtualVariable {
                        name: s_let.0.clone(),
                        inferred,
                    });

                    (
                        mir::StmtKind::Let(
                            mir::VariableReference(t, s_let.0.clone(), s_let.2.into()),
                            s_let.1.process(scope),
                        ),
                        s_let.2,
                    )
                }
                ast::BlockLevelStatement::Import(_) => todo!(),
                ast::BlockLevelStatement::Expression(e) => {
                    (StmtKind::Expression(e.process(scope)), e.1)
                }
                ast::BlockLevelStatement::Return(_, e) => {
                    (StmtKind::Expression(e.process(scope)), e.1)
                }
            };

            mir_statements.push(mir::Statement(kind, range.into()));
        }

        let return_expression = if let Some(r) = &self.1 {
            Some((r.0.into(), Box::new(r.1.process(scope))))
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
    fn process(&self, scope: &mut VirtualScope) -> mir::Expression {
        let kind = match &self.0 {
            ast::ExpressionKind::VariableName(name) => mir::ExprKind::Variable(VariableReference(
                if let Some(ty) = scope.get_var(name) {
                    ty.collapse(scope)
                } else {
                    mir::TypeKind::Vague(mir::VagueType::Unknown)
                },
                name.clone(),
                self.1.into(),
            )),
            ast::ExpressionKind::Literal(literal) => mir::ExprKind::Literal(literal.mir()),
            ast::ExpressionKind::Binop(binary_operator, lhs, rhs) => mir::ExprKind::BinOp(
                binary_operator.mir(),
                Box::new(lhs.process(scope)),
                Box::new(rhs.process(scope)),
            ),
            ast::ExpressionKind::FunctionCall(fn_call_expr) => {
                mir::ExprKind::FunctionCall(mir::FunctionCall {
                    name: fn_call_expr.0.clone(),
                    return_type: if let Some(r_type) = scope.get_return_type(&fn_call_expr.0) {
                        r_type
                    } else {
                        mir::TypeKind::Vague(mir::VagueType::Unknown)
                    },
                    parameters: fn_call_expr.1.iter().map(|e| e.process(scope)).collect(),
                })
            }
            ast::ExpressionKind::BlockExpr(block) => mir::ExprKind::Block(block.into_mir(scope)),
            ast::ExpressionKind::IfExpr(if_expression) => {
                let cond = if_expression.0.process(scope);
                let then_block = if_expression.1.into_mir(scope);
                let else_block = if let Some(el) = &if_expression.2 {
                    Some(el.into_mir(scope))
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
            ast::Literal::I32(v) => mir::Literal::I32(v),
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

impl From<Option<ast::Type>> for mir::TypeKind {
    fn from(value: Option<ast::Type>) -> Self {
        match value {
            Some(v) => v.into(),
            None => mir::TypeKind::Void,
        }
    }
}
