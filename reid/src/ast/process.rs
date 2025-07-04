use std::collections::HashMap;

use crate::{
    ast,
    mir::{self, StmtKind, VariableReference},
    token_stream::TokenRange,
};

#[derive(Clone)]
pub enum InferredType {
    FromVariable(String, TokenRange),
    FunctionReturn(String, TokenRange),
    Static(mir::TypeKind, TokenRange),
    OneOf(Vec<InferredType>),
    Void(TokenRange),
    DownstreamError(IntoMIRError, TokenRange),
}

fn all_ok<T, E>(result: Vec<Result<T, E>>) -> Option<Vec<T>> {
    let mut res = Vec::with_capacity(result.len());
    for item in result {
        if let Ok(item) = item {
            res.push(item);
        } else {
            return None;
        }
    }
    Some(res)
}

impl InferredType {
    fn collapse(
        &self,
        state: &mut State,
        scope: &VirtualScope,
    ) -> Result<mir::TypeKind, IntoMIRError> {
        match self {
            InferredType::FromVariable(name, token_range) => {
                if let Some(inferred) = scope.get_var(name) {
                    let temp = inferred.collapse(state, scope);
                    state.note(temp)
                } else {
                    state.err(IntoMIRError::VariableNotDefined(name.clone(), *token_range))
                }
            }
            InferredType::FunctionReturn(name, token_range) => {
                if let Some(type_kind) = scope.get_return_type(name) {
                    Ok(*type_kind)
                } else {
                    state.err(IntoMIRError::VariableNotDefined(name.clone(), *token_range))
                }
            }
            InferredType::Static(type_kind, _) => Ok(*type_kind),
            InferredType::OneOf(inferred_types) => {
                let collapsed = all_ok(
                    inferred_types
                        .iter()
                        .map(|t| {
                            let temp = t.collapse(state, scope);
                            state.note(temp)
                        })
                        .collect(),
                );
                if let Some(list) = collapsed {
                    if let Some(first) = list.first() {
                        if list.iter().all(|i| i == first) {
                            Ok((*first).into())
                        } else {
                            state.err(IntoMIRError::ConflictingType(self.get_range()))
                        }
                    } else {
                        state.err(IntoMIRError::VoidType(self.get_range()))
                    }
                } else {
                    state.err(IntoMIRError::DownstreamError(self.get_range()))
                }
            }
            InferredType::Void(token_range) => state.err(IntoMIRError::VoidType(*token_range)),
            InferredType::DownstreamError(e, _) => state.err(e.clone()),
        }
    }

    fn get_range(&self) -> TokenRange {
        match &self {
            InferredType::FromVariable(_, token_range) => *token_range,
            InferredType::FunctionReturn(_, token_range) => *token_range,
            InferredType::Static(_, token_range) => *token_range,
            InferredType::OneOf(inferred_types) => {
                inferred_types.iter().map(|i| i.get_range()).sum()
            }
            InferredType::Void(token_range) => *token_range,
            InferredType::DownstreamError(_, range) => *range,
        }
    }
}

pub struct VirtualVariable {
    name: String,
    inferred: InferredType,
    meta: mir::Metadata,
}

pub struct VirtualFunctionSignature {
    name: String,
    return_type: mir::TypeKind,
    parameter_types: Vec<mir::TypeKind>,
    metadata: mir::Metadata,
}

pub enum VirtualStorageError {
    KeyAlreadyExists(String),
}

pub struct VirtualStorage<T> {
    storage: HashMap<String, Vec<T>>,
}

impl<T> VirtualStorage<T> {
    fn set(&mut self, name: String, value: T) -> Result<(), VirtualStorageError> {
        let result = if let Some(list) = self.storage.get_mut(&name) {
            list.push(value);
            Err(VirtualStorageError::KeyAlreadyExists(name.clone()))
        } else {
            self.storage.insert(name, vec![value]);
            Ok(())
        };

        result
    }

    fn get(&self, name: &String) -> Option<&T> {
        if let Some(list) = self.storage.get(name) {
            list.first()
        } else {
            None
        }
    }
}

impl<T> Default for VirtualStorage<T> {
    fn default() -> Self {
        Self {
            storage: Default::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum IntoMIRError {
    DuplicateVariable(String, TokenRange),
    DuplicateFunction(String, TokenRange),
    VariableNotDefined(String, TokenRange),
    FunctionNotDefined(String, TokenRange),
    DownstreamError(TokenRange),
    ConflictingType(TokenRange),
    VoidType(TokenRange),
}

pub struct VirtualScope {
    variables: VirtualStorage<VirtualVariable>,
    functions: VirtualStorage<VirtualFunctionSignature>,
}

impl VirtualScope {
    pub fn set_var(&mut self, variable: VirtualVariable) -> Result<(), IntoMIRError> {
        let range = variable.meta.range;
        match self.variables.set(variable.name.clone(), variable) {
            Ok(_) => Ok(()),
            Err(VirtualStorageError::KeyAlreadyExists(n)) => {
                Err(IntoMIRError::DuplicateVariable(n, range))
            }
        }
    }

    pub fn set_fun(&mut self, function: VirtualFunctionSignature) -> Result<(), IntoMIRError> {
        let range = function.metadata.range;
        match self.functions.set(function.name.clone(), function) {
            Ok(_) => Ok(()),
            Err(VirtualStorageError::KeyAlreadyExists(n)) => {
                Err(IntoMIRError::DuplicateVariable(n, range))
            }
        }
    }

    pub fn get_var(&self, name: &String) -> Option<&InferredType> {
        self.variables.get(name).map(|v| &v.inferred)
    }

    pub fn get_return_type(&self, name: &String) -> Option<&mir::TypeKind> {
        self.functions.get(name).map(|v| &v.return_type)
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

#[derive(Debug)]
pub struct State {
    errors: Vec<IntoMIRError>,
    fatal: bool,
}

impl State {
    fn note<T: std::fmt::Debug>(
        &mut self,
        value: Result<T, IntoMIRError>,
    ) -> Result<T, IntoMIRError> {
        dbg!(&value);
        if let Err(e) = &value {
            self.errors.push(e.clone());
        }
        value
    }

    fn err<T>(&mut self, error: IntoMIRError) -> Result<T, IntoMIRError> {
        self.errors.push(error.clone());
        Err(error)
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            errors: Default::default(),
            fatal: false,
        }
    }
}

impl ast::Module {
    pub fn process(&self) -> mir::Module {
        let mut state = State::default();
        let mut scope = VirtualScope::default();

        for stmt in &self.top_level_statements {
            match stmt {
                FunctionDefinition(ast::FunctionDefinition(signature, _, range)) => {
                    state.note(scope.set_fun(VirtualFunctionSignature {
                        name: signature.name.clone(),
                        return_type: signature.return_type.into(),
                        parameter_types: signature.args.iter().map(|p| p.1.into()).collect(),
                        metadata: (*range).into(),
                    }));
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
                        state.note(scope.set_var(VirtualVariable {
                            name: name.clone(),
                            inferred: InferredType::Static((*ptype).into(), *range),
                            meta: ptype.1.into(),
                        }));
                    }

                    dbg!(&signature);

                    if let Some(mir_block) = block.process(&mut state, &mut scope) {
                        let def = mir::FunctionDefinition {
                            name: signature.name.clone(),
                            parameters: signature
                                .args
                                .iter()
                                .cloned()
                                .map(|p| (p.0, p.1.into()))
                                .collect(),
                            kind: mir::FunctionDefinitionKind::Local(mir_block, (*range).into()),
                        };
                        functions.push(def);
                    }
                }
            }
        }

        dbg!(&state);

        // TODO do something with state here

        mir::Module {
            name: self.name.clone(),
            imports,
            functions,
        }
    }
}

impl ast::Block {
    pub fn process(&self, state: &mut State, scope: &mut VirtualScope) -> Option<mir::Block> {
        let mut mir_statements = Vec::new();

        for statement in &self.0 {
            let (kind, range): (Option<mir::StmtKind>, TokenRange) = match statement {
                ast::BlockLevelStatement::Let(s_let) => {
                    let res = s_let.1.infer_return_type().collapse(state, scope);
                    let collapsed = state.note(res);
                    let inferred = match &collapsed {
                        Ok(t) => InferredType::Static(*t, s_let.2),
                        Err(e) => InferredType::DownstreamError(e.clone(), s_let.2),
                    };
                    scope.set_var(VirtualVariable {
                        name: s_let.0.clone(),
                        inferred,
                        meta: s_let.2.into(),
                    });

                    (
                        collapsed.ok().and_then(|t| {
                            s_let.1.process(state, scope).map(|e| {
                                mir::StmtKind::Let(
                                    mir::VariableReference(t, s_let.0.clone(), s_let.2.into()),
                                    e,
                                )
                            })
                        }),
                        s_let.2,
                    )
                }
                ast::BlockLevelStatement::Import(_) => todo!(),
                ast::BlockLevelStatement::Expression(e) => (
                    e.process(state, scope).map(|e| StmtKind::Expression(e)),
                    e.1,
                ),
                ast::BlockLevelStatement::Return(_, e) => (
                    e.process(state, scope).map(|e| StmtKind::Expression(e)),
                    e.1,
                ),
            };

            if let Some(kind) = kind {
                mir_statements.push(mir::Statement(kind, range.into()));
            } else {
                state.fatal = true;
            }
        }

        let return_expression = if let Some(r) = &self.1 {
            if let Some(expr) = r.1.process(state, scope) {
                Some((r.0.into(), Box::new(expr)))
            } else {
                None?
            }
        } else {
            None
        };

        Some(mir::Block {
            statements: mir_statements,
            return_expression,
            meta: self.2.into(),
        })
    }

    fn infer_return_type(&self) -> InferredType {
        self.1
            .as_ref()
            .map(|(_, expr)| expr.infer_return_type())
            .unwrap_or(InferredType::Void(self.2))
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
    fn process(&self, state: &mut State, scope: &mut VirtualScope) -> Option<mir::Expression> {
        let kind = match &self.0 {
            ast::ExpressionKind::VariableName(name) => {
                let ty = scope.get_var(name);
                if let Some(ty) = ty {
                    let res = ty.collapse(state, scope);
                    state
                        .note(res)
                        .map(|result| {
                            mir::ExprKind::Variable(VariableReference(
                                result,
                                name.clone(),
                                self.1.into(),
                            ))
                        })
                        .ok()
                } else {
                    state
                        .err(IntoMIRError::VariableNotDefined(
                            name.clone(),
                            self.1.into(),
                        ))
                        .ok()
                }
            }
            ast::ExpressionKind::Literal(literal) => Some(mir::ExprKind::Literal(literal.mir())),
            ast::ExpressionKind::Binop(binary_operator, lhs, rhs) => {
                let mir_lhs = lhs.process(state, scope);
                let mir_rhs = rhs.process(state, scope);
                Some(mir::ExprKind::BinOp(
                    binary_operator.mir(),
                    Box::new(mir_lhs?),
                    Box::new(mir_rhs?),
                ))
            }
            ast::ExpressionKind::FunctionCall(fn_call_expr) => {
                if let Some(fn_type) = scope.get_return_type(&fn_call_expr.0).cloned() {
                    let parameters = all_ok(
                        fn_call_expr
                            .1
                            .iter()
                            .map(|e| {
                                e.process(state, scope)
                                    .ok_or(IntoMIRError::DownstreamError(self.1.into()))
                            })
                            .collect(),
                    );
                    if let Some(parameters) = parameters {
                        Some(mir::ExprKind::FunctionCall(mir::FunctionCall {
                            name: fn_call_expr.0.clone(),
                            return_type: fn_type,
                            parameters,
                        }))
                    } else {
                        None
                    }
                } else {
                    state
                        .err(IntoMIRError::FunctionNotDefined(
                            fn_call_expr.0.clone(),
                            self.1,
                        ))
                        .ok()
                }
            }
            ast::ExpressionKind::BlockExpr(block) => {
                block.process(state, scope).map(|b| mir::ExprKind::Block(b))
            }
            ast::ExpressionKind::IfExpr(if_expression) => {
                let cond = if_expression.0.process(state, scope);
                let then_block = if_expression.1.process(state, scope);
                let else_block = if let Some(el) = &if_expression.2 {
                    Some(el.process(state, scope)?)
                } else {
                    None
                };
                Some(mir::ExprKind::If(mir::IfExpression(
                    Box::new(cond?),
                    then_block?,
                    else_block,
                )))
            }
        };

        kind.map(|k| mir::Expression(k, self.1.into()))
    }

    fn infer_return_type(&self) -> InferredType {
        use ast::ExpressionKind::*;
        match &self.0 {
            VariableName(name) => InferredType::FromVariable(name.clone(), self.1),
            Literal(lit) => InferredType::Static(lit.mir().as_type(), self.1),
            Binop(_, lhs, rhs) => {
                InferredType::OneOf(vec![lhs.infer_return_type(), rhs.infer_return_type()])
            }
            FunctionCall(fncall) => InferredType::FunctionReturn(fncall.0.clone(), self.1),
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
