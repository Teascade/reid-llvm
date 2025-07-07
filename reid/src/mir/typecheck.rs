use std::{collections::HashMap, convert::Infallible, iter};

/// This module contains code relevant to doing a type checking pass on the MIR.
use crate::{mir::*, util::try_all};
use TypeKind::*;
use VagueType::*;

#[derive(Debug, Clone)]
pub struct Error {
    metadata: Metadata,
    kind: ErrorKind,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error at {}: {}", self.metadata, self.kind)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum ErrorKind {
    #[error("NULL error, should never occur!")]
    Null,
    #[error("Type is vague: {0}")]
    TypeIsVague(VagueType),
    #[error("Types {0} and {1} are incompatible")]
    TypesIncompatible(TypeKind, TypeKind),
    #[error("Variable not defined: {0}")]
    VariableNotDefined(String),
    #[error("Function not defined: {0}")]
    FunctionNotDefined(String),
    #[error("Type is vague: {0}")]
    ReturnTypeMismatch(TypeKind, TypeKind),
}

#[derive(Clone)]
pub struct TypeStorage<T>(HashMap<String, T>);

impl<T> Default for TypeStorage<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: Collapsable> TypeStorage<T> {
    fn set(&mut self, key: String, value: T) -> Result<T, ErrorKind> {
        if let Some(inner) = self.0.get(&key) {
            match value.collapse_into(inner) {
                Ok(collapsed) => {
                    self.0.insert(key, collapsed.clone());
                    Ok(collapsed)
                }
                Err(e) => Err(e),
            }
        } else {
            self.0.insert(key, value.clone());
            Ok(value)
        }
    }

    fn get(&self, key: &String) -> Option<&T> {
        self.0.get(key)
    }
}

#[derive(Debug)]
pub struct State {
    pub errors: Vec<Error>,
}

impl State {
    fn new() -> State {
        State {
            errors: Default::default(),
        }
    }

    fn or_else<T: Into<Metadata> + Clone + Copy>(
        &mut self,
        result: Result<TypeKind, ErrorKind>,
        default: TypeKind,
        meta: T,
    ) -> TypeKind {
        match result {
            Ok(t) => t,
            Err(e) => {
                self.errors.push(Error {
                    metadata: meta.into(),
                    kind: e,
                });
                default
            }
        }
    }

    fn ok<T: Into<Metadata> + Clone + Copy, U>(&mut self, result: Result<U, ErrorKind>, meta: T) {
        if let Err(e) = result {
            self.errors.push(Error {
                metadata: meta.into(),
                kind: e,
            });
        }
    }
}

#[derive(Clone, Default)]
pub struct Scope {
    function_returns: TypeStorage<ScopeFunction>,
    variables: TypeStorage<TypeKind>,
}

#[derive(Clone)]
pub struct ScopeFunction {
    ret: TypeKind,
    params: Vec<TypeKind>,
}

impl Scope {
    fn inner(&self) -> Scope {
        Scope {
            function_returns: self.function_returns.clone(),
            variables: self.variables.clone(),
        }
    }
}

#[derive(Clone)]
pub enum Inferred {
    Type(TypeKind),
    Unresolved(u32),
}

impl Module {
    pub fn typecheck(&mut self) -> State {
        let mut state = State::new();
        let mut scope = Scope::default();

        for function in &self.functions {
            state.ok(
                scope.function_returns.set(
                    function.name.clone(),
                    ScopeFunction {
                        ret: function.return_type,
                        params: function.parameters.iter().map(|v| v.1).collect(),
                    },
                ),
                function.signature(),
            );
        }

        for function in &mut self.functions {
            let res = function.typecheck(&mut state, &mut scope);
            state.ok(res, function.block_meta());
        }

        state
    }
}

impl FunctionDefinition {
    fn typecheck(&mut self, state: &mut State, scope: &mut Scope) -> Result<TypeKind, ErrorKind> {
        for param in &self.parameters {
            let param_t = state.or_else(param.1.assert_known(), Vague(Unknown), self.signature());
            state.ok(
                scope.variables.set(param.0.clone(), param_t),
                self.signature(),
            );
        }

        let return_type = self.return_type.clone();
        let inferred = match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                block.typecheck(state, scope, Some(return_type), Some(return_type))
            }
            FunctionDefinitionKind::Extern => Ok(Vague(Unknown)),
        };

        match inferred {
            Ok(t) => try_collapse(&return_type, &t)
                .or(Err(ErrorKind::ReturnTypeMismatch(return_type, t))),
            Err(e) => Ok(state.or_else(Err(e), return_type, self.block_meta())),
        }
    }
}

impl Block {
    fn typecheck(
        &mut self,
        state: &mut State,
        scope: &mut Scope,
        soft_hint: Option<TypeKind>,
        hard_hint: Option<TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        let mut scope = scope.inner();

        for statement in &mut self.statements {
            match &mut statement.0 {
                StmtKind::Let(variable_reference, expression) => {
                    let res = expression.typecheck(
                        state,
                        &mut scope,
                        Some(variable_reference.0),
                        hard_hint,
                    );

                    // If expression resolution itself was erronous, resolve as
                    // Unknown.
                    let res = state.or_else(res, Vague(Unknown), expression.1);

                    // Make sure the expression and variable type really is the same
                    let res_t = state.or_else(
                        res.collapse_into(&variable_reference.0),
                        Vague(Unknown),
                        variable_reference.2 + expression.1,
                    );

                    // Make sure expression/variable type is NOT vague anymore
                    let res_t =
                        state.or_else(res_t.assert_known(), Vague(Unknown), variable_reference.2);

                    // Update typing to be more accurate
                    variable_reference.0 = res_t;

                    // Variable might already be defined, note error
                    state.ok(
                        scope
                            .variables
                            .set(variable_reference.1.clone(), variable_reference.0),
                        variable_reference.2,
                    );
                }
                StmtKind::Import(_) => todo!(),
                StmtKind::Expression(expression) => {
                    let res = expression.typecheck(state, &mut scope, soft_hint, hard_hint);
                    state.ok(res, expression.1);
                }
            }
        }

        if let Some((_, expr)) = &mut self.return_expression {
            let res = expr.typecheck(state, &mut scope, hard_hint, hard_hint);
            Ok(state.or_else(res, Vague(Unknown), expr.1))
        } else {
            Ok(Void)
        }
    }
}

impl Expression {
    fn typecheck(
        &mut self,
        state: &mut State,
        scope: &mut Scope,
        soft_hint: Option<TypeKind>,
        hard_hint: Option<TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var_ref) => {
                let existing = state.or_else(
                    scope
                        .variables
                        .get(&var_ref.1)
                        .copied()
                        .ok_or(ErrorKind::VariableNotDefined(var_ref.1.clone())),
                    Vague(Unknown),
                    var_ref.2,
                );

                // Update typing to be more accurate
                var_ref.0 = state.or_else(
                    var_ref.0.collapse_into(&existing),
                    Vague(Unknown),
                    var_ref.2,
                );

                Ok(var_ref.0)
            }
            ExprKind::Literal(literal) => {
                *literal = literal.try_coerce(soft_hint)?;
                Ok(literal.as_type())
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                // TODO make sure lhs and rhs can actually do this binary
                // operation once relevant
                let lhs_res = lhs.typecheck(state, scope, soft_hint, hard_hint); // TODO
                let lhs_type = state.or_else(lhs_res, Vague(Unknown), lhs.1);
                let rhs_res = rhs.typecheck(state, scope, Some(lhs_type), hard_hint); // TODO
                let rhs_type = state.or_else(rhs_res, Vague(Unknown), rhs.1);
                let res = lhs_type.binop_type(&op, &rhs_type)?;
                Ok(res)
            }
            ExprKind::FunctionCall(function_call) => {
                let true_function = scope
                    .function_returns
                    .get(&function_call.name)
                    .cloned()
                    .ok_or(ErrorKind::FunctionNotDefined(function_call.name.clone()));

                if let Ok(f) = true_function {
                    if function_call.parameters.len() != f.params.len() {
                        state.ok::<_, Infallible>(Err(ErrorKind::Null), self.1);
                    }

                    let true_params_iter = f.params.into_iter().chain(iter::repeat(Vague(Unknown)));

                    for (param, true_param_t) in
                        function_call.parameters.iter_mut().zip(true_params_iter)
                    {
                        let param_res =
                            param.typecheck(state, scope, Some(true_param_t), hard_hint);
                        let param_t = state.or_else(param_res, Vague(Unknown), param.1);
                        state.ok(param_t.collapse_into(&true_param_t), param.1);
                    }

                    // Make sure function return type is the same as the claimed
                    // return type
                    let ret_t = try_collapse(&f.ret, &function_call.return_type)?;
                    // Update typing to be more accurate
                    function_call.return_type = ret_t;
                    Ok(ret_t)
                } else {
                    Ok(function_call.return_type)
                }
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                // TODO make sure cond_res is Boolean here
                let cond_res = cond.typecheck(state, scope, Some(Bool), hard_hint);
                let cond_t = state.or_else(cond_res, Vague(Unknown), cond.1);
                state.ok(cond_t.collapse_into(&Bool), cond.1);

                let lhs_res = lhs.typecheck(state, scope, soft_hint, hard_hint);
                let lhs_type = state.or_else(lhs_res, Vague(Unknown), lhs.meta);
                let rhs_type = if let Some(rhs) = rhs {
                    let res = rhs.typecheck(state, scope, soft_hint, hard_hint);
                    state.or_else(res, Vague(Unknown), rhs.meta)
                } else {
                    Vague(Unknown)
                };
                lhs_type.collapse_into(&rhs_type)
            }
            ExprKind::Block(block) => block.typecheck(state, scope, soft_hint, hard_hint),
        }
    }
}

impl Literal {
    fn try_coerce(self, hint: Option<TypeKind>) -> Result<Self, ErrorKind> {
        if let Some(hint) = hint {
            use Literal as L;
            use VagueLiteral as VagueL;
            Ok(match (self, hint) {
                (L::I32(_), I32) => self,
                (L::I16(_), I16) => self,
                (L::Vague(VagueL::Number(v)), I32) => L::I32(v as i32),
                (L::Vague(VagueL::Number(v)), I16) => L::I16(v as i16),
                _ => Err(ErrorKind::TypesIncompatible(self.as_type(), hint))?,
            })
        } else {
            Ok(self)
        }
    }
}

impl TypeKind {
    fn assert_known(&self) -> Result<TypeKind, ErrorKind> {
        self.is_known().map_err(ErrorKind::TypeIsVague)
    }

    fn binop_type(&self, op: &BinaryOperator, other: &TypeKind) -> Result<TypeKind, ErrorKind> {
        let res = self.collapse_into(other)?;
        Ok(match op {
            BinaryOperator::Add => res,
            BinaryOperator::Minus => res,
            BinaryOperator::Mult => res,
            BinaryOperator::And => res,
            BinaryOperator::Logic(_) => Bool,
        })
    }
}

fn try_collapse(lhs: &TypeKind, rhs: &TypeKind) -> Result<TypeKind, ErrorKind> {
    lhs.collapse_into(rhs)
        .or(rhs.collapse_into(lhs))
        .or(Err(ErrorKind::TypesIncompatible(*lhs, *rhs)))
}

pub trait Collapsable: Sized + Clone {
    fn collapse_into(&self, other: &Self) -> Result<Self, ErrorKind>;
}

impl Collapsable for TypeKind {
    fn collapse_into(&self, other: &TypeKind) -> Result<TypeKind, ErrorKind> {
        if self == other {
            return Ok(self.clone());
        }

        match (self, other) {
            (Vague(Unknown), other) | (other, Vague(Unknown)) => Ok(other.clone()),
            _ => Err(ErrorKind::TypesIncompatible(*self, *other)),
        }
    }
}

impl Collapsable for ScopeFunction {
    fn collapse_into(&self, other: &ScopeFunction) -> Result<ScopeFunction, ErrorKind> {
        Ok(ScopeFunction {
            ret: self.ret.collapse_into(&other.ret)?,
            params: try_all(
                self.params
                    .iter()
                    .zip(&other.params)
                    .map(|(p1, p2)| p1.collapse_into(&p2))
                    .collect(),
            )
            .map_err(|e| e.first().unwrap().clone())?,
        })
    }
}
