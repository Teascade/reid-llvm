use std::{convert::Infallible, iter};

/// This module contains code relevant to doing a type checking pass on the MIR.
use crate::{mir::*, util::try_all};
use TypeKind::*;
use VagueType::*;

use super::pass::{Pass, PassState, ScopeFunction};

#[derive(thiserror::Error, Debug, Clone)]
pub enum ErrorKind {
    #[error("NULL error, should never occur!")]
    Null,
    #[error("Type is vague: {0}")]
    TypeIsVague(VagueType),
    #[error("Can not coerce {0} to vague type {1}")]
    HintIsVague(TypeKind, VagueType),
    #[error("Types {0} and {1} are incompatible")]
    TypesIncompatible(TypeKind, TypeKind),
    #[error("Variable not defined: {0}")]
    VariableNotDefined(String),
    #[error("Function not defined: {0}")]
    FunctionNotDefined(String),
    #[error("Type is vague: {0}")]
    ReturnTypeMismatch(TypeKind, TypeKind),
    #[error("Function not defined: {0}")]
    FunctionAlreadyDefined(String),
    #[error("Variable not defined: {0}")]
    VariableAlreadyDefined(String),
}

pub struct TypeCheck {}

impl Pass for TypeCheck {
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: PassState<ErrorKind>) {
        for function in &mut module.functions {
            let res = function.typecheck(&mut state);
            state.ok(res, function.block_meta());
        }
    }
}

impl FunctionDefinition {
    fn typecheck(&mut self, state: &mut PassState<ErrorKind>) -> Result<TypeKind, ErrorKind> {
        for param in &self.parameters {
            let param_t = state.or_else(param.1.assert_known(), Vague(Unknown), self.signature());
            let res = state
                .scope
                .variables
                .set(param.0.clone(), param_t)
                .or(Err(ErrorKind::VariableAlreadyDefined(param.0.clone())));
            state.ok(res, self.signature());
        }

        let return_type = self.return_type.clone();
        let inferred = match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = Some(self.return_type);
                block.typecheck(state, Some(return_type))
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
        state: &mut PassState<ErrorKind>,
        hint_t: Option<TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        let mut state = state.inner();

        for statement in &mut self.statements {
            match &mut statement.0 {
                StmtKind::Let(variable_reference, expression) => {
                    let res = expression.typecheck(&mut state, Some(variable_reference.0));

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
                        state.or_else(res_t.or_default(), Vague(Unknown), variable_reference.2);

                    // Update typing to be more accurate
                    variable_reference.0 = res_t;

                    // Variable might already be defined, note error
                    let res = state
                        .scope
                        .variables
                        .set(variable_reference.1.clone(), variable_reference.0)
                        .or(Err(ErrorKind::VariableAlreadyDefined(
                            variable_reference.1.clone(),
                        )));
                    state.ok(res, variable_reference.2);
                }
                StmtKind::Import(_) => todo!(),
                StmtKind::Expression(expression) => {
                    let res = expression.typecheck(&mut state, None);
                    state.ok(res, expression.1);
                }
            }
        }

        if let Some((return_kind, expr)) = &mut self.return_expression {
            let ret_hint_t = match return_kind {
                ReturnKind::Hard => state.scope.return_type_hint,
                ReturnKind::Soft => hint_t,
            };
            let res = expr.typecheck(&mut state, ret_hint_t);
            Ok(state.or_else(res, Vague(Unknown), expr.1))
        } else {
            Ok(Void)
        }
    }
}

impl Expression {
    fn typecheck(
        &mut self,
        state: &mut PassState<ErrorKind>,
        hint_t: Option<TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var_ref) => {
                dbg!(&state.scope);

                let existing = state.or_else(
                    state
                        .scope
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
                *literal = literal.try_coerce(hint_t)?;
                Ok(literal.as_type())
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                // TODO make sure lhs and rhs can actually do this binary
                // operation once relevant
                let lhs_res = lhs.typecheck(state, None); // TODO
                let lhs_type = state.or_else(lhs_res, Vague(Unknown), lhs.1);
                let rhs_res = rhs.typecheck(state, Some(lhs_type)); // TODO
                let rhs_type = state.or_else(rhs_res, Vague(Unknown), rhs.1);

                if let Some(collapsed) = state.ok(rhs_type.collapse_into(&rhs_type), self.1) {
                    // Try to coerce both sides again with collapsed type
                    lhs.typecheck(state, Some(collapsed)).ok();
                    rhs.typecheck(state, Some(collapsed)).ok();
                }

                let res = lhs_type.binop_type(&op, &rhs_type)?;
                Ok(res)
            }
            ExprKind::FunctionCall(function_call) => {
                let true_function = state
                    .scope
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
                        let param_res = param.typecheck(state, Some(true_param_t));
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
                let cond_res = cond.typecheck(state, Some(Bool));
                let cond_t = state.or_else(cond_res, Vague(Unknown), cond.1);
                state.ok(cond_t.collapse_into(&Bool), cond.1);

                let lhs_res = lhs.typecheck(state, hint_t);
                let lhs_type = state.or_else(lhs_res, Vague(Unknown), lhs.meta);
                let rhs_type = if let Some(rhs) = rhs {
                    let res = rhs.typecheck(state, hint_t);
                    state.or_else(res, Vague(Unknown), rhs.meta)
                } else {
                    Vague(Unknown)
                };
                lhs_type.collapse_into(&rhs_type)
            }
            ExprKind::Block(block) => block.typecheck(state, hint_t),
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
                // Default type for number literal if unable to find true type.
                (L::Vague(VagueL::Number(v)), Vague(Number)) => L::I32(v as i32),
                (_, Vague(_)) => self,
                _ => Err(ErrorKind::TypesIncompatible(self.as_type(), hint))?,
            })
        } else {
            Ok(self)
        }
    }
}

impl TypeKind {
    /// Assert that a type is already known and not vague. Return said type or
    /// error.
    fn assert_known(&self) -> Result<TypeKind, ErrorKind> {
        self.is_known().map_err(ErrorKind::TypeIsVague)
    }

    /// Try to collapse a type on itself producing a default type if one exists,
    /// Error if not.
    fn or_default(&self) -> Result<TypeKind, ErrorKind> {
        match self {
            Vague(vague_type) => match vague_type {
                Unknown => Err(ErrorKind::TypeIsVague(*vague_type)),
                Number => Ok(TypeKind::I32),
            },
            _ => Ok(*self),
        }
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
            (Vague(Number), other) | (other, Vague(Number)) => match other {
                Vague(Unknown) => Ok(Vague(Number)),
                Vague(Number) => Ok(Vague(Number)),
                I32 => Ok(I32),
                I16 => Ok(I16),
                _ => Err(ErrorKind::TypesIncompatible(*self, *other)),
            },
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
