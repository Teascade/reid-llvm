//! This module contains code relevant to doing a type checking pass on the MIR.
//! During typechecking relevant types are also coerced if possible.
use std::{
    cell::RefCell, collections::HashMap, convert::Infallible, iter, marker::PhantomData,
    thread::scope,
};

use crate::{mir::*, util::try_all};
use TypeKind::*;
use VagueType::*;

use super::{
    pass::{Pass, PassState, ScopeFunction, ScopeVariable, Storage},
    scopehints::{ScopeHints, TypeHints, TypeRef},
    types::{pick_return, ReturnType},
};

#[derive(thiserror::Error, Debug, Clone)]
pub enum ErrorKind {
    #[error("NULL error, should never occur!")]
    Null,
    #[error("Type is vague: {0}")]
    TypeIsVague(VagueType),
    #[error("Can not coerce {0} to vague type {1}")]
    HintIsVague(TypeKind, VagueType),
    #[error("Literal {0} can not be coerced to type {1}")]
    LiteralIncompatible(Literal, TypeKind),
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
    #[error("Variable not mutable: {0}")]
    VariableNotMutable(String),
    #[error("Function {0} was given {1} parameters, but {2} were expected")]
    InvalidAmountParameters(String, usize, usize),
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct TypeCheck;

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
                .set(
                    param.0.clone(),
                    ScopeVariable {
                        ty: param_t,
                        mutable: false,
                    },
                )
                .or(Err(ErrorKind::VariableAlreadyDefined(param.0.clone())));
            state.ok(res, self.signature());
        }

        let return_type = self.return_type.clone();
        let inferred = match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = Some(self.return_type);

                let types = TypeHints::default();
                let hints = ScopeHints::from(&types);
                if let Ok(_) = block.infer_hints(state, &hints) {
                    dbg!(&block, &hints);
                    // block.typecheck(state, &hints, Some(return_type))
                    Ok(Vague(Unknown))
                } else {
                    Ok(Vague(Unknown))
                }
            }
            FunctionDefinitionKind::Extern => Ok(Vague(Unknown)),
        };

        match inferred {
            Ok(t) => return_type
                .collapse_into(&t)
                .or(Err(ErrorKind::ReturnTypeMismatch(return_type, t))),
            Err(e) => Ok(state.or_else(Err(e), return_type, self.block_meta())),
        }
    }
}

impl Block {
    fn infer_hints<'s>(
        &mut self,
        state: &mut PassState<ErrorKind>,
        outer_hints: &'s ScopeHints,
    ) -> Result<(ReturnKind, TypeRef<'s>), ErrorKind> {
        let mut state = state.inner();
        let inner_hints = outer_hints.inner();

        for statement in &mut self.statements {
            match &mut statement.0 {
                StmtKind::Let(var, mutable, expr) => {
                    let mut var_ref =
                        state.ok(inner_hints.new_var(var.1.clone(), *mutable, var.0), var.2);
                    if let Some(var_ref) = &var_ref {
                        var.0 = var_ref.as_type();
                    }
                    let inferred = expr.infer_hints(&mut state, &inner_hints);
                    let mut expr_ty_ref = state.ok(inferred, expr.1);
                    if let (Some(var_ref), Some(expr_ty_ref)) =
                        (var_ref.as_mut(), expr_ty_ref.as_mut())
                    {
                        state.ok(var_ref.narrow(&expr_ty_ref), var.2 + expr.1);
                    }
                }
                StmtKind::Set(var, expr) => {
                    let var_ref = inner_hints.find_hint(&var.1);
                    dbg!(&var_ref);
                    if let Some((_, var_ref)) = &var_ref {
                        var.0 = var_ref.as_type()
                    }
                    let inferred = expr.infer_hints(&mut state, &inner_hints);
                    let expr_ty_ref = state.ok(inferred, expr.1);
                    if let (Some((_, mut var_ref)), Some(expr_ty_ref)) = (var_ref, expr_ty_ref) {
                        state.ok(var_ref.narrow(&expr_ty_ref), var.2 + expr.1);
                    }
                }
                StmtKind::Import(_) => todo!(),
                StmtKind::Expression(expr) => {
                    let expr_res = expr.infer_hints(&mut state, &inner_hints);
                    state.ok(expr_res, expr.1);
                }
            };
        }

        if let Some(ret_expr) = &mut self.return_expression {
            let ret_res = ret_expr.1.infer_hints(&mut state, &inner_hints);
            state.ok(ret_res, ret_expr.1 .1);
        }

        let (kind, ty) = self.return_type().ok().unwrap_or((ReturnKind::Soft, Void));
        let mut ret_type_ref = TypeRef::from_type(&outer_hints, ty);

        if kind == ReturnKind::Hard {
            if let Some(hint) = state.scope.return_type_hint {
                state.ok(
                    ret_type_ref.narrow(&mut TypeRef::from_type(outer_hints, hint)),
                    self.meta,
                );
            }
        }
        Ok((kind, ret_type_ref))
    }

    fn typecheck(
        &mut self,
        state: &mut PassState<ErrorKind>,
        hints: &ScopeHints,
        hint_t: Option<TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        let mut state = state.inner();
        let hints = hints.inner();

        let mut early_return = None;

        for statement in &mut self.statements {
            let ret = match &mut statement.0 {
                StmtKind::Let(variable_reference, mutable, expression) => {
                    let res = expression.typecheck(&mut state, &hints, Some(variable_reference.0));

                    // If expression resolution itself was erronous, resolve as
                    // Unknown.
                    let res = state.or_else(res, Vague(Unknown), expression.1);

                    // Make sure the expression and variable type really is the same
                    let res_t = state.or_else(
                        res.collapse_into(&variable_reference.0),
                        Vague(Unknown),
                        variable_reference.2 + expression.1,
                    );

                    let res_t = if res_t.known().is_err() {
                        // Unable to infer variable type even from expression! Default it
                        let res_t =
                            state.or_else(res_t.or_default(), Vague(Unknown), variable_reference.2);

                        // Re-typecheck and coerce expression to default type
                        let expr_res = expression.typecheck(&mut state, &hints, Some(res_t));
                        state.ok(expr_res, expression.1);

                        res_t
                    } else {
                        res_t
                    };

                    // Update typing to be more accurate
                    variable_reference.0 = res_t;

                    // Variable might already be defined, note error
                    let res = state
                        .scope
                        .variables
                        .set(
                            variable_reference.1.clone(),
                            ScopeVariable {
                                ty: variable_reference.0,
                                mutable: *mutable,
                            },
                        )
                        .or(Err(ErrorKind::VariableAlreadyDefined(
                            variable_reference.1.clone(),
                        )));
                    state.ok(res, variable_reference.2);
                    None
                }
                StmtKind::Set(variable_reference, expression) => {
                    if let Some(var) = state.scope.variables.get(&variable_reference.1).cloned() {
                        // Typecheck expression and coerce to variable type
                        let res = expression.typecheck(&mut state, &hints, Some(var.ty));

                        // If expression resolution itself was erronous, resolve as
                        // Unknown.
                        let expr_ty = state.or_else(res, Vague(Unknown), expression.1);

                        // Make sure the expression and variable type to really
                        // be the same
                        let res_t = state.or_else(
                            expr_ty.collapse_into(&variable_reference.0),
                            Vague(Unknown),
                            variable_reference.2 + expression.1,
                        );

                        // Update typing to be more accurate
                        variable_reference.0 = res_t;

                        if !var.mutable {
                            state.ok::<_, Infallible>(
                                Err(ErrorKind::VariableNotMutable(variable_reference.1.clone())),
                                variable_reference.2,
                            );
                        }

                        None
                    } else {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::VariableNotDefined(variable_reference.1.clone())),
                            variable_reference.2,
                        );
                        None
                    }
                }
                StmtKind::Import(_) => todo!(), // TODO
                StmtKind::Expression(expression) => {
                    let res = expression.typecheck(&mut state, &hints, None);
                    state.or_else(res, Void, expression.1);
                    if let Ok((kind, _)) = expression.return_type() {
                        Some((kind, expression))
                    } else {
                        None
                    }
                }
            };

            if let Some((ReturnKind::Hard, _)) = ret {
                early_return = early_return.or(ret);
            }
        }

        // TODO should actually probably prune all instructions after this one
        // as to not cause problems in codegen later (when unable to delete the
        // block)
        if let Some((ReturnKind::Hard, expr)) = early_return {
            let hint = state.scope.return_type_hint;
            let res = expr.typecheck(&mut state, &hints, hint);
            return Ok(state.or_else(res, Vague(Unknown), expr.1));
        }

        if let Some((return_kind, expr)) = &mut self.return_expression {
            // Use function return type as hint if return is hard.
            let ret_hint_t = match return_kind {
                ReturnKind::Hard => state.scope.return_type_hint,
                ReturnKind::Soft => hint_t,
            };
            let res = expr.typecheck(&mut state, &hints, ret_hint_t);
            Ok(state.or_else(res, Vague(Unknown), expr.1))
        } else {
            Ok(Void)
        }
    }
}

impl Expression {
    fn infer_hints<'s>(
        &mut self,
        state: &mut PassState<ErrorKind>,
        hints: &'s ScopeHints<'s>,
    ) -> Result<TypeRef<'s>, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var) => {
                let hint = hints
                    .find_hint(&var.1)
                    .map(|(_, hint)| hint)
                    .ok_or(ErrorKind::VariableNotDefined(var.1.clone()));
                if let Ok(hint) = &hint {
                    var.0 = hint.as_type()
                }
                Ok(TypeRef::Hint(hint?))
            }
            ExprKind::Literal(literal) => TypeRef::from_literal(hints, *literal),
            ExprKind::BinOp(op, lhs, rhs) => {
                let mut lhs_ref = lhs.infer_hints(state, hints)?;
                let mut rhs_ref = rhs.infer_hints(state, hints)?;
                hints.binop(op, &mut lhs_ref, &mut rhs_ref)
            }
            ExprKind::FunctionCall(function_call) => {
                let fn_call = state
                    .scope
                    .function_returns
                    .get(&function_call.name)
                    .ok_or(ErrorKind::FunctionNotDefined(function_call.name.clone()))?
                    .clone();

                let true_params_iter = fn_call.params.iter().chain(iter::repeat(&Vague(Unknown)));

                for (param_expr, param_t) in
                    function_call.parameters.iter_mut().zip(true_params_iter)
                {
                    let expr_res = param_expr.infer_hints(state, hints);
                    if let Some(mut param_ref) = state.ok(expr_res, param_expr.1) {
                        state.ok(
                            param_ref.narrow(&mut TypeRef::from_type(hints, *param_t)),
                            param_expr.1,
                        );
                    }
                }

                Ok(TypeRef::from_type(hints, fn_call.ret))
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                let cond_res = cond.infer_hints(state, hints);
                let cond_hints = state.ok(cond_res, cond.1);

                if let Some(mut cond_hints) = cond_hints {
                    state.ok(cond_hints.narrow(&mut TypeRef::Literal(Bool)), cond.1);
                }

                let lhs_res = lhs.infer_hints(state, hints);
                let lhs_hints = state.ok(lhs_res, cond.1);

                if let Some(rhs) = rhs {
                    let rhs_res = rhs.infer_hints(state, hints);
                    let rhs_hints = state.ok(rhs_res, cond.1);

                    if let (Some(mut lhs_hints), Some(mut rhs_hints)) = (lhs_hints, rhs_hints) {
                        state.ok(lhs_hints.1.narrow(&mut rhs_hints.1), self.1);
                        Ok(pick_return(lhs_hints, rhs_hints).1)
                    } else {
                        // Failed to retrieve types from either
                        Ok(TypeRef::from_type(hints, Vague(Unknown)))
                    }
                } else {
                    if let Some((_, type_ref)) = lhs_hints {
                        Ok(type_ref)
                    } else {
                        Ok(TypeRef::from_type(hints, Vague(Unknown)))
                    }
                }
            }
            ExprKind::Block(block) => {
                let block_ref = block.infer_hints(state, hints)?;
                match block_ref.0 {
                    ReturnKind::Hard => Ok(TypeRef::from_type(hints, Void)),
                    ReturnKind::Soft => Ok(block_ref.1),
                }
            }
        }
    }

    fn typecheck(
        &mut self,
        state: &mut PassState<ErrorKind>,
        hints: &ScopeHints,
        hint_t: Option<TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var_ref) => {
                let existing = state.or_else(
                    state
                        .scope
                        .variables
                        .get(&var_ref.1)
                        .map(|var| &var.ty)
                        .cloned()
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
                let lhs_res = lhs.typecheck(state, &hints, None);
                let lhs_type = state.or_else(lhs_res, Vague(Unknown), lhs.1);
                let rhs_res = rhs.typecheck(state, &hints, Some(lhs_type));
                let rhs_type = state.or_else(rhs_res, Vague(Unknown), rhs.1);

                if let Some(collapsed) = state.ok(rhs_type.collapse_into(&rhs_type), self.1) {
                    // Try to coerce both sides again with collapsed type
                    lhs.typecheck(state, &hints, Some(collapsed)).ok();
                    rhs.typecheck(state, &hints, Some(collapsed)).ok();
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
                    let param_len_given = function_call.parameters.len();
                    let param_len_expected = f.params.len();

                    // Check that there are the same number of parameters given
                    // as expected
                    if param_len_given != param_len_expected {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::InvalidAmountParameters(
                                function_call.name.clone(),
                                param_len_given,
                                param_len_expected,
                            )),
                            self.1,
                        );
                    }

                    let true_params_iter = f.params.into_iter().chain(iter::repeat(Vague(Unknown)));

                    for (param, true_param_t) in
                        function_call.parameters.iter_mut().zip(true_params_iter)
                    {
                        // Typecheck every param separately
                        let param_res = param.typecheck(state, &hints, Some(true_param_t));
                        let param_t = state.or_else(param_res, Vague(Unknown), param.1);
                        state.ok(param_t.collapse_into(&true_param_t), param.1);
                    }

                    // Make sure function return type is the same as the claimed
                    // return type
                    let ret_t = f.ret.collapse_into(&function_call.return_type)?;
                    // Update typing to be more accurate
                    function_call.return_type = ret_t;
                    Ok(ret_t)
                } else {
                    Ok(function_call.return_type)
                }
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                // TODO make sure cond_res is Boolean here
                let cond_res = cond.typecheck(state, &hints, Some(Bool));
                let cond_t = state.or_else(cond_res, Vague(Unknown), cond.1);
                state.ok(cond_t.collapse_into(&Bool), cond.1);

                // Typecheck then/else return types and make sure they are the
                // same, if else exists.
                let then_res = lhs.typecheck(state, &hints, hint_t);
                let then_ret_t = state.or_else(then_res, Vague(Unknown), lhs.meta);
                let else_ret_t = if let Some(else_block) = rhs {
                    let res = else_block.typecheck(state, &hints, hint_t);
                    state.or_else(res, Vague(Unknown), else_block.meta)
                } else {
                    // TODO assert that then_ret_t is Void
                    Vague(Unknown)
                };

                let collapsed = then_ret_t.collapse_into(&else_ret_t)?;
                if let Some(rhs) = rhs {
                    // If rhs existed, typecheck both sides to perform type
                    // coercion.
                    let lhs_res = lhs.typecheck(state, &hints, Some(collapsed));
                    let rhs_res = rhs.typecheck(state, &hints, Some(collapsed));
                    state.ok(lhs_res, lhs.meta);
                    state.ok(rhs_res, rhs.meta);
                }

                Ok(collapsed)
            }
            ExprKind::Block(block) => block.typecheck(state, &hints, hint_t),
        }
    }
}

impl Literal {
    /// Try to coerce this literal, ie. convert it to a more specific type in
    /// regards to the given hint if any.
    fn try_coerce(self, hint: Option<TypeKind>) -> Result<Self, ErrorKind> {
        if let Some(hint) = hint {
            use Literal as L;
            use VagueLiteral as VagueL;
            Ok(match (self, hint) {
                (L::I8(_), I8) => self,
                (L::I16(_), I16) => self,
                (L::I32(_), I32) => self,
                (L::I64(_), I64) => self,
                (L::I128(_), I128) => self,
                (L::U8(_), U8) => self,
                (L::U16(_), U16) => self,
                (L::U32(_), U32) => self,
                (L::U64(_), U64) => self,
                (L::U128(_), U128) => self,
                (L::Bool(_), Bool) => self,
                (L::Vague(VagueL::Number(v)), I8) => L::I8(v as i8),
                (L::Vague(VagueL::Number(v)), I16) => L::I16(v as i16),
                (L::Vague(VagueL::Number(v)), I32) => L::I32(v as i32),
                (L::Vague(VagueL::Number(v)), I64) => L::I64(v as i64),
                (L::Vague(VagueL::Number(v)), I128) => L::I128(v as i128),
                (L::Vague(VagueL::Number(v)), U8) => L::U8(v as u8),
                (L::Vague(VagueL::Number(v)), U16) => L::U16(v as u16),
                (L::Vague(VagueL::Number(v)), U32) => L::U32(v as u32),
                (L::Vague(VagueL::Number(v)), U64) => L::U64(v as u64),
                (L::Vague(VagueL::Number(v)), U128) => L::U128(v as u128),
                // Default type for number literal if unable to find true type.
                (L::Vague(VagueL::Number(v)), Vague(Number)) => L::I32(v as i32),
                (_, Vague(_)) => self,
                _ => Err(ErrorKind::LiteralIncompatible(self, hint))?,
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
        self.known().map_err(ErrorKind::TypeIsVague)
    }

    /// Try to collapse a type on itself producing a default type if one exists,
    /// Error if not.
    fn or_default(&self) -> Result<TypeKind, ErrorKind> {
        match self {
            Vague(vague_type) => match vague_type {
                Unknown => Err(ErrorKind::TypeIsVague(*vague_type)),
                Number => Ok(TypeKind::I32),
                Hinted(_) => panic!("Hinted default!"),
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
            BinaryOperator::Cmp(_) => Bool,
        })
    }
}

pub trait Collapsable: Sized + Clone {
    /// Try to narrow two types into one singular type. E.g. Vague(Number) and
    /// I32 could be narrowed to just I32.
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
                I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 => Ok(*other),
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
