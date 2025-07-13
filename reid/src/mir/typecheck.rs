//! This module contains code relevant to doing a type checking pass on the MIR.
//! During typechecking relevant types are also coerced if possible.
use std::{convert::Infallible, iter};

use crate::{mir::*, util::try_all};
use TypeKind::*;
use VagueType::*;

use super::{
    pass::{Pass, PassState, ScopeFunction, ScopeVariable},
    typerefs::TypeRefs,
    types::ReturnType,
};

#[derive(thiserror::Error, Debug, Clone)]
pub enum ErrorKind {
    #[error("NULL error, should never occur!")]
    Null,
    #[error("Type is vague: {0}")]
    TypeIsVague(VagueType),
    #[error("Literal {0} can not be coerced to type {1}")]
    LiteralIncompatible(Literal, TypeKind),
    #[error("Types {0} and {1} are incompatible")]
    TypesIncompatible(TypeKind, TypeKind),
    #[error("Variable not defined: {0}")]
    VariableNotDefined(String),
    #[error("Function not defined: {0}")]
    FunctionNotDefined(String),
    #[error("Expected a return type of {0}, got {1} instead")]
    ReturnTypeMismatch(TypeKind, TypeKind),
    #[error("Function not defined: {0}")]
    FunctionAlreadyDefined(String),
    #[error("Variable not defined: {0}")]
    VariableAlreadyDefined(String),
    #[error("Variable not mutable: {0}")]
    VariableNotMutable(String),
    #[error("Function {0} was given {1} parameters, but {2} were expected")]
    InvalidAmountParameters(String, usize, usize),
    #[error("Unable to infer type {0}")]
    TypeNotInferrable(TypeKind),
    #[error("Expected branch type to be {0}, found {1} instead")]
    BranchTypesDiffer(TypeKind, TypeKind),
    #[error("Attempted to index a non-array type of {0}")]
    TriedIndexingNonArray(TypeKind),
    #[error("Index {0} out of bounds ({1})")]
    IndexOutOfBounds(u64, u64),
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct TypeCheck<'t> {
    pub refs: &'t TypeRefs,
}

impl<'t> Pass for TypeCheck<'t> {
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: PassState<ErrorKind>) {
        for function in &mut module.functions {
            let res = function.typecheck(&self.refs, &mut state);
            state.ok(res, function.block_meta());
        }
    }
}

impl FunctionDefinition {
    fn typecheck(
        &mut self,
        hints: &TypeRefs,
        state: &mut PassState<ErrorKind>,
    ) -> Result<TypeKind, ErrorKind> {
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
                state.scope.return_type_hint = Some(self.return_type.clone());
                block.typecheck(state, &hints, Some(&return_type))
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
    fn typecheck(
        &mut self,
        state: &mut PassState<ErrorKind>,
        hints: &TypeRefs,
        hint_t: Option<&TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        let mut state = state.inner();

        let mut early_return = None;

        for statement in &mut self.statements {
            let ret = match &mut statement.0 {
                StmtKind::Let(variable_reference, mutable, expression) => {
                    // Resolve possible hint in var reference
                    let var_t_resolved = variable_reference.0.resolve_hinted(&hints);

                    // Typecheck (and coerce) expression with said type
                    let res = expression.typecheck(&mut state, &hints, Some(&var_t_resolved));

                    // If expression resolution itself was erronous, resolve as
                    // Unknown and note error.
                    let res = state.or_else(res, Vague(Unknown), expression.1);

                    // Make sure the expression and variable type really is the same
                    let res_t = state.or_else(
                        res.collapse_into(&var_t_resolved),
                        Vague(Unknown),
                        variable_reference.2 + expression.1,
                    );

                    let res_t = if res_t.known().is_err() {
                        // Unable to infer variable type even from expression! Default it
                        let res_t =
                            state.or_else(res_t.or_default(), Vague(Unknown), variable_reference.2);

                        // Re-typecheck and coerce expression to default type
                        let expr_res = expression.typecheck(&mut state, &hints, Some(&res_t));
                        state.ok(expr_res, expression.1);

                        res_t
                    } else {
                        res_t
                    };

                    // Update typing
                    variable_reference.0 = res_t;

                    // Variable might already be defined, note error
                    let res = state
                        .scope
                        .variables
                        .set(
                            variable_reference.1.clone(),
                            ScopeVariable {
                                ty: variable_reference.0.clone(),
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
                    todo!("Re-think how set needs to work with arrays")
                    // if let Some(var) = state.scope.variables.get(&variable_reference.1).cloned() {
                    //     // Typecheck expression and coerce to variable type
                    //     let res = expression.typecheck(&mut state, &hints, Some(&var.ty));

                    //     // If expression resolution itself was erronous, resolve as
                    //     // Unknown.
                    //     let expr_ty = state.or_else(res, Vague(Unknown), expression.1);

                    //     // Make sure the expression and variable type to really
                    //     // be the same
                    //     let res_t = state.or_else(
                    //         expr_ty.collapse_into(&variable_reference.0.resolve_hinted(&hints)),
                    //         Vague(Unknown),
                    //         variable_reference.2 + expression.1,
                    //     );

                    //     // Update typing to be more accurate
                    //     variable_reference.0 = res_t;

                    //     if !var.mutable {
                    //         state.ok::<_, Infallible>(
                    //             Err(ErrorKind::VariableNotMutable(variable_reference.1.clone())),
                    //             variable_reference.2,
                    //         );
                    //     }

                    //     None
                    // } else {
                    //     state.ok::<_, Infallible>(
                    //         Err(ErrorKind::VariableNotDefined(variable_reference.1.clone())),
                    //         variable_reference.2,
                    //     );
                    //     None
                    // }
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
            let hint = state.scope.return_type_hint.clone();
            let res = expr.typecheck(&mut state, &hints, hint.as_ref());
            return Ok(state.or_else(res, Vague(Unknown), expr.1));
        }

        if let Some((return_kind, expr)) = &mut self.return_expression {
            // Use function return type as hint if return is hard.
            let ret_hint_t = match return_kind {
                ReturnKind::Hard => state.scope.return_type_hint.clone(),
                ReturnKind::Soft => hint_t.cloned(),
            };
            let res = expr.typecheck(&mut state, &hints, ret_hint_t.as_ref());
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
        hints: &TypeRefs,
        hint_t: Option<&TypeKind>,
    ) -> Result<TypeKind, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var_ref) => {
                let existing = state
                    .or_else(
                        state
                            .scope
                            .variables
                            .get(&var_ref.1)
                            .map(|var| &var.ty)
                            .cloned()
                            .ok_or(ErrorKind::VariableNotDefined(var_ref.1.clone())),
                        Vague(Unknown),
                        var_ref.2,
                    )
                    .resolve_hinted(hints);

                // Update typing to be more accurate
                var_ref.0 = state.or_else(
                    var_ref.0.resolve_hinted(hints).collapse_into(&existing),
                    Vague(Unknown),
                    var_ref.2,
                );

                Ok(var_ref.0.clone())
            }
            ExprKind::Literal(literal) => {
                *literal = literal.try_coerce(hint_t.cloned())?;
                Ok(literal.as_type())
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                // TODO make sure lhs and rhs can actually do this binary
                // operation once relevant
                let lhs_res = lhs.typecheck(state, &hints, None);
                let lhs_type = state.or_else(lhs_res, Vague(Unknown), lhs.1);
                let rhs_res = rhs.typecheck(state, &hints, Some(&lhs_type));
                let rhs_type = state.or_else(rhs_res, Vague(Unknown), rhs.1);

                if let Some(collapsed) = state.ok(rhs_type.collapse_into(&rhs_type), self.1) {
                    // Try to coerce both sides again with collapsed type
                    lhs.typecheck(state, &hints, Some(&collapsed)).ok();
                    rhs.typecheck(state, &hints, Some(&collapsed)).ok();
                }

                let both_t = lhs_type.collapse_into(&rhs_type)?;
                Ok(both_t.binop_type(op))
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
                        let param_res = param.typecheck(state, &hints, Some(&true_param_t));
                        let param_t = state.or_else(param_res, Vague(Unknown), param.1);
                        state.ok(param_t.collapse_into(&true_param_t), param.1);
                    }

                    // Make sure function return type is the same as the claimed
                    // return type
                    let ret_t = f
                        .ret
                        .collapse_into(&function_call.return_type.resolve_hinted(hints))?;
                    // Update typing to be more accurate
                    function_call.return_type = ret_t.clone();
                    Ok(ret_t.resolve_hinted(hints))
                } else {
                    Ok(function_call.return_type.clone().resolve_hinted(hints))
                }
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                let cond_res = cond.typecheck(state, &hints, Some(&Bool));
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
                    // Else return type is Void if it does not exist
                    Void
                };

                // Make sure then and else -blocks have the same return type
                let collapsed = then_ret_t
                    .collapse_into(&else_ret_t)
                    .or(Err(ErrorKind::BranchTypesDiffer(then_ret_t, else_ret_t)))?;

                if let Some(rhs) = rhs {
                    // If rhs existed, typecheck both sides to perform type
                    // coercion.
                    let lhs_res = lhs.typecheck(state, &hints, Some(&collapsed));
                    let rhs_res = rhs.typecheck(state, &hints, Some(&collapsed));
                    state.ok(lhs_res, lhs.meta);
                    state.ok(rhs_res, rhs.meta);
                }

                Ok(collapsed)
            }
            ExprKind::Block(block) => block.typecheck(state, &hints, hint_t),
            ExprKind::Index(expression, elem_ty, idx) => {
                // Try to unwrap hint type from array if possible
                let hint_t = hint_t.map(|t| match t {
                    Array(type_kind, _) => &type_kind,
                    _ => t,
                });

                let expr_t = expression.typecheck(state, hints, hint_t)?;
                if let TypeKind::Array(inferred_ty, len) = expr_t {
                    if len < *idx {
                        return Err(ErrorKind::IndexOutOfBounds(*idx, len));
                    }
                    let ty = state.or_else(
                        elem_ty.resolve_hinted(hints).collapse_into(&inferred_ty),
                        TypeKind::Vague(Unknown),
                        self.1,
                    );
                    *elem_ty = ty.clone();
                    Ok(ty)
                } else {
                    Err(ErrorKind::TriedIndexingNonArray(expr_t))
                }
            }
            ExprKind::Array(expressions) => {
                // Try to unwrap hint type from array if possible
                let hint_t = hint_t.map(|t| match t {
                    Array(type_kind, _) => &type_kind,
                    _ => t,
                });

                let mut expr_result = try_all(
                    expressions
                        .iter_mut()
                        .map(|e| e.typecheck(state, hints, hint_t))
                        .collect(),
                );
                match &mut expr_result {
                    Ok(expr_types) => {
                        let mut iter = expr_types.iter_mut();
                        if let Some(first) = iter.next() {
                            for other in iter {
                                state.ok(first.collapse_into(other), self.1);
                            }
                            Ok(Array(Box::new(first.clone()), expressions.len() as u64))
                        } else {
                            Ok(Array(Box::new(Void), 0))
                        }
                    }
                    Err(errors) => {
                        state.note_errors(errors, self.1);
                        Ok(Array(Box::new(Vague(Unknown)), expressions.len() as u64))
                    }
                }
            }
        }
    }
}

impl Literal {
    /// Try to coerce this literal, ie. convert it to a more specific type in
    /// regards to the given hint if any.
    fn try_coerce(self, hint: Option<TypeKind>) -> Result<Self, ErrorKind> {
        if let Some(hint) = &hint {
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
                // TODO make sure that v is actually able to fit in the
                // requested type
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
                (_, Vague(_)) => self,
                _ => Err(ErrorKind::LiteralIncompatible(self, hint.clone()))?,
            })
        } else {
            Ok(self)
        }
    }
}

impl TypeKind {
    /// Assert that a type is already known and not vague. Return said type or
    /// error.
    pub fn assert_known(&self) -> Result<TypeKind, ErrorKind> {
        self.known().map_err(ErrorKind::TypeIsVague)
    }

    /// Try to collapse a type on itself producing a default type if one exists,
    /// Error if not.
    fn or_default(&self) -> Result<TypeKind, ErrorKind> {
        match self {
            Vague(vague_type) => match &vague_type {
                Unknown => Err(ErrorKind::TypeIsVague(*vague_type)),
                Number => Ok(TypeKind::I32),
                TypeRef(_) => panic!("Hinted default!"),
            },
            _ => Ok(self.clone()),
        }
    }

    fn resolve_hinted(&self, hints: &TypeRefs) -> TypeKind {
        let resolved = match self {
            Vague(TypeRef(idx)) => hints.retrieve_type(*idx).unwrap(),
            _ => self.clone(),
        };
        match resolved {
            Array(t, len) => Array(Box::new(t.resolve_hinted(hints)), len),
            _ => resolved,
        }
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
                I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 => Ok(other.clone()),
                _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
            },
            (Vague(Unknown), other) | (other, Vague(Unknown)) => Ok(other.clone()),
            _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
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
