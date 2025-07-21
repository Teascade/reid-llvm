//! Type Inference is a pass where all of the potentially vague types are went
//! through, stored in an intermediary storage [`TypeRefs`], and then the types
//! in MIR are changed to [`TypeKind::TypeRef`]s with the correct ID. This MIR
//! must then be passed through TypeCheck with the same [`TypeRefs`] in order to
//! place the correct types from the IDs and check that there are no issues.

use std::{convert::Infallible, iter};

use crate::{mir::TypeKind, util::try_all};

use super::{
    implement::pick_return,
    pass::{Pass, PassResult, PassState},
    typecheck::ErrorKind,
    typerefs::{ScopeTypeRefs, TypeRef, TypeRefs},
    Block, ExprKind, Expression, FunctionDefinition, FunctionDefinitionKind, IfExpression, Module,
    ReturnKind, StmtKind,
    TypeKind::*,
    VagueType::*,
};

/// Struct used to implement Type Inference, where an intermediary
/// TypeRefs-struct is used as a helper to go through the modules and change
/// types while inferring.
pub struct TypeInference<'t> {
    pub refs: &'t TypeRefs,
}

type TypeInferencePassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl<'t> Pass for TypeInference<'t> {
    type Data = ();
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: TypeInferencePassState) -> PassResult {
        for function in &mut module.functions {
            let res = function.infer_types(&self.refs, &mut state.inner());
            state.ok(res, function.block_meta());
        }
        Ok(())
    }
}

impl FunctionDefinition {
    fn infer_types(
        &mut self,
        type_refs: &TypeRefs,
        state: &mut TypeInferencePassState,
    ) -> Result<(), ErrorKind> {
        let scope_hints = ScopeTypeRefs::from(type_refs);
        for param in &self.parameters {
            let param_t = state.or_else(param.1.assert_known(), Vague(Unknown), self.signature());
            let res = scope_hints
                .new_var(param.0.clone(), false, &param_t)
                .or(Err(ErrorKind::VariableAlreadyDefined(param.0.clone())));
            state.ok(res, self.signature());
        }

        match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = Some(self.return_type.clone());

                // Infer block return type
                let ret_res = block.infer_types(state, &scope_hints);

                // Narrow block type to declared function type
                if let Some(mut ret_ty) = state.ok(ret_res.map(|(_, ty)| ty), self.block_meta()) {
                    ret_ty.narrow(&scope_hints.from_type(&self.return_type).unwrap());
                }
            }
            FunctionDefinitionKind::Extern(_) => {}
        };

        Ok(())
    }
}

impl Block {
    fn infer_types<'s>(
        &mut self,
        state: &mut TypeInferencePassState,
        outer_refs: &'s ScopeTypeRefs,
    ) -> Result<(ReturnKind, TypeRef<'s>), ErrorKind> {
        let mut state = state.inner();
        let inner_refs = outer_refs.inner();

        for statement in &mut self.statements {
            match &mut statement.0 {
                StmtKind::Let(var, mutable, expr) => {
                    // Get the TypeRef for this variable declaration
                    let mut var_ref =
                        state.ok(inner_refs.new_var(var.1.clone(), *mutable, &var.0), var.2);

                    // If ok, update the MIR type to this TypeRef
                    if let Some(var_ref) = &var_ref {
                        var.0 = var_ref.as_type();
                    }

                    // Infer hints for the expression itself
                    let inferred = expr.infer_types(&mut state, &inner_refs);
                    let mut expr_ty_ref = state.ok(inferred, expr.1);

                    // Try to narrow the variable type declaration with the
                    // expression
                    if let (Some(var_ref), Some(expr_ty_ref)) =
                        (var_ref.as_mut(), expr_ty_ref.as_mut())
                    {
                        var_ref.narrow(&expr_ty_ref);
                    }
                }
                StmtKind::Set(lhs, rhs) => {
                    // Infer hints for the expression itself
                    let lhs_infer = lhs.infer_types(&mut state, &inner_refs);
                    let lhs_ref = state.ok(lhs_infer, lhs.1);

                    // Infer hints for the expression itself
                    let rhs_infer = rhs.infer_types(&mut state, &inner_refs);
                    let rhs_ref = state.ok(rhs_infer, rhs.1);

                    // Try to narrow the lhs with rhs
                    if let (Some(mut lhs_ref), Some(rhs_ref)) = (lhs_ref, rhs_ref) {
                        lhs_ref.narrow(&rhs_ref);
                    }
                }
                StmtKind::Import(_) => panic!(),
                StmtKind::Expression(expr) => {
                    let expr_res = expr.infer_types(&mut state, &inner_refs);
                    state.ok(expr_res, expr.1);
                }
            };
        }

        // If there is a return expression, infer it's type
        if let Some(ret_expr) = &mut self.return_expression {
            let ret_res = ret_expr.1.infer_types(&mut state, &inner_refs);
            state.ok(ret_res, ret_expr.1 .1);
        }

        // Fetch the declared return type
        let (kind, ty) = self
            .return_type(inner_refs.types)
            .ok()
            .unwrap_or((ReturnKind::Soft, Void));
        let mut ret_type_ref = outer_refs.from_type(&ty).unwrap();

        // Narow return type to declared type if hard return
        if kind == ReturnKind::Hard {
            if let Some(hint) = &state.scope.return_type_hint {
                ret_type_ref.narrow(&mut outer_refs.from_type(&hint).unwrap());
            }
        }

        Ok((kind, ret_type_ref))
    }
}

impl Expression {
    fn infer_types<'s>(
        &mut self,
        state: &mut TypeInferencePassState,
        type_refs: &'s ScopeTypeRefs<'s>,
    ) -> Result<TypeRef<'s>, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var) => {
                // Find variable type
                let type_ref = type_refs
                    .find_var(&var.1)
                    .map(|(_, hint)| hint)
                    .ok_or(ErrorKind::VariableNotDefined(var.1.clone()));

                // Update MIR type to TypeRef if found
                if let Ok(hint) = &type_ref {
                    var.0 = hint.as_type()
                }

                type_ref
            }
            ExprKind::Literal(literal) => Ok(type_refs.from_type(&literal.as_type()).unwrap()),
            ExprKind::BinOp(op, lhs, rhs) => {
                // Infer LHS and RHS, and return binop type
                let mut lhs_ref = lhs.infer_types(state, type_refs)?;
                let mut rhs_ref = rhs.infer_types(state, type_refs)?;
                type_refs
                    .binop(op, &mut lhs_ref, &mut rhs_ref)
                    .ok_or(ErrorKind::TypesIncompatible(
                        lhs_ref.as_type(),
                        rhs_ref.as_type(),
                    ))
            }
            ExprKind::FunctionCall(function_call) => {
                // Get function definition and types
                let fn_call = state
                    .scope
                    .function_returns
                    .get(&function_call.name)
                    .ok_or(ErrorKind::FunctionNotDefined(function_call.name.clone()))?
                    .clone();

                // Infer param expression types and narrow them to the
                // expected function parameters (or Unknown types if too
                // many were provided)
                let true_params_iter = fn_call.params.iter().chain(iter::repeat(&Vague(Unknown)));

                for (param_expr, param_t) in
                    function_call.parameters.iter_mut().zip(true_params_iter)
                {
                    let expr_res = param_expr.infer_types(state, type_refs);
                    if let Some(mut param_ref) = state.ok(expr_res, param_expr.1) {
                        param_ref.narrow(&mut type_refs.from_type(param_t).unwrap());
                    }
                }

                // Provide function return type
                Ok(type_refs.from_type(&fn_call.ret).unwrap())
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                // Infer condition type
                let cond_res = cond.infer_types(state, type_refs);
                let cond_hints = state.ok(cond_res, cond.1);

                // Try to narrow condition type to boolean
                if let Some(mut cond_hints) = cond_hints {
                    cond_hints.narrow(&mut type_refs.from_type(&Bool).unwrap());
                }

                // Infer LHS return type
                let lhs_res = lhs.infer_types(state, type_refs);
                let lhs_hints = state.ok(lhs_res, cond.1);

                if let Some(rhs) = rhs {
                    // Infer RHS return type
                    let rhs_res = rhs.infer_types(state, type_refs);
                    let rhs_hints = state.ok(rhs_res, cond.1);

                    // Narrow LHS to the same type as RHS and return it's return type
                    if let (Some(mut lhs_hints), Some(mut rhs_hints)) = (lhs_hints, rhs_hints) {
                        lhs_hints.1.narrow(&mut rhs_hints.1);
                        Ok(pick_return(lhs_hints, rhs_hints).1)
                    } else {
                        // Failed to retrieve types from either
                        Ok(type_refs.from_type(&Vague(Unknown)).unwrap())
                    }
                } else {
                    // Return LHS return type
                    if let Some((_, type_ref)) = lhs_hints {
                        Ok(type_ref)
                    } else {
                        Ok(type_refs.from_type(&Vague(Unknown)).unwrap())
                    }
                }
            }
            ExprKind::Block(block) => {
                let block_ref = block.infer_types(state, type_refs)?;
                match block_ref.0 {
                    ReturnKind::Hard => Ok(type_refs.from_type(&Void).unwrap()),
                    ReturnKind::Soft => Ok(block_ref.1),
                }
            }
            ExprKind::Indexed(expression, index_ty, idx_expr) => {
                let expr_ty = expression.infer_types(state, type_refs)?;

                // Infer and narrow types used for indexing
                let mut idx_ty = idx_expr.infer_types(state, type_refs)?;
                idx_ty.narrow(&type_refs.from_type(&U32).unwrap());

                // Check that the resolved type is at least an array, no
                // need for further resolution.
                let kind = expr_ty.resolve_weak().unwrap();
                match kind {
                    Array(type_kind, _) | UserPtr(type_kind) => {
                        let elem_ty = type_refs.from_type(&type_kind).unwrap();
                        *index_ty = elem_ty.as_type().clone();
                        Ok(elem_ty)
                    }
                    _ => Err(ErrorKind::TriedIndexingNonIndexable(kind)),
                }
            }
            ExprKind::Array(expressions) => {
                let mut expr_types_result = try_all(
                    expressions
                        .iter_mut()
                        .map(|e| (*e).infer_types(state, type_refs))
                        .collect(),
                );
                match &mut expr_types_result {
                    Ok(expr_types) => {
                        let mut iter = expr_types.iter_mut();
                        if let Some(first) = iter.next() {
                            while let Some(other) = iter.next() {
                                first.narrow(other);
                            }

                            Ok(type_refs
                                .from_type(&Array(
                                    Box::new(first.as_type()),
                                    expressions.len() as u64,
                                ))
                                .unwrap())
                        } else {
                            Ok(type_refs
                                .from_type(&Array(Box::new(TypeKind::Void), 0))
                                .unwrap())
                        }
                    }
                    Err(errors) => {
                        state.note_errors(errors, self.1);
                        Ok(type_refs
                            .from_type(&TypeKind::Array(
                                Box::new(TypeKind::Vague(Unknown)),
                                expressions.len() as u64,
                            ))
                            .unwrap())
                    }
                }
            }
            ExprKind::Accessed(expression, type_kind, field_name) => {
                let expr_ty = expression.infer_types(state, type_refs)?;

                // Check that the resolved type is at least a struct, no
                // need for further resolution.
                let kind = expr_ty.resolve_weak().unwrap();
                match kind {
                    CustomType(name) => {
                        let struct_ty = state
                            .scope
                            .get_struct_type(&name)
                            .ok_or(ErrorKind::NoSuchType(name.clone()))?;
                        match struct_ty.get_field_ty(&field_name) {
                            Some(field_ty) => {
                                let mut elem_ty = type_refs.from_type(&type_kind).unwrap();
                                elem_ty.narrow(&type_refs.from_type(&field_ty).unwrap());
                                *type_kind = elem_ty.as_type().clone();
                                Ok(elem_ty)
                            }
                            None => Err(ErrorKind::NoSuchField(field_name.clone())),
                        }
                    }
                    _ => Err(ErrorKind::TriedAccessingNonStruct(kind)),
                }
            }
            ExprKind::Struct(struct_name, fields) => {
                let expected_struct_ty = state
                    .scope
                    .get_struct_type(&struct_name)
                    .ok_or(ErrorKind::NoSuchType(struct_name.clone()))?
                    .clone();
                for field in fields {
                    if let Some(expected_field_ty) = expected_struct_ty.get_field_ty(&field.0) {
                        let field_ty = field.1.infer_types(state, type_refs);
                        if let Some(mut field_ty) = state.ok(field_ty, field.1 .1) {
                            field_ty.narrow(&type_refs.from_type(&expected_field_ty).unwrap());
                        }
                    } else {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::NoSuchField(format!(
                                "{}.{}",
                                struct_name, field.0
                            ))),
                            field.1 .1,
                        );
                    }
                }
                Ok(type_refs
                    .from_type(&TypeKind::CustomType(struct_name.clone()))
                    .unwrap())
            }
            ExprKind::Borrow(var, mutable) => {
                // Find variable type
                let type_ref = type_refs
                    .find_var(&var.1)
                    .map(|(_, hint)| hint)
                    .ok_or(ErrorKind::VariableNotDefined(var.1.clone()));

                // Update MIR type to TypeRef if found
                if let Ok(hint) = &type_ref {
                    var.0 = hint.as_type();
                }

                Ok(type_refs
                    .from_type(&TypeKind::Borrow(Box::new(var.0.clone()), *mutable))
                    .unwrap())
            }
            ExprKind::Deref(var) => {
                // Find variable type
                let type_ref = type_refs
                    .find_var(&var.1)
                    .map(|(_, hint)| hint)
                    .ok_or(ErrorKind::VariableNotDefined(var.1.clone()));

                // Update MIR type to TypeRef if found
                if let Ok(hint) = &type_ref {
                    var.0 = hint.as_type();
                }

                match &var.0.resolve_weak(type_refs.types) {
                    Borrow(type_kind, _) => Ok(type_refs.from_type(&type_kind).unwrap()),
                    _ => Err(ErrorKind::AttemptedDerefNonBorrow(var.1.clone())),
                }
            }
            ExprKind::CastTo(expression, type_kind) => todo!(),
        }
    }
}
