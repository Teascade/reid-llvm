//! This module contains code relevant to doing a type checking pass on the MIR.
//! During typechecking relevant types are also coerced if possible.
use std::{collections::HashSet, convert::Infallible, iter};

use crate::{mir::*, util::try_all};
use VagueType as Vague;

use super::{
    super::pass::{Pass, PassResult, ScopeVariable},
    typerefs::TypeRefs,
    ErrorKind, HintKind, TypecheckPassState,
};

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct TypeCheck<'t> {
    pub refs: &'t TypeRefs,
}

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorTypedefKind {
    #[error("locally")]
    Local,
    #[error("as an extern")]
    Extern,
    #[error("as an intrinsic")]
    Intrinsic,
}

impl<'t> Pass for TypeCheck<'t> {
    type Data = ();
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: TypecheckPassState) -> PassResult {
        let mut defmap = HashMap::new();
        for typedef in &module.typedefs {
            let TypeDefinition { name, kind, meta, .. } = &typedef;

            match kind {
                TypeDefinitionKind::Struct(StructType(fields)) => {
                    let mut fieldmap = HashMap::new();
                    for StructField(name, field_ty, field_meta) in fields {
                        if let Some(_) = fieldmap.insert(name, field_ty) {
                            state.ok::<_, Infallible>(
                                Err(ErrorKind::DuplicateStructField(name.clone())),
                                field_meta.clone(),
                            );
                        }
                    }
                }
            }

            if typedef.source_module == module.module_id || typedef.importer == Some(module.module_id) {
                if let Some(_) = defmap.insert(&typedef.name, typedef) {
                    state.ok::<_, Infallible>(Err(ErrorKind::DuplicateTypeName(name.clone())), meta.clone());
                }
            }
        }

        for typedef in defmap.values() {
            let mut seen_types = HashSet::new();
            seen_types.insert(typedef.name.clone());
            check_typedefs_for_recursion(&defmap, typedef, HashSet::new(), &mut state);
        }

        for binop in &mut module.binop_defs {
            let res = binop.typecheck(&self.refs, &mut state.inner());
            state.ok(res, binop.block_meta().unwrap_or(binop.signature()));
        }

        for (_, function) in &mut module.associated_functions {
            let res = function.typecheck(&self.refs, &mut state.inner());
            state.ok(res, function.block_meta());
        }

        for function in &mut module.functions {
            let res = function.typecheck(&self.refs, &mut state.inner());
            state.ok(res, function.block_meta());
        }
        Ok(())
    }
}

fn check_typedefs_for_recursion<'a, 'b>(
    defmap: &'b HashMap<&'a String, &'b TypeDefinition>,
    typedef: &'b TypeDefinition,
    mut seen: HashSet<String>,
    state: &mut TypecheckPassState,
) {
    match &typedef.kind {
        TypeDefinitionKind::Struct(StructType(fields)) => {
            for field_ty in fields.iter().map(|StructField(_, ty, _)| ty) {
                if let TypeKind::CustomType(CustomTypeKey(name, _)) = field_ty {
                    if seen.contains(name) {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::RecursiveTypeDefinition(typedef.name.clone(), name.clone())),
                            typedef.meta,
                        );
                    } else {
                        if let Some(inner_typedef) = defmap.get(name) {
                            let mut inner_seen = seen.clone();
                            inner_seen.insert(name.clone());
                            check_typedefs_for_recursion(defmap, inner_typedef, inner_seen.clone(), state)
                        }
                    }
                }
            }
        }
    }
}

impl BinopDefinition {
    fn typecheck(&mut self, typerefs: &TypeRefs, state: &mut TypecheckPassState) -> Result<TypeKind, ErrorKind> {
        for param in vec![&self.lhs, &self.rhs] {
            let param_t = state.or_else(
                param.ty.assert_known(state),
                TypeKind::Vague(Vague::Unknown),
                self.signature(),
            );
            let res = state
                .scope
                .variables
                .set(
                    param.name.clone(),
                    ScopeVariable {
                        ty: param_t.clone(),
                        mutable: param_t.is_mutable(),
                    },
                )
                .or(Err(ErrorKind::VariableAlreadyDefined(param.name.clone())));
            state.ok(res, self.signature());
        }

        let return_type = self.return_type.clone().assert_known(state)?;

        state.scope.return_type_hint = Some(self.return_type.clone());
        let inferred = self
            .fn_kind
            .typecheck(&typerefs, &mut state.inner(), Some(return_type.clone()));

        match inferred {
            Ok(t) => return_type
                .narrow_into(&t.1)
                .or(Err(ErrorKind::ReturnTypeMismatch(return_type, t.1))),
            Err(e) => Ok(state.or_else(Err(e), return_type, self.block_meta().unwrap_or(self.signature()))),
        }
    }
}

impl FunctionDefinition {
    fn typecheck(&mut self, typerefs: &TypeRefs, state: &mut TypecheckPassState) -> Result<TypeKind, ErrorKind> {
        for param in &self.parameters {
            let param_t = state.or_else(
                param.ty.assert_known(state),
                TypeKind::Vague(Vague::Unknown),
                self.signature(),
            );
            let res = state
                .scope
                .variables
                .set(
                    param.name.clone(),
                    ScopeVariable {
                        ty: param_t.clone(),
                        mutable: param_t.is_mutable(),
                    },
                )
                .or(Err(ErrorKind::VariableAlreadyDefined(param.name.clone())));
            state.ok(res, self.signature());
        }

        let return_type = self.return_type.clone().assert_known(state)?;
        let inferred = self.kind.typecheck(typerefs, state, Some(self.return_type.clone()));

        match inferred {
            Ok(t) => return_type
                .narrow_into(&t.1)
                .or(Err(ErrorKind::ReturnTypeMismatch(return_type, t.1))),
            Err(e) => Ok(state.or_else(Err(e), return_type, self.block_meta())),
        }
    }
}

impl FunctionDefinitionKind {
    fn typecheck(
        &mut self,
        typerefs: &TypeRefs,
        state: &mut TypecheckPassState,
        hint: Option<TypeKind>,
    ) -> Result<(ReturnKind, TypeKind), ErrorKind> {
        match self {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = hint.clone();
                block.typecheck(&mut state.inner(), &typerefs, hint.into())
            }
            FunctionDefinitionKind::Extern(_) => Ok((ReturnKind::Soft, TypeKind::Vague(Vague::Unknown))),
            FunctionDefinitionKind::Intrinsic(intrinsic) => Ok((ReturnKind::Soft, TypeKind::Vague(Vague::Unknown))),
        }
    }
}

impl Block {
    fn typecheck(
        &mut self,
        state: &mut TypecheckPassState,
        typerefs: &TypeRefs,
        hint_t: HintKind,
    ) -> Result<(ReturnKind, TypeKind), ErrorKind> {
        let mut state = state.inner();

        let mut early_return = None;

        for statement in &mut self.statements {
            let ret = match &mut statement.0 {
                StmtKind::Let(variable_reference, mutable, expression) => {
                    // Resolve possible hint in var reference
                    let var_t_resolved = state.or_else(
                        variable_reference.0.resolve_ref(&typerefs).or_default(),
                        TypeKind::Vague(VagueType::Unknown),
                        variable_reference.2,
                    );

                    // Typecheck (and coerce) expression with said type
                    let res = expression.typecheck(&mut state, &typerefs, HintKind::Coerce(var_t_resolved.clone()));
                    // If expression resolution itself was erronous, resolve as
                    // Unknown and note error.
                    let res = state.or_else(res, TypeKind::Vague(Vague::Unknown), expression.1);

                    // Make sure the expression and variable type really is the same
                    let res_t = state.or_else(
                        res.narrow_into(&var_t_resolved),
                        TypeKind::Vague(Vague::Unknown),
                        variable_reference.2 + expression.1,
                    );

                    if *mutable && !res_t.is_mutable() {
                        state.note_errors(
                            &vec![ErrorKind::ImpossibleMutLet(variable_reference.1.clone())],
                            variable_reference.2,
                        );
                    }

                    let res_t = if res_t.known().is_err() {
                        // Unable to infer variable type even from expression! Default it
                        let res_t = state.or_else(
                            res_t.or_default(),
                            TypeKind::Vague(Vague::Unknown),
                            variable_reference.2,
                        );

                        // Re-typecheck and coerce expression to default type
                        let expr_res = expression.typecheck(&mut state, &typerefs, HintKind::Coerce(res_t.clone()));
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
                        .or(Err(ErrorKind::VariableAlreadyDefined(variable_reference.1.clone())));
                    state.ok(res, variable_reference.2);
                    None
                }
                StmtKind::Set(lhs, rhs) => {
                    // Typecheck expression and coerce to variable type
                    let lhs_res = lhs.typecheck(&mut state, typerefs, HintKind::Default);
                    // If expression resolution itself was erronous, resolve as
                    // Unknown.
                    let lhs_ty = state.or_else(lhs_res, TypeKind::Vague(Vague::Unknown), lhs.1);

                    // Typecheck expression and coerce to variable type
                    let res = rhs.typecheck(&mut state, &typerefs, HintKind::Coerce(lhs_ty.clone()));

                    // If expression resolution itself was erronous, resolve as
                    // Unknown.
                    let rhs_ty = state.or_else(res, TypeKind::Vague(Vague::Unknown), rhs.1);

                    // Make sure the expression and variable type to really
                    // be the same
                    state.ok(lhs_ty.narrow_into(&rhs_ty), lhs.1 + rhs.1);

                    if let Some(named_var) = lhs.backing_var() {
                        if let Some(scope_var) = state.scope.variables.get(&named_var.1) {
                            if !scope_var.mutable {
                                state.ok::<_, Infallible>(
                                    Err(ErrorKind::VariableNotMutable(named_var.1.clone())),
                                    lhs.1,
                                );
                            }
                        }
                    } else {
                        state.ok::<_, Infallible>(Err(ErrorKind::InvalidSetExpression), lhs.1);
                    }
                    // TODO add error about variable mutability, need to check
                    // that the expression is based on a variable first though..
                    // if true {
                    //     state.ok::<_, Infallible>(
                    //         Err(ErrorKind::VariableNotMutable(variable_reference.get_name())),
                    //         variable_reference.meta,
                    //     );
                    // }

                    None
                }
                StmtKind::Import(_) => todo!(),
                StmtKind::Expression(expression) => {
                    let res = expression.typecheck(&mut state, &typerefs, HintKind::None);
                    state.or_else(res, TypeKind::Void, expression.1);
                    if let Ok((kind, _)) = expression.return_type(typerefs, state.module_id.unwrap()) {
                        Some((kind, expression))
                    } else {
                        None
                    }
                }
                StmtKind::While(WhileStatement { condition, block, meta }) => {
                    let condition_ty = condition.typecheck(&mut state, typerefs, HintKind::Coerce(TypeKind::Bool))?;
                    if condition_ty.assert_known(&state)? != TypeKind::Bool {
                        state.note_errors(&vec![ErrorKind::TypesIncompatible(condition_ty, TypeKind::Bool)], *meta);
                    }

                    block.typecheck(&mut state, typerefs, HintKind::None)?;

                    None
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
            let res = expr.typecheck(&mut state, &typerefs, hint.into());
            return Ok((
                ReturnKind::Hard,
                state.or_else(res, TypeKind::Vague(Vague::Unknown), expr.1),
            ));
        }

        if let Some((return_kind, expr)) = &mut self.return_expression {
            // Use function return type as hint if return is hard.
            let ret_hint_t = match return_kind {
                ReturnKind::Hard => state.scope.return_type_hint.clone().into(),
                ReturnKind::Soft => hint_t,
            };
            if let Some(expr) = expr {
                let res = expr.typecheck(&mut state, &typerefs, ret_hint_t.into());
                Ok((
                    *return_kind,
                    state.or_else(res, TypeKind::Vague(Vague::Unknown), expr.1),
                ))
            } else {
                Ok((*return_kind, TypeKind::Void))
            }
        } else {
            Ok((ReturnKind::Soft, TypeKind::Void))
        }
    }
}

impl Expression {
    fn typecheck(
        &mut self,
        state: &mut TypecheckPassState,
        typerefs: &TypeRefs,
        hint_t: HintKind,
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
                        TypeKind::Vague(Vague::Unknown),
                        var_ref.2,
                    )
                    .resolve_ref(typerefs);

                // Update typing to be more accurate
                var_ref.0 = state.or_else(
                    var_ref.0.resolve_ref(typerefs).narrow_into(&existing),
                    TypeKind::Vague(Vague::Unknown),
                    var_ref.2,
                );

                Ok(var_ref.0.clone())
            }
            ExprKind::Literal(literal) => {
                *literal = literal.clone().try_coerce(hint_t)?;
                Ok(literal.as_type())
            }
            ExprKind::BinOp(op, lhs, rhs, ret_ty) => {
                // First find unfiltered parameters to binop
                let lhs_res = lhs.typecheck(state, &typerefs, HintKind::None);
                let rhs_res = rhs.typecheck(state, &typerefs, HintKind::None);
                let lhs_type = state.or_else(lhs_res, TypeKind::Vague(Vague::Unknown), lhs.1);
                let rhs_type = state.or_else(rhs_res, TypeKind::Vague(Vague::Unknown), rhs.1);

                let mut expected_return_ty = ret_ty.resolve_ref(typerefs);
                if let HintKind::Coerce(hint_t) = hint_t {
                    expected_return_ty = state.or_else(
                        expected_return_ty.narrow_into(&hint_t),
                        TypeKind::Vague(VagueType::Unknown),
                        self.1,
                    );
                };

                let binops = state.scope.binops.filter(&pass::BinopKey {
                    params: (lhs_type.clone(), rhs_type.clone()),
                    operator: *op,
                });

                if let Some(binop) = binops
                    .iter()
                    .filter(|f| f.1.return_ty.narrow_into(&expected_return_ty).is_ok())
                    .map(|v| (v.1.clone()))
                    .next()
                {
                    lhs.typecheck(state, &typerefs, HintKind::Coerce(binop.hands.0.clone()))?;
                    rhs.typecheck(state, &typerefs, HintKind::Coerce(binop.hands.1.clone()))?;
                    *ret_ty = binop.narrow(&lhs_type, &rhs_type).unwrap().2;
                    Ok(ret_ty.clone())
                } else {
                    panic!()
                }
            }
            ExprKind::FunctionCall(function_call) => {
                let true_function = state
                    .scope
                    .functions
                    .get(&function_call.name)
                    .cloned()
                    .ok_or(ErrorKind::FunctionNotDefined(function_call.name.clone()));

                if let Some(f) = state.ok(true_function, self.1) {
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

                    let true_params_iter = f
                        .params
                        .into_iter()
                        .chain(iter::repeat(TypeKind::Vague(Vague::Unknown)));

                    for (param, true_param_t) in function_call.parameters.iter_mut().zip(true_params_iter) {
                        // Typecheck every param separately
                        let param_res = param.typecheck(state, &typerefs, HintKind::Coerce(true_param_t.clone()));
                        let param_t = state.or_else(param_res, TypeKind::Vague(Vague::Unknown), param.1);
                        state.ok(param_t.narrow_into(&true_param_t), param.1);
                    }

                    // Make sure function return type is the same as the claimed
                    // return type
                    let ret_t = f.ret.narrow_into(&function_call.return_type.resolve_ref(typerefs))?;
                    // Update typing to be more accurate
                    function_call.return_type = ret_t.clone();
                    Ok(ret_t.resolve_ref(typerefs))
                } else {
                    Ok(function_call.return_type.clone().resolve_ref(typerefs))
                }
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                let cond_res = cond.typecheck(state, &typerefs, HintKind::Coerce(TypeKind::Bool));
                let cond_t = state.or_else(cond_res, TypeKind::Vague(Vague::Unknown), cond.1);
                state.ok(cond_t.narrow_into(&TypeKind::Bool), cond.1);

                // Typecheck then/else return types and make sure they are the
                // same, if else exists.
                let then_res = lhs.typecheck(state, &typerefs, hint_t.clone());
                let then_ret_t = state.or_else(then_res, TypeKind::Vague(Vague::Unknown), lhs.1);
                let else_ret_t = if let Some(else_expr) = rhs.as_mut() {
                    let res = else_expr.typecheck(state, &typerefs, hint_t.clone());
                    let else_ret_t = state.or_else(res, TypeKind::Vague(Vague::Unknown), else_expr.1);

                    else_ret_t
                } else {
                    // Else return type is Void if it does not exist
                    TypeKind::Void
                };
                let then_ret_t = then_ret_t;

                // Make sure then and else -blocks have the same return type
                let collapsed = then_ret_t
                    .narrow_into(&else_ret_t)
                    .or(Err(ErrorKind::BranchTypesDiffer(then_ret_t, else_ret_t)))?;

                if let Some(rhs) = rhs.as_mut() {
                    // If rhs existed, typecheck both sides to perform type
                    // coercion.
                    let lhs_res = lhs.typecheck(state, &typerefs, HintKind::Coerce(collapsed.clone()));
                    let rhs_res = rhs.typecheck(state, &typerefs, HintKind::Coerce(collapsed.clone()));
                    state.ok(lhs_res, lhs.1);
                    state.ok(rhs_res, rhs.1);
                }

                Ok(collapsed)
            }
            ExprKind::Block(block) => match block.typecheck(state, &typerefs, hint_t) {
                Ok((ReturnKind::Hard, _)) => Ok(TypeKind::Void),
                Ok((_, ty)) => Ok(ty),
                Err(e) => Err(e),
            },
            ExprKind::Indexed(expression, elem_ty, idx_expr) => {
                // Try to unwrap hint type from array if possible
                let hint_t = hint_t.map(|t| match t {
                    TypeKind::Array(type_kind, _) => *type_kind.clone(),
                    _ => t.clone(),
                });

                // Typecheck and narrow index-expression
                let idx_expr_res =
                    idx_expr.typecheck(state, typerefs, HintKind::Coerce(TypeKind::Vague(VagueType::Integer)));
                state.ok(idx_expr_res, idx_expr.1);

                // TODO it could be possible to check length against constants..

                let expr_t = expression.typecheck(state, typerefs, hint_t)?;
                match expr_t {
                    TypeKind::Array(inferred_ty, _) | TypeKind::UserPtr(inferred_ty) => {
                        let ty = state.or_else(
                            elem_ty.resolve_ref(typerefs).narrow_into(&inferred_ty),
                            TypeKind::Vague(Vague::Unknown),
                            self.1,
                        );
                        *elem_ty = ty.clone();
                        Ok(ty)
                    }
                    _ => Err(ErrorKind::TriedIndexingNonIndexable(expr_t)),
                }
            }
            ExprKind::Array(expressions) => {
                // Try to unwrap hint type from array if possible
                let hint_t = hint_t.map(|t| match t {
                    TypeKind::Array(type_kind, _) => *type_kind.clone(),
                    _ => t.clone(),
                });

                let mut expr_result = try_all(
                    expressions
                        .iter_mut()
                        .map(|e| e.typecheck(state, typerefs, hint_t.clone()))
                        .collect(),
                );
                match &mut expr_result {
                    Ok(expr_types) => {
                        let mut iter = expr_types.iter_mut();
                        if let Some(first) = iter.next() {
                            for other in iter {
                                state.ok(first.narrow_into(other), self.1);
                            }
                            Ok(TypeKind::Array(Box::new(first.clone()), expressions.len() as u64))
                        } else {
                            Ok(TypeKind::Array(Box::new(TypeKind::Void), 0))
                        }
                    }
                    Err(errors) => {
                        state.note_errors(errors, self.1);
                        Ok(TypeKind::Array(
                            Box::new(TypeKind::Vague(Vague::Unknown)),
                            expressions.len() as u64,
                        ))
                    }
                }
            }
            ExprKind::Accessed(expression, type_kind, field_name, _) => {
                // Resolve expected type
                let expected_ty = type_kind.resolve_ref(typerefs);

                // Typecheck expression
                let expr_res = expression.typecheck(state, typerefs, HintKind::Coerce(expected_ty.clone()));
                let expr_ty = state.or_else(expr_res, TypeKind::Vague(Vague::Unknown), expression.1);

                if let TypeKind::CustomType(key) = expr_ty {
                    let struct_type = state
                        .scope
                        .get_struct_type(&key)
                        .ok_or(ErrorKind::NoSuchType(key.0.clone(), key.1))?;
                    if let Some(expr_field_ty) = struct_type.get_field_ty(&field_name) {
                        // Make sure they are the same
                        let true_ty = state.or_else(
                            expr_field_ty.narrow_into(&expected_ty),
                            TypeKind::Vague(Vague::Unknown),
                            self.1,
                        );
                        *type_kind = true_ty.clone();
                        // Update possibly resolved type
                        Ok(true_ty)
                    } else {
                        Err(ErrorKind::NoSuchField(key.0.clone()))
                    }
                } else {
                    Err(ErrorKind::TriedAccessingNonStruct(expr_ty))
                }
            }
            ExprKind::Struct(struct_name, items) => {
                let type_key = CustomTypeKey(struct_name.clone(), state.module_id.unwrap());
                let struct_def = state
                    .scope
                    .get_struct_type(&type_key)
                    .ok_or(ErrorKind::NoSuchType(struct_name.clone(), type_key.1))?
                    .clone();

                let mut expected_fields = if let Some(struct_ty) = state.scope.get_struct_type(&type_key) {
                    struct_ty.0.iter().map(|f| f.0.clone()).collect()
                } else {
                    HashSet::new()
                };

                for (field_name, field_expr, _) in items {
                    // Get expected type, or error if field does not exist
                    let expected_ty = state.or_else(
                        struct_def
                            .get_field_ty(field_name)
                            .ok_or(ErrorKind::NoSuchField(format!("{}.{}", struct_name, field_name))),
                        &TypeKind::Vague(VagueType::Unknown),
                        field_expr.1,
                    );
                    expected_fields.remove(field_name);

                    // Typecheck the actual expression
                    let expr_res = field_expr.typecheck(state, typerefs, HintKind::Coerce(expected_ty.clone()));
                    let expr_ty = state.or_else(expr_res, TypeKind::Vague(Vague::Unknown), field_expr.1);

                    // Make sure both are the same type, report error if not
                    state.ok(expr_ty.narrow_into(&expr_ty), field_expr.1);
                }

                state.note_errors(
                    &expected_fields
                        .into_iter()
                        .map(|v| ErrorKind::MissingStructField(v))
                        .collect(),
                    self.1,
                );

                Ok(TypeKind::CustomType(type_key))
            }
            ExprKind::Borrow(expr, mutable) => {
                let hint_t = if let HintKind::Coerce(hint_t) = hint_t {
                    if let TypeKind::Borrow(inner, _) = hint_t {
                        HintKind::Coerce(*inner)
                    } else {
                        return Err(ErrorKind::TypesIncompatible(
                            hint_t,
                            TypeKind::Borrow(Box::new(TypeKind::Vague(VagueType::Unknown)), *mutable),
                        ));
                    }
                } else {
                    hint_t
                };

                let expr_ty = expr.typecheck(state, typerefs, hint_t)?.resolve_ref(typerefs);

                if let Some(backing_var) = expr.backing_var() {
                    if let Some(scope_var) = state.scope.variables.get(&backing_var.1) {
                        if !scope_var.mutable && *mutable {
                            return Err(ErrorKind::ImpossibleMutableBorrow(backing_var.1.clone()));
                        }
                    } else {
                        return Err(ErrorKind::VariableNotDefined(backing_var.1.clone()));
                    }
                } else {
                    return Err(ErrorKind::ImpossibleBorrow);
                }

                Ok(TypeKind::Borrow(Box::new(expr_ty.clone()), *mutable))
            }
            ExprKind::Deref(expr) => {
                let hint_t = if let HintKind::Coerce(hint_t) = hint_t {
                    HintKind::Coerce(TypeKind::Borrow(Box::new(hint_t), false))
                } else {
                    hint_t
                };

                let expr_ty = expr.typecheck(state, typerefs, hint_t)?.resolve_ref(typerefs);

                // Update typing to be more accurate
                let TypeKind::Borrow(inner, _) = expr_ty else {
                    return Err(ErrorKind::AttemptedDerefNonBorrow(expr_ty.clone()));
                };

                Ok(*inner)
            }
            ExprKind::CastTo(expression, type_kind) => {
                let expr = expression.typecheck(state, typerefs, HintKind::Default)?;
                expr.resolve_ref(typerefs).cast_into(type_kind)
            }
            ExprKind::AssociatedFunctionCall(type_kind, function_call) => {
                *type_kind = type_kind.or_default()?;
                let true_function = state
                    .scope
                    .get_associated_function(&pass::AssociatedFunctionKey(
                        type_kind.clone(),
                        function_call.name.clone(),
                    ))
                    .ok_or(ErrorKind::AssocFunctionNotDefined(
                        function_call.name.clone(),
                        type_kind.clone(),
                    ));

                if let Some(f) = state.ok(true_function, function_call.meta) {
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

                    let true_params_iter = f
                        .params
                        .into_iter()
                        .chain(iter::repeat(TypeKind::Vague(Vague::Unknown)));

                    for (param, true_param_t) in function_call.parameters.iter_mut().zip(true_params_iter) {
                        // Typecheck every param separately
                        let param_res = param.typecheck(state, &typerefs, HintKind::Coerce(true_param_t.clone()));
                        let param_t = state.or_else(param_res, TypeKind::Vague(Vague::Unknown), param.1);
                        state.ok(param_t.narrow_into(&true_param_t), param.1);
                    }

                    // Make sure function return type is the same as the claimed
                    // return type
                    let ret_t = f.ret.narrow_into(&function_call.return_type.resolve_ref(typerefs))?;
                    // Update typing to be more accurate
                    function_call.return_type = ret_t.clone();
                    Ok(ret_t.resolve_ref(typerefs))
                } else {
                    Ok(function_call.return_type.clone().resolve_ref(typerefs))
                }
            }
            ExprKind::GlobalRef(global_value, type_kind) => Ok(self
                .return_type(typerefs, state.scope.module_id.unwrap())
                .map(|r| r.1)
                .unwrap()),
        }
    }
}

impl Literal {
    /// Try to coerce this literal, ie. convert it to a more specific type in
    /// regards to the given hint if any.
    fn try_coerce(self, hint: HintKind) -> Result<Self, ErrorKind> {
        use Literal as L;
        use VagueLiteral as VagueL;

        if let HintKind::Coerce(hint) = &hint {
            if *hint == self.as_type() {
                return Ok(self);
            }

            Ok(match (self.clone(), hint) {
                // TODO make sure that v is actually able to fit in the
                // requested type
                (L::Vague(VagueL::Number(v)), TypeKind::I8) => L::I8(v as i8),
                (L::Vague(VagueL::Number(v)), TypeKind::I16) => L::I16(v as i16),
                (L::Vague(VagueL::Number(v)), TypeKind::I32) => L::I32(v as i32),
                (L::Vague(VagueL::Number(v)), TypeKind::I64) => L::I64(v as i64),
                (L::Vague(VagueL::Number(v)), TypeKind::I128) => L::I128(v as i128),
                (L::Vague(VagueL::Number(v)), TypeKind::U8) => L::U8(v as u8),
                (L::Vague(VagueL::Number(v)), TypeKind::U16) => L::U16(v as u16),
                (L::Vague(VagueL::Number(v)), TypeKind::U32) => L::U32(v as u32),
                (L::Vague(VagueL::Number(v)), TypeKind::U64) => L::U64(v as u64),
                (L::Vague(VagueL::Number(v)), TypeKind::U128) => L::U128(v as u128),
                (L::Vague(VagueL::Number(v)), TypeKind::F16) => L::F16(v as f32),
                (L::Vague(VagueL::Number(v)), TypeKind::F32) => L::F32(v as f32),
                (L::Vague(VagueL::Number(v)), TypeKind::F32B) => L::F32B(v as f32),
                (L::Vague(VagueL::Number(v)), TypeKind::F64) => L::F64(v as f64),
                (L::Vague(VagueL::Number(v)), TypeKind::F80) => L::F80(v as f64),
                (L::Vague(VagueL::Number(v)), TypeKind::F128) => L::F128(v as f64),
                (L::Vague(VagueL::Number(v)), TypeKind::F128PPC) => L::F128PPC(v as f64),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F16) => L::F16(v as f32),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F32) => L::F32(v as f32),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F32B) => L::F32B(v as f32),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F64) => L::F64(v as f64),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F80) => L::F80(v as f64),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F128) => L::F128(v as f64),
                (L::Vague(VagueL::Decimal(v)), TypeKind::F128PPC) => L::F128PPC(v as f64),
                (_, TypeKind::Vague(_)) => self,
                _ => Err(ErrorKind::LiteralIncompatible(self, hint.clone()))?,
            })
        } else if hint == HintKind::Default {
            match self {
                Literal::Vague(vague) => match vague {
                    VagueLiteral::Number(val) => Ok(L::I32(val as i32)),
                    VagueLiteral::Decimal(val) => Ok(L::F32(val as f32)),
                },
                _ => Ok(self),
            }
        } else {
            Ok(self)
        }
    }
}

impl TypeKind {}
