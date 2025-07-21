//! This module contains code relevant to doing a type checking pass on the MIR.
//! During typechecking relevant types are also coerced if possible.
use std::{collections::HashSet, convert::Infallible, iter};

use crate::{mir::*, util::try_all};
use VagueType as Vague;

use super::{
    implement::Collapsable,
    pass::{Pass, PassResult, PassState, ScopeVariable},
    typerefs::TypeRefs,
};

#[derive(thiserror::Error, Debug, Clone, PartialEq, PartialOrd)]
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
    #[error("Function already defined: {0}")]
    FunctionAlreadyDefined(String),
    #[error("Variable already defined: {0}")]
    VariableAlreadyDefined(String),
    #[error("Variable {0} is not declared as mutable")]
    VariableNotMutable(String),
    #[error("Function {0} was given {1} parameters, but {2} were expected")]
    InvalidAmountParameters(String, usize, usize),
    #[error("Unable to infer type {0}")]
    TypeNotInferrable(TypeKind),
    #[error("Expected branch type to be {0}, found {1} instead")]
    BranchTypesDiffer(TypeKind, TypeKind),
    #[error("Attempted to index a non-indexable type of {0}")]
    TriedIndexingNonIndexable(TypeKind),
    #[error("Index {0} out of bounds ({1})")]
    IndexOutOfBounds(u64, u64),
    #[error("No such type {0} could be found")]
    NoSuchType(String),
    #[error("Attempted to access field of non-struct type of {0}")]
    TriedAccessingNonStruct(TypeKind),
    #[error("No such struct-field on type {0}")]
    NoSuchField(String),
    #[error("Struct field declared twice {0}")]
    DuplicateStructField(String),
    #[error("Type declared twice {0}")]
    DuplicateTypeName(String),
    #[error("Recursive type definition: {0}.{1}")]
    RecursiveTypeDefinition(String, String),
    #[error("This type of expression can not be used for assignment")]
    InvalidSetExpression,
    #[error("Can not deref {0}, as it is not a borrow")]
    AttemptedDerefNonBorrow(String),
    #[error("Types {0} and {1} differ in mutability")]
    TypesDifferMutability(TypeKind, TypeKind),
    #[error("Cannot mutably borrow variable {0}, which is not declared as mutable")]
    ImpossibleMutableBorrow(String),
    #[error("Cannot declare variable {0} as mutable, when it's type is immutable")]
    ImpossibleMutLet(String),
    #[error("Cannot produce a negative unsigned value of type {0}!")]
    NegativeUnsignedValue(TypeKind),
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct TypeCheck<'t> {
    pub refs: &'t TypeRefs,
}

type TypecheckPassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl<'t> Pass for TypeCheck<'t> {
    type Data = ();
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: TypecheckPassState) -> PassResult {
        let mut defmap = HashMap::new();
        for typedef in &module.typedefs {
            let TypeDefinition { name, kind, meta } = &typedef;
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

            if let Some(_) = defmap.insert(&typedef.name, typedef) {
                state.ok::<_, Infallible>(
                    Err(ErrorKind::DuplicateTypeName(name.clone())),
                    meta.clone(),
                );
            }
        }

        let seen = HashSet::new();
        for typedef in defmap.values() {
            let mut curr = seen.clone();
            curr.insert(typedef.name.clone());
            check_typedefs_for_recursion(&defmap, typedef, HashSet::new(), &mut state);
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
                if let TypeKind::CustomType(name) = field_ty {
                    if seen.contains(name) {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::RecursiveTypeDefinition(
                                typedef.name.clone(),
                                name.clone(),
                            )),
                            typedef.meta,
                        );
                    } else {
                        seen.insert(name.clone());
                        if let Some(inner_typedef) = defmap.get(name) {
                            check_typedefs_for_recursion(defmap, inner_typedef, seen.clone(), state)
                        }
                    }
                }
            }
        }
    }
}

impl FunctionDefinition {
    fn typecheck(
        &mut self,
        hints: &TypeRefs,
        state: &mut TypecheckPassState,
    ) -> Result<TypeKind, ErrorKind> {
        for param in &self.parameters {
            let param_t = state.or_else(
                param.1.assert_known(),
                TypeKind::Vague(Vague::Unknown),
                self.signature(),
            );
            let res = state
                .scope
                .variables
                .set(
                    param.0.clone(),
                    ScopeVariable {
                        ty: param_t.clone(),
                        mutable: param_t.is_mutable(),
                    },
                )
                .or(Err(ErrorKind::VariableAlreadyDefined(param.0.clone())));
            state.ok(res, self.signature());
        }

        let return_type = self.return_type.clone();
        let inferred = match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = Some(self.return_type.clone());
                block.typecheck(&mut state.inner(), &hints, Some(&return_type))
            }
            FunctionDefinitionKind::Extern(_) => {
                Ok((ReturnKind::Soft, TypeKind::Vague(Vague::Unknown)))
            }
        };

        match inferred {
            Ok(t) => return_type
                .collapse_into(&t.1)
                .or(Err(ErrorKind::ReturnTypeMismatch(return_type, t.1))),
            Err(e) => Ok(state.or_else(Err(e), return_type, self.block_meta())),
        }
    }
}

impl Block {
    fn typecheck(
        &mut self,
        state: &mut TypecheckPassState,
        typerefs: &TypeRefs,
        hint_t: Option<&TypeKind>,
    ) -> Result<(ReturnKind, TypeKind), ErrorKind> {
        let mut state = state.inner();

        let mut early_return = None;

        for statement in &mut self.statements {
            let ret = match &mut statement.0 {
                StmtKind::Let(variable_reference, mutable, expression) => {
                    // Resolve possible hint in var reference
                    let var_t_resolved = variable_reference.0.resolve_ref(&typerefs);

                    // Typecheck (and coerce) expression with said type
                    let res = expression.typecheck(&mut state, &typerefs, Some(&var_t_resolved));

                    // If expression resolution itself was erronous, resolve as
                    // Unknown and note error.
                    let res = state.or_else(res, TypeKind::Vague(Vague::Unknown), expression.1);

                    // Make sure the expression and variable type really is the same
                    let res_t = state.or_else(
                        res.collapse_into(&var_t_resolved),
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
                        let expr_res = expression.typecheck(&mut state, &typerefs, Some(&res_t));
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
                StmtKind::Set(lhs, rhs) => {
                    // Typecheck expression and coerce to variable type
                    let lhs_res = lhs.typecheck(&mut state, typerefs, None);
                    // If expression resolution itself was erronous, resolve as
                    // Unknown.
                    let lhs_ty = state.or_else(lhs_res, TypeKind::Vague(Vague::Unknown), lhs.1);

                    // Typecheck expression and coerce to variable type
                    let res = rhs.typecheck(&mut state, &typerefs, Some(&lhs_ty));

                    // If expression resolution itself was erronous, resolve as
                    // Unknown.
                    let rhs_ty = state.or_else(res, TypeKind::Vague(Vague::Unknown), rhs.1);

                    // Make sure the expression and variable type to really
                    // be the same
                    state.ok(lhs_ty.collapse_into(&rhs_ty), lhs.1 + rhs.1);

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
                StmtKind::Import(_) => todo!(), // TODO
                StmtKind::Expression(expression) => {
                    let res = expression.typecheck(&mut state, &typerefs, None);
                    state.or_else(res, TypeKind::Void, expression.1);
                    if let Ok((kind, _)) = expression.return_type(typerefs) {
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
            let res = expr.typecheck(&mut state, &typerefs, hint.as_ref());
            return Ok((
                ReturnKind::Hard,
                state.or_else(res, TypeKind::Vague(Vague::Unknown), expr.1),
            ));
        }

        if let Some((return_kind, expr)) = &mut self.return_expression {
            // Use function return type as hint if return is hard.
            let ret_hint_t = match return_kind {
                ReturnKind::Hard => state.scope.return_type_hint.clone(),
                ReturnKind::Soft => hint_t.cloned(),
            };
            let res = expr.typecheck(&mut state, &typerefs, ret_hint_t.as_ref());
            Ok((
                *return_kind,
                state.or_else(res, TypeKind::Vague(Vague::Unknown), expr.1),
            ))
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
                        TypeKind::Vague(Vague::Unknown),
                        var_ref.2,
                    )
                    .resolve_ref(typerefs);

                // Update typing to be more accurate
                var_ref.0 = state.or_else(
                    var_ref.0.resolve_ref(typerefs).collapse_into(&existing),
                    TypeKind::Vague(Vague::Unknown),
                    var_ref.2,
                );

                Ok(var_ref.0.clone())
            }
            ExprKind::Literal(literal) => {
                *literal = literal.clone().try_coerce(hint_t.cloned())?;
                Ok(literal.as_type())
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                // TODO make sure lhs and rhs can actually do this binary
                // operation once relevant
                let lhs_res = lhs.typecheck(state, &typerefs, None);
                let lhs_type = state.or_else(lhs_res, TypeKind::Vague(Vague::Unknown), lhs.1);
                let rhs_res = rhs.typecheck(state, &typerefs, Some(&lhs_type));
                let rhs_type = state.or_else(rhs_res, TypeKind::Vague(Vague::Unknown), rhs.1);

                if let Some(collapsed) = state.ok(rhs_type.collapse_into(&rhs_type), self.1) {
                    // Try to coerce both sides again with collapsed type
                    lhs.typecheck(state, &typerefs, Some(&collapsed)).ok();
                    rhs.typecheck(state, &typerefs, Some(&collapsed)).ok();
                }

                let both_t = lhs_type.collapse_into(&rhs_type)?;

                if *op == BinaryOperator::Minus && !lhs_type.signed() {
                    if let (Some(lhs_val), Some(rhs_val)) = (lhs.num_value(), rhs.num_value()) {
                        if lhs_val < rhs_val {
                            return Err(ErrorKind::NegativeUnsignedValue(lhs_type));
                        }
                    }
                }

                Ok(both_t.binop_type(op))
            }
            ExprKind::FunctionCall(function_call) => {
                let true_function = state
                    .scope
                    .function_returns
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

                    for (param, true_param_t) in
                        function_call.parameters.iter_mut().zip(true_params_iter)
                    {
                        // Typecheck every param separately
                        let param_res = param.typecheck(state, &typerefs, Some(&true_param_t));
                        let param_t =
                            state.or_else(param_res, TypeKind::Vague(Vague::Unknown), param.1);
                        state.ok(param_t.collapse_into(&true_param_t), param.1);
                    }

                    // Make sure function return type is the same as the claimed
                    // return type
                    let ret_t = f
                        .ret
                        .collapse_into(&function_call.return_type.resolve_ref(typerefs))?;
                    // Update typing to be more accurate
                    function_call.return_type = ret_t.clone();
                    Ok(ret_t.resolve_ref(typerefs))
                } else {
                    Ok(function_call.return_type.clone().resolve_ref(typerefs))
                }
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                let cond_res = cond.typecheck(state, &typerefs, Some(&TypeKind::Bool));
                let cond_t = state.or_else(cond_res, TypeKind::Vague(Vague::Unknown), cond.1);
                state.ok(cond_t.collapse_into(&TypeKind::Bool), cond.1);

                // Typecheck then/else return types and make sure they are the
                // same, if else exists.
                let then_res = lhs.typecheck(state, &typerefs, hint_t);
                let (then_ret_kind, then_ret_t) = state.or_else(
                    then_res,
                    (ReturnKind::Soft, TypeKind::Vague(Vague::Unknown)),
                    lhs.meta,
                );
                let else_ret_t = if let Some(else_block) = rhs {
                    let res = else_block.typecheck(state, &typerefs, hint_t);
                    let (else_ret_kind, else_ret_t) = state.or_else(
                        res,
                        (ReturnKind::Soft, TypeKind::Vague(Vague::Unknown)),
                        else_block.meta,
                    );

                    if else_ret_kind == ReturnKind::Hard {
                        TypeKind::Void
                    } else {
                        else_ret_t
                    }
                } else {
                    // Else return type is Void if it does not exist
                    TypeKind::Void
                };
                let then_ret_t = if then_ret_kind == ReturnKind::Hard {
                    TypeKind::Void
                } else {
                    then_ret_t
                };

                // Make sure then and else -blocks have the same return type
                let collapsed = then_ret_t
                    .collapse_into(&else_ret_t)
                    .or(Err(ErrorKind::BranchTypesDiffer(then_ret_t, else_ret_t)))?;

                if let Some(rhs) = rhs {
                    // If rhs existed, typecheck both sides to perform type
                    // coercion.
                    let lhs_res = lhs.typecheck(state, &typerefs, Some(&collapsed));
                    let rhs_res = rhs.typecheck(state, &typerefs, Some(&collapsed));
                    state.ok(lhs_res, lhs.meta);
                    state.ok(rhs_res, rhs.meta);
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
                    TypeKind::Array(type_kind, _) => &type_kind,
                    _ => t,
                });

                // Typecheck and narrow index-expression
                let idx_expr_res = idx_expr.typecheck(state, typerefs, Some(&TypeKind::U32));
                state.ok(idx_expr_res, idx_expr.1);

                // TODO it could be possible to check length against constants..

                let expr_t = expression.typecheck(state, typerefs, hint_t)?;
                match expr_t {
                    TypeKind::Array(inferred_ty, _) | TypeKind::UserPtr(inferred_ty) => {
                        let ty = state.or_else(
                            elem_ty.resolve_ref(typerefs).collapse_into(&inferred_ty),
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
                    TypeKind::Array(type_kind, _) => &type_kind,
                    _ => t,
                });

                let mut expr_result = try_all(
                    expressions
                        .iter_mut()
                        .map(|e| e.typecheck(state, typerefs, hint_t))
                        .collect(),
                );
                match &mut expr_result {
                    Ok(expr_types) => {
                        let mut iter = expr_types.iter_mut();
                        if let Some(first) = iter.next() {
                            for other in iter {
                                state.ok(first.collapse_into(other), self.1);
                            }
                            Ok(TypeKind::Array(
                                Box::new(first.clone()),
                                expressions.len() as u64,
                            ))
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
            ExprKind::Accessed(expression, type_kind, field_name) => {
                // Resolve expected type
                let expected_ty = type_kind.resolve_ref(typerefs);

                // Typecheck expression
                let expr_res = expression.typecheck(state, typerefs, Some(&expected_ty));
                let expr_ty =
                    state.or_else(expr_res, TypeKind::Vague(Vague::Unknown), expression.1);

                if let TypeKind::CustomType(struct_name) = expr_ty {
                    let struct_type = state
                        .scope
                        .get_struct_type(&struct_name)
                        .ok_or(ErrorKind::NoSuchType(struct_name.clone()))?;
                    if let Some(expr_field_ty) = struct_type.get_field_ty(&field_name) {
                        // Make sure they are the same
                        let true_ty = state.or_else(
                            expr_field_ty.collapse_into(&expected_ty),
                            TypeKind::Vague(Vague::Unknown),
                            self.1,
                        );
                        *type_kind = true_ty.clone();
                        // Update possibly resolved type
                        Ok(true_ty)
                    } else {
                        Err(ErrorKind::NoSuchField(field_name.clone()))
                    }
                } else {
                    Err(ErrorKind::TriedAccessingNonStruct(expr_ty))
                }
            }
            ExprKind::Struct(struct_name, items) => {
                let struct_def = state
                    .scope
                    .get_struct_type(struct_name)
                    .ok_or(ErrorKind::NoSuchType(struct_name.clone()))?
                    .clone();
                for (field_name, field_expr) in items {
                    // Get expected type, or error if field does not exist
                    let expected_ty = state.or_else(
                        struct_def
                            .get_field_ty(field_name)
                            .ok_or(ErrorKind::NoSuchField(format!(
                                "{}.{}",
                                struct_name, field_name
                            ))),
                        &TypeKind::Vague(VagueType::Unknown),
                        field_expr.1,
                    );

                    // Typecheck the actual expression
                    let expr_res = field_expr.typecheck(state, typerefs, Some(expected_ty));
                    let expr_ty =
                        state.or_else(expr_res, TypeKind::Vague(Vague::Unknown), field_expr.1);

                    // Make sure both are the same type, report error if not
                    state.ok(expr_ty.collapse_into(&expr_ty), field_expr.1);
                }
                Ok(TypeKind::CustomType(struct_name.clone()))
            }
            ExprKind::Borrow(var_ref, mutable) => {
                let scope_var = state.scope.variables.get(&var_ref.1).cloned();

                let existing = state
                    .or_else(
                        scope_var
                            .clone()
                            .map(|var| var.ty)
                            .ok_or(ErrorKind::VariableNotDefined(var_ref.1.clone())),
                        TypeKind::Vague(Vague::Unknown),
                        var_ref.2,
                    )
                    .resolve_ref(typerefs);

                if let Some(scope_var) = scope_var {
                    if !scope_var.mutable && *mutable {
                        return Err(ErrorKind::ImpossibleMutableBorrow(var_ref.1.clone()));
                    }
                }

                // Update typing to be more accurate
                var_ref.0 = state.or_else(
                    var_ref.0.resolve_ref(typerefs).collapse_into(&existing),
                    TypeKind::Vague(Vague::Unknown),
                    var_ref.2,
                );

                Ok(TypeKind::Borrow(Box::new(var_ref.0.clone()), *mutable))
            }
            ExprKind::Deref(var_ref) => {
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
                let TypeKind::Borrow(inner, mutable) = state.or_else(
                    var_ref.0.resolve_ref(typerefs).collapse_into(&existing),
                    TypeKind::Vague(Vague::Unknown),
                    var_ref.2,
                ) else {
                    return Err(ErrorKind::AttemptedDerefNonBorrow(var_ref.1.clone()));
                };

                var_ref.0 = TypeKind::Borrow(inner.clone(), mutable);

                Ok(*inner)
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
            Ok(match (self.clone(), hint) {
                (L::I8(_), TypeKind::I8) => self,
                (L::I16(_), TypeKind::I16) => self,
                (L::I32(_), TypeKind::I32) => self,
                (L::I64(_), TypeKind::I64) => self,
                (L::I128(_), TypeKind::I128) => self,
                (L::U8(_), TypeKind::U8) => self,
                (L::U16(_), TypeKind::U16) => self,
                (L::U32(_), TypeKind::U32) => self,
                (L::U64(_), TypeKind::U64) => self,
                (L::U128(_), TypeKind::U128) => self,
                (L::Bool(_), TypeKind::Bool) => self,
                (L::String(_), TypeKind::StringPtr) => self,
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
                (_, TypeKind::Vague(_)) => self,
                _ => Err(ErrorKind::LiteralIncompatible(self, hint.clone()))?,
            })
        } else {
            Ok(self)
        }
    }
}
