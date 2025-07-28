//! Type Inference is a pass where all of the potentially vague types are went
//! through, stored in an intermediary storage [`TypeRefs`], and then the types
//! in MIR are changed to [`TypeKind::TypeRef`]s with the correct ID. This MIR
//! must then be passed through TypeCheck with the same [`TypeRefs`] in order to
//! place the correct types from the IDs and check that there are no issues.

use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
    iter,
};

use crate::{
    mir::{
        pass::{AssociatedFunctionKey, ScopeVariable},
        BinopDefinition, Block, CustomTypeKey, ExprKind, Expression, FunctionDefinition, FunctionDefinitionKind,
        IfExpression, Module, ReturnKind, StmtKind, TypeKind, WhileStatement,
    },
    util::try_all,
};

use super::{
    super::{
        pass::{BinopKey, Pass, PassResult},
        TypeKind::*,
        VagueType::*,
    },
    typecheck::ErrorTypedefKind,
    typerefs::{ScopeTypeRefs, TypeRef, TypeRefs},
    ErrorKind, TypecheckPassState,
};

/// Struct used to implement Type Inference, where an intermediary
/// TypeRefs-struct is used as a helper to go through the modules and change
/// types while inferring.
pub struct TypeInference<'t> {
    pub refs: &'t mut TypeRefs,
}

impl<'t> Pass for TypeInference<'t> {
    type Data = ();
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: TypecheckPassState) -> PassResult {
        let mut seen_functions = HashMap::new();
        for function in &mut module.functions {
            if let Some(kind) = seen_functions.get(&function.name) {
                state.note_errors(
                    &vec![ErrorKind::FunctionAlreadyDefined(function.name.clone(), *kind)],
                    function.signature(),
                );
            } else {
                seen_functions.insert(
                    function.name.clone(),
                    match function.kind {
                        FunctionDefinitionKind::Local(..) => ErrorTypedefKind::Local,
                        FunctionDefinitionKind::Extern(..) => ErrorTypedefKind::Extern,
                        FunctionDefinitionKind::Intrinsic(..) => ErrorTypedefKind::Intrinsic,
                    },
                );
            }
        }

        let mut seen_assoc_functions = HashMap::new();
        for (ty, function) in &mut module.associated_functions {
            if let Some(kind) = seen_assoc_functions.get(&(ty.clone(), function.name.clone())) {
                state.note_errors(
                    &vec![ErrorKind::AssocFunctionAlreadyDefined(
                        ty.clone(),
                        function.name.clone(),
                        *kind,
                    )],
                    function.signature(),
                );
            } else {
                seen_assoc_functions.insert(
                    (ty.clone(), function.name.clone()),
                    match function.kind {
                        FunctionDefinitionKind::Local(..) => ErrorTypedefKind::Local,
                        FunctionDefinitionKind::Extern(..) => ErrorTypedefKind::Extern,
                        FunctionDefinitionKind::Intrinsic(..) => ErrorTypedefKind::Intrinsic,
                    },
                );
            }
        }

        let mut seen_binops = HashSet::new();
        for binop in &module.binop_defs {
            let binop_key = BinopKey {
                params: (binop.lhs.1.clone(), binop.rhs.1.clone()),
                operator: binop.op,
            };
            if seen_binops.contains(&binop_key) || (binop.lhs == binop.rhs && binop.lhs.1.category().is_simple_maths())
            {
                state.note_errors(
                    &vec![ErrorKind::BinaryOpAlreadyDefined(
                        binop.op,
                        binop.lhs.1.clone(),
                        binop.rhs.1.clone(),
                    )],
                    binop.signature(),
                );
            } else {
                seen_binops.insert(binop_key.clone());
                self.refs
                    .binop_types
                    .set(
                        binop_key,
                        crate::mir::pass::ScopeBinopDef {
                            hands: (binop.lhs.1.clone(), binop.rhs.1.clone()),
                            operator: binop.op,
                            return_ty: binop.return_type.clone(),
                        },
                    )
                    .ok();
            }
        }

        for binop in &mut module.binop_defs {
            let res = binop.infer_types(&self.refs, &mut state.inner());
            state.ok(res, binop.block_meta().unwrap_or(binop.signature()));
        }

        for (_, function) in &mut module.associated_functions {
            let res = function.infer_types(&self.refs, &mut state.inner());
            state.ok(res, function.block_meta());
        }

        for function in &mut module.functions {
            let res = function.infer_types(&self.refs, &mut state.inner());
            state.ok(res, function.block_meta());
        }
        Ok(())
    }
}

impl BinopDefinition {
    fn infer_types(&mut self, type_refs: &TypeRefs, state: &mut TypecheckPassState) -> Result<(), ErrorKind> {
        let scope_hints = ScopeTypeRefs::from(type_refs);

        let lhs_ty = state.or_else(self.lhs.1.assert_unvague(), Vague(Unknown), self.signature());
        state.ok(
            scope_hints
                .new_var(self.lhs.0.clone(), false, &lhs_ty)
                .or(Err(ErrorKind::VariableAlreadyDefined(self.lhs.0.clone()))),
            self.signature(),
        );

        let rhs_ty = state.or_else(self.rhs.1.assert_unvague(), Vague(Unknown), self.signature());

        state.ok(
            scope_hints
                .new_var(self.rhs.0.clone(), false, &rhs_ty)
                .or(Err(ErrorKind::VariableAlreadyDefined(self.rhs.0.clone()))),
            self.signature(),
        );

        let ret_ty = self
            .fn_kind
            .infer_types(state, &scope_hints, Some(self.return_type.clone()))?;
        if let Some(mut ret_ty) = ret_ty {
            ret_ty.narrow(&scope_hints.from_type(&self.return_type).unwrap());
        }

        Ok(())
    }
}

impl FunctionDefinition {
    fn infer_types(&mut self, type_refs: &TypeRefs, state: &mut TypecheckPassState) -> Result<(), ErrorKind> {
        let scope_refs = ScopeTypeRefs::from(type_refs);
        for param in &self.parameters {
            let param_t = state.or_else(param.1.assert_unvague(), Vague(Unknown), self.signature());
            let res = scope_refs
                .new_var(param.0.clone(), false, &param_t)
                .or(Err(ErrorKind::VariableAlreadyDefined(param.0.clone())));
            state.ok(res, self.signature());
        }

        match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = Some(self.return_type.clone());

                // Infer block return type
                let ret_res = block.infer_types(state, &scope_refs);

                // Narrow block type to declared function type
                if let Some(mut ret_ty) = state.ok(ret_res.map(|(_, ty)| ty), self.block_meta()) {
                    ret_ty.narrow(&scope_refs.from_type(&self.return_type).unwrap());
                }
            }
            FunctionDefinitionKind::Extern(_) => {}
            FunctionDefinitionKind::Intrinsic(_) => {}
        };
        let return_ty = self
            .kind
            .infer_types(state, &scope_refs, Some(self.return_type.clone()));

        if let Some(Some(mut ret_ty)) = state.ok(return_ty, self.block_meta()) {
            ret_ty.narrow(&scope_refs.from_type(&self.return_type).unwrap());
        }

        Ok(())
    }
}

impl FunctionDefinitionKind {
    fn infer_types<'s>(
        &mut self,
        state: &mut TypecheckPassState,
        scope_refs: &'s ScopeTypeRefs,
        hint: Option<TypeKind>,
    ) -> Result<Option<TypeRef<'s>>, ErrorKind> {
        Ok(match self {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = hint;

                // Infer block return type
                let ret_res = block.infer_types(state, &scope_refs);

                // Narrow block type to declared function type
                Some(ret_res.map(|(_, ty)| ty)?)
            }
            FunctionDefinitionKind::Extern(_) => None,
            FunctionDefinitionKind::Intrinsic(_) => None,
        })
    }
}

impl Block {
    fn infer_types<'s>(
        &mut self,
        state: &mut TypecheckPassState,
        outer_refs: &'s ScopeTypeRefs,
    ) -> Result<(ReturnKind, TypeRef<'s>), ErrorKind> {
        let mut state = state.inner();
        let inner_refs = outer_refs.inner();

        // Keep track of variables created in this scope
        let mut scope_variables = Vec::new();

        for statement in &mut self.statements {
            match &mut statement.0 {
                StmtKind::Let(var, mutable, expr) => {
                    // Get the TypeRef for this variable declaration
                    let mut var_ref = state.ok(inner_refs.new_var(var.1.clone(), *mutable, &var.0), var.2);

                    // If ok, update the MIR type to this TypeRef
                    if let Some(var_ref) = &var_ref {
                        var.0 = var_ref.as_type();
                    }

                    state
                        .scope
                        .variables
                        .set(
                            var.1.clone(),
                            ScopeVariable {
                                ty: var.0.clone(),
                                mutable: *mutable,
                            },
                        )
                        .ok();

                    // Infer hints for the expression itself
                    let inferred = expr.infer_types(&mut state, &inner_refs);
                    let mut expr_ty_ref = state.ok(inferred, expr.1);

                    // Try to narrow the variable type declaration with the
                    // expression
                    if let (Some(var_ref), Some(expr_ty_ref)) = (var_ref.as_mut(), expr_ty_ref.as_mut()) {
                        var_ref.narrow(&expr_ty_ref);
                    }

                    // Add variable to list of tracked variables
                    scope_variables.push(var.clone());
                }
                StmtKind::Set(lhs, rhs) => {
                    // Infer hints for the expression itself
                    let lhs_infer = lhs.infer_types(&mut state, &inner_refs);
                    let lhs_ref = state.ok(lhs_infer, lhs.1);

                    // Infer hints for the expression itself
                    let rhs_infer = rhs.infer_types(&mut state, &inner_refs);
                    let rhs_ref = state.ok(rhs_infer, rhs.1);

                    // Try to narrow the lhs with rhs
                    if let (Some(mut lhs_ref), Some(mut rhs_ref)) = (lhs_ref, rhs_ref) {
                        rhs_ref.narrow(&lhs_ref);
                    }
                }
                StmtKind::Import(_) => panic!(),
                StmtKind::Expression(expr) => {
                    let expr_res = expr.infer_types(&mut state, &inner_refs);
                    state.ok(expr_res, expr.1);
                }
                StmtKind::While(WhileStatement { condition, block, .. }) => {
                    condition.infer_types(&mut state, &inner_refs)?;
                    block.infer_types(&mut state, &inner_refs)?;
                }
            };
        }

        // If there is a return expression, infer it's type
        if let Some(ret_expr) = &mut self.return_expression {
            if let Some(expr) = &mut ret_expr.1 {
                let ret_res = expr.infer_types(&mut state, &inner_refs);
                state.ok(ret_res, expr.1);
            }
        }

        // Fetch the declared return type
        let (kind, ty) = self
            .return_type(inner_refs.types, state.module_id.unwrap())
            .ok()
            .unwrap_or((ReturnKind::Soft, Void));
        let mut ret_type_ref = outer_refs.from_type(&ty).unwrap();

        // Narow return type to declared type if hard return
        if kind == ReturnKind::Hard {
            if let Some(hint) = &state.scope.return_type_hint {
                ret_type_ref.narrow(&mut outer_refs.from_type(&hint).unwrap());
            }
        }

        // if variables aren't known at this time, they will never be. Default
        // their types.
        for variable in scope_variables {
            inner_refs.try_default_deep(&variable.0);
        }

        Ok((kind, ret_type_ref))
    }
}

impl Expression {
    fn infer_types<'s>(
        &mut self,
        state: &mut TypecheckPassState,
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
            ExprKind::BinOp(op, lhs, rhs, return_ty) => {
                // Infer LHS and RHS, and return binop type
                let mut lhs_ref = lhs.infer_types(state, type_refs)?;
                let mut rhs_ref = rhs.infer_types(state, type_refs)?;

                let binops = if let (Some(lhs_ty), Some(rhs_ty)) = (lhs_ref.resolve_deep(), rhs_ref.resolve_deep()) {
                    let mut applying_binops = Vec::new();
                    for (_, binop) in state.scope.binops.iter() {
                        if binop.operator != *op {
                            continue;
                        }
                        if let Some(_) = binop.narrow(&lhs_ty, &rhs_ty) {
                            applying_binops.push(binop.clone());
                            continue;
                        }
                        if binop.operator.is_commutative() {
                            if let Some(_) = binop.narrow(&rhs_ty, &lhs_ty) {
                                applying_binops.push(binop.clone());
                                continue;
                            }
                        }
                    }
                    applying_binops
                } else {
                    Vec::new()
                };
                if binops.len() > 0 {
                    let binop = unsafe { binops.get_unchecked(0) };
                    let mut widened_lhs = binop.hands.0.clone();
                    let mut widened_rhs = binop.hands.1.clone();
                    for binop in binops.iter().skip(1) {
                        widened_lhs = widened_lhs.widen_into(&binop.hands.0);
                        widened_rhs = widened_rhs.widen_into(&binop.hands.1);
                    }
                    let binop_res = type_refs.from_binop(*op, &lhs_ref, &rhs_ref);
                    // dbg!(&return_ty);
                    // dbg!(&binop_res);
                    // dbg!(&lhs_ref, &rhs_ref, &binops, &widened_lhs, &widened_rhs);
                    lhs_ref.narrow(&type_refs.from_type(&widened_lhs).unwrap());
                    rhs_ref.narrow(&type_refs.from_type(&widened_rhs).unwrap());
                    *return_ty = binop_res.as_type();
                    Ok(binop_res)
                } else {
                    Err(ErrorKind::InvalidBinop(
                        *op,
                        lhs_ref.resolve_deep().unwrap(),
                        rhs_ref.resolve_deep().unwrap(),
                    ))
                }
            }
            ExprKind::FunctionCall(function_call) => {
                // Get function definition and types
                let fn_call = state
                    .scope
                    .functions
                    .get(&function_call.name)
                    .ok_or(ErrorKind::FunctionNotDefined(function_call.name.clone()))?
                    .clone();

                // Infer param expression types and narrow them to the
                // expected function parameters (or Unknown types if too
                // many were provided)
                let true_params_iter = fn_call.params.iter().chain(iter::repeat(&Vague(Unknown)));

                for (param_expr, param_t) in function_call.parameters.iter_mut().zip(true_params_iter) {
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

                if let Some(rhs) = rhs.as_mut() {
                    // Infer RHS return type
                    let rhs_res = rhs.infer_types(state, type_refs);
                    let rhs_hints = state.ok(rhs_res, cond.1);

                    // Narrow LHS to the same type as RHS and return it's return type
                    if let (Some(mut lhs_hints), Some(mut rhs_hints)) = (lhs_hints, rhs_hints) {
                        lhs_hints.narrow(&mut rhs_hints).ok_or(ErrorKind::TypesIncompatible(
                            lhs_hints.resolve_deep().unwrap(),
                            rhs_hints.resolve_deep().unwrap(),
                        ))
                    } else {
                        // Failed to retrieve types from either
                        Ok(type_refs.from_type(&Vague(Unknown)).unwrap())
                    }
                } else {
                    // Return LHS return type
                    if let Some(type_ref) = lhs_hints {
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
                                .from_type(&Array(Box::new(first.as_type()), expressions.len() as u64))
                                .unwrap())
                        } else {
                            Ok(type_refs.from_type(&Array(Box::new(TypeKind::Void), 0)).unwrap())
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
                    CustomType(key) => {
                        let struct_ty = state
                            .scope
                            .get_struct_type(&key)
                            .ok_or(ErrorKind::NoSuchType(key.0.clone(), key.1))?;
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
                let type_key = CustomTypeKey(struct_name.clone(), state.module_id.unwrap());
                let expected_struct_ty = state
                    .scope
                    .get_struct_type(&type_key)
                    .ok_or(ErrorKind::NoSuchType(struct_name.clone(), state.module_id.unwrap()))?
                    .clone();
                for field in fields {
                    if let Some(expected_field_ty) = expected_struct_ty.get_field_ty(&field.0) {
                        let field_ty = field.1.infer_types(state, type_refs);
                        if let Some(mut field_ty) = state.ok(field_ty, field.1 .1) {
                            field_ty.narrow(&type_refs.from_type(&expected_field_ty).unwrap());
                        }
                    } else {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::NoSuchField(format!("{}.{}", struct_name, field.0))),
                            field.1 .1,
                        );
                    }
                }
                Ok(type_refs.from_type(&TypeKind::CustomType(type_key.clone())).unwrap())
            }
            ExprKind::Borrow(expr, mutable) => {
                // Find variable type
                let type_ref = expr.infer_types(state, type_refs)?;

                Ok(type_refs
                    .from_type(&TypeKind::Borrow(Box::new(type_ref.as_type()), *mutable))
                    .unwrap())
            }
            ExprKind::Deref(expr) => {
                // Find variable type
                let type_ref = expr.infer_types(state, type_refs)?;

                // Update typing to be more accurate

                match type_ref.resolve_weak().unwrap() {
                    Borrow(inner, _) => Ok(type_refs.from_type(&inner).unwrap()),
                    _ => Err(ErrorKind::AttemptedDerefNonBorrow(type_ref.resolve_deep().unwrap())),
                }
            }
            ExprKind::CastTo(expression, type_kind) => {
                expression.infer_types(state, type_refs)?;
                Ok(type_refs.from_type(type_kind).unwrap())
            }
            ExprKind::AssociatedFunctionCall(type_kind, function_call) => {
                match type_kind.is_known(state) {
                    Ok(_) => {}
                    Err(ErrorKind::NoSuchType(name, mod_id)) => return Err(ErrorKind::NoSuchType(name, mod_id)),
                    Err(_) => {
                        let first_param = function_call
                            .parameters
                            .get_mut(0)
                            .expect("Unknown-type associated function NEEDS to always have at least one parameter!");
                        let param_ty = first_param.infer_types(state, type_refs).unwrap().resolve_deep();
                        *type_kind = state
                            .or_else(
                                param_ty.ok_or(ErrorKind::CouldNotInferType(format!("{}", first_param))),
                                Void,
                                first_param.1,
                            )
                            .resolve_ref(type_refs.types);
                        let backing_var = first_param.backing_var().expect("todo").1.clone();

                        if let TypeKind::Borrow(inner, _) = type_kind {
                            if let TypeKind::Borrow(..) = *inner.clone() {
                                *type_kind = type_kind.unroll_borrow();
                                let ExprKind::Borrow(val, _) = &first_param.0 else {
                                    panic!()
                                };
                                *first_param = *val.clone();
                            }
                        }

                        if let Some((mutable, _)) = type_refs.find_var(&backing_var) {
                            if !mutable {
                                first_param.remove_borrow_mutability();
                            }
                        } else {
                            return Err(ErrorKind::VariableNotDefined(backing_var));
                        }
                    }
                }

                // Get function definition and types
                let fn_call = state
                    .scope
                    .get_associated_function(&AssociatedFunctionKey(type_kind.clone(), function_call.name.clone()))
                    .ok_or(ErrorKind::AssocFunctionNotDefined(
                        function_call.name.clone(),
                        type_kind.clone(),
                    ))?
                    .clone();

                // Infer param expression types and narrow them to the
                // expected function parameters (or Unknown types if too
                // many were provided)
                let true_params_iter = fn_call.params.iter().chain(iter::repeat(&Vague(Unknown)));

                for (param_expr, param_t) in function_call.parameters.iter_mut().zip(true_params_iter) {
                    let expr_res = param_expr.infer_types(state, type_refs);
                    if let Some(mut param_ref) = state.ok(expr_res, param_expr.1) {
                        param_ref.narrow(&mut type_refs.from_type(param_t).unwrap());
                    }
                }

                // Provide function return type
                Ok(type_refs.from_type(&fn_call.ret).unwrap())
            }
        }
    }

    fn remove_borrow_mutability(&mut self) {
        match &mut self.0 {
            ExprKind::Variable(_) => {}
            ExprKind::Indexed(value_expr, ..) => value_expr.remove_borrow_mutability(),
            ExprKind::Accessed(value_expr, ..) => value_expr.remove_borrow_mutability(),
            ExprKind::Block(block) => {
                if let Some((_, Some(ret_expr))) = &mut block.return_expression {
                    ret_expr.remove_borrow_mutability();
                }
            }
            ExprKind::Borrow(_, mutable) => {
                *mutable = false;
            }
            _ => {}
        }
    }
}
