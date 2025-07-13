use std::iter;

use reid_lib::Function;

use super::{
    pass::{Pass, PassState, ScopeVariable},
    scopehints::{self, ScopeHints, TypeHint, TypeHints},
    typecheck::ErrorKind,
    types::{pick_return, ReturnType},
    Block, ExprKind, Expression, FunctionDefinition, FunctionDefinitionKind, IfExpression, Module,
    ReturnKind, StmtKind,
    TypeKind::*,
    VagueType::*,
};

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct TypeInference<'t> {
    pub hints: &'t TypeHints,
}

impl<'t> Pass for TypeInference<'t> {
    type TError = ErrorKind;

    fn module(&mut self, module: &mut Module, mut state: PassState<ErrorKind>) {
        for function in &mut module.functions {
            let res = function.infer_hints(&self.hints, &mut state);
            state.ok(res, function.block_meta());
        }
    }
}

impl FunctionDefinition {
    fn infer_hints(
        &mut self,
        hints: &TypeHints,
        state: &mut PassState<ErrorKind>,
    ) -> Result<(), ErrorKind> {
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
        let scope_hints = ScopeHints::from(hints);
        let return_type = self.return_type.clone();
        let return_type_hint = scope_hints.from_type(&return_type).unwrap();

        let mut ret = match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                state.scope.return_type_hint = Some(self.return_type);
                let block_res = block.infer_hints(state, &scope_hints);
                state.ok(block_res.map(|(_, ty)| ty), self.block_meta())
            }
            FunctionDefinitionKind::Extern => Some(scope_hints.from_type(&Vague(Unknown)).unwrap()),
        };

        if let Some(ret) = &mut ret {
            state.ok(ret.narrow(&return_type_hint), self.signature());
        }

        Ok(())
    }
}

impl Block {
    fn infer_hints<'s>(
        &mut self,
        state: &mut PassState<ErrorKind>,
        outer_hints: &'s ScopeHints,
    ) -> Result<(ReturnKind, TypeHint<'s>), ErrorKind> {
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
        let mut ret_type_ref = outer_hints.from_type(&ty).unwrap();

        if kind == ReturnKind::Hard {
            if let Some(hint) = state.scope.return_type_hint {
                state.ok(
                    ret_type_ref.narrow(&mut outer_hints.from_type(&hint).unwrap()),
                    self.meta,
                );
            }
        }
        Ok((kind, ret_type_ref))
    }
}

impl Expression {
    fn infer_hints<'s>(
        &mut self,
        state: &mut PassState<ErrorKind>,
        hints: &'s ScopeHints<'s>,
    ) -> Result<TypeHint<'s>, ErrorKind> {
        match &mut self.0 {
            ExprKind::Variable(var) => {
                let hint = hints
                    .find_hint(&var.1)
                    .map(|(_, hint)| hint)
                    .ok_or(ErrorKind::VariableNotDefined(var.1.clone()));
                if let Ok(hint) = &hint {
                    var.0 = hint.as_type()
                }
                hint
            }
            ExprKind::Literal(literal) => Ok(hints.from_type(&literal.as_type()).unwrap()),
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
                            param_ref.narrow(&mut hints.from_type(param_t).unwrap()),
                            param_expr.1,
                        );
                    }
                }

                Ok(hints.from_type(&fn_call.ret).unwrap())
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                let cond_res = cond.infer_hints(state, hints);
                let cond_hints = state.ok(cond_res, cond.1);

                if let Some(mut cond_hints) = cond_hints {
                    state.ok(
                        cond_hints.narrow(&mut hints.from_type(&Bool).unwrap()),
                        cond.1,
                    );
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
                        Ok(hints.from_type(&Vague(Unknown)).unwrap())
                    }
                } else {
                    if let Some((_, type_ref)) = lhs_hints {
                        Ok(type_ref)
                    } else {
                        Ok(hints.from_type(&Vague(Unknown)).unwrap())
                    }
                }
            }
            ExprKind::Block(block) => {
                let block_ref = block.infer_hints(state, hints)?;
                match block_ref.0 {
                    ReturnKind::Hard => Ok(hints.from_type(&Void).unwrap()),
                    ReturnKind::Soft => Ok(block_ref.1),
                }
            }
        }
    }
}
