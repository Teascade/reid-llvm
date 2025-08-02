use std::{collections::HashMap, path::PathBuf};

use dashmap::DashMap;
use reid::{
    ast::lexer::FullToken,
    compile_module,
    error_raporting::{ErrorModules, ReidError},
    mir::{self, Context, FunctionCall, IfExpression, SourceModuleId, StructType, TypeKind, WhileStatement},
    perform_all_passes,
};

use crate::CompileResult;

#[derive(Debug, Clone)]
pub struct StaticAnalysis {
    pub tokens: Vec<FullToken>,
    pub token_analysis: HashMap<usize, SemanticAnalysis>,
    pub error: Option<ReidError>,
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysis {
    pub ty: Option<TypeKind>,
}

pub fn analyze(
    module_id: SourceModuleId,
    tokens: Vec<FullToken>,
    path: PathBuf,
    map: &mut ErrorModules,
) -> Result<Option<StaticAnalysis>, ReidError> {
    let mut token_analysis = HashMap::new();

    let (module, error) = match compile_module(module_id, tokens, map, Some(path.clone()), true)? {
        Ok(module) => (module, None),
        Err((m, err)) => (m.process(module_id), Some(err)),
    };

    let module_id = module.module_id;
    let mut context = Context::from(vec![module], path.parent().unwrap().to_owned());
    perform_all_passes(&mut context, map)?;

    for module in context.modules.into_values() {
        if module.module_id != module_id {
            continue;
        }
        for idx in 0..module.tokens.len() {
            token_analysis.insert(
                idx,
                SemanticAnalysis {
                    ty: find_type_in_context(&module, idx),
                },
            );
        }

        return Ok(Some(StaticAnalysis {
            tokens: module.tokens,
            token_analysis: token_analysis,
            error,
        }));
    }
    return Ok(None);
}

pub fn find_type_in_context(module: &mir::Module, token_idx: usize) -> Option<TypeKind> {
    for import in &module.imports {
        if import.1.contains(token_idx) {
            return Some(TypeKind::CustomType(mir::CustomTypeKey(
                "d".to_owned(),
                SourceModuleId(1),
            )));
        }
    }
    for typedef in &module.typedefs {
        if !typedef.meta.contains(token_idx) {
            continue;
        }

        match &typedef.kind {
            mir::TypeDefinitionKind::Struct(StructType(fields)) => {
                for field in fields {
                    if field.2.contains(token_idx) {
                        return Some(field.1.clone());
                    }
                }
            }
        }
    }

    for binop in &module.binop_defs {
        if let Some(meta) = binop.block_meta() {
            if !meta.contains(token_idx) {
                continue;
            }
        } else {
            continue;
        }

        return match &binop.fn_kind {
            mir::FunctionDefinitionKind::Local(block, _) => find_type_in_block(&block, module.module_id, token_idx),
            mir::FunctionDefinitionKind::Extern(_) => None,
            mir::FunctionDefinitionKind::Intrinsic(_) => None,
        };
    }

    for (_, function) in &module.associated_functions {
        if !(function.signature() + function.block_meta()).contains(token_idx) {
            continue;
        }

        for param in &function.parameters {
            if param.meta.contains(token_idx) {
                return Some(param.ty.clone());
            }
        }

        return match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => find_type_in_block(&block, module.module_id, token_idx),
            mir::FunctionDefinitionKind::Extern(_) => None,
            mir::FunctionDefinitionKind::Intrinsic(_) => None,
        };
    }

    for function in &module.functions {
        if !(function.signature() + function.block_meta()).contains(token_idx) {
            continue;
        }

        for param in &function.parameters {
            if param.meta.contains(token_idx) {
                return Some(param.ty.clone());
            }
        }

        return match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => find_type_in_block(&block, module.module_id, token_idx),
            mir::FunctionDefinitionKind::Extern(_) => None,
            mir::FunctionDefinitionKind::Intrinsic(_) => None,
        };
    }
    None
}

pub fn find_type_in_block(block: &mir::Block, module_id: SourceModuleId, token_idx: usize) -> Option<TypeKind> {
    if !block.meta.contains(token_idx) {
        return Some(TypeKind::Bool);
    }

    for statement in &block.statements {
        if !statement.1.contains(token_idx) {
            continue;
        }
        match &statement.0 {
            mir::StmtKind::Let(named_variable_ref, _, expression) => {
                if named_variable_ref.2.contains(token_idx) {
                    return expression
                        .return_type(&Default::default(), module_id)
                        .ok()
                        .map(|(_, ty)| ty);
                } else {
                    return find_type_in_expr(&expression, module_id, token_idx);
                }
            }
            mir::StmtKind::Set(lhs, rhs) => {
                return find_type_in_expr(lhs, module_id, token_idx).or(find_type_in_expr(rhs, module_id, token_idx));
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expression) => return find_type_in_expr(expression, module_id, token_idx),
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                return find_type_in_expr(condition, module_id, token_idx)
                    .or(find_type_in_block(block, module_id, token_idx));
            }
        }
    }

    if let Some((_, Some(return_exp))) = &block.return_expression {
        if let Some(ty) = find_type_in_expr(return_exp, module_id, token_idx) {
            return Some(ty);
        }
    }

    None
}

pub fn find_type_in_expr(expr: &mir::Expression, module_id: SourceModuleId, token_idx: usize) -> Option<TypeKind> {
    if !expr.1.contains(token_idx) {
        return None;
    }

    match &expr.0 {
        mir::ExprKind::Variable(named_variable_ref) => Some(named_variable_ref.0.clone()),
        mir::ExprKind::Indexed(value, type_kind, index_expr) => Some(
            find_type_in_expr(&value, module_id, token_idx)
                .or(find_type_in_expr(&index_expr, module_id, token_idx))
                .unwrap_or(type_kind.clone()),
        ),
        mir::ExprKind::Accessed(expression, type_kind, _, meta) => {
            if meta.contains(token_idx) {
                Some(type_kind.clone())
            } else {
                find_type_in_expr(&expression, module_id, token_idx)
            }
        }
        mir::ExprKind::Array(expressions) => {
            for expr in expressions {
                if let Some(ty) = find_type_in_expr(expr, module_id, token_idx) {
                    return Some(ty);
                }
            }
            None
        }
        mir::ExprKind::Struct(name, items) => {
            for (_, expr, meta) in items {
                if meta.contains(token_idx) {
                    return expr.return_type(&Default::default(), module_id).map(|(_, t)| t).ok();
                }
                if let Some(ty) = find_type_in_expr(expr, module_id, token_idx) {
                    return Some(ty);
                }
            }
            Some(TypeKind::CustomType(mir::CustomTypeKey(name.clone(), module_id)))
        }
        mir::ExprKind::Literal(literal) => Some(literal.as_type()),
        mir::ExprKind::BinOp(_, lhs, rhs, type_kind) => {
            if let Some(ty) = find_type_in_expr(lhs, module_id, token_idx) {
                return Some(ty);
            }
            if let Some(ty) = find_type_in_expr(rhs, module_id, token_idx) {
                return Some(ty);
            }
            Some(type_kind.clone())
        }
        mir::ExprKind::FunctionCall(FunctionCall {
            return_type,
            parameters,
            ..
        }) => {
            for expr in parameters {
                if let Some(ty) = find_type_in_expr(expr, module_id, token_idx) {
                    return Some(ty);
                }
            }
            Some(return_type.clone())
        }
        mir::ExprKind::AssociatedFunctionCall(
            _,
            FunctionCall {
                return_type,
                parameters,
                ..
            },
        ) => {
            for expr in parameters {
                if let Some(ty) = find_type_in_expr(expr, module_id, token_idx) {
                    return Some(ty);
                }
            }
            Some(return_type.clone())
        }
        mir::ExprKind::If(IfExpression(cond, then_e, else_e)) => find_type_in_expr(&cond, module_id, token_idx)
            .or(find_type_in_expr(&then_e, module_id, token_idx))
            .or(else_e.clone().and_then(|e| find_type_in_expr(&e, module_id, token_idx))),
        mir::ExprKind::Block(block) => find_type_in_block(block, module_id, token_idx),
        mir::ExprKind::Borrow(expression, mutable) => {
            if let Some(ty) = find_type_in_expr(&expression, module_id, token_idx) {
                return Some(ty);
            }
            if let Ok(inner) = expression.return_type(&Default::default(), module_id).map(|(_, ty)| ty) {
                Some(TypeKind::Borrow(Box::new(inner.clone()), *mutable))
            } else {
                None
            }
        }
        mir::ExprKind::Deref(expression) => {
            if let Some(ty) = find_type_in_expr(&expression, module_id, token_idx) {
                return Some(ty);
            }
            if let Ok(TypeKind::Borrow(inner, _)) =
                expression.return_type(&Default::default(), module_id).map(|(_, ty)| ty)
            {
                Some(*inner.clone())
            } else {
                Some(TypeKind::CustomType(mir::CustomTypeKey(
                    "ä".to_owned(),
                    SourceModuleId(1),
                )))
            }
        }
        mir::ExprKind::CastTo(expression, type_kind) => {
            Some(find_type_in_expr(&expression, module_id, token_idx).unwrap_or(type_kind.clone()))
        }
        mir::ExprKind::GlobalRef(_, type_kind) => Some(type_kind.clone()),
    }
}
