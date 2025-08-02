use std::{collections::HashMap, path::PathBuf};

use reid::{
    ast::lexer::FullToken,
    compile_module,
    error_raporting::{ErrorModules, ReidError},
    mir::{self, Context, FunctionCall, IfExpression, SourceModuleId, StructType, TypeKind, WhileStatement},
    perform_all_passes,
};

type TokenAnalysisMap = HashMap<usize, SemanticAnalysis>;

#[derive(Debug, Clone)]
pub struct StaticAnalysis {
    pub tokens: Vec<FullToken>,
    pub token_analysis: TokenAnalysisMap,
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
    let (module, error) = match compile_module(module_id, tokens, map, Some(path.clone()), true)? {
        Ok(module) => (module, None),
        Err((m, err)) => (m.process(module_id), Some(err)),
    };

    let module_id = module.module_id;
    let mut context = Context::from(vec![module], path.parent().unwrap().to_owned());
    perform_all_passes(&mut context, map)?;

    for module in context.modules.values() {
        if module.module_id != module_id {
            continue;
        }
        return Ok(Some(analyze_context(&context, &module, error)));
    }
    return Ok(None);
}

pub fn set_tokens(map: &mut TokenAnalysisMap, meta: &mir::Metadata, analysis: SemanticAnalysis) {
    for token in meta.range.start..meta.range.end {
        map.insert(token, analysis.clone());
    }
}

pub fn analyze_context(context: &mir::Context, module: &mir::Module, error: Option<ReidError>) -> StaticAnalysis {
    let mut map = HashMap::new();
    for import in &module.imports {
        set_tokens(&mut map, &import.1, SemanticAnalysis { ty: None });
    }

    for typedef in &module.typedefs {
        match &typedef.kind {
            mir::TypeDefinitionKind::Struct(StructType(fields)) => {
                for field in fields {
                    set_tokens(
                        &mut map,
                        &field.2,
                        SemanticAnalysis {
                            ty: Some(field.1.clone()),
                        },
                    );
                }
            }
        }
    }

    for binop in &module.binop_defs {
        match &binop.fn_kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut map),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    for (_, function) in &module.associated_functions {
        for param in &function.parameters {
            set_tokens(
                &mut map,
                &param.meta,
                SemanticAnalysis {
                    ty: Some(param.ty.clone()),
                },
            );
        }

        match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut map),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    for function in &module.functions {
        for param in &function.parameters {
            set_tokens(
                &mut map,
                &param.meta,
                SemanticAnalysis {
                    ty: Some(param.ty.clone()),
                },
            );
        }

        match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut map),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    StaticAnalysis {
        tokens: module.tokens.clone(),
        token_analysis: map,
        error,
    }
}

pub fn analyze_block(
    context: &mir::Context,
    source_module: &mir::Module,
    block: &mir::Block,
    map: &mut TokenAnalysisMap,
) {
    for statement in &block.statements {
        match &statement.0 {
            mir::StmtKind::Let(named_variable_ref, _, expression) => {
                set_tokens(
                    map,
                    &named_variable_ref.2,
                    SemanticAnalysis {
                        ty: expression
                            .return_type(&Default::default(), source_module.module_id)
                            .ok()
                            .map(|(_, ty)| ty),
                    },
                );
                // return analyze_in_expr(&expression, module_id, token_idx);
            }
            mir::StmtKind::Set(lhs, rhs) => {
                analyze_expr(context, source_module, lhs, map);
                analyze_expr(context, source_module, rhs, map);
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expression) => {
                analyze_expr(context, source_module, expression, map);
            }
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                analyze_expr(context, source_module, condition, map);
                analyze_block(context, source_module, block, map);
            }
        }
    }

    if let Some((_, Some(return_exp))) = &block.return_expression {
        analyze_expr(context, source_module, return_exp, map)
    }
}

pub fn analyze_expr(
    context: &mir::Context,
    source_module: &mir::Module,
    expr: &mir::Expression,
    map: &mut TokenAnalysisMap,
) {
    set_tokens(
        map,
        &expr.1,
        SemanticAnalysis {
            ty: expr
                .return_type(&Default::default(), source_module.module_id)
                .ok()
                .map(|(_, t)| t),
        },
    );

    match &expr.0 {
        mir::ExprKind::Variable(_) => {}
        mir::ExprKind::Indexed(value, _, index_expr) => {
            analyze_expr(context, source_module, &value, map);
            analyze_expr(context, source_module, &index_expr, map);
        }
        mir::ExprKind::Accessed(expression, ..) => {
            analyze_expr(context, source_module, &expression, map);
        }
        mir::ExprKind::Array(expressions) => {
            for expr in expressions {
                analyze_expr(context, source_module, expr, map);
            }
        }
        mir::ExprKind::Struct(_, items) => {
            for (_, expr, _) in items {
                analyze_expr(context, source_module, expr, map);
            }
        }
        mir::ExprKind::Literal(_) => {}
        mir::ExprKind::BinOp(_, lhs, rhs, _) => {
            analyze_expr(context, source_module, &lhs, map);
            analyze_expr(context, source_module, &rhs, map);
        }
        mir::ExprKind::FunctionCall(FunctionCall { parameters, .. }) => {
            for expr in parameters {
                analyze_expr(context, source_module, expr, map);
            }
        }
        mir::ExprKind::AssociatedFunctionCall(_, FunctionCall { parameters, .. }) => {
            for expr in parameters {
                analyze_expr(context, source_module, expr, map);
            }
        }
        mir::ExprKind::If(IfExpression(cond, then_e, else_e)) => {
            analyze_expr(context, source_module, &cond, map);
            analyze_expr(context, source_module, &then_e, map);
            if let Some(else_e) = else_e.as_ref() {
                analyze_expr(context, source_module, &else_e, map);
            }
        }
        mir::ExprKind::Block(block) => analyze_block(context, source_module, block, map),
        mir::ExprKind::Borrow(expression, _) => {
            analyze_expr(context, source_module, &expression, map);
        }
        mir::ExprKind::Deref(expression) => {
            analyze_expr(context, source_module, &expression, map);
        }
        mir::ExprKind::CastTo(expression, _) => {
            analyze_expr(context, source_module, &expression, map);
        }
        mir::ExprKind::GlobalRef(_, _) => {}
    }
}
