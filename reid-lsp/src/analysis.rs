use std::{collections::HashMap, fmt::format, path::PathBuf};

use reid::{
    ast::{self, FunctionDefinition, lexer::FullToken, token_stream::TokenRange},
    codegen::intrinsics::get_intrinsic_assoc_functions,
    compile_module,
    error_raporting::{ErrorModules, ReidError},
    mir::{
        self, Context, FunctionCall, FunctionParam, IfExpression, SourceModuleId, StructType, TypeKind, WhileStatement,
        typecheck::typerefs::TypeRefs,
    },
    perform_all_passes,
};

#[derive(Debug, Clone)]
pub struct StaticAnalysis {
    pub tokens: Vec<FullToken>,
    pub token_analysis: AnalysisState,
    pub error: Option<ReidError>,
}

#[derive(Debug, Clone)]
pub struct TokenAnalysis {
    pub ty: Option<TypeKind>,
    pub autocomplete: Vec<Autocomplete>,
    pub symbol: Option<SymbolId>,
}

#[derive(Debug, Clone)]
pub struct Autocomplete {
    pub text: String,
    pub kind: AutocompleteKind,
}

#[derive(Debug, Clone)]
pub enum AutocompleteKind {
    Type,
    Field(TypeKind),
    Function(Vec<FunctionParam>, TypeKind),
}

impl ToString for AutocompleteKind {
    fn to_string(&self) -> String {
        match self {
            AutocompleteKind::Type => String::from("type"),
            AutocompleteKind::Function(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, p.ty))
                    .collect::<Vec<_>>();
                format!("({}) -> {}", params.join(", "), ret_ty)
            }
            AutocompleteKind::Field(type_kind) => format!("{}", type_kind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolId(usize);

#[derive(Debug, Clone)]
pub struct AnalysisState {
    pub map: HashMap<usize, TokenAnalysis>,
    pub symbol_table: Vec<SymbolKind>,
}

impl AnalysisState {
    pub fn init_types(&mut self, meta: &mir::Metadata, ty: Option<TypeKind>) {
        for token in meta.range.start..=meta.range.end {
            self.map.insert(
                token,
                TokenAnalysis {
                    ty: ty.clone(),
                    autocomplete: Vec::new(),
                    symbol: Default::default(),
                },
            );
        }
    }

    pub fn set_autocomplete(&mut self, token_idx: usize, autocomplete: Vec<Autocomplete>) {
        if let Some(token) = self.map.get_mut(&token_idx) {
            token.autocomplete = autocomplete.clone();
        } else {
            self.map.insert(
                token_idx,
                TokenAnalysis {
                    ty: None,
                    autocomplete: autocomplete.clone(),
                    symbol: Default::default(),
                },
            );
        }
    }

    pub fn set_symbol(&mut self, token_idx: usize, symbol: SymbolId) {
        if let Some(token) = self.map.get_mut(&token_idx) {
            token.symbol = Some(symbol);
        } else {
            self.map.insert(
                token_idx,
                TokenAnalysis {
                    ty: None,
                    autocomplete: Vec::new(),
                    symbol: Some(symbol),
                },
            );
        }
    }

    pub fn new_symbol(&mut self, kind: SymbolKind) -> SymbolId {
        let id = SymbolId(self.symbol_table.len());
        self.symbol_table.push(kind);
        id
    }
}

pub struct AnalysisScope<'a> {
    state: &'a mut AnalysisState,
    variables: HashMap<String, SymbolId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolKind {
    Default,
}

impl Default for SymbolKind {
    fn default() -> Self {
        SymbolKind::Default
    }
}

type TokenAnalysisMap = HashMap<usize, TokenAnalysis>;

pub fn analyze(
    module_id: SourceModuleId,
    tokens: Vec<FullToken>,
    path: PathBuf,
    map: &mut ErrorModules,
) -> Result<Option<StaticAnalysis>, ReidError> {
    let (module, mut parse_error) = match compile_module(module_id, tokens, map, Some(path.clone()), true)? {
        Ok(module) => (module, None),
        Err((m, err)) => (m.process(module_id), Some(err)),
    };

    let module_id = module.module_id;
    let mut context = Context::from(vec![module], path.parent().unwrap().to_owned());
    match perform_all_passes(&mut context, map) {
        Ok(_) => {}
        Err(pass_error) => {
            if let Some(err) = &mut parse_error {
                err.extend(pass_error);
            } else {
                parse_error = Some(pass_error)
            }
        }
    }

    for module in context.modules.values() {
        if module.module_id != module_id {
            continue;
        }
        return Ok(Some(analyze_context(&context, &module, parse_error)));
    }
    return Ok(None);
}

pub fn analyze_context(context: &mir::Context, module: &mir::Module, error: Option<ReidError>) -> StaticAnalysis {
    let mut state = AnalysisState {
        map: HashMap::new(),
        symbol_table: Vec::new(),
    };

    let mut scope = AnalysisScope {
        state: &mut state,
        variables: HashMap::new(),
    };
    for import in &module.imports {
        scope.state.init_types(&import.1, None);
        if let Some((module_name, _)) = import.0.get(0) {
            let (import_name, import_meta) = import.0.get(1).cloned().unwrap_or((
                String::new(),
                mir::Metadata {
                    source_module_id: module.module_id,
                    range: reid::ast::token_stream::TokenRange {
                        start: import.1.range.end - 1,
                        end: import.1.range.end - 1,
                    },
                    position: None,
                },
            ));
            let mut autocompletes = Vec::new();

            if let Some((_, module)) = context.modules.iter().find(|m| m.1.name == *module_name) {
                for function in &module.functions {
                    if !function.is_pub {
                        continue;
                    }
                    if function.name.starts_with(&import_name) {
                        autocompletes.push(Autocomplete {
                            text: function.name.clone(),
                            kind: AutocompleteKind::Function(function.parameters.clone(), function.return_type.clone()),
                        });
                    }
                }
                for typedef in &module.typedefs {
                    if typedef.name.starts_with(&import_name) {
                        autocompletes.push(Autocomplete {
                            text: typedef.name.clone(),
                            kind: AutocompleteKind::Type,
                        });
                    }
                }
            }
            scope.state.set_autocomplete(import_meta.range.end, autocompletes);
        }
    }

    for typedef in &module.typedefs {
        match &typedef.kind {
            mir::TypeDefinitionKind::Struct(StructType(fields)) => {
                for field in fields {
                    scope.state.init_types(&field.2, Some(field.1.clone()));
                }
            }
        }
    }

    for binop in &module.binop_defs {
        match &binop.fn_kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut scope),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    for (_, function) in &module.associated_functions {
        for param in &function.parameters {
            scope.state.init_types(&param.meta, Some(param.ty.clone()));
        }

        match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut scope),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    for function in &module.functions {
        for param in &function.parameters {
            scope.state.init_types(&param.meta, Some(param.ty.clone()));
        }

        match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut scope),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    StaticAnalysis {
        tokens: module.tokens.clone(),
        token_analysis: state,
        error,
    }
}

pub fn analyze_block(
    context: &mir::Context,
    source_module: &mir::Module,
    block: &mir::Block,
    scope: &mut AnalysisScope,
) {
    for statement in &block.statements {
        match &statement.0 {
            mir::StmtKind::Let(named_variable_ref, _, expression) => {
                scope.state.init_types(
                    &named_variable_ref.2,
                    expression
                        .return_type(&TypeRefs::unknown(), source_module.module_id)
                        .ok()
                        .map(|(_, ty)| ty),
                );
                // return analyze_in_expr(&expression, module_id, token_idx);
            }
            mir::StmtKind::Set(lhs, rhs) => {
                analyze_expr(context, source_module, lhs, scope);
                analyze_expr(context, source_module, rhs, scope);
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expression) => {
                analyze_expr(context, source_module, expression, scope);
            }
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                analyze_expr(context, source_module, condition, scope);
                analyze_block(context, source_module, block, scope);
            }
        }
    }

    if let Some((_, Some(return_exp))) = &block.return_expression {
        analyze_expr(context, source_module, return_exp, scope)
    }
}

pub fn analyze_expr(
    context: &mir::Context,
    source_module: &mir::Module,
    expr: &mir::Expression,
    scope: &mut AnalysisScope,
) {
    scope.state.init_types(
        &expr.1,
        expr.return_type(&TypeRefs::unknown(), source_module.module_id)
            .ok()
            .map(|(_, t)| t),
    );

    match &expr.0 {
        mir::ExprKind::Variable(_) => {}
        mir::ExprKind::Indexed(value, _, index_expr) => {
            analyze_expr(context, source_module, &value, scope);
            analyze_expr(context, source_module, &index_expr, scope);
        }
        mir::ExprKind::Accessed(expression, _, name, meta) => {
            analyze_expr(context, source_module, &expression, scope);

            let accessed_type = expression.return_type(&TypeRefs::unknown(), source_module.module_id);

            let mut autocompletes = Vec::new();
            match accessed_type {
                Ok((_, accessed_type)) => {
                    autocompletes.extend(
                        source_module
                            .associated_functions
                            .iter()
                            .filter(|(t, fun)| *t == accessed_type && fun.name.starts_with(name))
                            .map(|(_, fun)| Autocomplete {
                                text: fun.name.clone(),
                                kind: AutocompleteKind::Function(fun.parameters.clone(), fun.return_type.clone()),
                            }),
                    );
                    match accessed_type {
                        TypeKind::CustomType(ty_key) => {
                            let typedef = source_module
                                .typedefs
                                .iter()
                                .find(|t| t.name == ty_key.0 && t.source_module == ty_key.1);
                            if let Some(typedef) = typedef {
                                autocompletes.extend(match &typedef.kind {
                                    mir::TypeDefinitionKind::Struct(StructType(fields)) => {
                                        fields.iter().filter(|f| f.0.starts_with(name)).map(|f| Autocomplete {
                                            text: f.0.clone(),
                                            kind: AutocompleteKind::Field(f.1.clone()),
                                        })
                                    }
                                });
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }

            scope.state.set_autocomplete(meta.range.end, autocompletes);
        }
        mir::ExprKind::Array(expressions) => {
            for expr in expressions {
                analyze_expr(context, source_module, expr, scope);
            }
        }
        mir::ExprKind::Struct(_, items) => {
            for (_, expr, _) in items {
                analyze_expr(context, source_module, expr, scope);
            }
        }
        mir::ExprKind::Literal(_) => {}
        mir::ExprKind::BinOp(_, lhs, rhs, _) => {
            analyze_expr(context, source_module, &lhs, scope);
            analyze_expr(context, source_module, &rhs, scope);
        }
        mir::ExprKind::FunctionCall(FunctionCall { parameters, .. }) => {
            for expr in parameters {
                analyze_expr(context, source_module, expr, scope);
            }
        }
        mir::ExprKind::AssociatedFunctionCall(
            ty,
            FunctionCall {
                parameters, name, meta, ..
            },
        ) => {
            for expr in parameters {
                analyze_expr(context, source_module, expr, scope);
            }
            let mut function_autocomplete = source_module
                .associated_functions
                .iter()
                .filter(|(t, fun)| t == ty && fun.name.starts_with(name))
                .map(|(_, fun)| Autocomplete {
                    text: fun.name.clone(),
                    kind: AutocompleteKind::Function(fun.parameters.clone(), fun.return_type.clone()),
                })
                .collect::<Vec<_>>();
            function_autocomplete.extend(
                get_intrinsic_assoc_functions(ty)
                    .iter()
                    .filter_map(|(s, f)| f.as_ref().map(|f| (s, f)))
                    .filter(|(_, fun)| fun.name.starts_with(name))
                    .map(|(_, fun)| Autocomplete {
                        text: fun.name.clone(),
                        kind: AutocompleteKind::Function(fun.parameters.clone(), fun.return_type.clone()),
                    })
                    .collect::<Vec<_>>(),
            );
            scope
                .state
                .set_autocomplete(meta.range.start, function_autocomplete.clone());
            scope
                .state
                .set_autocomplete(meta.range.end, function_autocomplete.clone());
        }
        mir::ExprKind::If(IfExpression(cond, then_e, else_e)) => {
            analyze_expr(context, source_module, &cond, scope);
            analyze_expr(context, source_module, &then_e, scope);
            if let Some(else_e) = else_e.as_ref() {
                analyze_expr(context, source_module, &else_e, scope);
            }
        }
        mir::ExprKind::Block(block) => analyze_block(context, source_module, block, scope),
        mir::ExprKind::Borrow(expression, _) => {
            analyze_expr(context, source_module, &expression, scope);
        }
        mir::ExprKind::Deref(expression) => {
            analyze_expr(context, source_module, &expression, scope);
        }
        mir::ExprKind::CastTo(expression, _) => {
            analyze_expr(context, source_module, &expression, scope);
        }
        mir::ExprKind::GlobalRef(_, _) => {}
    }
}
