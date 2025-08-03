use std::{collections::HashMap, fmt::format, path::PathBuf};

use reid::{
    ast::{
        self, FunctionDefinition,
        lexer::{FullToken, Token},
        token_stream::TokenRange,
    },
    codegen::intrinsics::get_intrinsic_assoc_functions,
    compile_module,
    error_raporting::{ErrorModules, ReidError},
    mir::{
        self, Context, CustomTypeKey, FunctionCall, FunctionParam, IfExpression, Metadata, SourceModuleId, StructType,
        TypeKind, WhileStatement, typecheck::typerefs::TypeRefs,
    },
    perform_all_passes,
};
use tower_lsp::lsp_types::{SemanticTokenModifier, SemanticTokenType};

pub const TOKEN_LEGEND: [SemanticTokenType; 9] = [
    SemanticTokenType::VARIABLE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::STRUCT,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::COMMENT,
    SemanticTokenType::PROPERTY,
];

const SEMANTIC_REFERENCE: SemanticTokenModifier = SemanticTokenModifier::new("reference");

pub const MODIFIER_LEGEND: [SemanticTokenModifier; 3] = [
    SemanticTokenModifier::DEFINITION,
    SEMANTIC_REFERENCE,
    SemanticTokenModifier::DECLARATION,
];

#[derive(Debug, Clone)]
pub struct StaticAnalysis {
    pub tokens: Vec<FullToken>,
    pub state: AnalysisState,
    pub error: Option<ReidError>,
}

impl StaticAnalysis {
    pub fn find_definition(&self, token_idx: usize) -> Option<&FullToken> {
        let semantic_token = self.state.map.get(&token_idx)?;
        let symbol_id = semantic_token.symbol?;
        let definition_id = self.state.find_definition(&symbol_id);

        let def_token_idx = self.state.symbol_to_token.get(&definition_id)?;
        self.tokens.get(*def_token_idx)
    }

    pub fn find_references(&self, token_idx: usize) -> Option<Vec<SymbolId>> {
        let mut references = Vec::new();
        let semantic_token = self.state.map.get(&token_idx)?;
        let symbol_id = semantic_token.symbol?;
        let definition_id = self.state.find_definition(&symbol_id);
        references.push(definition_id);

        for (symbol_idx, semantic_symbol) in self.state.symbol_table.iter().enumerate() {
            if let SemanticKind::Reference(ref_idx) = semantic_symbol.kind {
                if ref_idx == definition_id {
                    references.push(SymbolId(symbol_idx));
                }
            }
        }

        dbg!(&references);

        Some(references)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticToken {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(usize);

#[derive(Debug, Clone)]
pub struct AnalysisState {
    /// TokenID -> Analysis map, containing SymbolIDs
    pub map: HashMap<usize, SemanticToken>,
    /// SymbolID -> Symbol
    symbol_table: Vec<Symbol>,
    /// SymbolID -> Symbol
    pub symbol_to_token: HashMap<SymbolId, usize>,
}

impl AnalysisState {
    pub fn get_symbol(&self, id: SymbolId) -> &Symbol {
        self.symbol_table.get(id.0).unwrap()
    }
}

impl AnalysisState {
    pub fn init_types(&mut self, meta: &mir::Metadata, ty: Option<TypeKind>) {
        for token in meta.range.start..=meta.range.end {
            self.map.insert(
                token,
                SemanticToken {
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
                SemanticToken {
                    ty: None,
                    autocomplete: autocomplete.clone(),
                    symbol: Default::default(),
                },
            );
        }
    }

    pub fn set_symbol(&mut self, idx: usize, symbol: SymbolId) {
        self.symbol_to_token.insert(symbol, idx);
        if let Some(token) = self.map.get_mut(&idx) {
            token.symbol = Some(symbol);
        } else {
            self.map.insert(
                idx,
                SemanticToken {
                    ty: None,
                    autocomplete: Vec::new(),
                    symbol: Some(symbol),
                },
            );
        }
    }

    pub fn new_symbol(&mut self, definition: usize, kind: SemanticKind) -> SymbolId {
        let id = SymbolId(self.symbol_table.len());
        self.symbol_table.push(Symbol { kind, definition });
        id
    }

    pub fn find_definition(&self, id: &SymbolId) -> SymbolId {
        let symbol = self.get_symbol(*id);
        match symbol.kind {
            SemanticKind::Reference(idx) => self.find_definition(&idx),
            _ => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SemanticKind,
    pub definition: usize,
}

pub struct AnalysisScope<'a> {
    state: &'a mut AnalysisState,
    tokens: &'a Vec<FullToken>,
    variables: HashMap<String, SymbolId>,
    functions: HashMap<String, SymbolId>,
    associated_functions: HashMap<(TypeKind, String), SymbolId>,
    properties: HashMap<(TypeKind, String), SymbolId>,
    types: HashMap<TypeKind, SymbolId>,
}

impl<'a> AnalysisScope<'a> {
    pub fn inner(&mut self) -> AnalysisScope {
        AnalysisScope {
            state: self.state,
            tokens: self.tokens,
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            associated_functions: self.associated_functions.clone(),
            properties: self.properties.clone(),
            types: self.types.clone(),
        }
    }

    pub fn token_idx<T: Copy>(&self, meta: &Metadata, pred: T) -> Option<usize>
    where
        T: FnOnce(&Token) -> bool,
    {
        for idx in meta.range.start..=meta.range.end {
            if let Some(token) = self.tokens.get(idx) {
                // dbg!(idx, token);
                if pred(&token.token) {
                    return Some(idx);
                }
            }
        }
        return None;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SemanticKind {
    Default,
    Variable,
    Function,
    String,
    Number,
    Property,
    Type,
    Struct,
    Comment,
    Operator,
    Keyword,
    Reference(SymbolId),
}

impl Default for SemanticKind {
    fn default() -> Self {
        SemanticKind::Default
    }
}

impl SemanticKind {
    pub fn into_token_idx(&self, state: &AnalysisState) -> Option<u32> {
        let token_type = match self {
            SemanticKind::Variable => SemanticTokenType::VARIABLE,
            SemanticKind::Function => SemanticTokenType::FUNCTION,
            SemanticKind::Type => SemanticTokenType::TYPE,
            SemanticKind::String => SemanticTokenType::STRING,
            SemanticKind::Number => SemanticTokenType::NUMBER,
            SemanticKind::Property => SemanticTokenType::PROPERTY,
            SemanticKind::Struct => SemanticTokenType::STRUCT,
            SemanticKind::Comment => SemanticTokenType::COMMENT,
            SemanticKind::Operator => SemanticTokenType::OPERATOR,
            SemanticKind::Keyword => SemanticTokenType::KEYWORD,
            SemanticKind::Default => return None,
            SemanticKind::Reference(symbol_id) => return state.get_symbol(*symbol_id).kind.into_token_idx(state),
        };
        TOKEN_LEGEND
            .iter()
            .enumerate()
            .find(|(_, t)| token_type == **t)
            .map(|(i, _)| i as u32)
    }

    pub fn get_modifier(&self) -> Option<u32> {
        let token_type = match self {
            SemanticKind::Variable => SemanticTokenModifier::DEFINITION,
            SemanticKind::Function => SemanticTokenModifier::DEFINITION,
            SemanticKind::Type => return None,
            SemanticKind::String => return None,
            SemanticKind::Number => return None,
            SemanticKind::Default => return None,
            SemanticKind::Property => SemanticTokenModifier::DECLARATION,
            SemanticKind::Struct => SemanticTokenModifier::DEFINITION,
            SemanticKind::Comment => return None,
            SemanticKind::Operator => return None,
            SemanticKind::Keyword => return None,
            SemanticKind::Reference(_) => SEMANTIC_REFERENCE,
        };
        MODIFIER_LEGEND
            .iter()
            .enumerate()
            .find(|(_, t)| token_type == **t)
            .map(|(i, _)| 1 << i)
    }
}

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
        symbol_to_token: HashMap::new(),
    };

    let mut scope = AnalysisScope {
        state: &mut state,
        tokens: &module.tokens,
        variables: HashMap::new(),
        functions: HashMap::new(),
        associated_functions: HashMap::new(),
        properties: HashMap::new(),
        types: HashMap::new(),
    };

    for (i, token) in module.tokens.iter().enumerate() {
        let semantic_token = match &token.token {
            Token::DecimalValue(_) => Some(SemanticKind::Number),
            Token::HexadecimalValue(_) => Some(SemanticKind::Number),
            Token::OctalValue(_) => Some(SemanticKind::Number),
            Token::BinaryValue(_) => Some(SemanticKind::Number),
            Token::CharLit(_) => Some(SemanticKind::String),
            Token::StringLit(_) => Some(SemanticKind::String),
            Token::LetKeyword
            | Token::MutKeyword
            | Token::ImportKeyword
            | Token::ReturnKeyword
            | Token::FnKeyword
            | Token::PubKeyword
            | Token::AsKeyword
            | Token::If
            | Token::Else
            | Token::True
            | Token::False
            | Token::Extern
            | Token::Struct
            | Token::While
            | Token::For
            | Token::In
            | Token::Impl
            | Token::Binop => Some(SemanticKind::Keyword),
            Token::Equals
            | Token::Plus
            | Token::Star
            | Token::Minus
            | Token::Slash
            | Token::Percent
            | Token::GreaterThan
            | Token::LessThan
            | Token::Et
            | Token::Pipe
            | Token::Hat
            | Token::Exclamation
            | Token::Comment(_) => Some(SemanticKind::Comment),
            _ => None,
        };
        if let Some(semantic) = semantic_token {
            let symbol = scope.state.new_symbol(i, semantic);
            scope.state.set_symbol(i, symbol);
        }
    }

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
        if typedef.source_module != module.module_id {
            continue;
        }

        match &typedef.kind {
            mir::TypeDefinitionKind::Struct(StructType(fields)) => {
                let struct_idx = scope
                    .token_idx(&typedef.meta, |t| matches!(t, Token::Identifier(_)))
                    .unwrap_or(typedef.meta.range.end);
                let struct_symbol = scope.state.new_symbol(struct_idx, SemanticKind::Struct);
                scope.state.set_symbol(struct_idx, struct_symbol);
                scope.types.insert(
                    TypeKind::CustomType(CustomTypeKey(typedef.name.clone(), typedef.source_module)),
                    struct_symbol,
                );

                for field in fields {
                    let field_idx = scope
                        .token_idx(&field.2, |t| matches!(t, Token::Identifier(_)))
                        .unwrap_or(field.2.range.end);

                    scope.state.init_types(
                        &Metadata {
                            source_module_id: field.2.source_module_id,
                            range: TokenRange {
                                start: field_idx,
                                end: field_idx,
                            },
                            position: None,
                        },
                        Some(field.1.clone()),
                    );

                    let field_symbol = scope.state.new_symbol(field_idx, SemanticKind::Property);
                    scope.state.set_symbol(field_idx, field_symbol);

                    scope.properties.insert(
                        (
                            TypeKind::CustomType(CustomTypeKey(typedef.name.clone(), typedef.source_module)),
                            field.0.clone(),
                        ),
                        field_symbol,
                    );
                }
            }
        }
    }

    for binop in &module.binop_defs {
        if binop.meta.source_module_id == module.module_id {
            for param in [&binop.lhs, &binop.rhs] {
                scope.state.init_types(&param.meta, Some(param.ty.clone()));
                let idx = scope
                    .token_idx(&param.meta, |t| matches!(t, Token::Identifier(_)))
                    .unwrap_or(param.meta.range.end);
                let symbol = scope.state.new_symbol(idx, SemanticKind::Variable);
                scope.state.set_symbol(idx, symbol);
                scope.variables.insert(param.name.clone(), symbol);
            }
        }

        match &binop.fn_kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut scope),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    for (ty, function) in &module.associated_functions {
        let idx = scope
            .token_idx(&function.signature(), |t| matches!(t, Token::Identifier(_)))
            .unwrap_or(function.signature().range.end);
        let symbol = scope.state.new_symbol(idx, SemanticKind::Function);
        scope.state.set_symbol(idx, symbol);
        scope
            .associated_functions
            .insert((ty.clone(), function.name.clone()), symbol);

        for param in &function.parameters {
            scope.state.init_types(&param.meta, Some(param.ty.clone()));

            if param.meta.source_module_id == module.module_id {
                let param_idx = scope
                    .token_idx(&param.meta, |t| matches!(t, Token::Identifier(_)))
                    .unwrap_or(function.signature().range.end);
                let param_symbol = scope.state.new_symbol(param_idx, SemanticKind::Variable);
                scope.state.set_symbol(param_idx, param_symbol);
                scope.variables.insert(param.name.clone(), param_symbol);
            }
        }

        match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut scope),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    for function in &module.functions {
        scope
            .state
            .init_types(&function.signature(), Some(function.return_type.clone()));

        let idx = scope
            .token_idx(&function.signature(), |t| matches!(t, Token::Identifier(_)))
            .unwrap_or(function.signature().range.end);
        let function_symbol = scope.state.new_symbol(idx, SemanticKind::Function);
        scope.state.set_symbol(idx, function_symbol);
        scope.functions.insert(function.name.clone(), function_symbol);
    }

    for function in &module.functions {
        for param in &function.parameters {
            scope.state.init_types(&param.meta, Some(param.ty.clone()));
            if param.meta.source_module_id == module.module_id {
                let idx = scope
                    .token_idx(&param.meta, |t| matches!(t, Token::Identifier(_)))
                    .unwrap_or(function.signature().range.end);
                let symbol = scope.state.new_symbol(idx, SemanticKind::Variable);
                scope.state.set_symbol(idx, symbol);
                scope.variables.insert(param.name.clone(), symbol);
            }
        }

        match &function.kind {
            mir::FunctionDefinitionKind::Local(block, _) => analyze_block(context, module, block, &mut scope),
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        };
    }

    StaticAnalysis {
        tokens: module.tokens.clone(),
        state,
        error,
    }
}

pub fn analyze_block(
    context: &mir::Context,
    source_module: &mir::Module,
    block: &mir::Block,
    scope: &mut AnalysisScope,
) {
    let scope = &mut scope.inner();

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
                let idx = scope
                    .token_idx(&named_variable_ref.2, |t| matches!(t, Token::Identifier(_)))
                    .unwrap_or(named_variable_ref.2.range.end);
                let symbol = scope.state.new_symbol(idx, SemanticKind::Variable);
                scope.state.set_symbol(idx, symbol);
                scope.variables.insert(named_variable_ref.1.clone(), symbol);

                analyze_expr(context, source_module, expression, scope);
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
        mir::ExprKind::Variable(var_ref) => {
            scope.state.init_types(&var_ref.2, Some(var_ref.0.clone()));

            let idx = scope
                .token_idx(&var_ref.2, |t| matches!(t, Token::Identifier(_)))
                .unwrap_or(var_ref.2.range.end);
            let symbol = if let Some(symbol_id) = scope.variables.get(&var_ref.1) {
                scope.state.new_symbol(idx, SemanticKind::Reference(*symbol_id))
            } else {
                scope.state.new_symbol(idx, SemanticKind::Type)
            };
            scope.state.set_symbol(idx, symbol);
        }
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
                    match &accessed_type {
                        TypeKind::CustomType(ty_key) => {
                            let typedef = source_module
                                .typedefs
                                .iter()
                                .find(|t| t.name == ty_key.0 && t.source_module == ty_key.1);

                            let field_idx = scope
                                .token_idx(&meta, |t| matches!(t, Token::Identifier(_)))
                                .unwrap_or(meta.range.end);

                            let field_symbol =
                                if let Some(symbol_id) = scope.properties.get(&(accessed_type.clone(), name.clone())) {
                                    scope.state.new_symbol(field_idx, SemanticKind::Reference(*symbol_id))
                                } else {
                                    scope.state.new_symbol(field_idx, SemanticKind::Property)
                                };
                            scope.state.set_symbol(field_idx, field_symbol);

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
        mir::ExprKind::Struct(struct_name, items) => {
            let struct_type = TypeKind::CustomType(CustomTypeKey(struct_name.clone(), source_module.module_id));
            let struct_idx = scope
                .token_idx(&expr.1, |t| matches!(t, Token::Identifier(_)))
                .unwrap_or(expr.1.range.end);

            let struct_symbol = if let Some(symbol_id) = scope.types.get(&struct_type) {
                scope.state.new_symbol(struct_idx, SemanticKind::Reference(*symbol_id))
            } else {
                scope.state.new_symbol(struct_idx, SemanticKind::Struct)
            };
            scope.state.set_symbol(struct_idx, struct_symbol);

            for (field_name, expr, field_meta) in items {
                let field_idx = scope
                    .token_idx(&field_meta, |t| matches!(t, Token::Identifier(_)))
                    .unwrap_or(field_meta.range.end);

                let field_symbol =
                    if let Some(symbol_id) = scope.properties.get(&(struct_type.clone(), field_name.clone())) {
                        scope.state.new_symbol(field_idx, SemanticKind::Reference(*symbol_id))
                    } else {
                        scope.state.new_symbol(field_idx, SemanticKind::Property)
                    };

                scope.state.set_symbol(field_idx, field_symbol);

                analyze_expr(context, source_module, expr, scope);
            }
        }
        mir::ExprKind::Literal(_) => {
            if let Some(idx) = scope.token_idx(&expr.1, |t| matches!(t, Token::StringLit(_) | Token::CharLit(_))) {
                scope.state.new_symbol(idx, SemanticKind::String);
            } else if let Some(idx) = scope.token_idx(&expr.1, |t| {
                matches!(
                    t,
                    Token::DecimalValue(_) | Token::HexadecimalValue(_) | Token::OctalValue(_) | Token::BinaryValue(_)
                )
            }) {
                scope.state.new_symbol(idx, SemanticKind::Number);
            }
        }
        mir::ExprKind::BinOp(_, lhs, rhs, _) => {
            analyze_expr(context, source_module, &lhs, scope);
            analyze_expr(context, source_module, &rhs, scope);
        }
        mir::ExprKind::FunctionCall(FunctionCall {
            parameters, meta, name, ..
        }) => {
            for expr in parameters {
                analyze_expr(context, source_module, expr, scope);
            }

            let idx = scope
                .token_idx(&meta, |t| matches!(t, Token::Identifier(_)))
                .unwrap_or(meta.range.end);
            let symbol = if let Some(symbol_id) = scope.functions.get(name) {
                scope.state.new_symbol(idx, SemanticKind::Reference(*symbol_id))
            } else {
                scope.state.new_symbol(idx, SemanticKind::Function)
            };
            scope.state.set_symbol(idx, symbol);
        }
        mir::ExprKind::AssociatedFunctionCall(
            ty,
            FunctionCall {
                parameters, name, meta, ..
            },
        ) => {
            let type_idx = scope
                .token_idx(&expr.1, |t| matches!(t, Token::Identifier(_)))
                .unwrap_or(expr.1.range.end);
            let invoked_ty = if let TypeKind::Borrow(inner, _) = ty {
                *inner.clone()
            } else {
                ty.clone()
            };

            let type_symbol = if let Some(symbol_id) = scope.types.get(&invoked_ty) {
                scope.state.new_symbol(type_idx, SemanticKind::Reference(*symbol_id))
            } else {
                scope.state.new_symbol(type_idx, SemanticKind::Type)
            };
            scope.state.set_symbol(type_idx, type_symbol);

            let fn_idx = scope
                .token_idx(&meta, |t| matches!(t, Token::Identifier(_)))
                .unwrap_or(meta.range.end);
            let fn_symbol = if let Some(symbol_id) = scope.associated_functions.get(&(invoked_ty.clone(), name.clone()))
            {
                scope.state.new_symbol(fn_idx, SemanticKind::Reference(*symbol_id))
            } else {
                scope.state.new_symbol(fn_idx, SemanticKind::Function)
            };
            scope.state.set_symbol(fn_idx, fn_symbol);

            for expr in parameters {
                analyze_expr(context, source_module, expr, scope);
            }
            let mut function_autocomplete = source_module
                .associated_functions
                .iter()
                .filter(|(t, fun)| *t == invoked_ty && fun.name.starts_with(name))
                .map(|(_, fun)| Autocomplete {
                    text: fun.name.clone(),
                    kind: AutocompleteKind::Function(fun.parameters.clone(), fun.return_type.clone()),
                })
                .collect::<Vec<_>>();
            function_autocomplete.extend(
                get_intrinsic_assoc_functions(&invoked_ty)
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
