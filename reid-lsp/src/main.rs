use std::collections::HashMap;
use std::path::PathBuf;

use dashmap::DashMap;
use reid::ast::lexer::{FullToken, Position};
use reid::error_raporting::{ErrorModules, ReidError};
use reid::mir::{
    self, Context, FunctionCall, FunctionDefinition, FunctionParam, IfExpression, SourceModuleId, StructType, TypeKind,
    WhileStatement,
};
use reid::{compile_module, parse_module, perform_all_passes};
use tower_lsp::lsp_types::{
    self, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MarkedString, MessageType, OneOf, Range, ServerCapabilities,
    TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc};

#[derive(Debug)]
struct Backend {
    client: Client,
    tokens: DashMap<String, Vec<FullToken>>,
    ast: DashMap<String, reid::ast::Module>,
    types: DashMap<String, DashMap<FullToken, Option<TypeKind>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Initializing Reid Language Server")
            .await;

        let sync = TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::FULL),
            will_save: None,
            will_save_wait_until: None,
            save: None,
        };
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions { ..Default::default() }),
                text_document_sync: Some(TextDocumentSyncCapability::Options(sync)),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Reid Language Server initialized hello!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn completion(&self, params: CompletionParams) -> jsonrpc::Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let path = PathBuf::from(params.text_document_position_params.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let tokens = self.tokens.get(&file_name);
        let position = params.text_document_position_params.position;

        let token = if let Some(tokens) = &tokens {
            tokens.iter().find(|tok| {
                tok.position.1 == position.line + 1
                    && (tok.position.0 <= position.character + 1
                        && (tok.position.0 + tok.token.len() as u32) > position.character + 1)
            })
        } else {
            None
        };

        let ty = if let Some(token) = token {
            if let Some(possible_ty) = self.types.get(&file_name).unwrap().get(token) {
                if let Some(ty) = possible_ty.clone() {
                    format!("{}", ty)
                } else {
                    String::from("no type")
                }
            } else {
                String::from("no token")
            }
        } else {
            String::from("no token")
        };

        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(format!("{}", ty))),
            range: None,
        }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.recompile(TextDocumentItem {
            uri: params.text_document.uri,
            language_id: params.text_document.language_id,
            version: params.text_document.version,
            text: params.text_document.text,
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.recompile(TextDocumentItem {
            text: params.content_changes[0].text.clone(),
            uri: params.text_document.uri,
            version: params.text_document.version,
            language_id: String::new(),
        })
        .await
    }
}

impl Backend {
    async fn recompile(&self, params: TextDocumentItem) {
        let path = PathBuf::from(params.uri.clone().path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();

        let mut map = Default::default();
        let parse_res = parse(&params.text, path.clone(), &mut map);
        let (tokens, result) = match parse_res {
            Ok((module_id, tokens)) => (tokens.clone(), compile(module_id, tokens, path, &mut map)),
            Err(e) => (Vec::new(), Err(e)),
        };

        let mut diagnostics = Vec::new();
        match result {
            Ok(Some(result)) => {
                self.tokens.insert(file_name.clone(), result.tokens);
                self.types.insert(file_name.clone(), result.types);
            }
            Ok(_) => {}
            Err(mut reid_error) => {
                reid_error.errors.dedup();
                for error in reid_error.errors {
                    let meta = error.get_meta();
                    let positions = meta
                        .range
                        .into_position(&tokens)
                        .unwrap_or((Position(0, 0), Position(0, 0)));
                    self.client.log_message(MessageType::INFO, format!("{:?}", &meta)).await;
                    self.client
                        .log_message(MessageType::INFO, format!("{:?}", &positions))
                        .await;

                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: lsp_types::Position {
                                line: ((positions.0.1 as i32) - 1).max(0) as u32,
                                character: ((positions.0.0 as i32) - 1).max(0) as u32,
                            },
                            end: lsp_types::Position {
                                line: ((positions.1.1 as i32) - 1).max(0) as u32,
                                character: ((positions.1.0 as i32) - 1).max(0) as u32,
                            },
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some(error.get_type_str().to_owned()),
                        message: format!("{}", error),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                    self.client.log_message(MessageType::INFO, format!("{}", error)).await;
                }
            }
        }

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
    }
}

struct CompileResult {
    tokens: Vec<FullToken>,
    types: DashMap<FullToken, Option<TypeKind>>,
}

fn parse(source: &str, path: PathBuf, map: &mut ErrorModules) -> Result<(SourceModuleId, Vec<FullToken>), ReidError> {
    let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();

    Ok(parse_module(source, file_name.clone(), map)?)
}

fn compile(
    module_id: SourceModuleId,
    tokens: Vec<FullToken>,
    path: PathBuf,
    map: &mut ErrorModules,
) -> Result<Option<CompileResult>, ReidError> {
    let token_types = DashMap::new();

    let module = compile_module(module_id, tokens, map, Some(path.clone()), true)?;

    let module_id = module.module_id;
    let mut context = Context::from(vec![module], path.parent().unwrap().to_owned());
    perform_all_passes(&mut context, map)?;

    for module in context.modules.into_values() {
        if module.module_id != module_id {
            continue;
        }
        for (idx, token) in module.tokens.iter().enumerate() {
            token_types.insert(token.clone(), find_type_in_context(&module, idx));
        }

        return Ok(Some(CompileResult {
            tokens: module.tokens,
            types: token_types,
        }));
    }
    return Ok(None);
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        ast: DashMap::new(),
        tokens: DashMap::new(),
        types: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

pub fn find_type_in_context(module: &mir::Module, token_idx: usize) -> Option<TypeKind> {
    for import in &module.imports {
        if import.1.contains(token_idx) {
            return None;
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
        return None;
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
        mir::ExprKind::BinOp(binary_operator, lhs, rhs, type_kind) => {
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
                None
            }
        }
        mir::ExprKind::CastTo(expression, type_kind) => {
            Some(find_type_in_expr(&expression, module_id, token_idx).unwrap_or(type_kind.clone()))
        }
        mir::ExprKind::GlobalRef(_, type_kind) => Some(type_kind.clone()),
    }
}
