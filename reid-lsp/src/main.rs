use std::path::PathBuf;

use dashmap::DashMap;
use reid::ast::lexer::{FullToken, Position};
use reid::error_raporting::{self, ErrorModules, ReidError};
use reid::mir::{SourceModuleId, TypeKind};
use reid::parse_module;
use tower_lsp::lsp_types::{
    self, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MarkupContent, MarkupKind, MessageType, OneOf, Range,
    ServerCapabilities, TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc};

use crate::analysis::{StaticAnalysis, analyze};

mod analysis;

#[derive(Debug)]
struct Backend {
    client: Client,
    analysis: DashMap<String, StaticAnalysis>,
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
        let analysis = self.analysis.get(&file_name);
        let position = params.text_document_position_params.position;

        let token = if let Some(analysis) = &analysis {
            analysis.tokens.iter().enumerate().find(|(_, tok)| {
                tok.position.1 == position.line + 1
                    && (tok.position.0 <= position.character + 1
                        && (tok.position.0 + tok.token.len() as u32) > position.character + 1)
            })
        } else {
            None
        };

        let (range, ty) = if let Some((idx, token)) = token {
            if let Some(analysis) = self.analysis.get(&file_name).unwrap().token_analysis.get(&idx) {
                let start = token.position;
                let end = token.position.add(token.token.len() as u32);
                let range = Range {
                    start: lsp_types::Position {
                        line: (start.1 as i32 - 1).max(0) as u32,
                        character: (start.0 as i32 - 1).max(0) as u32,
                    },
                    end: lsp_types::Position {
                        line: (end.1 as i32 - 1).max(0) as u32,
                        character: (end.0 as i32 - 1).max(0) as u32,
                    },
                };
                if let Some(ty) = analysis.ty.clone() {
                    (Some(range), format!("{}", ty))
                } else {
                    (Some(range), String::from("None type"))
                }
            } else {
                (None, String::from("no type"))
            }
        } else {
            (None, String::from("no token"))
        };

        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("`{ty}`"),
        });

        Ok(Some(Hover { contents, range }))
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
            Ok((module_id, tokens)) => (tokens.clone(), analyze(module_id, tokens, path, &mut map)),
            Err(e) => (Vec::new(), Err(e)),
        };

        let mut diagnostics = Vec::new();
        match result {
            Ok(Some(mut analysis)) => {
                if let Some(reid_error) = &mut analysis.error {
                    self.client
                        .log_message(
                            MessageType::INFO,
                            format!("Successfully compiled despite parsing errors!"),
                        )
                        .await;
                    reid_error.errors.dedup();
                    for error in &reid_error.errors {
                        diagnostics.push(reid_error_into_diagnostic(error, &tokens));
                        self.client.log_message(MessageType::INFO, format!("{}", error)).await;
                    }
                }
                self.analysis.insert(file_name.clone(), analysis);
            }
            Ok(_) => {}
            Err(mut reid_error) => {
                reid_error.errors.dedup();
                for error in &reid_error.errors {
                    diagnostics.push(reid_error_into_diagnostic(error, &tokens));
                    self.client.log_message(MessageType::INFO, format!("{}", error)).await;
                }
            }
        }

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
    }
}

fn reid_error_into_diagnostic(error: &error_raporting::ErrorKind, tokens: &Vec<FullToken>) -> Diagnostic {
    let meta = error.get_meta();
    let positions = meta
        .range
        .into_position(&tokens)
        .unwrap_or((Position(0, 0), Position(0, 0)));

    Diagnostic {
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
    }
}

fn parse(source: &str, path: PathBuf, map: &mut ErrorModules) -> Result<(SourceModuleId, Vec<FullToken>), ReidError> {
    let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();

    Ok(parse_module(source, file_name.clone(), map)?)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        analysis: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
