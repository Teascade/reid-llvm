use std::path::PathBuf;

use dashmap::DashMap;
use reid::ast::lexer::{FullToken, Position};
use reid::{compile_module, parse_module};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    self, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MarkedString, MessageType, OneOf, Range, ServerCapabilities,
    TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    tokens: DashMap<String, Vec<FullToken>>,
    ast: DashMap<String, reid::ast::Module>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let path = PathBuf::from(params.text_document_position_params.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let tokens = self.tokens.get(&file_name).unwrap();
        let position = params.text_document_position_params.position;

        self.client
            .log_message(
                MessageType::INFO,
                format!("line {}, col {}", position.line, position.character),
            )
            .await;
        let token = tokens.iter().find(|tok| {
            tok.position.1 == position.line + 1
                && (tok.position.0 <= position.character + 1
                    && (tok.position.0 + tok.token.len() as u32) > position.character + 1)
        });

        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(format!("{:?}", token))),
            range: None,
        }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "opened!").await;
        self.recompile(TextDocumentItem {
            uri: params.text_document.uri,
            language_id: params.text_document.language_id,
            version: params.text_document.version,
            text: params.text_document.text,
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "changed!").await;
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
        let mut map = Default::default();
        let path = PathBuf::from(params.uri.clone().path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();

        let mut reid_error = None;
        let mut tokens = None;

        match parse_module(&params.text, file_name.clone(), &mut map) {
            Ok(module) => {
                self.client
                    .log_message(MessageType::INFO, format!("successfully parsed!"))
                    .await;
                tokens = Some(module.1.clone());
                match compile_module(module.0, module.1, &mut map, Some(path), true) {
                    Ok(_) => {}
                    Err(e) => {
                        reid_error = Some(e);
                    }
                }
            }
            Err(_) => {}
        }

        if let Some(tokens) = &tokens {
            if let Some(reid_error) = reid_error {
                let mut diagnostics = Vec::new();
                for error in reid_error.errors {
                    let meta = error.get_meta();
                    let positions = meta
                        .range
                        .into_position(&tokens)
                        .unwrap_or((Position(0, 0), Position(0, 0)));
                    self.client.log_message(MessageType::INFO, format!("{:?}", &meta)).await;
                    self.client
                        .log_message(MessageType::INFO, format!("{:?}", &tokens))
                        .await;
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
                self.client
                    .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
                    .await;
            } else {
                self.client
                    .publish_diagnostics(params.uri.clone(), Vec::new(), Some(params.version))
                    .await;
            }
        }

        if let Some(tokens) = tokens.take() {
            self.tokens.insert(file_name.clone(), tokens);
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        ast: DashMap::new(),
        tokens: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
