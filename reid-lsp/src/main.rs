use std::collections::HashMap;
use std::path::PathBuf;

use dashmap::DashMap;
use reid::ast::lexer::{FullToken, Position};
use reid::error_raporting::{self, ErrorModules, ReidError};
use reid::mir::SourceModuleId;
use reid::parse_module;
use tower_lsp::lsp_types::{
    self, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentFilter, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability, InitializeParams,
    InitializeResult, InitializedParams, Location, MarkupContent, MarkupKind, MessageType, OneOf, Range,
    ReferenceParams, RenameParams, SemanticToken, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentItem,
    TextDocumentRegistrationOptions, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextEdit, WorkspaceEdit, WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc};

use crate::analysis::{MODIFIER_LEGEND, StaticAnalysis, TOKEN_LEGEND, analyze};

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

        let capabilities = ServerCapabilities {
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
            semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                lsp_types::SemanticTokensRegistrationOptions {
                    text_document_registration_options: TextDocumentRegistrationOptions {
                        document_selector: Some(vec![DocumentFilter {
                            language: Some("reid".to_owned()),
                            scheme: Some("file".to_owned()),
                            pattern: None,
                        }]),
                    },
                    semantic_tokens_options: SemanticTokensOptions {
                        work_done_progress_options: Default::default(),
                        legend: SemanticTokensLegend {
                            token_types: TOKEN_LEGEND.into(),
                            token_modifiers: MODIFIER_LEGEND.into(),
                        },
                        range: None,
                        full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                    },
                    static_registration_options: Default::default(),
                },
            )),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Left(true)),
            ..Default::default()
        };
        Ok(InitializeResult {
            capabilities,
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
        let path = PathBuf::from(params.text_document_position.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let analysis = self.analysis.get(&file_name);
        let position = params.text_document_position.position;

        let token = if let Some(analysis) = &analysis {
            analysis.tokens.iter().enumerate().find(|(_, tok)| {
                tok.position.1 == position.line + 1
                    && (tok.position.0 <= position.character
                        && (tok.position.0 + tok.token.len() as u32) > position.character)
            })
        } else {
            None
        };

        // dbg!(position, token);

        let list = if let Some((idx, _)) = token {
            if let Some(analysis) = self.analysis.get(&file_name).unwrap().state.map.get(&idx) {
                dbg!(&analysis);
                analysis
                    .autocomplete
                    .iter()
                    .map(|s| CompletionItem::new_simple(s.text.to_string(), s.kind.to_string()))
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        // dbg!(&list);
        Ok(Some(CompletionResponse::Array(list)))
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
            if let Some(analysis) = self.analysis.get(&file_name).unwrap().state.map.get(&idx) {
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        let path = PathBuf::from(params.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let analysis = self.analysis.get(&file_name);

        let mut semantic_tokens = Vec::new();
        if let Some(analysis) = analysis {
            let mut prev_line = 0;
            let mut prev_start = 0;
            for (i, token) in analysis.tokens.iter().enumerate() {
                let vscode_line = token.position.1.max(1) - 1;
                let vscode_col = token.position.0.max(1) - 1;

                let delta_line = vscode_line - prev_line;
                let delta_start = if delta_line == 0 {
                    vscode_col - prev_start
                } else {
                    vscode_col
                };

                if let Some(token_analysis) = analysis.state.map.get(&i) {
                    if let Some(symbol_id) = token_analysis.symbol {
                        let symbol = analysis.state.get_symbol(symbol_id);
                        if let Some(idx) = symbol.kind.into_token_idx(&analysis.state) {
                            let semantic_token = SemanticToken {
                                delta_line,
                                delta_start,
                                length: token.token.len() as u32,
                                token_type: idx,
                                token_modifiers_bitset: symbol.kind.get_modifier().unwrap_or(0),
                            };
                            semantic_tokens.push(semantic_token);
                            prev_line = vscode_line;
                            prev_start = vscode_col;
                        }
                    }
                }
            }
        }

        Ok(Some(SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let path = PathBuf::from(params.text_document_position_params.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let analysis = self.analysis.get(&file_name);
        let position = params.text_document_position_params.position;

        if let Some(analysis) = &analysis {
            let token = analysis.tokens.iter().enumerate().find(|(_, tok)| {
                tok.position.1 == position.line + 1
                    && (tok.position.0 <= position.character + 1
                        && (tok.position.0 + tok.token.len() as u32) > position.character + 1)
            });

            if let Some(token) = token {
                if let Some(def_token) = analysis.find_definition(token.0) {
                    return Ok(Some(GotoDefinitionResponse::Scalar(lsp_types::Location {
                        uri: params.text_document_position_params.text_document.uri,
                        range: token_to_range(def_token),
                    })));
                }
            }
        };

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> jsonrpc::Result<Option<Vec<Location>>> {
        let path = PathBuf::from(params.text_document_position.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let analysis = self.analysis.get(&file_name);
        let position = params.text_document_position.position;

        if let Some(analysis) = &analysis {
            let token = analysis.tokens.iter().enumerate().find(|(_, tok)| {
                tok.position.1 == position.line + 1
                    && (tok.position.0 <= position.character + 1
                        && (tok.position.0 + tok.token.len() as u32) > position.character + 1)
            });
            if let Some(token) = token {
                let tokens = analysis.find_references(token.0).map(|symbols| {
                    symbols
                        .iter()
                        .map(|symbol_id| analysis.state.symbol_to_token.get(&symbol_id).cloned().unwrap())
                        .collect::<Vec<_>>()
                });
                let mut locations = Vec::new();
                if let Some(tokens) = tokens {
                    for token_idx in tokens {
                        let token = analysis.tokens.get(token_idx).unwrap();
                        locations.push(Location {
                            uri: params.text_document_position.text_document.uri.clone(),
                            range: token_to_range(token),
                        });
                    }
                }
                Ok(Some(locations))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> jsonrpc::Result<Option<WorkspaceEdit>> {
        let path = PathBuf::from(params.text_document_position.text_document.uri.path());
        let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
        let analysis = self.analysis.get(&file_name);
        let position = params.text_document_position.position;

        if let Some(analysis) = &analysis {
            let token = analysis.tokens.iter().enumerate().find(|(_, tok)| {
                tok.position.1 == position.line + 1
                    && (tok.position.0 <= position.character + 1
                        && (tok.position.0 + tok.token.len() as u32) > position.character + 1)
            });
            if let Some(token) = token {
                let tokens = analysis.find_references(token.0).map(|symbols| {
                    symbols
                        .iter()
                        .map(|symbol_id| analysis.state.symbol_to_token.get(&symbol_id).cloned().unwrap())
                        .collect::<Vec<_>>()
                });
                let mut edits = Vec::new();
                if let Some(tokens) = tokens {
                    for token_idx in tokens {
                        let token = analysis.tokens.get(token_idx).unwrap();
                        edits.push(TextEdit {
                            range: token_to_range(token),
                            new_text: params.new_name.clone(),
                        });
                    }
                }
                let mut changes = HashMap::new();
                changes.insert(params.text_document_position.text_document.uri, edits);
                Ok(Some(WorkspaceEdit {
                    changes: Some(changes),
                    document_changes: None,
                    change_annotations: None,
                }))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

fn token_to_range(token: &FullToken) -> lsp_types::Range {
    Range {
        start: lsp_types::Position {
            line: token.position.1.max(1) - 1,
            character: token.position.0.max(1) - 1,
        },
        end: lsp_types::Position {
            line: token.position.1.max(1) - 1,
            character: token.position.0.max(1) - 1 + token.token.len() as u32,
        },
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
