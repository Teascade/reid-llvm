/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';

import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';


let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const configuration = workspace.getConfiguration('reid-language-server');
	let server_path: string = process.env.SERVER_PATH ?? configuration.get("language-server-path") ?? 'reid-language-server';
	const regex = /\$(\w+)/;
	while (regex.test(server_path)) {
		let envVar = regex.exec(server_path)?.[1];
		const envVal = envVar ? process.env[envVar] : undefined;
		if (envVar === undefined || envVal === undefined) {
			console.error(`No such environment variables as ${envVar}`);
		}
		server_path = server_path.replaceAll(`$${envVar}`, envVal ?? '');
	}

	const run: Executable = {
		command: server_path,
		options: {
			env: {
				...process.env,
				RUST_LOG: "debug",
				RUST_BACKTRACE: 1,
			}
		}
	};

	const serverOptions: ServerOptions = {
		run,
		debug: run,
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'reid' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'reid-language-server',
		'Reid Language Server',
		serverOptions,
		clientOptions
	);
	client.info(JSON.stringify(server_path));

	client.info(`Loaded Reid Language Server from ${server_path}`);


	workspace.onDidOpenTextDocument((e) => {
	});

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
