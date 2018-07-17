'use strict';

import {
    workspace,
    ExtensionContext
  } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
  } from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const serverPath = 'eta-lsp';
    const serverOptions: ServerOptions = {
        run: { command: serverPath, transport: TransportKind.stdio, args: ['--lsp'] },
        debug: { command: serverPath, transport: TransportKind.stdio, args: ['--lsp'] }
    };
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
          { scheme: 'file', language: 'eta' }
        ],
        synchronize: {
          configurationSection: 'languageServerEta',
          fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    const langName = 'Eta';
    const langClient = new LanguageClient(langName, langName, serverOptions, clientOptions, true);
    langClient.start();
}

export function deactivate(): Thenable<void> {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
