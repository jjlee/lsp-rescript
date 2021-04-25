[lsp-mode](https://emacs-lsp.github.io/lsp-mode/) client configuration for
[rescript-vscode](https://github.com/rescript-lang/rescript-vscode) LSP server
for the [ReScript Language](https://rescript-lang.org/).

For documentation see https://github.com/jjlee/rescript-mode

The server provides type information, compiler errors, completion, and code
formatting (currently there's an `lsp-mode` bug that prevents formatting
working).

This is [experimental](https://github.com/jjlee/rescript-mode#support) and Emacs
is NOT SUPPORTED by the ReScript core team and the --stdio flag that this
depends on, or the LSP server itself, may go away in a future release of
rescript-vscode.
