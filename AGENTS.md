# Repository Guidelines

## Project Structure & Module Organization
`mcp-server.el` is the entry point and owns server lifecycle. Transport abstractions live in `mcp-server-transport.el`, `mcp-server-transport-unix.el`, and `mcp-server-transport-tcp.el`. Tool registration and dispatch live in `mcp-server-tools.el`; tool loading lives in `mcp-server-emacs-tools.el`; concrete tool implementations belong in `tools/*.el`. Security and permission checks are centralized in `mcp-server-security.el`. Tests live under `test/`: `unit/` for ERT files, `integration/` for socket-level shell tests, `scripts/` for runners and sample clients, `fixtures/` for shared helpers, and `config/` for test-only setup. `demo/` contains README GIF assets.

## Build, Test, and Development Commands
- `make test` runs the full suite: batch ERT plus integration tests.
- `make test-unit` runs unit tests in batch Emacs.
- `make test-integration` runs `test/scripts/test-runner.sh`; it expects `socat` and `python3`.
- `make test-file FILE=test/unit/test-mcp-server-full.el` runs one ERT file.
- `make test-pattern PATTERN=jsonrpc` runs tests matching a name pattern.
- `emacs --batch -L . --eval "(byte-compile-file \"mcp-server.el\")"` matches the CI byte-compilation check.

## Coding Style & Naming Conventions
Use idiomatic Emacs Lisp with `lexical-binding: t` and standard Elisp indentation. Public symbols should use the `mcp-server-` prefix; module-private helpers use `--`, for example `mcp-server--log`. Use `defcustom` for user-facing settings and `defvar` for internal state. Prefer `string=` for string equality. Public functions need docstrings. When emitting JSON, use `t` and `:false`; do not use `:json-false`.

## Testing Guidelines
Write unit tests with `ert-deftest`. Keep filenames in `test/unit/` as `test-*.el`, and prefer descriptive test names such as `mcp-test-*`. Add unit coverage for behavior changes, and update integration coverage when modifying transport, protocol handling, socket lifecycle, or security-sensitive flows. Reuse helpers from `test/fixtures/test-helpers.el` to keep tests isolated.

## Commit & Pull Request Guidelines
Recent history mostly follows Conventional Commits with optional scopes, for example `feat(server): ...`, `fix(transport-unix): ...`, `docs: ...`, and `refactor(security): ...`. Keep commit subjects imperative and focused. Pull requests should summarize the behavior change, note any security or protocol impact, link the related issue, and list the commands you ran, such as `make test-unit` and `make test-integration`.

## Security & Compatibility
Changes around `eval-elisp`, file access, permissions, or transport code need extra scrutiny and tests. Keep compatibility with Emacs 27.1+ even though CI currently exercises newer releases.
