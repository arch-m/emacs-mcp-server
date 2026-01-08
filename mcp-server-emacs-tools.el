;;; mcp-server-emacs-tools.el --- Emacs-specific MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module loads Emacs-specific MCP tools from the tools/ directory.
;; Tools self-register on require.  Use `mcp-server-emacs-tools-enabled'
;; to control which tools are exposed to LLM clients at runtime.

;;; Code:

(require 'mcp-server-tools)

(defgroup mcp-server-emacs-tools nil
  "Emacs-specific MCP tools configuration."
  :group 'mcp-server
  :prefix "mcp-server-emacs-tools-")

(defcustom mcp-server-emacs-tools-enabled 'all
  "Which MCP tools to enable.
Can be `all' to enable all available tools, or a list of tool
names (symbols) to enable selectively.

Available tools:
- `eval-elisp' - Execute arbitrary Elisp expressions
- `get-diagnostics' - Get flycheck/flymake diagnostics

Example: \\='(get-diagnostics) to enable only diagnostics.

Changes take effect immediately - disabled tools are hidden from
LLM clients and cannot be called."
  :type '(choice (const :tag "All tools" all)
                 (repeat :tag "Selected tools" symbol))
  :group 'mcp-server-emacs-tools)

(defconst mcp-server-emacs-tools--available
  '((eval-elisp . mcp-server-emacs-tools-eval-elisp)
    (get-diagnostics . mcp-server-emacs-tools-diagnostics))
  "Alist mapping tool names (symbols) to their feature names.")

;; Add tools directory to load path
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "tools" (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))

(defun mcp-server-emacs-tools--tool-enabled-p (tool-name)
  "Return non-nil if TOOL-NAME is enabled.
TOOL-NAME can be a string or symbol."
  (let ((name-sym (if (stringp tool-name) (intern tool-name) tool-name)))
    (or (eq mcp-server-emacs-tools-enabled 'all)
        (memq name-sym mcp-server-emacs-tools-enabled))))

;; Set up the filter for mcp-server-tools
(setq mcp-server-tools-filter #'mcp-server-emacs-tools--tool-enabled-p)

;; Load all tool modules (they self-register)
(dolist (tool-spec mcp-server-emacs-tools--available)
  (require (cdr tool-spec)))

(provide 'mcp-server-emacs-tools)

;;; mcp-server-emacs-tools.el ends here
