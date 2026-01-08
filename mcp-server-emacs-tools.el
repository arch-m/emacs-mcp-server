;;; mcp-server-emacs-tools.el --- Emacs-specific MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module loads Emacs-specific MCP tools from the tools/ directory.
;; Each tool self-registers when loaded via `mcp-server-register-tool'.

;;; Code:

(require 'mcp-server-tools)

;; Add tools directory to load path
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "tools" (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))

;; Load tool modules (each self-registers on load)
(require 'mcp-server-emacs-tools-eval-elisp)
(require 'mcp-server-emacs-tools-diagnostics)

(defun mcp-server-emacs-tools-register ()
  "Register Emacs MCP tools.
This is a no-op kept for backward compatibility.
Tools now self-register when their modules are loaded."
  nil)

(provide 'mcp-server-emacs-tools)

;;; mcp-server-emacs-tools.el ends here
