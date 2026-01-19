;;; mcp-server-emacs-tools-eval-elisp.el --- Eval Elisp MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for executing arbitrary Elisp expressions.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-security)

(defun mcp-server-emacs-tools--eval-elisp-handler (args)
  "Handle eval-elisp tool invocation with ARGS."
  (let ((expression (alist-get 'expression args)))
    (condition-case err
        (let ((form (read-from-string expression)))
          (format "%S" (mcp-server-security-safe-eval (car form))))
      (error (format "Error: %s" (error-message-string err))))))

;; Register tool on load
(mcp-server-register-tool
 (make-mcp-server-tool
  :name "eval-elisp"
  :title "Execute Elisp Expression"
  :description "Execute arbitrary Elisp code and return the result."
  :input-schema '((type . "object")
                  (properties . ((expression . ((type . "string")
                                                (description . "The Elisp expression to evaluate")))))
                  (required . ["expression"]))
  :function #'mcp-server-emacs-tools--eval-elisp-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-eval-elisp)

;;; mcp-server-emacs-tools-eval-elisp.el ends here
