;;; mcp-server-emacs-tools-diagnostics.el --- Diagnostics MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for retrieving flycheck/flymake diagnostics from project buffers.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)
(require 'json)
(require 'seq)

;; Declare external functions to silence byte-compiler warnings
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-end-line "flycheck" (err))
(declare-function flycheck-error-end-column "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-id "flycheck" (err))
(declare-function flycheck-error-checker "flycheck" (err))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function project-buffers "project" (project))
(declare-function project-current "project" (&optional maybe-prompt directory))

(defun mcp-server-emacs-tools--flycheck-error-to-alist (err)
  "Convert flycheck error ERR to an alist."
  `((line . ,(flycheck-error-line err))
    (column . ,(flycheck-error-column err))
    (end_line . ,(flycheck-error-end-line err))
    (end_column . ,(flycheck-error-end-column err))
    (message . ,(flycheck-error-message err))
    (severity . ,(symbol-name (flycheck-error-level err)))
    (id . ,(flycheck-error-id err))
    (source . ,(symbol-name (flycheck-error-checker err)))))

(defun mcp-server-emacs-tools--flymake-diag-to-alist (diag)
  "Convert flymake diagnostic DIAG to an alist."
  (let* ((beg (flymake-diagnostic-beg diag))
         (end (flymake-diagnostic-end diag))
         (type (flymake-diagnostic-type diag))
         (severity (cond
                    ((memq type '(:error flymake-error)) "error")
                    ((memq type '(:warning flymake-warning)) "warning")
                    (t "info"))))
    `((line . ,(line-number-at-pos beg))
      (column . ,(save-excursion (goto-char beg) (current-column)))
      (end_line . ,(line-number-at-pos end))
      (end_column . ,(save-excursion (goto-char end) (current-column)))
      (message . ,(flymake-diagnostic-text diag))
      (severity . ,severity)
      (id . nil)
      (source . ,(format "%s" (flymake-diagnostic-backend diag))))))

(defun mcp-server-emacs-tools--get-buffer-diagnostics (buf)
  "Get diagnostics from buffer BUF, auto-detecting flycheck or flymake."
  (with-current-buffer buf
    (cond
     ;; Flycheck active
     ((and (bound-and-true-p flycheck-mode)
           (bound-and-true-p flycheck-current-errors))
      (mapcar #'mcp-server-emacs-tools--flycheck-error-to-alist
              flycheck-current-errors))
     ;; Flymake active
     ((and (bound-and-true-p flymake-mode)
           (fboundp 'flymake-diagnostics))
      (mapcar #'mcp-server-emacs-tools--flymake-diag-to-alist
              (flymake-diagnostics)))
     (t nil))))

(defun mcp-server-emacs-tools--severity-priority (severity)
  "Return sort priority for SEVERITY (lower = higher priority)."
  (pcase severity
    ("error" 0)
    ("warning" 1)
    (_ 2)))

(defun mcp-server-emacs-tools--sort-diagnostics (diags)
  "Sort DIAGS by severity (errors first), then by line number."
  (sort diags
        (lambda (a b)
          (let ((sev-a (mcp-server-emacs-tools--severity-priority (alist-get 'severity a)))
                (sev-b (mcp-server-emacs-tools--severity-priority (alist-get 'severity b))))
            (if (= sev-a sev-b)
                (< (or (alist-get 'line a) 0) (or (alist-get 'line b) 0))
              (< sev-a sev-b))))))

(defun mcp-server-emacs-tools--count-by-severity (diags)
  "Count diagnostics in DIAGS by severity."
  (let ((errors 0) (warnings 0) (info 0))
    (dolist (d diags)
      (pcase (alist-get 'severity d)
        ("error" (cl-incf errors))
        ("warning" (cl-incf warnings))
        (_ (cl-incf info))))
    `((errors . ,errors) (warnings . ,warnings) (info . ,info))))

(defun mcp-server-emacs-tools--get-diagnostics (&optional file-path severity-filter)
  "Get diagnostics grouped by file.
If FILE-PATH is given, only return diagnostics for that file.
SEVERITY-FILTER can be \"error\", \"warning\", \"info\", or nil for all."
  (let ((result-files '())
        (total-errors 0)
        (total-warnings 0)
        (total-info 0)
        (buffers (if file-path
                     (let ((buf (get-file-buffer file-path)))
                       (if buf (list buf) nil))
                   (when-let ((proj (project-current)))
                     (seq-filter #'buffer-file-name (project-buffers proj))))))
    (dolist (buf buffers)
      (when-let* ((file (buffer-file-name buf))
                  (diags (mcp-server-emacs-tools--get-buffer-diagnostics buf)))
        (when severity-filter
          (setq diags (seq-filter
                       (lambda (d) (equal (alist-get 'severity d) severity-filter))
                       diags)))
        (when diags
          (let* ((sorted (mcp-server-emacs-tools--sort-diagnostics diags))
                 (counts (mcp-server-emacs-tools--count-by-severity sorted)))
            (cl-incf total-errors (alist-get 'errors counts))
            (cl-incf total-warnings (alist-get 'warnings counts))
            (cl-incf total-info (alist-get 'info counts))
            (push `(,file . ((error_count . ,(alist-get 'errors counts))
                             (warning_count . ,(alist-get 'warnings counts))
                             (info_count . ,(alist-get 'info counts))
                             (diagnostics . ,(vconcat sorted))))
                  result-files)))))
    `((summary . ((total . ,(+ total-errors total-warnings total-info))
                  (errors . ,total-errors)
                  (warnings . ,total-warnings)
                  (info . ,total-info)))
      (files . ,result-files))))

(defun mcp-server-emacs-tools--diagnostics-handler (args)
  "Handle get-diagnostics tool invocation with ARGS."
  (condition-case err
      (let ((file-path (alist-get 'file_path args))
            (severity (alist-get 'severity args)))
        (json-encode (mcp-server-emacs-tools--get-diagnostics file-path severity)))
    (error (json-encode `((error . ,(error-message-string err)))))))

;; Register tool on load
(mcp-server-register-tool
 (make-mcp-server-tool
  :name "get-diagnostics"
  :title "Get Diagnostics"
  :description "Get flycheck/flymake diagnostics from project buffers. Returns errors and warnings grouped by file, sorted by severity."
  :input-schema '((type . "object")
                  (properties . ((file_path . ((type . "string")
                                               (description . "Optional file path to get diagnostics for a specific file only")))
                                 (severity . ((type . "string")
                                              (enum . ("error" "warning" "info"))
                                              (description . "Optional filter by severity level")))))
                  (required . []))
  :function #'mcp-server-emacs-tools--diagnostics-handler))

(provide 'mcp-server-emacs-tools-diagnostics)

;;; mcp-server-emacs-tools-diagnostics.el ends here
