;;; test-mcp-emacs-tools.el --- Tests for Emacs MCP Tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the Emacs-specific MCP tools: eval-elisp and get-diagnostics.

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Load MCP server modules
(require 'mcp-server-tools)
(require 'mcp-server-security)

;; Load tool modules (they self-register on require)
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools" (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))
(require 'mcp-server-emacs-tools-eval-elisp)
(require 'mcp-server-emacs-tools-diagnostics)

;;; mcp-server-register-tool Tests

(ert-deftest mcp-test-register-tool-basic ()
  "Test basic tool registration with mcp-server-register-tool."
  (mcp-test-with-mock-server
   (let ((tool (mcp-server-register-tool
                (make-mcp-server-tool
                 :name "test-tool"
                 :title "Test Tool"
                 :description "A test tool"
                 :input-schema '((type . "object"))
                 :function (lambda (args) "result")))))
     (should tool)
     (should (mcp-server-tools-exists-p "test-tool"))
     (should (equal (mcp-server-tool-name tool) "test-tool"))
     (should (equal (mcp-server-tool-title tool) "Test Tool")))))

(ert-deftest mcp-test-register-tool-requires-struct ()
  "Test that mcp-server-register-tool requires a proper struct."
  (mcp-test-with-mock-server
   (should-error (mcp-server-register-tool "not-a-struct"))
   (should-error (mcp-server-register-tool '((name . "test"))))))

(ert-deftest mcp-test-register-tool-requires-name ()
  "Test that mcp-server-register-tool requires a name."
  (mcp-test-with-mock-server
   (should-error
    (mcp-server-register-tool
     (make-mcp-server-tool
      :title "No Name Tool"
      :description "Missing name"
      :function (lambda (args) nil))))))

(ert-deftest mcp-test-register-tool-overwrites ()
  "Test that registering a tool with same name overwrites."
  (mcp-test-with-mock-server
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "overwrite-test"
     :title "Original"
     :description "Original description"
     :function (lambda (args) "original")))
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "overwrite-test"
     :title "Updated"
     :description "Updated description"
     :function (lambda (args) "updated")))
   (let ((tool (mcp-server-tools-get "overwrite-test")))
     (should (equal (mcp-server-tool-title tool) "Updated")))))

;;; eval-elisp Tool Tests

(ert-deftest mcp-test-eval-elisp-simple-expression ()
  "Test eval-elisp with a simple arithmetic expression."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "(+ 1 2 3)")))))
     (should (equal result "6")))))

(ert-deftest mcp-test-eval-elisp-string-result ()
  "Test eval-elisp returning a string."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "\"hello world\"")))))
     (should (equal result "\"hello world\"")))))

(ert-deftest mcp-test-eval-elisp-list-result ()
  "Test eval-elisp returning a list."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "'(1 2 3)")))))
     (should (equal result "(1 2 3)")))))

(ert-deftest mcp-test-eval-elisp-nil-result ()
  "Test eval-elisp returning nil."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "nil")))))
     (should (equal result "nil")))))

(ert-deftest mcp-test-eval-elisp-buffer-operations ()
  "Test eval-elisp with buffer operations."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "(with-temp-buffer (insert \"test\") (buffer-string))")))))
     (should (equal result "\"test\"")))))

(ert-deftest mcp-test-eval-elisp-syntax-error ()
  "Test eval-elisp handles syntax errors gracefully."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "(+ 1 2")))))
     (should (string-prefix-p "Error:" result)))))

(ert-deftest mcp-test-eval-elisp-runtime-error ()
  "Test eval-elisp handles runtime errors gracefully."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                  '((expression . "(/ 1 0)")))))
     (should (string-prefix-p "Error:" result)))))

;;; get-diagnostics Tool Tests

(ert-deftest mcp-test-diagnostics-empty-result ()
  "Test get-diagnostics returns proper structure when no diagnostics."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--get-diagnostics)))
     (should (assq 'summary result))
     (should (assq 'files result))
     (let ((summary (alist-get 'summary result)))
       (should (assq 'total summary))
       (should (assq 'errors summary))
       (should (assq 'warnings summary))
       (should (assq 'info summary))))))

(ert-deftest mcp-test-diagnostics-handler-returns-json ()
  "Test get-diagnostics handler returns valid JSON."
  (mcp-test-with-mock-server
   (let ((result (mcp-server-emacs-tools--diagnostics-handler '())))
     (should (stringp result))
     ;; Should be valid JSON
     (let ((parsed (json-read-from-string result)))
       (should (assq 'summary parsed))
       (should (assq 'files parsed))))))

(ert-deftest mcp-test-diagnostics-severity-filter ()
  "Test get-diagnostics respects severity filter parameter."
  (mcp-test-with-mock-server
   ;; This tests the filtering logic without actual flycheck errors
   (let ((result (mcp-server-emacs-tools--get-diagnostics nil "error")))
     (should (assq 'summary result))
     ;; With no buffers having errors, should return empty
     (should (equal (alist-get 'total (alist-get 'summary result)) 0)))))

(ert-deftest mcp-test-diagnostics-file-path-filter ()
  "Test get-diagnostics respects file_path filter parameter."
  (mcp-test-with-mock-server
   ;; Test with a non-existent file
   (let ((result (mcp-server-emacs-tools--get-diagnostics "/nonexistent/file.el")))
     (should (assq 'summary result))
     (should (equal (alist-get 'total (alist-get 'summary result)) 0)))))

;;; Diagnostics Helper Function Tests

(ert-deftest mcp-test-diagnostics-severity-priority ()
  "Test severity priority ordering."
  (should (< (mcp-server-emacs-tools--severity-priority "error")
             (mcp-server-emacs-tools--severity-priority "warning")))
  (should (< (mcp-server-emacs-tools--severity-priority "warning")
             (mcp-server-emacs-tools--severity-priority "info")))
  (should (= (mcp-server-emacs-tools--severity-priority "info")
             (mcp-server-emacs-tools--severity-priority "unknown"))))

(ert-deftest mcp-test-diagnostics-count-by-severity ()
  "Test counting diagnostics by severity."
  (let ((diags '(((severity . "error") (message . "e1"))
                 ((severity . "error") (message . "e2"))
                 ((severity . "warning") (message . "w1"))
                 ((severity . "info") (message . "i1")))))
    (let ((counts (mcp-server-emacs-tools--count-by-severity diags)))
      (should (equal (alist-get 'errors counts) 2))
      (should (equal (alist-get 'warnings counts) 1))
      (should (equal (alist-get 'info counts) 1)))))

(ert-deftest mcp-test-diagnostics-sort-by-severity ()
  "Test diagnostics are sorted by severity then line."
  (let* ((diags '(((severity . "warning") (line . 10))
                  ((severity . "error") (line . 20))
                  ((severity . "info") (line . 5))
                  ((severity . "error") (line . 10))))
         (sorted (mcp-server-emacs-tools--sort-diagnostics diags)))
    ;; First two should be errors (sorted by line)
    (should (equal (alist-get 'severity (nth 0 sorted)) "error"))
    (should (equal (alist-get 'line (nth 0 sorted)) 10))
    (should (equal (alist-get 'severity (nth 1 sorted)) "error"))
    (should (equal (alist-get 'line (nth 1 sorted)) 20))
    ;; Then warning
    (should (equal (alist-get 'severity (nth 2 sorted)) "warning"))
    ;; Then info
    (should (equal (alist-get 'severity (nth 3 sorted)) "info"))))

;;; Tool Registration Verification

(ert-deftest mcp-test-eval-elisp-registered ()
  "Test that eval-elisp tool is properly registered."
  (should (mcp-server-tools-exists-p "eval-elisp"))
  (let ((tool (mcp-server-tools-get "eval-elisp")))
    (should tool)
    (should (equal (mcp-server-tool-name tool) "eval-elisp"))
    (should (mcp-server-tool-function tool))
    (should (mcp-server-tool-input-schema tool))))

(ert-deftest mcp-test-diagnostics-registered ()
  "Test that get-diagnostics tool is properly registered."
  (should (mcp-server-tools-exists-p "get-diagnostics"))
  (let ((tool (mcp-server-tools-get "get-diagnostics")))
    (should tool)
    (should (equal (mcp-server-tool-name tool) "get-diagnostics"))
    (should (mcp-server-tool-function tool))
    (should (mcp-server-tool-input-schema tool))))

;;; Tool Filtering Tests

(ert-deftest mcp-test-tools-filter-hides-disabled ()
  "Test that disabled tools are hidden from tools-list."
  (mcp-test-with-mock-server
   ;; Register a test tool
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "filter-test"
     :title "Filter Test"
     :description "Tool for testing filtering"
     :input-schema '((type . "object"))
     :function (lambda (args) "result")))
   ;; With no filter, tool should be listed
   (let ((mcp-server-tools-filter nil))
     (should (cl-find "filter-test" (mcp-server-tools-list)
                      :key (lambda (t) (alist-get 'name t)) :test #'equal)))
   ;; With filter that excludes it, tool should not be listed
   (let ((mcp-server-tools-filter (lambda (name) (not (equal name "filter-test")))))
     (should-not (cl-find "filter-test" (mcp-server-tools-list)
                          :key (lambda (t) (alist-get 'name t)) :test #'equal)))))

(ert-deftest mcp-test-tools-filter-blocks-call ()
  "Test that disabled tools cannot be called."
  (mcp-test-with-mock-server
   ;; Register a test tool
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "call-filter-test"
     :title "Call Filter Test"
     :description "Tool for testing call filtering"
     :input-schema '((type . "object"))
     :function (lambda (args) "success")))
   ;; With no filter, tool can be called
   (let ((mcp-server-tools-filter nil))
     (should (mcp-server-tools-call "call-filter-test" '())))
   ;; With filter that excludes it, call should error
   (let ((mcp-server-tools-filter (lambda (name) (not (equal name "call-filter-test")))))
     (should-error (mcp-server-tools-call "call-filter-test" '())))))

(provide 'test-mcp-emacs-tools)
;;; test-mcp-emacs-tools.el ends here
