;;; peteches-go-ts.el --- Go tree-sitter helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Helper functions for snippets and commands that inspect Go syntax trees.
;;; Load this from `go-ts-mode' buffers so startup does not depend on the Go
;;; grammar being available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'peteches-treesit)

(defconst peteches/go-ts-query-get-func-return-types-str
  "
  [
          (function_declaration)
          (method_declaration)
  ] result: (_) @type
  [
          (function_declaration)
          (method_declaration)
  ] result: (
          parameter_list (
          parameter_declaration type: (_) @type_list
      )
  )
  "
  "Tree-sitter query used to find Go function return types.")

(defvar peteches/go-ts-query-get-func-return-types nil
  "Compiled tree-sitter query for Go function return types.")

(defun peteches/go-ts--ensure-return-type-query ()
  "Return the compiled Go return-type query, compiling it if necessary."
  (when (and (null peteches/go-ts-query-get-func-return-types)
             (peteches/treesit-language-ready-p 'go)
             (treesit-query-validate 'go peteches/go-ts-query-get-func-return-types-str))
    (setq peteches/go-ts-query-get-func-return-types
          (treesit-query-compile 'go peteches/go-ts-query-get-func-return-types-str)))
  peteches/go-ts-query-get-func-return-types)

(defun peteches/go-ts-get-struct-fieldname (&optional p)
  "Use tree-sitter to get the struct field name at P, or point."
  (let* ((point (or p (point)))
         (node (treesit-node-at point))
         (parent (and node (treesit-node-parent node)))
         (name (and parent (treesit-node-child-by-field-name parent "name"))))
    (when name
      (treesit-node-text name))))

(defun peteches/go-ts-get-func-node (&optional p)
  "Use tree-sitter to get the surrounding Go function node at P, or point."
  (let ((point (or p (point))))
    (treesit-parent-until
     (treesit-node-at point)
     (lambda (node)
       (member (treesit-node-type node)
               '("function_declaration" "method_declaration"))))))

;; Liberated from https://blog.meain.io/2021/intelligent-snippets-treesitter/
(defun peteches/go-ts-initialise-zero-value (type &optional place-num expand-error)
  "Return an initialised zero value of Go TYPE.
PLACE-NUM is the yasnippet placeholder number, defaulting to 1.
If EXPAND-ERROR is non-nil, initialise errors with `fmt.Errorf'."
  (setq place-num (or place-num 1))
  (when (treesit-node-p type)
    (setq type (treesit-node-text type)))
  (pcase type
    ("error" (peteches/go-ts-initialise-error place-num expand-error))
    ("string" (format "\"${%d:str}\"" place-num))
    ("rune" (format "${%d:0}" place-num))
    ("bool" (format "${%d:false}" place-num))
    ("chan" (format "${%d:nil}" place-num))
    ((pred (lambda (s) (string-match-p "^u?int\\(8\\|16\\|32\\|64\\)?$" s)))
     (format "${%d:0}" place-num))
    ((pred (lambda (s) (string-match-p "^float\\(32\\|64\\)$" s)))
     (format "${%d:0.0}" place-num))
    ((pred (lambda (s) (string-prefix-p "<-" s)))
     (format "${%d:nil}" place-num))
    ((pred (lambda (s) (string-prefix-p "[" s)))
     (format "${%d:nil}" place-num))
    ((pred (lambda (s) (string-prefix-p "*" s)))
     (format "${%d:nil}" place-num))
    ((pred (lambda (s) (string-match " " s)))
     (format "${%d:nil}" place-num))
    (_
     (format "${%d:%s\\{\\}}" place-num type))))

(defun peteches/go-ts--if-condition-left-text (&optional p)
  "Return the left side of the surrounding if-condition at P, or nil."
  (when-let* ((node (treesit-node-at (or p (point))))
              (if-node (treesit-parent-until
                        node
                        (lambda (candidate)
                          (string= "if_statement" (treesit-node-type candidate)))))
              (condition (treesit-node-child-by-field-name if-node "condition"))
              (left (treesit-node-child-by-field-name condition "left")))
    (treesit-node-text left)))

(defun peteches/go-ts-initialise-error (&optional place-num expand-error p)
  "Use tree-sitter to determine the best zero value for an error.
PLACE-NUM is the yasnippet placeholder number.
If EXPAND-ERROR is non-nil, produce `fmt.Errorf'.
If P is nil, use point."
  (setq place-num (or place-num 1))
  (let ((wrapped-error (peteches/go-ts--if-condition-left-text p)))
    (cond
     (wrapped-error
      (format "fmt.Errorf(\"${%d:error detected}: %%w\", ${%d:%s})"
              place-num
              (1+ place-num)
              wrapped-error))
     (expand-error
      (format "fmt.Errorf(\"${%d:error detected}\")" place-num))
     (t
      (format "${%d:nil}" place-num)))))

(defun peteches/go-ts-get-initialised-return-types (place-num &optional p expand-error)
  "Use tree-sitter to turn a Go function's return values into a snippet.
PLACE-NUM is the placeholder number to start with.
If P is nil, use point.  If EXPAND-ERROR is non-nil, use `fmt.Errorf' for
errors."
  (let ((func-node (peteches/go-ts-get-func-node p))
        (query (peteches/go-ts--ensure-return-type-query)))
    (when (and func-node query)
      (string-join
       (remove nil
               (cl-loop for type in (treesit-query-capture func-node query nil nil t)
                        for x from place-num
                        collect (peteches/go-ts-initialise-zero-value type x expand-error)))
       ", "))))

;; Compatibility aliases for older snippets or ad-hoc commands.
(defalias 'go-ts-get-struct-fieldname #'peteches/go-ts-get-struct-fieldname)
(defalias 'go-ts-get-func-node #'peteches/go-ts-get-func-node)
(defalias 'go-ts-initialise-zero-value #'peteches/go-ts-initialise-zero-value)
(defalias 'go-ts-initialise-error #'peteches/go-ts-initialise-error)
(defalias 'go-ts-get-initialised-return-types #'peteches/go-ts-get-initialised-return-types)
(defvaralias 'go-ts-query-get-func-return-types-str
  'peteches/go-ts-query-get-func-return-types-str)
(defvaralias 'go-ts-query-get-func-return-types
  'peteches/go-ts-query-get-func-return-types)

(provide 'peteches-go-ts)
;;; peteches-go-ts.el ends here
