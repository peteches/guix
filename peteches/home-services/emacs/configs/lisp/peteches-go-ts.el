;;; peteches-go-ts --- My go treesitter functions
;;;
;;; Commentary:
;;; 
;;;
;;; Code:

(defun go-ts-get-struct-fieldname (&optional p)
  "Use Treesitter to get fieldname of struct field.  If P not set use point."
  (or p (setq p (point)))
  (treesit-node-text
   (treesit-node-child-by-field-name
    (treesit-node-parent (treesit-node-at p))
    "name")))

(defun go-ts-get-func-node (&optional p)
  "Use Treesitter to get return Types of a function.  If P not set use point."
  (or p (setq p (point)))
  (treesit-parent-until (treesit-node-at p)
                        (lambda(n)
                          (or (string-match-p "function_declaration" (treesit-node-type n))
                              (string-match-p "method_declaration" (treesit-node-type n))))))

;; liberated from https://blog.meain.io/2021/intelligent-snippets-treesitter/
(defun go-ts-initialise-zero-value (type &optional place-num expandError)
  "Return an initialised zero value of type TYPE.
PLACE-NUM is the yasnippet place-holder, defaults to 1.
If EXPANDERROR non-nil initialised error will use fmt.Errorf."
  (or place-num (setq place-num 1))
  (if (treesit-node-p type)
      (setq type (treesit-node-text type)))
  (pcase type
    ("error" (go-ts-initialise-error place-num expandError))
    ("string" (format "\"${%d:str}\"" place-num))
    ("rune" (format "${%d:0}" place-num))
    ("bool" (format "${%d:false}" place-num))
    ("chan" (format "${%d:nil}" place-num))
    ((pred (lambda (s) (string-match-p "^u?int\\(8\\|16\\|32\\|64\\)?$" s))) (format "${%d:0}" place-num))
    ((pred (lambda (s) (string-match-p "^float\\(32\\|64\\)$" s))) (format "${%d:0.0}" place-num)) ; any float type

    ((pred (lambda (s) (string-prefix-p "<-" s))) (format "${%d:nil}" place-num)) ; channels

    ((pred (lambda (s) (string-prefix-p "[" s))) (format "${%d:nil}" place-num)) ; arrays

    ((pred (lambda (s) (string-prefix-p "*" s))) (format "${%d:nil}" place-num)) ; pointers to things

    ((pred (lambda (s) (string-match " " s))) (format "${%d:nil}" place-num)) ; for situations with return name

    (_ (format "${%d:%s\\{\\}}" place-num type))))

(defun go-ts-initialise-error (&optional place-num expandError)
  "Use Treesitter to dermine best value of error.
PLACE-NUM is the yas-snippet placeholder.
If EXPANDERROR is non-nil fmt.Errorf() is produced"
  (or place-num (setq place-num 1))
  (or p (setq p (point)))
  (let ((parent-node (treesit-parent-until (treesit-node-at p)
                                           (lambda(n)
                                             (string-match-p "if_statement" (treesit-node-type n))))))
  (if (or parent-node expandError)
        (format "fmt.Errorf(\"${%d:error detected}: %%w\", ${%d:%s})"
                place-num
                (+ place-num)
                (treesit-node-text (treesit-node-child-by-field-name (treesit-node-child-by-field-name parent-node "condition") "left")))
      (format "${%d:nil}" place-num))))

(defun go-ts-get-initialised-return-types (place-num &optional p expandError)
  "Use Treesitter to turn a functions return values into a snippet.
PLACE-NUM is the placeholder number to start with.
If P not set use point.  If EXPANDERROR non-nil fmt.Errorf() will be used."
  (or p (setq p (point)))
  (string-join
   (remove nil
	   (cl-loop for type in (treesit-query-capture (go-ts-get-func-node p)
			     go-ts-query-get-func-return-types nil nil t)
		    for x from place-num
		    collect (go-ts-initialise-zero-value type x expandError)))
   ", "))

(setq go-ts-query-get-func-return-types-str "
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
  ")
(if (treesit-query-validate 'go go-ts-query-get-func-return-types-str)
    (setq go-ts-query-get-func-return-types (treesit-query-compile 'go go-ts-query-get-func-return-types-str)))

(provide 'peteches-go-ts)
;;; peteches-go-ts.el ends here
