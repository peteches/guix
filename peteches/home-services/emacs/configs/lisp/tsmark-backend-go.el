
(require 'tsmark-core)
(require 'treesit)

(defvar tsmark-go--function-nodes '("function_declaration" "method_declaration"))

(defun tsmark-go--predicate ()
  (let ((lang (treesit-language-at (point))))
    (or (eq lang 'go) (derived-mode-p 'go-ts-mode))))

(defun tsmark-go--ancestor-where (node pred)
  (let ((n node))
    (while (and n (not (funcall pred n)))
      (setq n (treesit-node-parent n)))
    n))

(defun tsmark-go--delimited-bounds (node inside)
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node)))
    (if inside
        (cons (1+ beg) (1- end))
      (cons beg end))))

(defun tsmark-go--enclosing-function-node ()
  (tsmark-go--ancestor-where
   (treesit-node-at (point))
   (lambda (n) (member (treesit-node-type n) tsmark-go--function-nodes))))

(defun tsmark-go--range-func (inside)
  (let ((fn (tsmark-go--enclosing-function-node)))
    (unless fn (user-error "No enclosing Go function"))
    (if (not inside)
        (cons (treesit-node-start fn) (treesit-node-end fn))
      (let ((body (treesit-node-child-by-field-name fn "body")))
        (unless body (user-error "Function has no body"))
        (let ((b (1+ (treesit-node-start body)))
              (e (1- (treesit-node-end body))))
          (cons b e))))))

(defun tsmark-go--enclosing-call-node ()
  (tsmark-go--ancestor-where
   (treesit-node-at (point))
   (lambda (n) (string= (treesit-node-type n) "call_expression"))))

(defun tsmark-go--range-call (inside)
  (let ((call (tsmark-go--enclosing-call-node)))
    (unless call (user-error "No enclosing Go call"))
    (if (not inside)
        (cons (treesit-node-start call) (treesit-node-end call))
      (let ((args (treesit-node-child-by-field-name call "arguments")))
        (unless args (user-error "Call has no arguments"))
        (tsmark-go--delimited-bounds args t)))))

(defun tsmark-go--range-delimited (inside)
  (let* ((start (treesit-node-at (point)))
         (node (tsmark-go--ancestor-where
                start
                (lambda (n)
                  (let* ((beg (treesit-node-start n))
                         (end (treesit-node-end n))
                         (begc (char-after beg))
                         (endc (char-before end)))
                    (or (member (treesit-node-type n)
                                '("interpreted_string_literal" "raw_string_literal"))
                        (and begc endc (or (and (= begc ?\() (= endc ?\)))
                                           (and (= begc ?\{) (= endc ?\}))
                                           (and (= begc ?\[) (= endc ?\])))))))))
    (unless node (user-error "No enclosing string/paren/brace/bracket"))
    (if inside
        (tsmark-go--delimited-bounds node t)
      (cons (treesit-node-start node) (treesit-node-end node))))))

(tsmark-register-backend
 (make-tsmark-backend
  :name 'go
  :predicate #'tsmark-go--predicate
  :range-func #'tsmark-go--range-func
  :range-call #'tsmark-go--range-call
  :range-delimited #'tsmark-go--range-delimited))

(provide 'tsmark-backend-go)
;;; tsmark-backend-go.el ends here
