;;; tsmark-backend-python.el --- Python backend for tsmark -*- lexical-binding: t; -*-

(require 'tsmark-core)
(require 'treesit)

(defun tsmark-python--predicate ()
  (let ((lang (treesit-language-at (point))))
    (or (eq lang 'python) (derived-mode-p 'python-ts-mode 'python-mode))) )

(defun tsmark-python--ancestor-where (node pred)
  (let ((n node))
    (while (and n (not (funcall pred n)))
      (setq n (treesit-node-parent n)))
    n))

(defun tsmark-python--range-func (inside)
  ;; Node type for def: "function_definition"; body is a block after colon.
  (let ((def (tsmark-python--ancestor-where
              (treesit-node-at (point))
              (lambda (n) (string= (treesit-node-type n) "function_definition")))))
    (unless def (user-error "No enclosing Python function"))
    (if (not inside)
        (cons (treesit-node-start def) (treesit-node-end def))
      (let ((body (treesit-node-child-by-field-name def "body")))
        (unless body (user-error "Function has no body"))
        (cons (treesit-node-start body) (treesit-node-end body))))))

(defun tsmark-python--range-call (inside)
  ;; Calls are "call" with fields function / arguments
  (let ((call (tsmark-python--ancestor-where
               (treesit-node-at (point))
               (lambda (n) (string= (treesit-node-type n) "call")))))
    (unless call (user-error "No enclosing call"))
    (if (not inside)
        (cons (treesit-node-start call) (treesit-node-end call))
      (let ((args (treesit-node-child-by-field-name call "arguments")))
        (unless args (user-error "Call has no arguments"))
        (cons (1+ (treesit-node-start args)) (1- (treesit-node-end args)))))))

(defun tsmark-python--range-delimited (inside)
  ;; strings, parenthesized_expression, list, dictionary, set, tuple
  (let* ((n (tsmark-python--ancestor-where
             (treesit-node-at (point))
             (lambda (x)
               (let* ((t (treesit-node-type x))
                      (beg (treesit-node-start x)) (end (treesit-node-end x))
                      (b (char-after beg)) (e (char-before end)))
                 (or (member t '("string" "concatenated_string"))
                     (member t '("parenthesized_expression" "list" "dictionary" "set" "tuple"))
                     (and b e (or (and (= b ?\() (= e ?\)))
                                  (and (= b ?\[) (= e ?\]))
                                  (and (= b ?\{) (= e ?\})))))))))
    (unless n (user-error "No enclosing string/paren/brace/bracket"))
    (if inside
        (cons (1+ (treesit-node-start n)) (1- (treesit-node-end n)))
      (cons (treesit-node-start n) (treesit-node-end n))))))

(tsmark-register-backend
 (make-tsmark-backend
  :name 'python
  :predicate #'tsmark-python--predicate
  :range-func #'tsmark-python--range-func
  :range-call #'tsmark-python--range-call
  :range-delimited #'tsmark-python--range-delimited))

(provide 'tsmark-backend-python)
;;; tsmark-backend-python.el ends here
