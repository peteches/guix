
;;; tsmark-backend-sh.el --- Shell (bash/sh) backend for tsmark -*- lexical-binding: t; -*-

(require 'tsmark-core)
(require 'treesit)

(defun tsmark-sh--predicate ()
  (let ((lang (treesit-language-at (point))))
    (or (memq lang '(bash sh)) (derived-mode-p 'bash-ts-mode 'sh-mode 'bash-mode))) )

(defun tsmark-sh--ancestor-where (node pred)
  (let ((n node))
    (while (and n (not (funcall pred n)))
      (setq n (treesit-node-parent n)))
    n))

(defun tsmark-sh--range-func (inside)
  ;; bash function is "function_definition"; body is a compound_statement / body
  (let ((def (tsmark-sh--ancestor-where
              (treesit-node-at (point))
              (lambda (n) (string= (treesit-node-type n) "function_definition")))))
    (unless def (user-error "No enclosing shell function"))
    (if (not inside)
        (cons (treesit-node-start def) (treesit-node-end def))
      (let ((body (or (treesit-node-child-by-field-name def "body")
                      (treesit-node-child-by-field-name def "command"))))
        (unless body (user-error "Function has no body"))
        ;; try to trim one char for delimiters if braces present
        (let ((beg (treesit-node-start body)) (end (treesit-node-end body)))
          (when (and (= (char-after beg) ?{) (= (char-before end) ?}))
            (setq beg (1+ beg) end (1- end)))
          (cons beg end))))))

(defun tsmark-sh--range-call (inside)
  ;; A simple command line: node type "command" (or "command_name" etc.)
  (let ((cmd (tsmark-sh--ancestor-where
              (treesit-node-at (point))
              (lambda (n) (member (treesit-node-type n) '("command" "simple_command"))))))
    (unless cmd (user-error "No enclosing command"))
    (if (not inside)
        (cons (treesit-node-start cmd) (treesit-node-end cmd))
      ;; inside = arguments only (after the command name)
      (let* ((name (treesit-node-child-by-field-name cmd "name"))
             (args-start (if name (treesit-node-end name) (treesit-node-start cmd)))
             (end (treesit-node-end cmd)))
        (cons args-start end)))))

(defun tsmark-sh--range-delimited (inside)
  ;; strings, subshell (parentheses), brace group
  (let* ((n (tsmark-sh--ancestor-where
             (treesit-node-at (point))
             (lambda (x)
               (let* ((t (treesit-node-type x))
                      (beg (treesit-node-start x)) (end (treesit-node-end x))
                      (b (char-after beg)) (e (char-before end)))
                 (or (member t '("string" "raw_string" "ansi_c_string"))
                     (member t '("subshell" "compound_statement"))
                     (and b e (or (and (= b ?\() (= e ?\)))
                                  (and (= b ?\{) (= e ?\}))
                                  (and (= b ?\[) (= e ?\])))))))))
    (unless n (user-error "No enclosing string/paren/brace/bracket"))
    (if inside
        (cons (1+ (treesit-node-start n)) (1- (treesit-node-end n)))
      (cons (treesit-node-start n) (treesit-node-end n))))))

(tsmark-register-backend
 (make-tsmark-backend
  :name 'sh
  :predicate #'tsmark-sh--predicate
  :range-func #'tsmark-sh--range-func
  :range-call #'tsmark-sh--range-call
  :range-delimited #'tsmark-sh--range-delimited))

(provide 'tsmark-backend-sh)
;;; tsmark-backend-sh.el ends here
