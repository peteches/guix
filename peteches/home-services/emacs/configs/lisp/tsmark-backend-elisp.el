;;; tsmark-backend-elisp.el --- Emacs Lisp backend for tsmark -*- lexical-binding: t; -*-

(require 'tsmark-core)
(require 'treesit)
(require 'cl-lib)

(defvar tsmark-elisp--defun-heads
  '("defun" "cl-defun" "defmacro" "defsubst" "cl-defgeneric" "cl-defmethod"))

(defun tsmark-elisp--predicate ()
  (let ((lang (treesit-language-at (point))))
    (or (eq lang 'elisp) (derived-mode-p 'emacs-lisp-mode))) )

(defun tsmark-elisp--named-children (node)
  (cl-loop for i from 0 below (treesit-node-child-count node)
           for ch = (treesit-node-child node i)
           when (treesit-node-named-p ch) collect ch))

(defun tsmark-elisp--first-symbol (list-node)
  (let* ((kids (tsmark-elisp--named-children list-node))
         (sym  (car kids)))
    (when (and sym (member (treesit-node-type sym) '("symbol" "identifier")))
      (buffer-substring-no-properties (treesit-node-start sym) (treesit-node-end sym)))) )

(defun tsmark-elisp--defun-node-p (n)
  (and (string= (treesit-node-type n) "list")
       (let ((head (tsmark-elisp--first-symbol n)))
         (and head (member head tsmark-elisp--defun-heads)))) )

(defun tsmark-elisp--ancestor-where (node pred)
  (let ((n node))
    (while (and n (not (funcall pred n)))
      (setq n (treesit-node-parent n)))
    n))

(defun tsmark-elisp--range-func (inside)
  (let ((def (tsmark-elisp--ancestor-where (treesit-node-at (point)) #'tsmark-elisp--defun-node-p)))
    (unless def (user-error "No enclosing defun form"))
    (if (not inside)
        (cons (treesit-node-start def) (treesit-node-end def))
      (let* ((kids (tsmark-elisp--named-children def))
             (args (nth 2 kids))
             (after-args (and args (treesit-node-end args)))
             (doc (cl-find-if (lambda (n) (string= (treesit-node-type n) "string")) (cdddr kids)))
             (beg (or (and doc (treesit-node-end doc)) after-args))
             (end (1- (treesit-node-end def))))
        (unless beg (user-error "Malformed defun form"))
        (cons beg end)))) )

(defun tsmark-elisp--range-call (inside)
  (let ((call (tsmark-elisp--ancestor-where
               (treesit-node-at (point))
               (lambda (n) (and (string= (treesit-node-type n) "list")
                                (not (tsmark-elisp--defun-node-p n)))))))
    (unless call (user-error "No enclosing call"))
    (if (not inside)
        (cons (treesit-node-start call) (treesit-node-end call))
      (let* ((kids (tsmark-elisp--named-children call))
             (rest (cdr kids)))
        (if (null rest)
            (cons (1+ (treesit-node-start call)) (1- (treesit-node-end call)))
          (cons (treesit-node-start (car rest)) (treesit-node-end (car (last rest)))))))) )

(defun tsmark-elisp--range-delimited (inside)
  (let* ((node (tsmark-elisp--ancestor-where
                (treesit-node-at (point))
                (lambda (n)
                  (let* ((beg (treesit-node-start n)) (end (treesit-node-end n))
                         (begc (char-after beg)) (endc (char-before end)))
                    (or (string= (treesit-node-type n) "string")
                        (and begc endc (or (and (= begc ?\() (= endc ?\)))
                                           (and (= begc ?\[) (= endc ?\])))))))))
    (unless node (user-error "No enclosing string/paren/bracket"))
    (if inside
        (cons (1+ (treesit-node-start node)) (1- (treesit-node-end node)))
      (cons (treesit-node-start node) (treesit-node-end node))))))

(tsmark-register-backend
 (make-tsmark-backend
  :name 'elisp
  :predicate #'tsmark-elisp--predicate
  :range-func #'tsmark-elisp--range-func
  :range-call #'tsmark-elisp--range-call
  :range-delimited #'tsmark-elisp--range-delimited))

(provide 'tsmark-backend-elisp)
;;; tsmark-backend-elisp.el ends here
