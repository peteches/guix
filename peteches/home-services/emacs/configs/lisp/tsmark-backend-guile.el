
;;; tsmark-backend-guile.el --- Guile/Scheme backend for tsmark -*- lexical-binding: t; -*-

(require 'tsmark-core)
(require 'treesit)
(require 'cl-lib)

(defvar tsmark-guile--def-heads '("define" "define*" "define-syntax" "define-public" "define-module" "define-macro"))

(defun tsmark-guile--predicate ()
  (let ((lang (treesit-language-at (point))))
    (or (eq lang 'scheme) (derived-mode-p 'scheme-mode))))

(defun tsmark-guile--named-children (node)
  (cl-loop for i from 0 below (treesit-node-child-count node)
           for ch = (treesit-node-child node i)
           when (treesit-node-named-p ch) collect ch))

(defun tsmark-guile--list-p (n)
  (string= (treesit-node-type n) "list"))

(defun tsmark-guile--first-symbol (list-node)
  (let* ((kids (tsmark-guile--named-children list-node))
         (sym  (car kids)))
    (when (and sym (member (treesit-node-type sym) '("symbol" "identifier")))
      (buffer-substring-no-properties (treesit-node-start sym) (treesit-node-end sym)))) )

(defun tsmark-guile--def-node-p (n)
  (and (tsmark-guile--list-p n)
       (let ((head (tsmark-guile--first-symbol n)))
         (and head (member head tsmark-guile--def-heads)))) )

(defun tsmark-guile--ancestor-where (node pred)
  (let ((n node))
    (while (and n (not (funcall pred n)))
      (setq n (treesit-node-parent n)))
    n))

(defun tsmark-guile--range-func (inside)
  (let ((def (tsmark-guile--ancestor-where (treesit-node-at (point)) #'tsmark-guile--def-node-p)))
    (unless def (user-error "No enclosing define form"))
    (if (not inside)
        (cons (treesit-node-start def) (treesit-node-end def))
      ;; Inside: everything after the head & signature
      (let* ((kids (tsmark-guile--named-children def))
             ;; (define (name args...) body...)
             ;; (define name (lambda ...) ...)
             (beg
              (cond
               ;; form 1: (define (name args...) body...)
               ((and (>= (length kids) 2)
                     (string= (treesit-node-type (nth 1 kids)) "list"))
                (treesit-node-end (nth 1 kids)))
               ;; form 2: after second child (symbol or (lambda ...))
               ((>= (length kids) 2)
                (treesit-node-end (nth 1 kids)))
               (t (1+ (treesit-node-start def)))))
             (end (1- (treesit-node-end def))))
        (cons beg end)))) )

(defun tsmark-guile--range-call (inside)
  (let ((call (tsmark-guile--ancestor-where (treesit-node-at (point))
                                            (lambda (n) (and (string= (treesit-node-type n) "list")
                                                             (not (tsmark-guile--def-node-p n)))))))
    (unless call (user-error "No enclosing call"))
    (if (not inside)
        (cons (treesit-node-start call) (treesit-node-end call))
      (let* ((kids (tsmark-guile--named-children call)))
        (if (<= (length kids) 1)
            (cons (1+ (treesit-node-start call)) (1- (treesit-node-end call)))
          (cons (treesit-node-start (nth 1 kids)) (treesit-node-end (car (last kids)))))))) )

(defun tsmark-guile--range-delimited (inside)
  (let* ((n (tsmark-guile--ancestor-where
             (treesit-node-at (point))
             (lambda (x)
               (let* ((beg (treesit-node-start x)) (end (treesit-node-end x))
                      (begc (char-after beg)) (endc (char-before end)))
                 (or (string= (treesit-node-type x) "string")
                     (and begc endc (or (and (= begc ?\() (= endc ?\)))
                                        (and (= begc ?\[) (= endc ?\])))))))))
    (unless n (user-error "No enclosing string/paren/bracket"))
    (if inside
        (cons (1+ (treesit-node-start n)) (1- (treesit-node-end n)))
      (cons (treesit-node-start n) (treesit-node-end n))))))

(tsmark-register-backend
 (make-tsmark-backend
  :name 'guile
  :predicate #'tsmark-guile--predicate
  :range-func #'tsmark-guile--range-func
  :range-call #'tsmark-guile--range-call
  :range-delimited #'tsmark-guile--range-delimited))

(provide 'tsmark-backend-guile)
;;; tsmark-backend-guile.el ends here
