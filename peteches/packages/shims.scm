;; Simple alias so older channels referring to 'python-six-bootstrap' still work.
(define-module (peteches packages shims)
  #:use-module (guix packages)
  #:use-module (gnu packages python-xyz)   ; for python-six
  #:export (python-six-bootstrap))

;; Provide the old name, but point at the current package.
(define-public python-six-bootstrap
  (hidden-package python-six))
