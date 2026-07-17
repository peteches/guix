;;; peteches/repository.scm — resolve repo-relative paths via %load-path.
;;;
;;; The home modules need to reference non-Scheme assets that live in this
;;; repo (configs/emacs, configs/hypr, configs/bin, …) from inside
;;; `local-file' forms.  A literal relative path would be resolved against
;;; the current working directory, which is wrong under `guix home
;;; reconfigure -L .' and wrong again inside a git worktree.  Instead these
;;; helpers search %load-path — which always contains the repo root because
;;; every guix invocation here passes `-L .' (or GUILE_LOAD_PATH is set by
;;; the `peteches-guile-load-path' home service in (peteches home modules
;;; base)).
;;;
;;;   source-path            — locate a FILE, error if absent.
;;;   repo-directory         — locate a DIRECTORY, error if absent.
;;;   search-load-path-directory — like repo-directory but returns #f.
;;;
;;; Both erroring variants fail at module-load time, so a missing asset
;;; surfaces as a `guix home reconfigure' error rather than a broken
;;; symlink in the built profile.

(define-module (peteches repository)
  #:export (source-path
            directory-exists?
            search-load-path-directory
            repo-directory))

(define (source-path path)
  (or (search-path %load-path path)
      (error "could not find file in %load-path" %load-path path)))

(define (directory-exists? path)
  (and (file-exists? path)
       (eq? 'directory (stat:type (stat path)))))

(define (search-load-path-directory path)
  (let loop ((dirs %load-path))
    (if (null? dirs)
        #f
        (let ((candidate (string-append (car dirs) "/" path)))
          (if (directory-exists? candidate)
              candidate
              (loop (cdr dirs)))))))

(define (repo-directory path)
  (or (search-load-path-directory path)
      (error "could not find directory in %load-path" %load-path path)))
