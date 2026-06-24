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
