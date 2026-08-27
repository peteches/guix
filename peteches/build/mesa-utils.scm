(define-module (peteches build mesa-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:export (patch-wrap-file))

(define* (patch-wrap-file subproject-name
                           source-dir
                           #:key
                           (subprojects-dir "subprojects")
                           (overlay-dir (string-append
                                         subprojects-dir
                                         "/packagefiles/"
                                         subproject-name))
                           (provides '()))
  "Generate a local directory with SOURCE-DIR recursively copied to it, and
then patches the wrap file for SUBPROJECT-NAME to use it.  This is
particularly useful for wrap files that would otherwise attempt to fetch the
content from the Internet.

SUBPROJECT-NAME is the name of the subproject that the wrap file will apply
to.  It should match the base name of the wrap file (without the \".wrap\"
extension) to patch.

SOURCE-DIR is a string directory path.  This is the path that SUBPROJECT-NAME
will be patched to use.

SUBPROJECTS-DIR is a string path to the directory where wrap files are
located for a project.

OVERLAY-DIR specifies the directory where files to overlay on the subproject
directory are located.  By default, this is expected to be in
\"SUBPROJECTS-DIR/packagefiles/SUBPROJECT-NAME\".

The [provide] section of the wrap file can also be configured via an alist
specification provided to PROVIDES."
  (let* ((wrap-file (string-append
                      subprojects-dir "/" subproject-name ".wrap"))
         (subproject-dest (string-append
                            subprojects-dir "/" subproject-name)))
    (copy-recursively source-dir subproject-dest)
    (when (file-exists? overlay-dir)
      (copy-recursively overlay-dir subproject-dest))
    (call-with-output-file wrap-file
      (lambda (port)
        (format port
                "[wrap-file]
directory = ~a
"
                subproject-name)
        (unless (null? provides)
          (format port
                  "[provide]
~a"
                  (string-join
                   (map (match-lambda
                          ((key . value)
                           (string-append key " = " value)))
                        provides)
                   "\n"
                   'suffix)))))))
