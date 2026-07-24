;;; emacs-anvil.scm — anvil.el packaged as a first-class Guix package.
;;;
;;; Extracted from the retired containers/claude.scm wrapper: this package
;;; itself was never container-specific, it just happened to live alongside
;;; it. Byte-compilation is deferred to Emacs at load time to avoid failing
;;; on optional-integration modules. Consumed by (peteches home modules
;;; claude-workstation) and (peteches home modules base) to run the Anvil
;;; MCP bridge -- headless via emacs-no-x on claude-workstation, GUI via the
;;; desktop's own Emacs on nug/nyarlothotep.

(define-module (peteches packages emacs-anvil)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (emacs-anvil))

(define anvil-commit "574568a95a2bd8fceca6c9cd3bec0f94ecf0e6a9")
(define anvil-revision "1")

(define-public emacs-anvil
  (package
    (name "emacs-anvil")
    (version (git-version "1.3.0" anvil-revision anvil-commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zawatton/anvil.el")
             (commit anvil-commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b766fv7fqnpx3qxlcdj272dr59626g4k36yxpzlvj0pm5j1iz6g"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils) (ice-9 ftw))
      #:builder
      #~(begin
          (use-modules (guix build utils) (ice-9 ftw))
          (let* ((src       #$source)
                 (out       #$output)
                 (site-lisp (string-append out "/share/emacs/site-lisp/anvil"))
                 (bin       (string-append out "/bin")))
            (mkdir-p site-lisp)
            (mkdir-p bin)
            (for-each
             (lambda (f)
               (copy-file (string-append src "/" f)
                          (string-append site-lisp "/" f)))
             (scandir src
                      (lambda (n)
                        (and (string-suffix? ".el" n)
                             (not (string-prefix? "." n))))))
            (let* ((stdio (string-append src "/anvil-stdio.sh"))
                   (dest  (string-append bin "/anvil-stdio.sh")))
              (copy-file stdio dest)
              (chmod dest #o755))))))
    (synopsis "Emacs MCP server bridging LLM agents to Emacs")
    (description
     "Anvil turns Emacs into an AI-ready workbench via the Model Context
Protocol (MCP).  It exposes file editing, org-mode operations, system
inspection, and Elisp evaluation as MCP tools that any LLM agent can
call over stdio.  This package installs the elisp modules and the
@command{anvil-stdio.sh} launcher.")
    (home-page "https://github.com/zawatton/anvil.el")
    (license license:gpl3+)))
