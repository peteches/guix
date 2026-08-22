(define-module (peteches packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module ((gnu packages emacs-xyz)
                #:renamer (symbol-prefix-proc 'upstream:))
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public emacs-slack
  ;; Update the commit and revision
  (let ((commit "22ae94de5ddf0c9012afe298b5eb8b2e18d45931")
        ;; ← replace with the new commit
        (revision "13"))
    ;; increment the revision
    (package
      (inherit upstream:emacs-slack)
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yuya373/emacs-slack")
               (commit commit)))
         (file-name (git-file-name "emacs-slack" commit))
         (sha256
          (base32 "12gpmlg9k2xfjd57bj4jmyrpks00p92h8ip6lcimjqwkl4vf2dwk")))))))

(define-public emacs-mcp
  (package
    (name "emacs-mcp")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lizqwerscott/mcp.el")
             (commit "2d172809cbdb2a40d86b28ad73bd65547cefe0e1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03zvdvjh724kpb2p7j208fm0z73g21vij0jms8azdxbazyw6yj77"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/lizqwerscott/mcp.el")
    (synopsis "Prompt manager for interacting with LLMs in Emacs")
    (description
     "mcp.el is an Emacs interface for managing reusable prompt templates and interacting with LLMs like ChatGPT via GPTel.")
    (license gpl3+)))

(define-public emacs-linear
  (package
    (name "emacs-linear")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anegg0/linear-emacs.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1whmyzvncf78nvhzdyhliw94z3kqvy594n27a35nvmhm7vqkyv08"))))
    (build-system emacs-build-system)
    (inputs (list emacs-s upstream:emacs-request emacs-dash))
    (home-page "git@codeberg.org/gyamtso/linear-emacs.git")
    (synopsis "Linear.app integration plugin for Emacs + Orgmode")
    (description
     "This package provides integration between Emacs and Linear.app, allowing you to view and manage your Linear issues without leaving Emacs. I was just sick of leaving Emacs for some corporation's UI.")
    (license gpl3+)))
