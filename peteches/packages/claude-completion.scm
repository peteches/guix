;;; claude-completion.scm — bash completion for the `claude' CLI.
;;;
;;; Installs configs/claude/claude-completion.bash to the conventional
;;; share/bash-completion/completions/claude path, so it lands in any
;;; profile that includes this package. Nothing on claude-workstation or
;;; the desktops runs bash-completion's lazy loader, so accounts also
;;; source it directly from ~/.guix-home/profile via a bashrc snippet --
;;; see (peteches home modules claude-workstation) and (peteches home
;;; modules base).

(define-module (peteches packages claude-completion)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (peteches repository)
  #:export (claude-completion))

(define-public claude-completion
  (package
    (name "claude-completion")
    (version "0.1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((comp (string-append #$output "/share/bash-completion/completions")))
            (mkdir-p comp)
            (copy-file #$(local-file (source-path "configs/claude/claude-completion.bash"))
                       (string-append comp "/claude"))))))
    (synopsis "Bash completion for the claude CLI")
    (description
     "Static bash-completion script for Claude Code's @command{claude}
command-line interface: subcommands, flags, and value completion for
@code{--model}, @code{--agent}, @code{--permission-mode}, and similar
options.")
    (home-page "https://github.com/anthropics/claude-code")
    (license license:gpl3+)))
