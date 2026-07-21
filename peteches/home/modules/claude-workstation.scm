;;; claude-workstation.scm — home-environment constructor for the
;;; claude-workstation VM.  ONE constructor, instantiated once per account
;;; user in peteches/home/configs/claude-workstation-<user>.scm.
;;;
;;; Deliberately minimal and headless — no desktop, no GUI emacs, none of the
;;; base-services stack the desktops pull in.  It provides:
;;;
;;;   * claude-code on PATH, plus git / node / ripgrep / jq / curl.
;;;   * ~/.claude seeded from configs/claude/defaults (CLAUDE.md, skills,
;;;     statusline) via home-claude-service-type, which ALSO registers this
;;;     account's MCP servers with `claude mcp add' at activation.
;;;   * the Anvil MCP bridge (#:with-anvil?, default #t): an emacs-no-x
;;;     --fg-daemon supervised by home-shepherd, loading emacs-anvil from a
;;;     baked store path, plus the `anvil' / `anvil-emacs-eval' MCP servers.
;;;     This is the headless sibling of the container's anvil setup.
;;;   * ~/area_51/<repo> pre-cloned on `guix home reconfigure' — idempotent
;;;     (existing checkouts are skipped) and non-fatal (a clone that fails for
;;;     want of a key or network just warns and is retried next reconfigure,
;;;     so it never blocks `guix home').
;;;   * a git identity and any non-secret MCP env (PLANE_BASE_URL, …).
;;;
;;; SECRETS are deliberately NOT here: the store is world-readable, so an
;;; API key set via home-environment-variables would leak.  Deliver them via
;;; sops once the VM's age key exists, and source /run/secrets/… from the
;;; user's bash profile — exactly as configs/claude/critical-grind/{env,
;;; secrets.env} do for the container path.

(define-module (peteches home modules claude-workstation)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (peteches repository)
  #:use-module (peteches packages claude-code)
  #:use-module (peteches home modules claude)
  #:use-module (containers claude)
  #:export (make-claude-workstation-home))

(define (spec name) (specification->package name))

(define %claude-workstation-base-packages
  (list claude-code
        (spec "git")
        (spec "openssh")
        (spec "node")
        (spec "ripgrep")
        (spec "jq")
        (spec "curl")
        (spec "coreutils")
        (spec "less")))

;; --- Anvil headless emacs daemon --------------------------------------
;; init.el mirrors the container's anvil bootstrap (containers/claude.scm),
;; but bakes emacs-anvil's site-lisp onto the load-path directly so it needs
;; no EMACSLOADPATH plumbing, and starts the MCP server in-daemon (guarded)
;; rather than via a post-hoc emacsclient poke.
(define anvil-init-file
  (mixed-text-file
   "anvil-init.el"
   ";;; init.el --- Anvil bootstrap for claude-workstation -*- lexical-binding: t; -*-\n"
   "(add-to-list 'load-path \""
   (file-append emacs-anvil "/share/emacs/site-lisp/anvil") "\")\n"
   "(setq anvil-modules\n"
   "      '(worker eval file fs org text emacs elisp git proc host data\n"
   "               clipboard net http))\n"
   "(defvar anvil-optional-modules nil)\n"
   "(dolist (m '(context orchestrator memory))\n"
   "  (add-to-list 'anvil-optional-modules m))\n"
   "(require 'anvil)\n"
   "(when (fboundp 'anvil-enable) (anvil-enable))\n"
   "(ignore-errors (require 'anvil-server-commands nil t))\n"
   "(when (and (fboundp 'anvil-server-start)\n"
   "           (not (bound-and-true-p anvil-server--running)))\n"
   "  (ignore-errors (anvil-server-start)))\n"
   "(provide 'init)\n"
   ";;; init.el ends here\n"))

(define (anvil-services)
  "home-files + shepherd services standing up a headless anvil emacs daemon."
  (list
   (simple-service 'anvil-emacs-init
                   home-files-service-type
                   (list (list ".config/emacs/init.el" anvil-init-file)))
   (simple-service
    'anvil-emacs-daemon
    home-shepherd-service-type
    (list (shepherd-service
           (provision '(anvil-emacs))
           (documentation "Headless Emacs daemon hosting the Anvil MCP server.")
           (start #~(make-forkexec-constructor
                     (list #$(file-append emacs-no-x "/bin/emacs") "--fg-daemon")
                     #:log-file
                     (string-append
                      (or (getenv "XDG_CONFIG_HOME")
                          (string-append (getenv "HOME") "/.config"))
                      "/emacs/daemon.log")))
           (stop #~(make-kill-destructor))
           (respawn? #t))))))

(define (anvil-mcp-servers)
  "The two anvil MCP bridges, matching the container registration."
  (let ((script (file-append emacs-anvil "/bin/anvil-stdio.sh"))
        (bash   (file-append (spec "bash") "/bin/bash")))
    (list (home-claude-mcp-server
           (name "anvil")
           (command bash)
           (args (list script "--server-id=anvil")))
          (home-claude-mcp-server
           (name "anvil-emacs-eval")
           (command bash)
           (args (list script "--server-id=emacs-eval"))))))

;; --- repo pre-clone ----------------------------------------------------
;; Build the activation gexp that clones REPOS — a list of (NAME URL)
;; two-element lists — into ~/area_51/NAME, skipping any already present.
(define (repos-activation repos)
  (let ((git (file-append (spec "git") "/bin/git")))
    #~(begin
        (use-modules (ice-9 format))
        (let* ((home (getenv "HOME"))
               (base (string-append home "/area_51")))
          (unless (file-exists? base) (mkdir base))
          (for-each
           (lambda (repo)
             (let* ((name (car repo))
                    (url  (cadr repo))
                    (dst  (string-append base "/" name)))
               (if (file-exists? dst)
                   (format #t "claude-workstation: ~a already present, skipping~%"
                           name)
                   (begin
                     (format #t "claude-workstation: cloning ~a -> ~a~%" name dst)
                     (unless (zero? (system* #$git "clone" url dst))
                       (format #t "claude-workstation: WARNING clone of ~a failed; \
will retry next reconfigure~%" name))))))
           '#$repos)))))

(define (git-config-file git-name git-email)
  (plain-file
   "gitconfig"
   (string-append
    "[user]\n"
    "\tname = " git-name "\n"
    "\temail = " git-email "\n"
    "[init]\n\tdefaultBranch = main\n"
    "[pull]\n\trebase = true\n")))

(define* (make-claude-workstation-home
          #:key
          (git-name "Pete 'Peteches' McCabe")
          (git-email "claude@peteches.co.uk")
          (repos '())
          (mcp-servers '())
          (mcp-env '())
          (extra-packages '())
          (with-anvil? #t))
  "Return a headless home-environment for one Claude account on
claude-workstation.  REPOS is a list of (NAME URL) cloned into ~/area_51.
MCP-SERVERS is a list of <home-claude-mcp-server> (the anvil bridges are added
automatically when WITH-ANVIL?).  MCP-ENV is a NON-SECRET alist of environment
variables the MCP servers inherit.  EXTRA-PACKAGES are added to the base tool
set."
  (home-environment
   (packages (append %claude-workstation-base-packages
                     (if with-anvil? (list emacs-no-x emacs-anvil) '())
                     extra-packages))
   (services
    (append
     (list
      (service home-bash-service-type
               (home-bash-configuration
                (guix-defaults? #t)
                (environment-variables
                 '(("PATH" . "$HOME/.local/bin:$PATH")))))
      (simple-service 'mcp-env
                      home-environment-variables-service-type
                      mcp-env)
      (simple-service 'git-identity
                      home-files-service-type
                      (list (list ".config/git/config"
                                  (git-config-file git-name git-email))))
      (simple-service 'clone-area51-repos
                      home-activation-service-type
                      (repos-activation repos))
      (service home-claude-service-type
               (home-claude-configuration
                (config-directory (repo-directory "configs/claude/defaults"))
                (mcp-servers (append (if with-anvil? (anvil-mcp-servers) '())
                                     mcp-servers)))))
     (if with-anvil? (anvil-services) '())))))
