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
;;;     account's MCP servers with `claude mcp add' at activation.  An
;;;     account can layer its own extra ~/.claude/ files on top via
;;;     #:extra-claude-files, without touching the shared defaults set.
;;;   * the Anvil MCP bridge (#:with-anvil?, default #t): an emacs-no-x
;;;     --fg-daemon supervised by home-shepherd, loading emacs-anvil from a
;;;     baked store path, plus the `anvil' / `anvil-emacs-eval' MCP servers.
;;;   * ~/area_51/<repo> pre-cloned on `guix home reconfigure' — idempotent
;;;     (existing checkouts are skipped) and non-fatal (a clone that fails for
;;;     want of a key or network just warns and is retried next reconfigure,
;;;     so it never blocks `guix home').
;;;   * a git identity and any non-secret MCP env (PLANE_BASE_URL, …).
;;;
;;; SECRETS are deliberately NOT here: the store is world-readable, so an
;;; API key set via home-environment-variables would leak.  They arrive via
;;; the VM's own sops-secrets (see #:sops-secrets in claude-workstation.scm,
;;; decrypting to /run/secrets/…) and are exported into the shell by
;;; #:secret-env-vars below — see criticalgrind's config for an example.

(define-module (peteches home modules claude-workstation)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs)
  ;; Package modules are imported directly (with #:select) rather than looked
  ;; up via `specification->package'.  A top-level `specification->package'
  ;; call fires `fold-packages' over `%package-module-path' — and `guix
  ;; deploy'/`guix system' put the repo (`-L .') on that path, so the scan
  ;; re-enters THIS module (and its siblings) while they are still compiling,
  ;; yielding a cascade of bogus "unbound variable" errors.  `guix repl' never
  ;; sets `%package-module-path', which is why it worked there.  Direct
  ;; imports avoid the scan entirely.
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages node) #:select (node))
  #:use-module ((gnu packages rust-apps) #:select (ripgrep))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages less) #:select (less))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module (peteches repository)
  #:use-module (peteches packages claude-code)
  #:use-module (peteches packages claude-completion)
  #:use-module (peteches home modules claude)
  #:use-module (peteches packages emacs-anvil)
  #:use-module (peteches packages graphify)
  #:export (make-claude-workstation-home))

(define %claude-workstation-base-packages
  (list claude-code claude-completion git openssh node ripgrep jq curl
        coreutils less graphify))

;; --- Anvil headless emacs daemon --------------------------------------
;; Bakes emacs-anvil's site-lisp onto the load-path directly so it needs
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
  (let ((script    (file-append emacs-anvil "/bin/anvil-stdio.sh"))
        (bash-path (file-append bash "/bin/bash")))
    (list (home-claude-mcp-server
           (name "anvil")
           (command bash-path)
           (args (list script "--server-id=anvil")))
          (home-claude-mcp-server
           (name "anvil-emacs-eval")
           (command bash-path)
           (args (list script "--server-id=emacs-eval"))))))

;; Stdio only (see graphify.scm on why the HTTP transport isn't available).
;; No args: it defaults to reading graphify-out/graph.json relative to the
;; launch cwd, which Claude Code sets to the current project directory.
(define graphify-mcp-server
  (home-claude-mcp-server
   (name "graphify")
   (command (file-append graphify "/bin/graphify-mcp"))))

;; --- repo pre-clone ----------------------------------------------------
;; Build the activation gexp that clones REPOS — a list of (NAME URL)
;; two-element lists — into ~/area_51/NAME, skipping any already present.
(define (repos-activation repos)
  (let ((git (file-append git "/bin/git")))
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

;; Build a bashrc snippet exporting each (ENV-VAR . RUN-SECRETS-PATH) pair
;; from SECRET-ENV-VARS, guarded so a missing/unreadable file (wrong
;; account, secret not yet wired) is silently skipped rather than erroring.
;; The path itself is baked into the world-readable store, same as the
;; anvil launcher paths above -- only the *value* at that path, read at
;; shell-start time from tmpfs, is sensitive.
;; bash-completion's lazy loader is not installed, so source the
;; claude-completion package's file directly. The path is stable across
;; generations; the guard keeps the shell quiet if the package is ever
;; dropped from the profile. Matches (peteches home modules base)'s
;; identical snippet for the desktop accounts.
(define claude-completion-bashrc
  (plain-file
   "claude-completion.bash"
   "\
_claude_completion=\"$HOME/.guix-home/profile/share/bash-completion/completions/claude\"
if [ -r \"$_claude_completion\" ]; then
  . \"$_claude_completion\"
fi
unset _claude_completion
"))

(define (secret-env-bashrc secret-env-vars)
  (plain-file
   "secret-env.sh"
   (string-append
    "### -*- mode: sh -*-\n"
    (apply string-append
           (map (lambda (pair)
                  (let ((var (car pair)) (path (cdr pair)))
                    (string-append
                     "if [ -r \"" path "\" ]; then export " var
                     "=\"$(cat \"" path "\")\"; fi\n")))
                secret-env-vars)))))

(define* (make-claude-workstation-home
          #:key
          (git-name "Pete 'Peteches' McCabe")
          (git-email "claude@peteches.co.uk")
          (repos '())
          (mcp-servers '())
          (mcp-env '())
          (secret-env-vars '())
          (extra-packages '())
          (extra-claude-files '())
          (with-anvil? #t))
  "Return a headless home-environment for one Claude account on
claude-workstation.  REPOS is a list of (NAME URL) cloned into ~/area_51.
MCP-SERVERS is a list of <home-claude-mcp-server> (the anvil bridges are added
automatically when WITH-ANVIL?, and graphify unconditionally).  MCP-ENV is a
NON-SECRET alist of environment variables the MCP servers inherit.
SECRET-ENV-VARS is an alist of (ENV-VAR .
RUN-SECRETS-PATH): at shell startup each ENV-VAR is exported from the contents
of RUN-SECRETS-PATH (normally a sops-secret's /run/secrets/... path) if that
path is readable -- see claude-workstation.scm's #:sops-secrets. EXTRA-PACKAGES
are added to the base tool set.  EXTRA-CLAUDE-FILES is an alist of (RELATIVE-
PATH . FILE-LIKE), each landing at ~/.claude/RELATIVE-PATH alongside (not
replacing) the shared configs/claude/defaults set -- for an account-specific
agent, skill or similar that doesn't belong to any one project (a project's
own CLAUDE.md/.claude/agents/ takes precedence over an account-wide file for
project-specific instructions). A RELATIVE-PATH colliding with a defaults/
entry is a build-time error, not a silent override."
  (home-environment
   (packages (append %claude-workstation-base-packages
                     (if with-anvil? (list emacs-no-x emacs-anvil) '())
                     extra-packages))
   (services
    (append
     (list
      ;; This VM has no elogind/PAM session management, so /run/user/<uid>
      ;; is never created -- yet XDG_RUNTIME_DIR is still exported pointing
      ;; at it (picked up from some ambient default). `emacs --daemon` uses
      ;; that variable to place its server socket and aborts with "Unable
      ;; to start daemon: Creating directory: Permission denied, /run/user"
      ;; because a plain user can't create entries directly under /run.
      ;; Point it at a per-account directory under $HOME instead, which is
      ;; always writable. The SessionStart hook (configs/claude/defaults/
      ;; settings.json) creates the directory and starts the daemon.
      (service home-bash-service-type
               (home-bash-configuration
                (guix-defaults? #t)
                (environment-variables
                 '(("PATH" . "$HOME/.local/bin:$PATH")
                   ("XDG_RUNTIME_DIR" . "$HOME/.cache/xdg-runtime")))
                (bashrc
                 (cons claude-completion-bashrc
                       (if (null? secret-env-vars)
                           '()
                           (list (secret-env-bashrc secret-env-vars)))))))
      (simple-service 'mcp-env
                      home-environment-variables-service-type
                      mcp-env)
      (simple-service 'git-identity
                      home-files-service-type
                      (list (list ".config/git/config"
                                  (git-config-file git-name git-email))))
      ;; Pins `guix pull' (no `-C' needed) to this repo's channel set, the
      ;; same way `scripts/deploy.scm' pins the system layer -- except this
      ;; is per-account and only takes effect on the next `guix home
      ;; reconfigure', same as every other file below. Without this,
      ;; `guix pull' falls back to plain upstream guix with none of
      ;; nonguix/guix-science(-nonfree)/sops-guix/critical-grind, and
      ;; `(peteches services ...)'/`(peteches packages ...)' modules that
      ;; touch those channels fail to resolve.
      (simple-service 'guix-pull-channels
                      home-files-service-type
                      (list (list ".config/guix/channels.scm"
                                  (local-file (source-path "peteches/channels/manual.scm")))))
      ;; Declarative known_hosts for the repos this account clones over SSH.
      ;; Written to ~/.ssh/known_hosts2 (a store symlink) so the non-interactive
      ;; git clone in the repos activation below can verify github.com,
      ;; codeberg.org and git.peteches.co.uk without a TTY to accept the host
      ;; key.  ~/.ssh/known_hosts is deliberately left alone — OpenSSH reads
      ;; both files by default, so hosts added interactively still persist
      ;; there and survive `guix home reconfigure'.
      (service home-openssh-service-type
               (home-openssh-configuration
                (known-hosts2
                 (list (local-file "claude-workstation-known-hosts")))))
      (simple-service 'clone-area51-repos
                      home-activation-service-type
                      (repos-activation repos))
      (service home-claude-service-type
               (home-claude-configuration
                (config-directory (repo-directory "configs/claude/defaults"))
                (mcp-servers (append (if with-anvil? (anvil-mcp-servers) '())
                                     (list graphify-mcp-server)
                                     mcp-servers)))))
     (if (null? extra-claude-files)
         '()
         (list (simple-service 'claude-extra-files
                               home-files-service-type
                               (map (lambda (pair)
                                      (list (string-append ".claude/" (car pair))
                                            (cdr pair)))
                                    extra-claude-files))))
     (if with-anvil? (anvil-services) '())))))
