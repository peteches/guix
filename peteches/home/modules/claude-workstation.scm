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
  #:use-module (ice-9 match)
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
  #:use-module ((gnu packages networking) #:select (proxychains-ng))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages less) #:select (less))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages python) #:select (python-minimal))
  #:use-module (peteches repository)
  #:use-module (peteches packages claude-code)
  #:use-module (peteches packages claude-completion)
  #:use-module (peteches home modules claude)
  #:use-module (peteches home modules gpg)
  #:use-module (peteches home modules pi)
  #:use-module ((peteches packages pi-coding-agent) #:select (pi-mcp-adapter))
  #:use-module (peteches packages emacs-anvil)
  #:use-module (peteches packages graphify)
  #:use-module (peteches packages herdr)
  #:use-module (peteches packages github-cli)
  #:use-module (peteches packages onepassword-cli)
  #:export (make-claude-workstation-home))

(define %claude-workstation-base-packages
  (list claude-code claude-completion git openssh node ripgrep jq curl
        coreutils less graphify herdr python-minimal github-cli
        onepassword-cli proxychains-ng))

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

;; --- Herdr headless server ---------------------------------------------
;; `herdr server` runs the persistent-session server in the foreground --
;; the same shape as `emacs --fg-daemon' above, so it fits the same
;; make-forkexec-constructor pattern.  Its own client-side logging
;; (~/.config/herdr/herdr.log et al) is separate from this shepherd log,
;; which only catches anything printed before that takes over.
(define (herdr-services)
  "shepherd service supervising a per-account headless herdr server."
  (list
   (simple-service
    'herdr-server
    home-shepherd-service-type
    (list (shepherd-service
           (provision '(herdr-server))
           (documentation "Headless herdr server for persistent agent sessions.")
           (start #~(make-forkexec-constructor
                     (list #$(file-append herdr "/bin/herdr") "server")
                     #:log-file
                     (string-append
                      (or (getenv "XDG_CONFIG_HOME")
                          (string-append (getenv "HOME") "/.config"))
                      "/herdr/shepherd.log")))
           (stop #~(make-kill-destructor))
           (respawn? #t))))))

;; --- Herdr standing spaces (opt-in, peteches only in practice) ---------
;; herdr's socket API has no declarative "start these workspaces" config,
;; so this builds a shell script that idempotently recreates a fixed set of
;; named spaces (herdr's term for what the sidebar calls a "workspace").
;;
;; Two of the four spaces belong to a DIFFERENT account (criticalgrind/ygo
;; -- see this module's docstring for why they're separate OS users). An
;; earlier version of this script reached them via `sudo -iu`, but that
;; fails on two independent grounds: `herdr agent start` refuses a pane
;; whose foreground process is `sudo` (its monitor-process architecture
;; means the real shell it spawns is never the process herdr's detection
;; sees), and even the self-report path Claude Code's own hook uses can't
;; connect to peteches's herdr.sock (mode 0600, owned by peteches -- a
;; process running as criticalgrind can't open it regardless of what env
;; vars survive the sudo boundary). Both were confirmed by hand against
;; the running claude-workstation VM before rewriting this.
;;
;; The fix: never cross the account boundary at the socket level. For a
;; REMOTE-USER space, ensure_remote_space SSHes into that account's OWN
;; herdr server (as itself, over loopback, using
;; AUTOMATION-SSH-IDENTITY -- see %automation-authorized-key-secret in
;; (peteches systems vm-base)) and creates+tracks the workspace there,
;; where the socket is that account's own and every native mechanism
;; (agent start, the self-report hook) just works. Only then does it
;; create a purely COSMETIC pane in the calling account's own session and
;; nest-attach into that already-tracked remote session with `herdr
;; --remote`, which needs experimental.allow_nested = true locally (set
;; below) since it is itself launched from inside a herdr-managed pane.
(define (herdr-spaces-bootstrap-script specs)
  "SPECS is a list of (NAME RELATIVE-PATH REMOTE-USER) triples. NAME
becomes both the space label and the herdr agent name (must match
[a-z][a-z0-9_-]{0,31} and be unique among live agents, on whichever
server it ends up tracked). RELATIVE-PATH is relative to $HOME -- the
calling account's home when REMOTE-USER is #f, otherwise REMOTE-USER's
own home on ITS OWN herdr server."
  (define (ensure-call spec)
    (match spec
      ((name relative-path #f)
       (string-append "ensure_local_space \"" name "\" \"" relative-path "\"\n"))
      ((name relative-path remote-user)
       (string-append "ensure_remote_space \"" name "\" \"" relative-path
                       "\" \"" remote-user "\"\n"))))
  (mixed-text-file
   "herdr-spaces-bootstrap.sh"
   "#!/bin/sh\nset -eu\n\n"
   "HERDR=" (file-append herdr "/bin/herdr") "\n"
   "JQ=" (file-append jq "/bin/jq") "\n"
   "SSH=" (file-append openssh "/bin/ssh") "\n\n"
   "wait_for_socket() {\n"
   "  i=0\n"
   "  while [ \"$i\" -lt 30 ]; do\n"
   "    \"$HERDR\" workspace list >/dev/null 2>&1 && return 0\n"
   "    i=$((i + 1))\n"
   "    sleep 1\n"
   "  done\n"
   "  return 1\n"
   "}\n\n"
   ;; A space already exists (matched by label) on a shepherd restart or a
   ;; second run of this one-shot -- but a herdr server restart (as opposed
   ;; to a client detach) brings panes back EMPTY, with no process inside
   ;; them, unless herdr's own native session-resume fires (which needs a
   ;; supported CLI version and a previously-reported session ref -- see
   ;; herdr-agent-state.sh). An existing label therefore does NOT imply a
   ;; live claude agent; reuse the workspace's existing pane and (re)start
   ;; claude in it whenever `herdr pane list` shows no `.agent=="claude"'
   ;; entry there, instead of returning early on label match alone -- that
   ;; early return was confirmed live (2026-08-19) to leave standing spaces
   ;; permanently claude-less after any boot where the space already existed
   ;; but its agent hadn't (re)started.
   "ensure_local_space() {\n"
   "  name=\"$1\"; relpath=\"$2\"\n"
   "  existing=$(\"$HERDR\" workspace list | \"$JQ\" -r --arg n \"$name\" \\\n"
   "    '.result.workspaces[] | select(.label==$n) | .workspace_id' | head -n1)\n"
   "  if [ -n \"$existing\" ]; then\n"
   "    has_agent=$(\"$HERDR\" pane list --workspace \"$existing\" | \"$JQ\" -r \\\n"
   "      '.result.panes[] | select(.agent==\"claude\") | .pane_id' | head -n1)\n"
   "    [ -n \"$has_agent\" ] && return 0\n"
   "    pane=$(\"$HERDR\" pane list --workspace \"$existing\" | \"$JQ\" -r '.result.panes[0].pane_id')\n"
   "  else\n"
   "    created=$(\"$HERDR\" workspace create --cwd \"$HOME\" --label \"$name\" --no-focus)\n"
   "    pane=$(printf '%s' \"$created\" | \"$JQ\" -r '.result.root_pane.pane_id')\n"
   "  fi\n"
   "  marker=\"__herdr_ready_${name}__\"\n"
   "  \"$HERDR\" pane run \"$pane\" \"cd $relpath && echo $marker\"\n"
   "  \"$HERDR\" pane wait-output \"$pane\" --match \"$marker\" --timeout 30000 >/dev/null 2>&1 || true\n"
   "  \"$HERDR\" agent start \"$name\" --kind claude --pane \"$pane\" >/dev/null 2>&1 || true\n"
   "}\n\n"
   ;; Guix Home's on-first-login (the thing that starts shepherd-for-home,
   ;; and with it that account's OWN herdr-server) only fires for a login
   ;; shell -- ~/.bash_profile sources ~/.profile, which a plain
   ;; non-interactive `ssh target cmd` never reads. `bash -lc' forces
   ;; that, and the inner wait loop covers the gap between shepherd-for-
   ;; home starting and its herdr-server actually accepting connections,
   ;; so this is the one call in this script that has to succeed cold
   ;; (fresh boot, target account never yet logged in) -- everything else
   ;; in ensure_remote_space assumes the socket it just proved reachable.
   ;; Same "label exists != agent running" fix as ensure_local_space, applied
   ;; twice here: once to the REMOTE account's own claude agent, and once to
   ;; the LOCAL cosmetic pane, whose only job is to stay nest-attached via a
   ;; live `herdr --remote $target' process. A herdr server restart on
   ;; either side clears the pane back to a plain shell without killing the
   ;; workspace/label, so re-check the actual foreground process
   ;; (`herdr pane process-info') rather than trusting the label.\n"
   "ensure_remote_space() {\n"
   "  name=\"$1\"; relpath=\"$2\"; remote_user=\"$3\"; target=\"$remote_user@localhost\"\n"
   "  remote_existing=$(\"$SSH\" \"$target\" bash -lc \\\n"
   "    'i=0; while ! herdr workspace list >/dev/null 2>&1; do i=$((i + 1)); [ \"$i\" -ge 30 ] && exit 1; sleep 1; done; herdr workspace list' \\\n"
   "    | \"$JQ\" -r --arg n \"$name\" '.result.workspaces[] | select(.label==$n) | .workspace_id' | head -n1)\n"
   "  if [ -n \"$remote_existing\" ]; then\n"
   "    remote_has_agent=$(\"$SSH\" \"$target\" \"herdr pane list --workspace $remote_existing\" | \"$JQ\" -r \\\n"
   "      '.result.panes[] | select(.agent==\"claude\") | .pane_id' | head -n1)\n"
   "    if [ -z \"$remote_has_agent\" ]; then\n"
   "      remote_pane=$(\"$SSH\" \"$target\" \"herdr pane list --workspace $remote_existing\" | \"$JQ\" -r '.result.panes[0].pane_id')\n"
   "      \"$SSH\" \"$target\" \"herdr agent start $name --kind claude --pane $remote_pane\" >/dev/null 2>&1 || true\n"
   "    fi\n"
   "  else\n"
   "    remote_created=$(\"$SSH\" \"$target\" \"herdr workspace create --cwd \\$HOME/$relpath --label $name --no-focus\")\n"
   "    remote_pane=$(printf '%s' \"$remote_created\" | \"$JQ\" -r '.result.root_pane.pane_id')\n"
   "    \"$SSH\" \"$target\" \"herdr agent start $name --kind claude --pane $remote_pane\" >/dev/null 2>&1 || true\n"
   "  fi\n"
   "  local_existing=$(\"$HERDR\" workspace list | \"$JQ\" -r --arg n \"$name\" \\\n"
   "    '.result.workspaces[] | select(.label==$n) | .workspace_id' | head -n1)\n"
   "  if [ -n \"$local_existing\" ]; then\n"
   "    local_pane=$(\"$HERDR\" pane list --workspace \"$local_existing\" | \"$JQ\" -r '.result.panes[0].pane_id')\n"
   "    fg=$(\"$HERDR\" pane process-info --pane \"$local_pane\" | \"$JQ\" -r \\\n"
   "      '.result.process_info.foreground_processes[0].name // empty')\n"
   "    [ \"$fg\" = \"herdr\" ] && return 0\n"
   "    \"$HERDR\" pane run \"$local_pane\" \"exec $HERDR --remote $target\"\n"
   "    return 0\n"
   "  fi\n"
   "  created=$(\"$HERDR\" workspace create --cwd \"$HOME\" --label \"$name\" --no-focus)\n"
   "  pane=$(printf '%s' \"$created\" | \"$JQ\" -r '.result.root_pane.pane_id')\n"
   "  \"$HERDR\" pane run \"$pane\" \"exec $HERDR --remote $target\"\n"
   "}\n\n"
   "wait_for_socket || exit 0\n\n"
   (apply string-append (map ensure-call specs))))

;; herdr's own config.toml has no include mechanism (see
;; configs/matugen/templates/herdr-config.toml, which is why the desktop
;; accounts get it via matugen instead), so on the VM accounts -- which
;; have no wallpaper/matugen pipeline at all -- this appends the one line
;; nesting needs directly, idempotently, leaving any hand-edited content
;; alone.
(define (herdr-allow-nested-activation)
  #~(begin
      (use-modules (ice-9 textual-ports) (srfi srfi-13))
      (let* ((home (getenv "HOME"))
             (config-dir (string-append home "/.config/herdr"))
             (config-file (string-append config-dir "/config.toml"))
             (marker "allow_nested = true")
             (existing (if (file-exists? config-file)
                           (call-with-input-file config-file get-string-all)
                           "")))
        (mkdir-p config-dir)
        (unless (string-contains existing marker)
          (call-with-output-file config-file
            (lambda (port)
              (display existing port)
              (display "\n[experimental]\nallow_nested = true\n" port)))))))

(define (herdr-spaces-services specs)
  "Shepherd one-shot service that runs HERDR-SPACES-BOOTSTRAP-SCRIPT once
herdr-server is up, after ensuring experimental.allow_nested is set (any
REMOTE-USER space nest-attaches via `herdr --remote`). No-op when SPECS is
empty."
  (if (null? specs)
      '()
      (list
       (simple-service 'herdr-allow-nested
                       home-activation-service-type
                       (herdr-allow-nested-activation))
       (simple-service
        'herdr-spaces-bootstrap
        home-shepherd-service-type
        (list (shepherd-service
               (provision '(herdr-spaces-bootstrap))
               (requirement '(herdr-server))
               (one-shot? #t)
               (auto-start? #t)
               (documentation
                "Idempotently ensure this account's standing herdr spaces exist, each running Claude Code.")
               (start #~(lambda _
                          (zero? (system* #$(file-append bash "/bin/bash")
                                           #$(herdr-spaces-bootstrap-script specs)))))))))))

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
  (let ((git (file-append git "/bin/git"))
        (ssh (file-append openssh "/bin/ssh")))
    #~(begin
        (use-modules (ice-9 format))
        ;; guix-home-service-type runs this activation via shepherd with a
        ;; minimal PATH that doesn't include openssh's store directory. `git
        ;; clone' over ssh:// forks `ssh' by looking it up on PATH, so an
        ;; ssh:// clone here fails with "error: cannot run ssh: No such file
        ;; or directory" even though `git' itself was found (it's invoked by
        ;; absolute path). Point git at ssh's absolute path explicitly.
        (setenv "GIT_SSH_COMMAND" #$ssh)
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

;; Points proxychains-ng at the SOCKS5 proxy exposed by
;; wireguard-socks5-service-type (peteches/services/wireguard-socks5.scm,
;; default port 1080) -- shared by all three accounts on this VM, so
;; `proxychains4 <cmd>` routes CMD's traffic over the split-tunnel WireGuard
;; whenever that service's netns/tunnel/proxy trio has been started (see its
;; module docstring for the `herd start` order; nothing here starts it).
;; microsocks runs inside the wg0ns network namespace, bound to the netns
;; side of the host<->netns veth pair (10.200.0.2), not to loopback -- see
;; wsc-veth-netns-address in wireguard-socks5.scm.
(define %proxychains-config-file
  (plain-file
   "proxychains.conf"
   "strict_chain\nproxy_dns\ntcp_read_time_out 15000\ntcp_connect_time_out 8000\n\n[ProxyList]\nsocks5 10.200.0.2 1080\n"))

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
          (with-anvil? #t)
          (with-herdr? #t)
          (herdr-spaces '())
          (automation-ssh-identity #f))
  "Return a headless home-environment for one Claude account on
claude-workstation.  REPOS is a list of (NAME URL) cloned into ~/area_51.
MCP-SERVERS is a list of <home-claude-mcp-server> (the anvil bridges are added
automatically when WITH-ANVIL?, and graphify unconditionally).  MCP-ENV is a
NON-SECRET alist baked into each of MCP-SERVERS' own `-e' flags at `claude
mcp add' time (not left to ambient shell inheritance -- see the ENV field
of <home-claude-mcp-server> for why).  It is also exported shell-side via
home-environment-variables-service-type for convenience, but that copy is
NOT what the MCP servers themselves see.
WITH-HERDR? (default #t) supervises a per-account `herdr server' under
home-shepherd, so Claude Code sessions started inside it survive an SSH
disconnect and can be reattached later -- either locally on the VM or via
`herdr --remote <account>@claude-workstation' from another machine, which
tunnels over the same SSH auth already configured for this account.
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
entry is a build-time error, not a silent override. HERDR-SPACES is a list
of (NAME RELATIVE-PATH SUDO-USER) triples (see
herdr-spaces-bootstrap-script) -- when non-empty and WITH-HERDR?, a
shepherd one-shot idempotently creates these herdr workspaces once the
server is up and starts Claude Code in each. Only meaningful for an
account other clients reach via `herdr --remote' (currently just
peteches -- see configs/hypr/peteches/apps/herdr.lua). AUTOMATION-SSH-
IDENTITY is a runtime path (typically a sops-decrypted /run/secrets/...
path) to the private half of the peteches automation SSH keypair -- see
%automation-authorized-key-secret in (peteches systems vm-base). When set,
it becomes the IdentityFile for any `ssh <user>@localhost', which is how
HERDR-SPACES reaches the criticalgrind/ygo accounts' own herdr servers."
  (let* ((%all-mcp-servers
          (append (if with-anvil? (anvil-mcp-servers) '())
                  (list graphify-mcp-server)
                  ;; MCP-ENV is baked into each caller-supplied stdio
                  ;; server's own `-e' flags (see home-claude-mcp-server's
                  ;; ENV field) rather than left to ambient shell
                  ;; inheritance -- this account's Claude Code process can
                  ;; be reattached via herdr/shepherd for weeks without
                  ;; ever re-sourcing ~/.profile, so a value that only
                  ;; lived in the shell environment could go stale until
                  ;; that whole chain was torn down and restarted.
                  (map (lambda (server)
                         (home-claude-mcp-server
                          (inherit server)
                          (env (append (home-claude-mcp-server-env server)
                                       mcp-env))))
                       mcp-servers))))
    (home-environment
   (packages (append %claude-workstation-base-packages
                     (if with-anvil? (list emacs-no-x emacs-anvil) '())
                     extra-packages))
   (services
    (append
     (list
      base-log-dir-service

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
                   ("PROXYCHAINS_CONF_FILE" . "$HOME/.config/proxychains.conf")))
                (bashrc
                 (cons* claude-completion-bashrc
                        home-pi-koboldcpp-bashrc
                        (if (null? secret-env-vars)
                            '()
                            (list (secret-env-bashrc secret-env-vars)))))))
      ;; XDG_RUNTIME_DIR must go through home-environment-variables-service-type
      ;; (which lands in ~/.guix-home/setup-environment, sourced by ~/.profile),
      ;; NOT home-bash-configuration's own environment-variables field. Guix
      ;; Home's ~/.profile does:
      ;;   . $HOME_ENVIRONMENT/setup-environment
      ;;   $HOME_ENVIRONMENT/on-first-login
      ;; while ~/.bash_profile only sources ~/.profile and ~/.bashrc BEFORE
      ;; applying home-bash-configuration's own environment-variables field --
      ;; so a bash-scoped export here would still be unset when on-first-login
      ;; (which starts shepherd-for-home, anvil and herdr-server) runs its
      ;; XDG_RUNTIME_DIR check, producing the "XDG_RUNTIME_DIR doesn't exists"
      ;; warning even with the tmpfs mounted underneath it.
      (simple-service 'xdg-runtime-dir
                      home-environment-variables-service-type
                      '(("XDG_RUNTIME_DIR" . "$HOME/.cache/xdg-runtime")))
      (simple-service 'mcp-env
                      home-environment-variables-service-type
                      mcp-env)
      (simple-service 'git-identity
                      home-files-service-type
                      (list (list ".config/git/config"
                                  (git-config-file git-name git-email))))
      (simple-service 'proxychains-config
                      home-files-service-type
                      (list (list ".config/proxychains.conf"
                                  %proxychains-config-file)))
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
                 (list (local-file "claude-workstation-known-hosts")
                       (local-file "claude-workstation-loopback-known-hosts")))
                ;; Only peteches passes AUTOMATION-SSH-IDENTITY: a single
                ;; Host block applies to any `ssh <user>@localhost`
                ;; regardless of the requested user, since IdentityFile is
                ;; host-scoped, not user-scoped -- see
                ;; herdr-spaces-bootstrap-script, which connects as
                ;; criticalgrind@localhost / ygo@localhost.
                (hosts
                 (if automation-ssh-identity
                     (list (openssh-host
                            (name "localhost")
                            (identity-file automation-ssh-identity)))
                     '()))))
      (simple-service 'clone-area51-repos
                      home-activation-service-type
                      (repos-activation repos))
      ;; Built once and reused for both home-claude-service-type and
      ;; home-pi-service-type below -- pi-mcp-adapter's mcp.json wants the
      ;; exact same set of servers this account's Claude Code gets (see
      ;; MCP-SERVERS' docstring in (peteches home modules pi)), so there is
      ;; no separate "pi servers" list to keep in sync by hand.
      (service home-claude-service-type
               (home-claude-configuration
                (config-directory (repo-directory "configs/claude/defaults"))
                (mcp-servers %all-mcp-servers)))
      (service home-pi-service-type
               (home-pi-configuration
                (config-directory (repo-directory "configs/pi/defaults"))
                (extensions (list pi-mcp-adapter))
                (mcp-servers %all-mcp-servers))))
     (if (null? extra-claude-files)
         '()
         (list (simple-service 'claude-extra-files
                               home-files-service-type
                               (map (lambda (pair)
                                      (list (string-append ".claude/" (car pair))
                                            (cdr pair)))
                                    extra-claude-files))))
     (if with-anvil? (anvil-services) '())
     (if with-herdr? (herdr-services) '())
     (if (and with-herdr? (not (null? herdr-spaces)))
         (herdr-spaces-services herdr-spaces)
         '()))))))
