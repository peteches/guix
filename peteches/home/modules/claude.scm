;;; peteches/home/modules/claude.scm — home service type for Claude Code.
;;;
;;; Unlike its siblings this module defines a *service type*, not just
;;; configuration values.  It does two things:
;;;
;;;   1. Symlinks each child of `config-directory' into ~/.claude/ via
;;;      home-files-service-type.  Called with configs/claude/defaults in
;;;      (peteches home modules base).
;;;   2. Registers `mcp-servers' by shelling out to `claude mcp add' during
;;;      home activation.  Each server is either `stdio' (a local
;;;      COMMAND/ARGS, the default) or `http' (a hosted endpoint registered
;;;      by URL, e.g. Linear/Notion/Granola's remote MCP servers) — see
;;;      <home-claude-mcp-server>.
;;;
;;;      stdio servers are removed then re-added every activation so the
;;;      entry always reflects current config; the `remove' is expected to
;;;      fail on first run and its exit status is deliberately ignored.
;;;
;;;      http servers are handled idempotently instead: activation skips
;;;      remove/add when `~/.claude.json' already has this name registered
;;;      against the same URL.  A blind remove/add here would reset the
;;;      entry to url-only on every activation, discarding the OAuth grant
;;;      the account got via `/mcp' and the `oauth.scopes' hint below —
;;;      `claude mcp add' has no flag for it, so when OAUTH-SCOPES is set
;;;      activation patches it into `~/.claude.json' with `jq' after the
;;;      entry exists.
;;;
;;; Why (2) shells out rather than writing ~/.claude.json directly: that
;;; file is also written by Claude Code itself at runtime (project history,
;;; auth state), so hand-generating it would clobber live state.
;;;
;;; The service type is `extend'-able — extensions are appended to
;;; mcp-servers — though nothing currently extends it.

(define-module (peteches home modules claude)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module (peteches packages claude-code)
  #:export (home-claude-service-type
            home-claude-configuration
            home-claude-mcp-server
            home-claude-mcp-server-env))

;; TRANSPORT is "stdio" (default) or "http". stdio servers run a local
;; COMMAND/ARGS; http servers are hosted endpoints registered by URL only
;; (COMMAND/ARGS unused) — OAuth for these happens interactively the first
;; time the account runs `/mcp` inside a Claude Code session, not at
;; activation time, so no secret wiring is needed here.
;;
;; OAUTH-SCOPES (http only) is not a secret — it is the space-separated
;; scope string the provider expects (e.g. one that includes
;; "offline_access" so the resulting grant carries a refresh token). It is
;; a hint for the OAuth flow, not a credential; leave it #f for a provider
;; whose default scope is already correct.
;;
;; ENV (stdio only) is a NON-SECRET alist of (NAME . VALUE) strings baked
;; into this server's ~/.claude.json entry via `claude mcp add -e` at
;; activation time. Deliberately NOT ambient-shell-inherited: this server's
;; subprocess is spawned by Claude Code itself, which on claude-workstation
;; may be reattached for weeks via herdr/shepherd without ever re-sourcing
;; ~/.profile, so a value exported only via
;; home-environment-variables-service-type can silently go stale until the
;; whole herdr/shepherd chain is torn down and restarted. Baking it here
;; instead makes a `guix home reconfigure' + MCP reconnect sufficient.
(define-record-type* <home-claude-mcp-server>
  home-claude-mcp-server make-home-claude-mcp-server
  home-claude-mcp-server?
  (name         home-claude-mcp-server-name)
  (command      home-claude-mcp-server-command      (default #f))
  (args         home-claude-mcp-server-args         (default '()))
  (transport    home-claude-mcp-server-transport    (default "stdio"))
  (env          home-claude-mcp-server-env          (default '()))
  (url          home-claude-mcp-server-url          (default #f))
  (oauth-scopes home-claude-mcp-server-oauth-scopes (default #f))
  (scope        home-claude-mcp-server-scope        (default "user")))

(define-record-type* <home-claude-configuration>
  home-claude-configuration make-home-claude-configuration
  home-claude-configuration?
  (config-directory home-claude-configuration-config-directory
                    (default #f))
  (mcp-servers      home-claude-configuration-mcp-servers
                    (default '())))

(define (directory-children directory)
  "Return the non-special immediate children of DIRECTORY."
  (filter (lambda (e) (not (member e '("." ".."))))
          (scandir directory)))

;; ENTRY is a top-level child of DIR (e.g. "skills", "CLAUDE.md"). A plain
;; file becomes one (DST FILE-LIKE) pair as before. A directory is expanded
;; one level -- each of ITS children becomes its own pair -- rather than
;; being symlinked whole. That lets an account's #:extra-claude-files add a
;; sibling entry *inside* "skills" or "agents" (e.g. an account-specific
;; skill) without colliding with this directory's own contents: two
;; home-files entries can't both claim ".claude/skills" (one as a single
;; whole-directory symlink, the other needing it to be a real directory to
;; hold a nested entry) -- home-files-service-type only conflicts if two
;; entries share the exact same destination path, and per-child entries
;; never do.
(define (home-claude-entry dir entry)
  (let ((src (string-append dir "/" entry)))
    (if (file-is-directory? src)
        (map (lambda (child)
               (let ((child-src (string-append src "/" child)))
                 (list (string-append ".claude/" entry "/" child)
                       (local-file child-src
                                   #:recursive? (file-is-directory? child-src)))))
             (directory-children src))
        (list (list (string-append ".claude/" entry) (local-file src))))))

(define (home-claude-files-service config)
  (let ((dir (home-claude-configuration-config-directory config)))
    (if dir
        (append-map (lambda (entry) (home-claude-entry dir entry))
                    (directory-children dir))
        '())))

;; Positional-arg bash script backing the http branch below. Takes
;; NAME URL SCOPE TRANSPORT OAUTH-SCOPES CLAUDE-BIN JQ-BIN as $1..$7.
;; Registers the server only if `~/.claude.json' doesn't already have this
;; name pointed at this URL -- see the module docstring for why a blind
;; remove/add is wrong for http servers -- then, if OAUTH-SCOPES is
;; non-empty, patches it into that entry's `oauth.scopes' with jq (no
;; `claude mcp add' flag exists for it).
(define %home-claude-http-mcp-script "\
set -eu
name=\"$1\"; url=\"$2\"; scope=\"$3\"; transport=\"$4\"; oauth_scopes=\"$5\"
claude_bin=\"$6\"; jq_bin=\"$7\"
cfg=\"$HOME/.claude.json\"
cur_url=$(\"$jq_bin\" -r --arg n \"$name\" '.mcpServers[$n].url // empty' \"$cfg\" 2>/dev/null || true)
if [ \"$cur_url\" != \"$url\" ]; then
  \"$claude_bin\" mcp remove --scope \"$scope\" \"$name\" || true
  \"$claude_bin\" mcp add --scope \"$scope\" --transport \"$transport\" \"$name\" \"$url\"
fi
if [ -n \"$oauth_scopes\" ]; then
  tmp=\"$cfg.oauth-scopes.tmp\"
  \"$jq_bin\" --arg n \"$name\" --arg s \"$oauth_scopes\" \\
    '.mcpServers[$n].oauth.scopes = $s' \"$cfg\" > \"$tmp\" && mv \"$tmp\" \"$cfg\"
fi
")

(define (home-claude-activation-service config)
  (let* ((servers    (home-claude-configuration-mcp-servers config))
         (claude-bin (file-append claude-code "/bin/claude"))
         (bash-bin   (file-append bash "/bin/bash"))
         (jq-bin     (file-append jq "/bin/jq")))
    #~(begin
        #$@(map (lambda (server)
                  (let* ((name         (home-claude-mcp-server-name server))
                         (scope        (home-claude-mcp-server-scope server))
                         (transport    (home-claude-mcp-server-transport server))
                         (cmd          (home-claude-mcp-server-command server))
                         (args         (home-claude-mcp-server-args server))
                         (env          (home-claude-mcp-server-env server))
                         (env-flags    (append-map
                                        (lambda (pair)
                                          (list "-e" (string-append (car pair)
                                                                     "=" (cdr pair))))
                                        env))
                         (url          (home-claude-mcp-server-url server))
                         (oauth-scopes (or (home-claude-mcp-server-oauth-scopes server)
                                           "")))
                    (if (string=? transport "stdio")
                        #~(begin
                            ;; Remove existing entry; non-zero exit is harmless.
                            (system* #$claude-bin
                                     "mcp" "remove" "--scope" #$scope #$name)
                            ;; Re-add with current config. -e KEY=VALUE
                            ;; flags (from ENV) go AFTER the positional name
                            ;; -- `-e' is a variadic option that otherwise
                            ;; swallows the name token too -- and `--'
                            ;; separates claude's own flags from the
                            ;; subprocess command and its args.
                            (apply system*
                                   #$claude-bin "mcp" "add"
                                   "--scope" #$scope
                                   "--transport" "stdio"
                                   #$name
                                   (append (list #$@env-flags)
                                           (list "--" #$cmd)
                                           (list #$@args))))
                        ;; http (or other remote) transport: idempotent
                        ;; registration + optional oauth.scopes patch, see
                        ;; %home-claude-http-mcp-script.
                        #~(system* #$bash-bin "-c" #$%home-claude-http-mcp-script
                                   "home-claude-http-mcp"
                                   #$name #$url #$scope #$transport
                                   #$oauth-scopes #$claude-bin #$jq-bin))))
                servers))))

(define-public home-claude-service-type
  (service-type
   (name 'home-claude)
   (description "Manage Claude Code CLI: static config files and MCP servers.")
   (compose concatenate)
   (extend (lambda (config extensions)
             (home-claude-configuration
              (inherit config)
              (mcp-servers (append (home-claude-configuration-mcp-servers config)
                                   extensions)))))
   (default-value (home-claude-configuration))
   (extensions
    (list (service-extension home-files-service-type
                             home-claude-files-service)
          (service-extension home-activation-service-type
                             home-claude-activation-service)))))
