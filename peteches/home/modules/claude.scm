;;; peteches/home/modules/claude.scm — home service type for Claude Code.
;;;
;;; Unlike its siblings this module defines a *service type*, not just
;;; configuration values.  It does two things:
;;;
;;;   1. Symlinks each child of `config-directory' into ~/.claude/ via
;;;      home-files-service-type.  Called with configs/claude/defaults in
;;;      (peteches home modules base).
;;;   2. Registers `mcp-servers' by shelling out to `claude mcp add' during
;;;      home activation — removing then re-adding each one so the entry
;;;      always reflects current config.  The `remove' is expected to fail
;;;      on first run; its exit status is deliberately ignored.  Each server
;;      is either `stdio' (a local COMMAND/ARGS, the default) or `http' (a
;;      hosted endpoint registered by URL, e.g. Linear/Notion/Granola's
;;      remote MCP servers) — see <home-claude-mcp-server>.
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
  #:use-module (peteches packages claude-code)
  #:export (home-claude-service-type
            home-claude-configuration
            home-claude-mcp-server))

;; TRANSPORT is "stdio" (default) or "http". stdio servers run a local
;; COMMAND/ARGS; http servers are hosted endpoints registered by URL only
;; (COMMAND/ARGS unused) — OAuth for these happens interactively the first
;; time the account runs `/mcp` inside a Claude Code session, not at
;; activation time, so no secret wiring is needed here.
(define-record-type* <home-claude-mcp-server>
  home-claude-mcp-server make-home-claude-mcp-server
  home-claude-mcp-server?
  (name      home-claude-mcp-server-name)
  (command   home-claude-mcp-server-command   (default #f))
  (args      home-claude-mcp-server-args      (default '()))
  (transport home-claude-mcp-server-transport (default "stdio"))
  (url       home-claude-mcp-server-url       (default #f))
  (scope     home-claude-mcp-server-scope     (default "user")))

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

(define (home-claude-activation-service config)
  (let* ((servers    (home-claude-configuration-mcp-servers config))
         (claude-bin (file-append claude-code "/bin/claude")))
    #~(begin
        #$@(map (lambda (server)
                  (let* ((name      (home-claude-mcp-server-name server))
                         (scope     (home-claude-mcp-server-scope server))
                         (transport (home-claude-mcp-server-transport server))
                         (cmd       (home-claude-mcp-server-command server))
                         (args      (home-claude-mcp-server-args server))
                         (url       (home-claude-mcp-server-url server)))
                    (if (string=? transport "stdio")
                        #~(begin
                            ;; Remove existing entry; non-zero exit is harmless.
                            (system* #$claude-bin
                                     "mcp" "remove" "--scope" #$scope #$name)
                            ;; Re-add with current config.  -- separates claude
                            ;; flags from the subprocess command and its args.
                            (apply system*
                                   #$claude-bin "mcp" "add"
                                   "--scope" #$scope
                                   "--transport" "stdio"
                                   #$name "--" #$cmd
                                   (list #$@args)))
                        ;; http (or other remote) transport: URL only, no
                        ;; local command. OAuth happens later via `/mcp'.
                        #~(begin
                            (system* #$claude-bin
                                     "mcp" "remove" "--scope" #$scope #$name)
                            (system* #$claude-bin "mcp" "add"
                                     "--scope" #$scope
                                     "--transport" #$transport
                                     #$name #$url)))))
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
