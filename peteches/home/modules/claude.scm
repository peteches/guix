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
;;;      on first run; its exit status is deliberately ignored.
;;;
;;; Why (2) shells out rather than writing ~/.claude.json directly: that
;;; file is also written by Claude Code itself at runtime (project history,
;;; auth state), so hand-generating it would clobber live state.
;;;
;;; The service type is `extend'-able — extensions are appended to
;;; mcp-servers — though nothing currently extends it.
;;;
;;; Note the split: this covers the *host* Claude Code install.  The
;;; containerised launcher is a separate package in (containers claude),
;;; which does its own MCP registration inside the container.

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

(define-record-type* <home-claude-mcp-server>
  home-claude-mcp-server make-home-claude-mcp-server
  home-claude-mcp-server?
  (name    home-claude-mcp-server-name)
  (command home-claude-mcp-server-command)
  (args    home-claude-mcp-server-args  (default '()))
  (scope   home-claude-mcp-server-scope (default "user")))

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

(define (home-claude-files-service config)
  (let ((dir (home-claude-configuration-config-directory config)))
    (if dir
        (map (lambda (entry)
               (let ((src (string-append dir "/" entry))
                     (dst (string-append ".claude/" entry)))
                 `(,dst ,(local-file src #:recursive? (file-is-directory? src)))))
             (directory-children dir))
        '())))

(define (home-claude-activation-service config)
  (let* ((servers    (home-claude-configuration-mcp-servers config))
         (claude-bin (file-append claude-code "/bin/claude")))
    #~(begin
        #$@(map (lambda (server)
                  (let ((name  (home-claude-mcp-server-name server))
                        (scope (home-claude-mcp-server-scope server))
                        (cmd   (home-claude-mcp-server-command server))
                        (args  (home-claude-mcp-server-args server)))
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
                               (list #$@args)))))
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
