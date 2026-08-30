;;; peteches/home/modules/pi.scm — home service type for the `pi` coding
;;; agent (https://pi.dev, packaged in peteches/packages/pi-coding-agent.scm).
;;;
;;; Mirrors (peteches home modules claude)'s config-directory mechanism:
;;; symlinks each child of CONFIG-DIRECTORY into ~/.pi/agent/ -- pi's
;;; getAgentDir() (dist/config.js) resolves to join(homedir(), ".pi",
;;; "agent"), NOT ~/.pi/ itself; auth.json, models-store.json and
;;; models.json all live there. Currently that's just models.json
;;; (configs/pi/defaults/models.json), which declares
;;; custom OpenAI-completions-compatible providers/models per pi's
;;; core/model-config.js schema — pi loads this file itself at startup, so
;;; unlike Claude Code's ~/.claude.json there is no runtime-mutated state
;;; here to protect and no activation script is needed.
;;;
;;; The default models.json wires up nug's koboldcpp server as a custom
;;; provider. Its cert (Let's Encrypt, CN nug.peteches.co.uk) doesn't cover
;;; the Tailscale IPv6 literal the baseUrl connects through, so pi's own
;;; HTTPS calls need certificate verification disabled — see the
;;; `pi-koboldcpp' shell function this module installs, which sets
;;; NODE_TLS_REJECT_UNAUTHORIZED=0 only for that wrapped invocation rather
;;; than exporting it into the whole shell environment.
;;;
;;; EXTENSIONS (a list of packages, e.g. pi-mcp-adapter from peteches
;;; packages pi-coding-agent) symlinks each package's own
;;; lib/node_modules/<package-name> output to
;;; ~/.pi/agent/extensions/<package-name> -- pi's extension loader
;;; (dist/core/extensions/loader.js:discoverAndLoadExtensions) walks
;;; <agent-dir>/extensions/ for exactly this shape: a subdirectory with a
;;; package.json declaring a "pi.extensions" field. Declarative equivalent
;;; of `pi install npm:<name>', without a runtime npm/network step.
;;;
;;; MCP-SERVERS reuses <home-claude-mcp-server> from (peteches home modules
;;; claude) rather than a parallel record type -- pi-mcp-adapter's
;;; mcp.json has the same {name: {command, args, env}} shape Claude Code's
;;; mcp-servers already carry, so the exact list built for an account's
;;; Claude config (anvil bridges, graphify, comfyui, …) is passed straight
;;; through here too; only NAME/COMMAND/ARGS/ENV are read -- TRANSPORT/URL/
;;; OAUTH-SCOPES/SCOPE (claude-specific, e.g. for hosted http servers) are
;;; ignored, since no account here configures one of those for pi. Written
;;; to ~/.pi/agent/mcp.json, the adapter's own global-override file (see
;;; its README's file-layout precedence table) -- a fully static file,
;;; unlike claude.scm's activation-time `claude mcp add', because
;;; pi-mcp-adapter reads mcp.json directly at startup with no analogous
;;; runtime-mutated state to protect.

(define-module (peteches home modules pi)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module ((peteches home modules claude)
                #:select (home-claude-mcp-server-name
                          home-claude-mcp-server-command
                          home-claude-mcp-server-args
                          home-claude-mcp-server-env))
  #:export (home-pi-service-type
            home-pi-configuration))

(define-record-type* <home-pi-configuration>
  home-pi-configuration make-home-pi-configuration
  home-pi-configuration?
  (config-directory home-pi-configuration-config-directory
                    (default #f))
  (extensions       home-pi-configuration-extensions
                    (default '()))
  (mcp-servers      home-pi-configuration-mcp-servers
                    (default '())))

(define (directory-children directory)
  "Return the non-special immediate children of DIRECTORY."
  (filter (lambda (e) (not (member e '("." ".."))))
          (scandir directory)))

(define (home-pi-entry dir entry)
  (let ((src (string-append dir "/" entry)))
    (list (string-append ".pi/agent/" entry)
          (local-file src #:recursive? (file-is-directory? src)))))

(define (home-pi-extension-entry pkg)
  "Symlink PKG's global npm-install output (lib/node_modules/<name>) to
~/.pi/agent/extensions/<name>, where pi's extension loader discovers it."
  (list (string-append ".pi/agent/extensions/" (package-name pkg))
        (file-append pkg "/lib/node_modules/" (package-name pkg))))

;; Interleave SEP between the elements of LST -- used below to join JSON
;; fragment-lists with "," without a trailing/leading comma.
(define (intersperse sep lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        (else (cons (car lst) (cons sep (intersperse sep (cdr lst)))))))

;; Render one <home-claude-mcp-server> as a {"name":{"command":…,"args":
;; […],"env":{…}}} JSON fragment -- a list of strings/file-like objects
;; for MIXED-TEXT-FILE, since COMMAND is itself typically a file-like
;; object (e.g. (file-append bash "/bin/bash")) whose real store path is
;; only known once built, not a literal string this module could splice
;; by hand. None of the servers built by (peteches home modules claude-
;; workstation) use characters needing JSON escaping (paths, flag names).
(define (json-string-list strs)
  (apply append (intersperse (list ",") (map (lambda (s) (list "\"" s "\"")) strs))))

(define (home-pi-mcp-json-entry server)
  (let ((name (home-claude-mcp-server-name server))
        (cmd  (home-claude-mcp-server-command server))
        (args (home-claude-mcp-server-args server))
        (env  (home-claude-mcp-server-env server)))
    (append
     (list "\"" name "\":{\"command\":\"") (list cmd) (list "\"")
     (list ",\"args\":[") (json-string-list args) (list "]")
     (if (null? env)
         '()
         (append
          (list ",\"env\":{")
          (apply append
                 (intersperse (list ",")
                               (map (lambda (pair)
                                      (list "\"" (car pair) "\":\"" (cdr pair) "\""))
                                    env)))
          (list "}")))
     (list "}"))))

(define (home-pi-mcp-json servers)
  (apply mixed-text-file "mcp.json"
         (append
          (list "{\"mcpServers\":{")
          (apply append
                 (intersperse (list ",") (map home-pi-mcp-json-entry servers)))
          (list "}}"))))

(define (home-pi-files-service config)
  (let ((dir        (home-pi-configuration-config-directory config))
        (extensions (home-pi-configuration-extensions config))
        (servers    (home-pi-configuration-mcp-servers config)))
    (append
     (if dir
         (map (lambda (entry) (home-pi-entry dir entry))
              (directory-children dir))
         '())
     (map home-pi-extension-entry extensions)
     (if (null? servers)
         '()
         (list (list ".pi/agent/mcp.json" (home-pi-mcp-json servers)))))))

(define %pi-koboldcpp-bashrc "\
# koboldcpp's cert (CN nug.peteches.co.uk) doesn't cover the Tailscale
# IPv6 literal models.json points pi at, so wrap invocations that need it
# rather than disabling TLS verification for the whole shell.
pi-koboldcpp() {
  NODE_TLS_REJECT_UNAUTHORIZED=0 command pi --provider koboldcpp \"$@\"
}
")

(define-public home-pi-koboldcpp-bashrc
  (plain-file "pi-koboldcpp.bash" %pi-koboldcpp-bashrc))

(define-public home-pi-service-type
  (service-type
   (name 'home-pi)
   (description "Manage the pi coding-agent CLI's ~/.pi config: models.json,
MCP-adapter extensions, and mcp.json.")
   (extensions
    (list (service-extension home-files-service-type
                             home-pi-files-service)))
   (default-value (home-pi-configuration))))
