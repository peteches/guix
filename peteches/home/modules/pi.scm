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
;;; configs/pi/defaults/agents/ carries user-level overrides for the
;;; pi-interactive-subagents bundled agent definitions (researcher.md,
;;; scout.md, worker.md). The extension resolves each agent name in order:
;;; <cwd>/.pi/agents/<name>.md, then <agent-dir>/agents/<name>.md, then its
;;; own store-bundled agents/ — so the files here shadow the packaged ones
;;; for every account. They are byte-identical to the bundled definitions
;;; except for the `model:'/`thinking:' frontmatter lines: the fork pins
;;; openrouter/z-ai/glm-5.3, and none of the workstation accounts have an
;;; openrouter API key, so pinned spawns died with "No API key found for
;;; openrouter". With the pin removed the spawned pi gets no --model flag
;;; and pi's automatic startup selection picks the first model with
;;; configured auth — for these accounts that is the local koboldcpp model
;;; declared in models.json above (the only provider with a key; the static
;;; apiKey "local" in models.json counts as configured auth). Keep the
;;; bodies in sync with the fork when the package pin is bumped.
;;;
;;; configs/pi/defaults/settings.json (if present) is NOT symlinked like
;;; every other config-directory child -- pi's SettingsManager actively
;;; rewrites ~/.pi/agent/settings.json at runtime (defaultProvider,
;;; defaultModel, lastChangelogVersion, compaction, etc. all get saved
;;; there), same category of problem as Claude Code's ~/.claude.json (see
;;; (peteches home modules claude)'s module docstring). A hard symlink into
;;; the store would either fight every runtime save (if pi writes through
;;; it) or -- what actually happened on the peteches account the first time
;;; this was tried, 2026-09-27 -- get silently skipped by Guix Home's
;;; file-conflict handling on any account that already has a real
;;; settings.json, so the declared defaults never reach it and the mistake
;;; is invisible (no error, no backup file, just silence). Instead
;;; HOME-PI-SETTINGS-ACTIVATION below jq-merges configs/pi/defaults/
;;; settings.json into ~/.pi/agent/settings.json on every activation
;;; (creating it if absent), with the declared defaults taking precedence
;;; on any overlapping key -- the same "shell out at activation, merge with
;;; jq instead of symlinking" idiom claude.scm uses for ~/.claude.json's
;;; oauth.scopes patch.
;;;
;;; The default models.json wires up koboldcpp.ts.peteches.co.uk (Caddy's
;;; reverse proxy onto the comfyui VM's koboldcpp instance) as a custom
;;; provider. This replaced nug's own koboldcpp instance and its Tailscale
;;; IPv6-literal baseUrl following nug's decommission; the `pi-koboldcpp'
;;; shell function below (NODE_TLS_REJECT_UNAUTHORIZED=0 wrapper) predates
;;; that move and may no longer be necessary now that the baseUrl is a
;;; plain domain behind Caddy's own cert — left in place, unverified,
;;; since it's harmless if unneeded.
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
;;; EXTRA-EXTENSION-FILES (a list of (NAME . FILE) pairs) places FILE at
;;; ~/.pi/agent/extensions/NAME for single-file extensions -- pi's loader
;;; discovers loose .ts files in extensions/ as well as package
;;; directories (a typical install carries auto-continue.ts). This is how
;;; the repo ships herdr's bundled pi integration (herdr-agent-state.ts,
;;; the file `herdr integration install pi' would otherwise write by
;;; hand): committed under configs/pi/ so the integration is declarative
;;; and survives a fresh machine. The content is versioned with the herdr
;;; binary (HERDR_INTEGRATION_VERSION in its header; `herdr integration
;;; status' checks it), so re-copy it from a running install when herdr
;;; updates the integration.
;;;
;;; MCP-SERVERS reuses <home-claude-mcp-server> from (peteches home modules
;;; claude) rather than a parallel record type -- pi-mcp-adapter's
;;; mcp.json has the same {name: {command, args, env}} shape Claude Code's
;;; mcp-servers already carry, so the exact list built for an account's
;;; Claude config (anvil bridges, graphify, comfyui, …) is passed straight
;;; through here too; only NAME/COMMAND/ARGS/ENV are read for each entry
;;; actually rendered -- TRANSPORT is also read, but only to filter: an
;;; http-transport server (e.g. ygo's hosted Linear/Notion/Granola/Better
;;; Stack servers, see claude-workstation-ygo.scm) has no COMMAND to
;;; invoke, so it's dropped from mcp.json rather than rendered
;;; (pi-mcp-adapter has no http-transport support wired up here).
;;; URL/OAUTH-SCOPES/SCOPE are unused. Written
;;; to ~/.pi/agent/mcp.json, the adapter's own global-override file (see
;;; its README's file-layout precedence table) -- a fully static file,
;;; unlike claude.scm's activation-time `claude mcp add', because
;;; pi-mcp-adapter reads mcp.json directly at startup with no analogous
;;; runtime-mutated state to protect.

(define-module (peteches home modules pi)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((peteches home modules claude)
                #:select (home-claude-mcp-server-name
                          home-claude-mcp-server-command
                          home-claude-mcp-server-args
                          home-claude-mcp-server-env
                          home-claude-mcp-server-transport))
  #:export (home-pi-service-type
            home-pi-configuration))

(define-record-type* <home-pi-configuration>
  home-pi-configuration make-home-pi-configuration
  home-pi-configuration?
  (config-directory home-pi-configuration-config-directory
                    (default #f))
  (extensions       home-pi-configuration-extensions
                    (default '()))
  (extra-extension-files home-pi-configuration-extra-extension-files
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

;; Render one (NAME . FILE) pair from EXTRA-EXTENSION-FILES as a home-files
;; entry placing FILE at ~/.pi/agent/extensions/NAME. pi's extension loader
;; discovers loose files as well as package directories (a typical install
;; carries auto-continue.ts), so a single-file extension such as herdr's
;; bundled herdr-agent-state.ts (`herdr integration install pi') can be
;; committed to the repo and placed here without the runtime step.
(define (home-pi-extra-extension-entry pair)
  (list (string-append ".pi/agent/extensions/" (car pair))
        (cdr pair)))

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

;; SERVERS is shared verbatim with this account's Claude Code config (see
;; MCP-SERVERS' docstring above), so it can include http-transport entries
;; (e.g. ygo's Linear/Notion/Granola/Better Stack servers -- see
;; claude-workstation-ygo.scm) whose COMMAND is #f. home-pi-mcp-json-entry
;; only knows how to render a stdio COMMAND/ARGS invocation, so those are
;; dropped here rather than crashing mixed-text-file on a #f command.
(define (home-pi-mcp-json servers)
  (let ((stdio-servers
         (filter (lambda (s)
                   (string=? (home-claude-mcp-server-transport s) "stdio"))
                 servers)))
    (apply mixed-text-file "mcp.json"
           (append
            (list "{\"mcpServers\":{")
            (apply append
                   (intersperse (list ",") (map home-pi-mcp-json-entry stdio-servers)))
            (list "}}")))))

;; settings.json is excluded here -- it is runtime-mutated by pi itself, so
;; it is jq-merged at activation (HOME-PI-SETTINGS-ACTIVATION below)
;; instead of symlinked. See the module docstring.
(define (home-pi-files-service config)
  (let ((dir        (home-pi-configuration-config-directory config))
        (extensions (home-pi-configuration-extensions config))
        (extra      (home-pi-configuration-extra-extension-files config))
        (servers    (home-pi-configuration-mcp-servers config)))
    (append
     (if dir
         (map (lambda (entry) (home-pi-entry dir entry))
              (remove (lambda (entry) (string=? entry "settings.json"))
                      (directory-children dir)))
         '())
     (map home-pi-extension-entry extensions)
     (map home-pi-extra-extension-entry extra)
     (if (null? servers)
         '()
         (list (list ".pi/agent/mcp.json" (home-pi-mcp-json servers)))))))

(define %pi-koboldcpp-bashrc "\
# Predates the move to koboldcpp.ts.peteches.co.uk (Caddy proxy, own cert) --
# originally worked around nug's cert not covering the Tailscale IPv6
# literal models.json pointed pi at. Wraps invocations rather than
# disabling TLS verification for the whole shell.
pi-koboldcpp() {
  NODE_TLS_REJECT_UNAUTHORIZED=0 command pi --provider koboldcpp \"$@\"
}
")

(define-public home-pi-koboldcpp-bashrc
  (plain-file "pi-koboldcpp.bash" %pi-koboldcpp-bashrc))

;; Positional-arg bash script merging DEFAULTS into ~/.pi/agent/settings.json
;; -- $1 is the defaults file, $2 the jq binary. Declared defaults win on any
;; overlapping key (jq's `*' operator merges right-biased, recursively for
;; nested objects), which is what lets a fleet-wide default like
;; httpIdleTimeoutMs stay enforced across reconfigures without clobbering
;; pi's own runtime keys (defaultProvider, defaultModel, compaction, …) that
;; aren't present in the defaults file. Creates the file (a plain copy, not
;; a symlink, so pi can keep rewriting it afterwards) if it doesn't exist
;; yet, e.g. on a brand new account.
(define %home-pi-settings-merge-script "\
set -eu
defaults=\"$1\"; jq_bin=\"$2\"
target=\"$HOME/.pi/agent/settings.json\"
mkdir -p \"$(dirname \"$target\")\"
if [ -f \"$target\" ]; then
  tmp=\"$target.merge.tmp\"
  \"$jq_bin\" -s '.[0] * .[1]' \"$target\" \"$defaults\" > \"$tmp\" && mv \"$tmp\" \"$target\"
else
  cp \"$defaults\" \"$target\"
fi
")

(define (home-pi-settings-activation config)
  (let ((dir (home-pi-configuration-config-directory config)))
    (if (and dir (file-exists? (string-append dir "/settings.json")))
        (let ((defaults (local-file (string-append dir "/settings.json")))
              (bash-bin (file-append bash "/bin/bash"))
              (jq-bin   (file-append jq "/bin/jq")))
          #~(system* #$bash-bin "-c" #$%home-pi-settings-merge-script
                     "home-pi-settings-merge" #$defaults #$jq-bin))
        #~(begin))))

(define-public home-pi-service-type
  (service-type
   (name 'home-pi)
   (description "Manage the pi coding-agent CLI's ~/.pi config: models.json,
MCP-adapter extensions, and mcp.json.")
   (extensions
    (list (service-extension home-files-service-type
                             home-pi-files-service)
          (service-extension home-activation-service-type
                             home-pi-settings-activation)))
   (default-value (home-pi-configuration))))
