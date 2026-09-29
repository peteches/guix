(define-module (peteches packages pi-interactive-subagents)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system copy))

;; pi-interactive-subagents (https://github.com/peteches/pi-interactive-subagents)
;; -- fork of amosblomqvist/pi-interactive-subagents (itself a tmux-only fork
;; of HazAT/pi-interactive-subagents) with herdr support added: subagent
;; panes can run in herdr in addition to tmux (pane splits labeled with the
;; subagent's display name, pane wait-output as the exit-detection event
;; path).  Since 3.8.0 the herdr surface also lays the panes out -- parent
;; keeps 30% of the width (PI_SUBAGENT_HERDR_PARENT_RATIO), subagents stack
;; evenly in the other 70%, re-applied after every spawn and close -- and
;; /clear-subagents sweeps panes that outlived their run, using a per-session
;; pane registry under artifacts/<sessionId>/subagent-panes.json.  See the
;; fork's README "Terminal multiplexers" section.
;;
;; Packaged as the plain repository tree under
;; lib/node_modules/pi-interactive-subagents so that
;; (peteches home modules pi)'s EXTENSIONS field can symlink it to
;; ~/.pi/agent/extensions/pi-interactive-subagents -- pi's extension loader
;; (dist/core/extensions/loader.js) discovers a subdirectory whose
;; package.json declares a "pi.extensions" field, which this package.json
;; does ("./pi-extension/subagents/index.ts").  No npm dependency closure is
;; needed: pi's extension loader transpiles the .ts entry with its bundled
;; jiti and resolves the pi APIs (@mariozechner/pi-coding-agent, pi-tui,
;; @sinclair/typebox) through its own loader aliases, so the extension has
;; no runtime npm dependencies of its own.
;;
;; The commit pin is the fork's main; bump it (plus the version, which
;; tracks the fork's package.json) to update, then redeploy -- the
;; home-pi-service-type instances on claude-workstation pick it up for all
;; three accounts in one system reconfigure.

(define-public pi-interactive-subagents
  (package
    (name "pi-interactive-subagents")
    (version "3.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peteches/pi-interactive-subagents")
             (commit "c07dc58726dcc950b1db32449e0d39015cfc1639")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14x0031lgrf1sg5wsz94jbr0pk3nlb1m2r9gz7mwvzaqffqbx9nf"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; The four things pi needs at runtime: the package.json manifest,
      ;; the status config (config.json.example -- the extension's
      ;; readStatusConfigFile falls back to it when no config.json exists,
      ;; and parseStatusConfig accepts its {status:{enabled}} shape), the
      ;; extension sources, and the bundled agent definitions.  test/ and
      ;; the docs are not needed by the loader.
      #:install-plan
      #~(list (list "package.json" "lib/node_modules/pi-interactive-subagents/package.json")
              (list "config.json.example" "lib/node_modules/pi-interactive-subagents/config.json.example")
              (list "pi-extension" "lib/node_modules/pi-interactive-subagents/pi-extension")
              (list "agents" "lib/node_modules/pi-interactive-subagents/agents"))))
    (home-page "https://github.com/peteches/pi-interactive-subagents")
    (license expat)
    (synopsis "Interactive subagents for the pi coding agent (tmux or herdr panes)")
    (description
     "pi-interactive-subagents is a pi extension that runs sub-agents in
terminal panes: spawn a sub-agent with the @code{subagent} tool, keep
working in the main session, and get the result steered back when it
finishes.  Panes run in tmux or, on this fork, in herdr -- herdr panes are
labeled with the sub-agent's display name and exit detection uses herdr's
@command{pane wait-output} as the blocking event path.")))