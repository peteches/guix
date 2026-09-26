(define-module (peteches packages pi-observational-memory)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system copy))

;; pi-observational-memory
;; (https://github.com/elpapi42/pi-observational-memory) -- V3
;; "observational memory" for pi: background observer, reflector and
;; dropper agents record observations and reflections into the session
;; ledger as the conversation grows, and pi's compaction renders a
;; deterministic, cache-friendly, tiered projection of that memory
;; instead of a plain summary.
;;
;; NO runtime npm dependencies: pi's extension loader transpiles the .ts
;; source with its bundled jiti and resolves the pi APIs
;; (@earendil-works/pi-coding-agent, pi-ai, pi-agent-core, pi-tui)
;; through its own loader aliases -- the same packaging shape as
;; pi-dictate.  The repo's package.json declares "pi.extensions"
;; ["./src/index.ts"]; unlike pi-dictate's single file, src/ here is a
;; multi-file tree (agents/, commands/, hooks/, session-ledger/, tools/),
;; so the whole tree is installed below in one recursive entry (the copy
;; build system's install phase copies directory entries recursively).
;; Packaged under lib/node_modules/pi-observational-memory so that
;; (peteches home modules pi)'s EXTENSIONS field can symlink it to
;; ~/.pi/agent/extensions/pi-observational-memory -- the loader's
;; discoverExtensionsInDir follows directory symlinks and
;; resolveExtensionEntries picks up the manifest there.
;;
;; Runtime requirements (NOT npm -- nothing to wire):
;;   - No API key, no environment variable.  Memory workers use the
;;     current session model by default (the "model" setting is unset);
;;     if no usable model or credentials are available the relevant
;;     worker skips safely rather than failing the session.
;;   - Settings live under the "observational-memory" namespace of
;;     ~/.pi/agent/settings.json, which pi ITSELF mutates at runtime
;;     (first-time setup, `pi config', analytics opt-in) -- so it is
;;     deliberately NOT shipped from configs/pi/defaults (whose children
;;     are symlinked into the agent dir and would clobber that runtime
;;     state on every reconfigure).  The extension's defaults are
;;     sufficient (see its docs/configuration.md); tune it later by
;;     editing that file interactively if ever wanted.
;;   - The extension registers the /om:status and /om:view commands and
;;     a recall tool; /om:status is the smoke test that it loaded.
;;
;; V3 note: settings are loaded once per pi runtime and old V2 keys are
;; ignored -- a fresh pi session is required after enabling (existing
;; sessions keep running without it).
;;
;; The commit pin is upstream master; bump it (plus the version, which
;; tracks the repo's package.json) to update, then redeploy -- the
;; home-pi-service-type instance on claude-workstation picks it up for
;; the account that passes it via #:pi-extensions.

(define-public pi-observational-memory
  (package
    (name "pi-observational-memory")
    (version "3.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elpapi42/pi-observational-memory")
             (commit "e891667d10fba3c70dd137fa55823ac1440f6f0d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wir8n39f24qv49jgkn7b3gfn8040wdhlb6lpjx363p12kbvvcj1"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; The three things pi's extension loader needs: the package.json
      ;; manifest (declares "pi.extensions"), the whole extension source
      ;; tree (src/index.ts entry plus its submodules), and the license.
      ;; tests/, scripts/, docs/, the vitest toolchain and the
      ;; TypeScript dev config are not needed at runtime.
      #:install-plan
      #~(list (list "package.json"
                    "lib/node_modules/pi-observational-memory/package.json")
              (list "src"
                    "lib/node_modules/pi-observational-memory/src")
              (list "LICENSE"
                    "lib/node_modules/pi-observational-memory/LICENSE"))))
    (home-page "https://github.com/elpapi42/pi-observational-memory")
    (license expat)
    (synopsis "Observational memory for the pi coding agent (V3 tiered compaction)")
    (description
     "pi-observational-memory is a pi extension adding V3 observational
memory: background observer, reflector and dropper agents record
observations and reflections into the session ledger as the
conversation grows, and pi's compaction renders a deterministic,
cache-friendly, tiered projection of that memory instead of a plain
summary.  Memory workers use the current session model by default; no
API key or environment variable is needed.  The @command{/om:status}
and @command{/om:view} commands and a recall tool are registered for
inspecting and querying the memory.")))
