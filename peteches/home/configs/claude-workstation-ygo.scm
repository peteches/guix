;;; Home config for the `ygo' account on claude-workstation.
;;;
;;; Run ON THE VM, as the ygo user:
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-ygo.scm
;;;
;;; Bootstrap: clone the guix repo by hand into ~/area_51/guix first, then run
;;; the command above.
;;;
;;; One account-specific addition: a skill telling this account's Claude
;;; about the split-tunnel WireGuard + SOCKS5 proxy on this VM (see
;;; peteches/services/wireguard-socks5.scm). It is deliberately NOT passed
;;; to the peteches/criticalgrind configs, so only ygo's Claude knows it
;;; exists.
;;;
;;; Also registers Linear, Notion and Granola as `http'-transport MCP
;;; servers -- all three are hosted, OAuth-authenticated endpoints (not
;;; local packages), so there is no package to build and no API key to
;;; wire through sops-secrets. Registration just points Claude Code at the
;;; URL; the first `/mcp' run inside a session on this account opens the
;;; OAuth flow interactively.
;;;
;;; Evaluates to a bare `home-environment' as its final expression.

(define-module (peteches home configs claude-workstation-ygo)
  #:use-module (guix gexp)
  ;; Import packages directly instead of `specification->package'.  A top-level
  ;; `specification->package' fires `fold-packages' over `%package-module-path',
  ;; which `guix deploy'/`guix system' populate with the repo (`-L .'); the scan
  ;; then re-enters this half-loaded module tree and every module fails to bind.
  ;; See the note in (peteches home modules claude-workstation).
  #:use-module ((gnu packages golang) #:select (go))
  #:use-module ((gnu packages containers) #:select (podman))
  #:use-module ((gnu packages databases) #:select (postgresql redis))
  #:use-module (peteches packages go-tools)
  #:use-module (peteches packages yarn)
  #:use-module ((peteches packages mcp) #:select (slack-mcp-server))
  #:use-module (peteches repository)
  #:use-module (peteches home modules claude-workstation)
  #:use-module (peteches home modules claude))

;; EDIT ME: the repos this account works on. SSH clone URL -- requires an
;; SSH key authorized against the repo loaded for this account; a clone
;; that can't authenticate just warns and retries next reconfigure (see
;; repos-activation in (peteches home modules claude-workstation)).
(define %ygo-repos
  '(("ygocloud" "git@github.com:ygotrips/ygocloud.git")))

;; Linear, Notion and Granola all ship hosted MCP servers reached over
;; Streamable HTTP with browser-based OAuth -- see the module docstring.
(define %ygo-mcp-servers
  (list (home-claude-mcp-server
         (name "linear")
         (transport "http")
         (url "https://mcp.linear.app/mcp"))
        (home-claude-mcp-server
         (name "notion")
         (transport "http")
         (url "https://mcp.notion.com/mcp"))
        (home-claude-mcp-server
         (name "granola")
         (transport "http")
         (url "https://mcp.granola.ai/mcp"))
        ;; Slack, unlike the three above, has no hosted OAuth MCP endpoint --
        ;; it runs locally (stdio) and reads its auth token from the
        ;; environment. SLACK_MCP_XOXP_TOKEN below (via #:secret-env-vars)
        ;; supplies it; see claude-workstation.scm's #:sops-secrets for
        ;; where /run/secrets/slack-mcp-xoxp-token comes from.
        (home-claude-mcp-server
         (name "slack")
         (command (file-append slack-mcp-server "/bin/slack-mcp-server")))))

(define-public claude-workstation-ygo-home
  (make-claude-workstation-home
   #:git-name "Peter McCabe"
   #:git-email "peter.mccabe@ygo.ai"
   #:repos %ygo-repos
   ;; go/go-golangci-lint/yarn for the ygocloud toolchain; podman so this
   ;; account can also drive containers ad hoc (`podman ps' etc against its
   ;; own rootless storage) on top of the always-on dev Postgres/TimescaleDB/
   ;; Redis containers managed at the system level -- see the oci-service-type
   ;; extensions in claude-workstation.scm. postgresql/redis are pulled in
   ;; purely for their client binaries (psql, redis-cli) to talk to those
   ;; same dev containers on 127.0.0.1:5432/5433/6379 -- neither package has
   ;; a lighter client-only variant in this channel (same tradeoff
   ;; criticalgrind's config already makes for psql).
   #:extra-packages (list go go-golangci-lint yarn podman postgresql redis)
   #:mcp-servers %ygo-mcp-servers
   ;; Non-secret feature flag: the slack-mcp-server binary registers
   ;; conversations_add_message (message posting) only when this is set --
   ;; off by default even with chat:write granted on the token. "true"
   ;; allows posting to any channel the xoxp- token can see; set it to a
   ;; comma-separated channel-ID allowlist instead to scope it down.
   #:mcp-env '(("SLACK_MCP_ADD_MESSAGE_TOOL" . "true"))
   ;; Secret: sops-decrypted at system boot into /run/secrets/... (see
   ;; #:sops-secrets in peteches/systems/claude-workstation.scm), exported
   ;; into the shell from there -- never baked into the store. The slack MCP
   ;; server (spawned by Claude Code, a child of this shell) inherits it.
   #:secret-env-vars '(("SLACK_MCP_XOXP_TOKEN" . "/run/secrets/slack-mcp-xoxp-token"))
   #:extra-claude-files
   (list (cons "skills/wireguard-socks5/SKILL.md"
               (local-file (source-path
                            "configs/claude/ygo-only/skills/wireguard-socks5/SKILL.md"))))))

claude-workstation-ygo-home
