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
  #:use-module (peteches packages go-tools)
  #:use-module (peteches packages yarn)
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
         (url "https://mcp.granola.ai/mcp"))))

(define-public claude-workstation-ygo-home
  (make-claude-workstation-home
   #:git-name "Peter McCabe"
   #:git-email "peter.mccabe@ygo.ai"
   #:repos %ygo-repos
   ;; go/go-golangci-lint/yarn for the ygocloud toolchain; podman so this
   ;; account can also drive containers ad hoc (`podman ps' etc against its
   ;; own rootless storage) on top of the always-on dev Postgres/TimescaleDB/
   ;; Redis containers managed at the system level -- see the oci-service-type
   ;; extensions in claude-workstation.scm.
   #:extra-packages (list go go-golangci-lint yarn podman)
   #:mcp-servers %ygo-mcp-servers
   #:extra-claude-files
   (list (cons "skills/wireguard-socks5/SKILL.md"
               (local-file (source-path
                            "configs/claude/ygo-only/skills/wireguard-socks5/SKILL.md"))))))

claude-workstation-ygo-home
