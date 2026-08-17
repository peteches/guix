;;; Home config for the `ygo' account on claude-workstation.
;;;
;;; Run ON THE VM, as the ygo user:
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-ygo.scm
;;;
;;; Bootstrap: clone the guix repo by hand into ~/area_51/guix first, then run
;;; the command above.
;;;
;;; Bare-bones account: no extra repos, MCP servers or packages yet beyond
;;; the shared constructor's defaults (claude-code, anvil, graphify).
;;;
;;; One account-specific addition: a skill telling this account's Claude
;;; about the split-tunnel WireGuard + SOCKS5 proxy on this VM (see
;;; peteches/services/wireguard-socks5.scm). It is deliberately NOT passed
;;; to the peteches/criticalgrind configs, so only ygo's Claude knows it
;;; exists.
;;;
;;; Evaluates to a bare `home-environment' as its final expression.

(define-module (peteches home configs claude-workstation-ygo)
  #:use-module (guix gexp)
  #:use-module (peteches repository)
  #:use-module (peteches home modules claude-workstation))

(define-public claude-workstation-ygo-home
  (make-claude-workstation-home
   #:git-name "Peter McCabe"
   #:git-email "peter.mccabe@ygo.ai"
   #:extra-claude-files
   (list (cons "skills/wireguard-socks5/SKILL.md"
               (local-file (source-path
                            "configs/claude/ygo-only/skills/wireguard-socks5/SKILL.md"))))))

claude-workstation-ygo-home
