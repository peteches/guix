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
;;; Evaluates to a bare `home-environment' as its final expression.

(define-module (peteches home configs claude-workstation-ygo)
  #:use-module (peteches home modules claude-workstation))

(define-public claude-workstation-ygo-home
  (make-claude-workstation-home
   #:git-name "Peter McCabe"
   #:git-email "peter.mccabe@ygo.ai"))

claude-workstation-ygo-home
