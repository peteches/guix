;;; Home config for the `peteches' account on claude-workstation.
;;;
;;; NOT applied via a standalone `guix home reconfigure' on the VM --
;;; claude-workstation-os wires this value straight into its own
;;; `guix-home-service-type' instance (see peteches/systems/claude-
;;; workstation.scm), so a system-level `guix system reconfigure'/`guix
;;; deploy' (scripts/deploy.scm, or the deploy-vms skill) activates it as the
;;; guix-home-peteches shepherd service in the same run. Editing this file
;;; just needs a redeploy of claude-workstation, same as any other change to
;;; that system.
;;;
;;; (Manual fallback, e.g. to test a change before a full redeploy: run ON
;;; THE VM as the peteches user --
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-peteches.scm
;;; -- which needs the guix repo cloned by hand into ~/area_51/guix first,
;;; since this config clones nothing and does not carry the repo it lives in.)
;;;
;;; Evaluates to a bare `home-environment' as its final expression, which is
;;; what `guix home' consumes.
;;;
;;; WHAT THIS ACCOUNT IS NOW: claude-workstation is a pure jump box -- the
;;; split-tunnel WireGuard + SOCKS5 tunnel (peteches/services/
;;; wireguard-socks5.scm) that the desktops' ygo-server / ygoswiss1 /
;;; ygoswiss2 ssh hosts relay through. So this account needs exactly what
;;; (peteches home modules vm-jumpbox) builds: git, ssh, proxychains-ng, a git
;;; identity, the proxychains.conf that points at that tunnel, and a pinned
;;; ~/.config/guix/channels.scm. No Claude Code, no pi, no MCP servers, no
;;; anvil daemon, no herdr server.
;;;
;;; THE THREE THINGS PASSED IN BEYOND THE CONSTRUCTOR'S DEFAULTS:
;;;   * netcat -- the desktops' ProxyCommand for the tunnel-only hosts is
;;;     `ssh claude-workstation -- proxychains4 -q -f ~/.config/proxychains.conf
;;;      nc %h %p' (see (peteches home modules ssh)), and that `nc' runs HERE
;;;     in this account's non-interactive shell, so this VM needs it even
;;;     though `agents' does not. It is deliberately absent from vm-jumpbox's
;;;     base package set -- see that module's note -- so it is asked for here.
;;;   * known-hosts2 -- this account still has ~/area_51/guix checked out (the
;;;     copy `scripts/deploy.scm' runs from when this VM deploys the fleet),
;;;     so a non-interactive git fetch needs github.com / codeberg.org /
;;;     git.peteches.co.uk host keys without a TTY. Those two asset files
;;;     stayed where the retired (peteches home modules claude-workstation)
;;;     kept them -- co-located in peteches/home/modules/, next to this
;;;     module's own sibling assets -- and are located by name through
;;;     (peteches repository)'s source-path rather than by a relative path,
;;;     which would break under `-L .' and in a worktree.
;;;   * ssh-hosts -- one `Host *' block whose IdentityFile is
;;;     /run/secrets/peteches-automation-ssh-key, the private half of the
;;;     automation keypair that every VM authorizes for peteches (see
;;;     %automation-authorized-key-secret and the Match block in
;;;     peteches/systems/vm-base.scm). That keeps this VM able to ssh into any
;;;     other VM's peteches account: it is still a listed deploy coordinator
;;;     (%authorize-coordinator-key in peteches/systems/common.scm) and can
;;;     still push deploys. The key is a PATH, not a value, so unlike an API
;;;     key it never lands in the world-readable store -- ssh reads it from
;;;     tmpfs at connect time.
;;;
;;; DROPPED, ALL OF IT CLAUDE-CODE SCAFFOLDING that make-claude-workstation-home
;;; (now deleted) used to add: claude-code + ~/.claude, pi + ~/.pi, the anvil
;;; emacs daemon, the herdr server and its 4 standing spaces, the ~/area_51
;;; repo pre-cloning activation, graphify / github-cli / onepassword-cli / fly,
;;; the comfyui MCP server, and pi-dictate with its `rec' wrapper package, its
;;; PULSE_SERVER export and its ~/.pi/agent/prompts/dictate.md. Voice
;;; dictation does not go away -- it stays on the desktops, where the DMS
;;; dictate plugin does the same job from the shared Deepgram key (see
;;; configs/dms/plugins/dictate/ and secrets/shared/deepgram.yaml); only this
;;; VM's copy of the feature is retired, and with it the desktop-side PulseAudio
;;; TCP listener that existed solely to serve it (see the note in
;;; configs/hypr/peteches/autostart.lua -- left loaded for now, harmless).

(define-module (peteches home configs claude-workstation-peteches)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services ssh)   ;; openssh-host for #:ssh-hosts below
  ;; Direct import with #:select, not `specification->package' -- the reason
  ;; is the `%package-module-path' fold-packages scan documented in
  ;; (peteches home modules vm-jumpbox)'s header.
  #:use-module ((gnu packages admin) #:select (netcat))
  #:use-module (peteches home modules vm-jumpbox)
  #:use-module (peteches repository))

;; Host keys for the remotes this account's git talks to (github.com,
;; codeberg.org, git.peteches.co.uk) plus the VM's own loopback keys.  They
;; keep the names the retired claude-workstation home module gave them, so the
;; diff that deleted that module is a pure deletion -- renaming them here
;; would have to rename the store file names too, and nothing else reads
;; those.
(define %claude-workstation-known-hosts
  (list (local-file (source-path "peteches/home/modules/claude-workstation-known-hosts"))
        (local-file (source-path "peteches/home/modules/claude-workstation-loopback-known-hosts"))))

;; The private half of the automation keypair, decrypted at system boot by the
;; #:sops-secrets entry in peteches/systems/claude-workstation.scm.
(define %automation-ssh-key "/run/secrets/peteches-automation-ssh-key")

(define-public claude-workstation-peteches-home
  (make-vm-jumpbox-home
   #:extra-packages (list netcat)
   #:known-hosts2 %claude-workstation-known-hosts
   #:ssh-hosts (list (openssh-host
                      (name "*")
                      (identity-file %automation-ssh-key)))))

claude-workstation-peteches-home
