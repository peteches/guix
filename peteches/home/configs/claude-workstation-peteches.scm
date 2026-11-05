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
;;; THE ONE THING PASSED IN BEYOND THE CONSTRUCTOR'S DEFAULTS:
;;;   * netcat -- the desktops' ProxyCommand for the tunnel-only hosts is
;;;     `ssh claude-workstation -- proxychains4 -q -f ~/.config/proxychains.conf
;;;      nc %h %p' (see (peteches home modules ssh)), and that `nc' runs HERE
;;;     in this account's non-interactive shell, so this VM needs it even
;;;     though `agents' does not. It is deliberately absent from vm-jumpbox's
;;;     base package set -- see that module's note -- so it is asked for here.
;;;
;;; THE TWO THINGS THAT USED TO BE PASSED HERE AND MOVED TO `agents' ON
;;; 2026-11-05. Both belonged to this box's old deploy-coordinator role, and
;;; that role stopped making sense the moment the box became a pure jump box:
;;;   * ssh-hosts -- a `Host *' block whose IdentityFile was
;;;     /run/secrets/peteches-automation-ssh-key, the private half of the
;;;     automation keypair that every VM authorizes for peteches (see
;;;     %automation-authorized-key-secret and the Match block in
;;;     peteches/systems/vm-base.scm). That is what let this box ssh into any
;;;     other VM's peteches account, and so push deploys, without borrowing a
;;;     desktop's admin key. The key FILE is gone from the system config too
;;;     -- see the #:sops-secrets note in peteches/systems/claude-workstation.scm
;;;     -- and `agents' now carries both the key and this `Host *' block (see
;;;     peteches/home/configs/agents-peteches.scm).
;;;   * known-hosts2 -- declarative github.com / codeberg.org /
;;;     git.peteches.co.uk host keys, so a non-interactive git fetch of the
;;;     ~/area_51/guix checkout that `scripts/deploy.scm' runs from needs no
;;;     TTY. Two things were wrong with keeping them here: that checkout is
;;;     not on this box (checked live on 2026-11-05 -- ~/area_51 exists and is
;;;     EMPTY, so the justification these lines had been carrying was already
;;;     false), and the deploy origin they were written for is `agents' now,
;;;     which has no checkout either YET but is the box deploys will run from.
;;;     The two asset files stay where the retired (peteches home modules
;;;     claude-workstation) put them -- co-located in peteches/home/modules/,
;;;     next to that module's own sibling assets -- and agents-peteches.scm
;;;     now reads them by name through (peteches repository)'s source-path,
;;;     which works under `-L .' and in a worktree where a relative path would
;;;     not. Their store file names keep the claude-workstation spelling
;;;     instead of being renamed for the new consumer: renaming them would
;;;     have churned the diff that deleted that module into more than a pure
;;;     deletion, and nothing keys off those names.
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
  ;; Direct import with #:select, not `specification->package' -- the reason
  ;; is the `%package-module-path' fold-packages scan documented in
  ;; (peteches home modules vm-jumpbox)'s header.
  #:use-module ((gnu packages admin) #:select (netcat))
  #:use-module (peteches home modules vm-jumpbox))

;; Nothing else is passed here. The two hooks this file used to fill in --
;; #:ssh-hosts (the `Host *' automation identity) and #:known-hosts2 (the git
;; host keys for a repo checkout) -- moved to agents-peteches.scm on
;; 2026-11-05 with the deploy-coordinator role that needed them; see the
;; header above. What is left is the ONE thing this jump box still needs that
;; the constructor does not give every VM: `netcat', because ssh's
;; ProxyCommand for the tunnel-only hosts execs `nc' in THIS account's shell.
(define-public claude-workstation-peteches-home
  (make-vm-jumpbox-home
   #:extra-packages (list netcat)))

claude-workstation-peteches-home
