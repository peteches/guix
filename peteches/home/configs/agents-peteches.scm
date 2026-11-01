;;; Home config for the `peteches' account on the `agents' VM -- the first
;;; headless account to get one, and the reason (peteches home modules
;;; vm-jumpbox) exists.  The three claude-workstation accounts used to be
;;; built by (peteches home modules claude-workstation); that module is gone,
;;; and claude-workstation now runs the jump-box variant of this same
;;; constructor (peteches/home/configs/claude-workstation-peteches.scm).
;;;
;;; NOT applied via a standalone `guix home reconfigure' -- agents-os wires
;;; this value straight into its own `guix-home-service-type' instance (see
;;; peteches/systems/agents.scm), so a system-level `guix system
;;; reconfigure'/`guix deploy' (scripts/deploy.scm, or the deploy-vms skill)
;;; activates it as the guix-home-peteches shepherd service in the same run.
;;; Editing this file just needs a redeploy of agents.
;;;
;;; WHAT THIS ACCOUNT IS: the box's agent account -- `pi' plus the shared pi
;;; config the desktops already use, on top of the base tool set the
;;; constructor installs (git, ssh, openssh, proxychains-ng, coreutils, less,
;;; bash, a pinned ~/.config/guix/channels.scm and the canonical git identity).
;;; That is all the user asked for when Claude Code came off every machine:
;;; `pi' on agents under peteches, with standard VM tooling.
;;;
;;; WHAT IT DELIBERATELY DOES NOT GET:
;;;   * Claude Code at all -- claude-code, ~/.claude, the Claude MCP servers.
;;;     The harness is `pi' now (strata on comfyui and koboldcpp on the
;;;     desktop as its local backends), so nothing on this box needs Anthropic's
;;;     CLI.  It is the reason this file does not import (peteches home modules
;;;     claude) or (peteches home modules base).
;;;   * no anvil emacs daemon, no herdr server, no MCP servers -- there is no X
;;;     session and no Emacs on this VM, and pi's config is the plain shared
;;;     one; the desktops keep their own MCP wiring through (peteches home
;;;     modules base).
;;;   * no ~/.ssh identity beyond what openssh picks up by default.  This box
;;;     has no automation-key secret (secrets/hosts/agents/ carries none, and
;;;     vm-base only decrypts the automation key where a #:sops-secrets entry
;;;     asks for it), so `Host *' keeps the default identities and ssh-out from
;;;     this account is limited to whatever ~/.ssh/id_* the operator puts there.
;;;     Nothing depends on it: the deploy path runs FROM the desktops /
;;;     claude-workstation, never from agents, so `known-hosts2' stays empty
;;;     too -- see the constructor's KNOWN-HOSTS2 note for why an empty list
;;;     still means an empty managed file.
;;;
;;; CHANNELS, and the one thing a reader should know about them: the
;;; constructor pins ~/.config/guix/channels.scm to
;;; peteches/channels/manual.scm -- the full six-channel set.  That file
;;; contains the private `critical-grind' channel, fetched over ssh, and
;;; (guix git) authenticates ssh:// channels ONLY through an ssh agent
;;; ((%make-auth-ssh-agent), never ~/.ssh/config or a key file): agents has no
;;; deploy key in an agent, so `guix pull' on this box fails on that channel
;;; with `remote rejected authentication'.  It is already true today -- agents
;;; has no critical-grind key either, and (critical-grind) does not resolve on
;;; this box -- and the pin is still an improvement, because it replaces this
;;; account's hand-made channel file, which still lists the retired codeberg
;;; `peteches' channel whose contents were folded back into this repo.  If this
;;; box ever genuinely needs (critical-grind ...), wire a deploy-key secret and
;;; the reply-with-ssh-add replyer, as claude-workstation's automation-key
;;; entry does; do not solve it by dropping the pin.

(define-module (peteches home configs agents-peteches)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  ;; Direct import with #:select, not `specification->package' -- the
  ;; `%package-module-path' fold-packages scan documented in
  ;; (peteches home modules vm-jumpbox)'s header.
  #:use-module ((peteches packages pi-coding-agent) #:select (pi-coding-agent))
  #:use-module (peteches home modules vm-jumpbox)
  #:use-module (peteches home modules pi)   ;; home-pi-service-type
  #:use-module (peteches repository))

;; The same pi defaults the desktop profiles use -- repo-directory resolves
;; through %load-path so this works under `-L .' and in a worktree, exactly as
;; the retired (peteches home modules claude-workstation) passed it.  Only the
;; config files: no extensions, no MCP servers, no pi-interactive agents.
(define %pi-config-defaults
  (repo-directory "configs/pi/defaults"))

(define-public agents-peteches-home
  (make-vm-jumpbox-home
   ;; pi in this account's OWN profile: sshd's non-interactive shell gets
   ;; ~/.guix-home/profile/bin on PATH from Guix System's /etc/profile, so
   ;; `ssh agents "pi …"' works without putting pi in the system profile --
   ;; verified against the live claude-workstation box, whose home-managed PATH
   ;; is the same shape.  Keeping it out of the system profile also means
   ;; `pi' never appears for the rules-lawyer/sillytavern service accounts
   ;; this box runs.
   #:extra-packages (list pi-coding-agent)
   #:extra-services
   (list (service home-pi-service-type
                  (home-pi-configuration
                   (config-directory %pi-config-defaults))))))

agents-peteches-home
