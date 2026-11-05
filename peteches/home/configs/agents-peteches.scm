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
;;;   * no anvil emacs daemon and no MCP servers -- there is no X session and
;;;     no Emacs on this VM, and pi's config is the plain shared one; the
;;;     desktops keep their own MCP wiring through (peteches home modules
;;;     base).  herdr IS the one daemon this box runs, but its supervision is
;;;     not here: the server is a system service (see (peteches services
;;;     herdr) and peteches/systems/agents.scm), because user shepherd never
;;;     starts on a headless VM and home-shepherd would therefore never have
;;;     started it.  What this profile has to supply is the BINARY: the
;;;     attaching client's add-remote worker asks the remote `command -v
;;;     herdr' and only reports AlreadyInstalled when what it finds matches
;;;     the client's FULL version string and protocol -- and an mx build
;;;     cannot seed a remote that has nothing on it, because
;;;     src/remote/attach.rs download_release_asset refuses every non-stable
;;;     channel.  One package definition, both ends, so the probe passes and
;;;     nothing is ever written to ~/.local/bin/herdr.
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
  ;; Same #:select discipline as the line above, for the same reason.
  #:use-module ((peteches packages herdr-mx) #:select (herdr-mx))
  #:use-module ((peteches packages pi-interactive-subagents) #:select (pi-interactive-subagents))
  #:use-module (peteches home modules vm-jumpbox)
  #:use-module (peteches home modules pi)   ;; home-pi-service-type
  #:use-module (peteches repository))

;; The same pi defaults the desktop profiles use -- repo-directory resolves
;; through %load-path so this works under `-L .' and in a worktree, exactly as
;; the retired (peteches home modules claude-workstation) passed it.  Only the
;; config files plus the ONE package extension added below (the subagent tool)
;; and the loose herdr hook; still no MCP servers, so no ~/.pi/agent/mcp.json
;; is rendered for this account.  Nothing here installs a PATH package: an
;; extension is a store symlink, not a profile entry (see pi.scm's EXTENSIONS
;; note).
(define %pi-config-defaults
  (repo-directory "configs/pi/defaults"))

;; herdr's own pi integration, committed under configs/pi/ so it is
;; declarative instead of written by hand with `herdr integration install pi'
;; (see (peteches home modules pi)'s header for that mechanism).  It is the
;; file that lets a pi session running inside a herdr pane tell the sidebar it
;; is working / blocked / idle over HERDR_SOCKET_PATH, so it only does
;; anything once this box has a server -- which it now does, above.  The
;; content is versioned with the herdr binary: HERDR_INTEGRATION_VERSION=8 in
;; its header is what the pinned herdr-mx (79803d18) bundles, checked against
;; src/integration/mod.rs PI_INTEGRATION_VERSION at that commit -- re-copy it
;; from a running install whenever the pin moves.
(define %pi-herdr-integration
  (local-file (source-path "configs/pi/herdr-agent-state.ts")))

(define-public agents-peteches-home
  (make-vm-jumpbox-home
   ;; pi in this account's OWN profile: sshd's non-interactive shell gets
   ;; ~/.guix-home/profile/bin on PATH from Guix System's /etc/profile, so
   ;; `ssh agents "pi …"' works without putting pi in the system profile --
   ;; verified against the live claude-workstation box, whose home-managed PATH
   ;; is the same shape.  Keeping it out of the system profile also means
   ;; `pi' never appears for the rules-lawyer/sillytavern service accounts
   ;; this box runs.
   ;;
   ;; herdr-mx goes in the SAME profile for the same reason, and it is the
   ;; only place it can go: the ssh PATH that the client's add-remote probe
   ;; sees is whatever /etc/profile builds, and that profile list already
   ;; includes ~/.guix-home/profile/bin (verified on this box -- that is how
   ;; `ssh agents "echo $PATH"' answers), so a package installed here is
   ;; found by `command -v herdr' from a plain `ssh agents COMMAND'.  Putting
   ;; it in the system profile would work too and is deliberately avoided for
   ;; the same reason pi is kept out of it.
   #:extra-packages (list pi-coding-agent herdr-mx)
   #:extra-services
   (list (service home-pi-service-type
                  (home-pi-configuration
                   (config-directory %pi-config-defaults)
                   ;; The subagent tool -- scout/researcher/worker run as
                   ;; panes.  Symlinked straight from the store into
                   ;; ~/.pi/agent/extensions/pi-interactive-subagents; it is
                   ;; also the consumer that configs/pi/defaults/agents/*.md
                   ;; finally has, because pi core has no agents/ directory of
                   ;; its own (see the table in pi.scm's commentary), so those
                   ;; three .md overrides were inert on this box until now. On
                   ;; the desktops the same extension landed in 6d6b820.
                   ;; Caveat: the fork drives subagents through HERDR_* pane
                   ;; context, which only exists INSIDE a herdr pane -- pi
                   ;; started from a plain ssh session has none, so run pi from
                   ;; an agents workspace pane if you want the tool to work.
                   (extensions (list pi-interactive-subagents))
                   ;; Loose .ts file, not an npm package directory -- pi's
                   ;; extension loader picks up both (see pi.scm's
                   ;; EXTRA-EXTENSION-FILES note).  Placed at
                   ;; ~/.pi/agent/extensions/herdr-agent-state.ts.
                   (extra-extension-files
                    (list (cons "herdr-agent-state.ts"
                                %pi-herdr-integration))))))))

agents-peteches-home
