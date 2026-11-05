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
;;; WHAT THIS ACCOUNT GOT ON 2026-11-05: THE FLEET DEPLOY IDENTITY, moved here
;;; from claude-workstation's peteches account, which no longer needs it now
;;; that that box is a pure WireGuard/SOCKS jump box (see
;;; peteches/home/configs/claude-workstation-peteches.scm). Two pieces:
;;;   * `Host *' -> IdentityFile /run/secrets/peteches-automation-ssh-key --
;;;     the private half of the peteches automation keypair, whose public half
;;;     EVERY vm authorizes for the peteches login (vm-base.scm's
;;;     %automation-authorized-key-secret plus its sshd Match block).  One line
;;;     makes the whole fleet ssh-reachable from this account as peteches, and
;;;     %deploy-identity in peteches/machines.scm picks the same path up, which
;;;     is how scripts/deploy.scm / the deploy-vms skill authenticate from
;;;     here. It is a PATH, not a value: unlike an API key it never reaches the
;;;     world-readable store, ssh reads it from tmpfs at connect time, and only
;;;     this box decrypts it -- see the #:sops-secrets entry in
;;;     peteches/systems/agents.scm.
;;;   * known-hosts2 -- github.com / codeberg.org / git.peteches.co.uk host
;;;     keys, so a non-interactive git fetch needs no TTY. Forward-looking:
;;;     this account has no ~/area_51/guix checkout today (checked live on
;;;     2026-11-05) so nothing reads them yet, but the box that now holds the
;;;     deploy identity is the box a fleet deploy will run from, and it needs
;;;     them the moment it has a repo to fetch. The two asset files are the
;;;     ones the retired (peteches home modules claude-workstation) wrote; they
;;;     stay in peteches/home/modules/ and their store names still say
;;;     claude-workstation, so that module's deletion stayed a pure deletion.
;;; The half of "can this box deploy" that is NOT a ssh key: its own
;;; guix-daemon signing key is listed in %authorize-coordinator-key
;;; (peteches/systems/common.scm). Without that, `guix deploy' from here still
;;; connects and still switches the target's generation, but the LAST step --
;;; the live upgrade-shepherd-services archive send -- fails with
;;; `unauthorized public key', which is exactly the failure mode
;;; docs/fleet-deployment.org §4 documents for claude-workstation.
;;;
;;; STILL ABSENT: any personal ~/.ssh/id_* key. There was none here before and
;;; there is none now -- `Host *' names the automation key, and vm-base only
;;; ever decrypts automation keys where a #:sops-secrets entry asks for one.
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
;;; the reply-with-ssh-add replyer -- the pattern the retired
;;; (peteches home modules claude-workstation) used for its own automation
;;; key -- and note that a `Host *' IdentityFile does not help here at all,
;;; because guix's git fetch never reads ~/.ssh/config; do not solve it by
;;; dropping the pin.

(define-module (peteches home configs agents-peteches)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  ;; home-openssh-service-type / home-openssh-configuration / openssh-host all
  ;; live in (gnu home services ssh) -- needed for the #:ssh-hosts `Host *'
  ;; entry below, and importing (gnu services ssh) instead would drag in the
  ;; SYSTEM openssh-host record of the same name.
  #:use-module (gnu home services ssh)
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

;; Host keys for the git remotes this account fetches from (github.com,
;; codeberg.org, git.peteches.co.uk) plus the VM's own loopback keys.  Same
;; files claude-workstation's account used until 2026-11-05 -- read by name
;; through (peteches repository)'s source-path so they resolve under `-L .'
;; and in a worktree, and their store names still carry the old
;; claude-workstation spelling on purpose (see that config's header for why
;; renaming them was not worth the diff churn).
(define %agents-known-hosts
  (list (local-file (source-path "peteches/home/modules/claude-workstation-known-hosts"))
        (local-file (source-path "peteches/home/modules/claude-workstation-loopback-known-hosts"))))

;; The private half of the peteches automation keypair, decrypted at system
;; boot by the #:sops-secrets entry in peteches/systems/agents.scm -- the same
;; path claude-workstation used to decrypt it to, so %deploy-identity in
;; peteches/machines.scm resolves on this box now.
(define %automation-ssh-key "/run/secrets/peteches-automation-ssh-key")

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
   ;; `Host *' -> the automation key, so ssh-ing from this account into any
   ;; other VM's peteches login authenticates -- and so %deploy-identity has
   ;; something real to resolve to here. ONE instance rule: these two hooks
   ;; share a single home-openssh-service-type, see the constructor's
   ;; SSH-HOSTS/KNOWN-HOSTS2 comment for why two instances would conflict.
   #:ssh-hosts (list (openssh-host
                      (name "*")
                      (identity-file %automation-ssh-key)))
   #:known-hosts2 %agents-known-hosts
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
