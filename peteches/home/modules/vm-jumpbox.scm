;;; peteches/home/modules/vm-jumpbox.scm — minimal headless home-environment
;;; constructor for a VM's `peteches' account.
;;;
;;; This is what claude-workstation's peteches account now uses, after that VM
;;; was reduced to a pure split-tunnel WireGuard + SOCKS5 jump box: it no
;;; longer hosts Claude Code or pi for any account, so it needs neither the
;;; anvil daemon nor a herdr server — just git/ssh plus the proxychains-ng
;;; config those two need in order to *use* the tunnel this VM provides. It is
;;; also what `agents' uses: agents has no agent tooling of its own (its LLMs
;;; live on comfyui) and needs only `pi', so it passes pi via #:extra-packages
;;; here rather than growing a second, near-identical constructor.
;;;
;;; Deliberately NOT provided — the things (peteches home modules
;;; claude-workstation) does provide, all of which exist to serve Claude Code:
;;;   * claude-code / claude-completion, and ~/.claude from configs/claude/
;;;   * pi, ~/.pi, and the anvil/herdr MCP-server plumbing shared by both
;;;   * home-claude-service-type's activation-time `claude mcp add' step
;;;   * graphify (a Claude/pi skill) and github-cli/onepassword-cli/fly
;;;   * proxychains-ng's companion `netcat', which only ssh's ProxyCommand
;;;     needs — see the #:extra-packages note below for why agents skips it
;;;   * the per-account XDG_RUNTIME_DIR redirect, which exists because the
;;;     anvil and herdr daemons need a runtime dir and this VM fleet has no
;;;     elogind to make /run/user/<uid>. With no daemons there is nothing to
;;;     redirect; see that module's docstring before re-adding it.
;;;   * ~/area_51/<repo> pre-cloning, which only claude-workstation needs any
;;;     more (agents has no repos and does not clone the guix repo either)
;;;   * build-offload key wiring: offload is vm-base.scm's job at the system
;;;     level (#:with-nug-offload? + a /run/secrets/guix-offload-key
;;;     sops-secret), not this module's, and never was — so dropping it here
;;;     changes nothing for either VM.
;;;
;;; SECRETS are deliberately NOT here: the store is world-readable, so an API
;;; key set via home-environment-variables would leak. Anything secret arrives
;;; through the VM's own #:sops-secrets (decrypting to /run/secrets/… at boot)
;;; and is exported into the shell by #:secret-env-vars below — see
;;; claude-workstation.scm for an instance of that path.

(define-module (peteches home modules vm-jumpbox)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  ;; home-openssh-service-type / home-openssh-configuration live in
  ;; (gnu home services ssh), not (gnu home services shells). Without this the
  ;; file loads fine and the known-hosts2 branch below is "Unbound variable"
  ;; only when a home config actually evaluates it.
  #:use-module (gnu home services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  ;; Package modules are imported directly (with #:select) rather than looked
  ;; up via `specification->package'. A top-level `specification->package'
  ;; call fires `fold-packages' over `%package-module-path' — and `guix
  ;; deploy'/`guix system' put the repo (`-L .') on that path, so the scan
  ;; re-enters THIS module (and its siblings) while they are still compiling,
  ;; yielding a cascade of bogus "unbound variable" errors. `guix repl' never
  ;; sets `%package-module-path', which is why it worked there. Direct imports
  ;; avoid the scan entirely. Same reason the three claude-workstation-*-*.scm
  ;; configs used to carry the identical comment.
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages networking) #:select (proxychains-ng))
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages less) #:select (less))
  ;; base-log-dir-service is defined and exported by (peteches home modules
  ;; gpg) -- the same import (peteches home modules claude-workstation) has for
  ;; the identical line in its own `(append ...)'. #:select keeps gpg-agent's
  ;; service definitions out of this module's scope: a jump box runs no gpg-agent.
  #:use-module ((peteches home modules gpg) #:select (base-log-dir-service))
  #:use-module (peteches repository)
  #:export (make-vm-jumpbox-home))

(define %vm-jumpbox-base-packages
  ;; `netcat' is deliberately absent: its only consumer here was ssh's
  ;; ProxyCommand for the ygo hosts, and that command runs on the DESKTOPS
  ;; (`ssh claude-workstation -- proxychains4 … nc %h %p', see
  ;; (peteches home modules ssh)), not inside this VM. claude-workstation
  ;; still gets it — its own account config passes it explicitly, because
  ;; there `nc' does run locally: see the note in that module's
  ;; #:extra-packages.
  (list git openssh proxychains-ng coreutils less bash))

;; Points proxychains-ng at the SOCKS5 proxy exposed by
;; wireguard-socks5-service-type (peteches/services/wireguard-socks5.scm,
;; default port 1080). microsocks runs inside the wg0ns network namespace,
;; bound to the netns side of the host<->netns veth pair (10.200.0.2), not to
;; loopback — see wsc-veth-netns-address in wireguard-socks5.scm. Byte-identical
;; to %proxychains-config-file in (peteches home modules claude-workstation);
;; it is duplicated rather than shared because that module's version is read
;; through PROXYCHAINS_CONF_FILE by accounts that no longer exist on this VM,
;; and a jump box with nothing behind the proxy needs no divergence today.
(define %proxychains-config-file
  (plain-file
   "proxychains.conf"
   "strict_chain\nproxy_dns\ntcp_read_time_out 15000\ntcp_connect_time_out 8000\n\n[ProxyList]\nsocks5 10.200.0.2 1080\n"))

(define (git-config-file git-name git-email)
  (plain-file
   "gitconfig"
   (string-append
    "[user]\n"
    "\tname = " git-name "\n"
    "\temail = " git-email "\n"
    "[init]\n\tdefaultBranch = main\n"
    "[pull]\n\trebase = true\n")))

;; Build a bashrc snippet exporting each (ENV-VAR . RUN-SECRETS-PATH) pair
;; from SECRET-ENV-VARS, guarded so a missing/unreadable file (wrong account,
;; secret not yet wired) is silently skipped rather than erroring. The PATH
;; itself is baked into the world-readable store — only the *value* at that
;; path, read at shell-start time from tmpfs, is sensitive. Same mechanism as
;; (peteches home modules claude-workstation)'s secret-env-bashrc.
(define (secret-env-bashrc secret-env-vars)
  (plain-file
   "secret-env.sh"
   (string-append
    "### -*- mode: sh -*-\n"
    (apply string-append
           (map (lambda (pair)
                  (let ((var (car pair)) (path (cdr pair)))
                    (string-append
                     "if [ -r \"" path "\" ]; then export " var
                     "=\"$(cat \"" path "\")\"; fi\n")))
                secret-env-vars)))))

(define* (make-vm-jumpbox-home
          #:key
          (git-name "Pete 'Peteches' McCabe")
          (git-email "peteches@peteches.co.uk")
          (extra-packages '())
          (env-vars '())
          (secret-env-vars '())
          (extra-services '())
          (known-hosts2 '()))
  "Return a headless home-environment for one VM's `peteches' account.
EXTRA-PACKAGES are added to the base tool set — this is how an agent host gets
its agent CLI, e.g. pi-coding-agent on `agents'. ENV-VARS is a plain
(non-secret) alist of (ENV-VAR . VALUE) exported via
home-environment-variables-service-type (i.e. into
~/.guix-home/setup-environment, sourced by ~/.profile); $HOME in a value is
expanded by the shell when the file is sourced. SECRET-ENV-VARS is an alist of
(ENV-VAR . RUN-SECRETS-PATH): at shell startup each ENV-VAR is exported from
the contents of RUN-SECRETS-PATH (normally a sops-secret's /run/secrets/… path)
if that path is readable. KNOWN-HOSTS2 is a list of file-like records placed at
~/.ssh/known_hosts2 — OpenSSH reads known_hosts AND known_hosts2 by default, so
hosts added interactively still live in ~/.ssh/known_hosts and survive `guix
home reconfigure', while the declarative entries here let a non-interactive git
clone verify its host key without a TTY. EXTRA-SERVICES is a list of additional
home services for this account."
  ;; KNOWN-HOSTS2 is empty on a host that clones nothing over SSH (agents),
  ;; and non-empty on the jump box (claude-workstation, whose peteches account
  ;; still has ~/area_51/guix to run deploys from). Split it out of the
  ;; `(list ...)' block below rather than branching inside it: an `(if (null? ...)
  ;; '() (list ...))' sitting among service records would splice a bare ()
  ;; into append's argument list on the empty branch -- a type error at compile
  ;; time, not a paren error -- and an otherwise-correct
  ;; home-openssh-configuration with #:known-hosts2 '() still writes an empty
  ;; ~/.ssh/known_hosts2. KNOWN-HOSTS-SERVICES is bound once here and spliced
  ;; in as append's second argument.
  (let ((known-hosts-services
         (if (null? known-hosts2)
             '()
             (list (service home-openssh-service-type
                            (home-openssh-configuration
                             (known-hosts2 known-hosts2)))))))
    (home-environment
   (packages (append %vm-jumpbox-base-packages extra-packages))
   (services
    ;; `list', never `cons*': cons* makes its LAST argument the tail, so
    ;; `(cons* a b c)' is the improper list `(a b . c)' -- and `append' rejects
    ;; that with "Wrong type argument in position 1 (expecting empty list)",
    ;; which is also why the tail comment below says five closes, not six.
    (append
     (list base-log-dir-service
      ;; PROXYCHAINS_CONF_FILE is exported here rather than in
      ;; home-environment-variables-service-type for the same reason
      ;; (peteches home modules claude-workstation) does it: this is shell
      ;; config, not a value anything outside the shell reads. Note it is only
      ;; visible to login/interactive shells, which is why the desktops'
      ;; ProxyCommand passes -f explicitly — see (peteches home modules ssh).
      (service home-bash-service-type
               (home-bash-configuration
                (guix-defaults? #t)
                (environment-variables
                 '(("PATH" . "$HOME/.local/bin:$PATH")
                   ("PROXYCHAINS_CONF_FILE" . "$HOME/.config/proxychains.conf")))
                (bashrc
                 (if (null? secret-env-vars)
                     '()
                     (list (secret-env-bashrc secret-env-vars))))))

      (simple-service 'env-vars
                      home-environment-variables-service-type
                      env-vars)
      (simple-service 'git-identity
                      home-files-service-type
                      (list (list ".config/git/config"
                                  (git-config-file git-name git-email))))
      (simple-service 'proxychains-config
                      home-files-service-type
                      (list (list ".config/proxychains.conf"
                                  %proxychains-config-file)))
      ;; Pins `guix pull' (no `-C' needed) to this repo's channel set, the same
      ;; way scripts/deploy.scm pins the system layer — except this is
      ;; per-account and only takes effect on the next `guix home reconfigure',
      ;; same as every other file below. Without this, `guix pull' falls back to
      ;; plain upstream guix with none of nonguix/guix-science(-nonfree)/sops-
      ;; guix/critical-grind, and `(peteches services ...)'/`(peteches packages
      ;; ...)' modules that touch those channels fail to resolve.
      (simple-service 'guix-pull-channels
                      home-files-service-type
                      (list (list ".config/guix/channels.scm"
                                  (local-file (source-path "peteches/channels/manual.scm")))))
      ;; Declarative known_hosts for the repos this account clones over SSH,
      ;; written to ~/.ssh/known_hosts2 so a non-interactive git clone can
      ;; verify github.com / codeberg.org / git.peteches.co.uk without a TTY.
      ;; OpenSSH reads known_hosts AND known_hosts2, so hosts added
      ;; interactively still live in ~/.ssh/known_hosts and survive `guix home
      ;; reconfigure'. The service itself is built in the `let' above -- this
      ;; comment only marks where it lands: spliced in as append's second
      ;; argument, right after the `(list ...)' block.
      )
     known-hosts-services
     ;; `(list ...)' closes above: its list is `append''s first argument and
     ;; known-hosts-services / extra-services are `append''s remaining
     ;; arguments -- so this line needs five closes (append, services,
     ;; home-environment, let, define*), not six.
     extra-services)))))
