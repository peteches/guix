(define-module (peteches services herdr)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (srfi srfi-1)
  #:use-module ((gnu packages bash) #:select (bash))
  ;; Direct import with #:select, not specification->package: see the
  ;; %package-module-path / fold-packages note in
  ;; (peteches home modules vm-jumpbox)'s header — a top-level
  ;; specification->package call re-enters modules that are still compiling
  ;; when `guix deploy' has the repo on %package-module-path.
  #:use-module ((peteches packages herdr-mx) #:select (herdr-mx))
  #:export (herdr-server-service-type
            herdr-server-configuration
            herdr-server-configuration?
            herdr-server-configuration-package
            herdr-server-configuration-user
            herdr-server-configuration-group
            herdr-server-configuration-extra-path-directories
            herdr-server-configuration-require-guix-home?
            herdr-server-configuration-log-file
            herdr-server-configuration-respawn?))

;;; Commentary:
;;;
;;; Service type for the herdr headless server (see (peteches packages
;;; herdr-mx)): `herdr server' supervises persistent terminal sessions so an
;;; agent CLI (pi, on `agents') keeps running across SSH disconnects, and the
;;; desktop's multi-remote sidebar attaches to it over SSH instead of nesting
;;; SSH sessions.  Runs as ONE named account per instance, so the server owns
;;; that account's own ~/.config/herdr.
;;;
;;; WHY THE SYSTEM SHEPHERD LAYER, NOT HOME-SHEPHERD (this is the part that
;;; bit us).  The retired (peteches home modules claude-workstation) supervised
;;; its per-account herdr servers under home-shepherd-service-type, and that
;;; only ever works where a *login shell* runs at least once:
;;;
;;;   * Home shepherd is started by ~/.guix-home/on-first-login, which
;;;     ~/.profile runs — i.e. login shells only.  `ssh host COMMAND' is
;;;     bash -c, not a login shell: Guix's managed ~/.bashrc sources
;;;     /etc/profile for the non-interactive SSH case and then RETURNS
;;;     early, so on-first-login never runs on a headless box's normal
;;;     command traffic.
;;;   * Worse, on-first-login is itself guarded by the existence of
;;;     XDG_RUNTIME_DIR; this VM fleet has no elogind/PAM session, so
;;;     /run/user/<uid> is never created and the script prints
;;;     "XDG_RUNTIME_DIR doesn't exists, on-first-login script won't
;;;     execute anything" and does nothing at all.
;;;   * And the boot-time activation one-shot runs with
;;;     GUIX_SYSTEM_IS_RUNNING_HOME_ACTIVATE=t, which is exactly the flag
;;;     home-shepherd's ensure-shepherd gexp checks to decide NOT to launch
;;;     shepherd during system activation.
;;;
;;; All three were confirmed on `agents' on 2026-11-05: `herd status' there
;;; failed with /run/user/1000/shepherd/socket missing — that account has no
;;; user shepherd at all, so a home-shepherd herdr-server would never have
;;; started.  Supervising at the system layer instead removes the login-shell
;;; dependency entirely: the server is up at boot, respawns on crash, and is
;;; up *before* anything attaches to it.
;;;
;;; WHY THIS BUILD MUST BE THE ONE THE CLIENT RUNS.  herdr-mx is a protocol
;;; fork (PROTOCOL_VERSION 21 / WIRE_ABI_EPOCH 3; upstream 0.8.x is 20), so its
;;; client cannot attach to an upstream server at all.  The client's add-remote
;;; worker asks the remote `command -v herdr' and then probes the answer with
;;; `--version', `status client --json' and the two bridge subcommands; it only
;;; reports AlreadyInstalled when the version line matches the client's FULL
;;; version (src/version.rs version_line_matches_current) AND the protocol
;;; matches.  When it does not match, it tries to seed the remote — and a
;;; channel build cannot: src/remote/attach.rs download_release_asset refuses
;;; anything whose channel is not "stable", because a manifest download would
;;; fetch a foreign upstream binary.  So for an mx client the remote MUST
;;; already carry the identical build; serving it from the Guix store on PATH
;;; is the only automatic path that satisfies the probe.  Same package
;;; definition, both machines ⇒ same version string ⇒ no self-provisioning.
;;;
;;; WHY ~/.local/bin HERDR MUST NOT EXIST.  ~/.local/bin/herdr is the client's
;;; self-provisioning target.  Two ways it goes wrong: (a) a stale copy that
;;; cannot execute makes the probe report HERDR_PROBE_NO_VERSION/Unintelligible
;;; — this is the 2026-09-26 crash-loop class of incident; (b) even a
;;; *working* copy is invisible to `command -v herdr', because ~/.local/bin is
;;; only exported by login-shell config, which the non-interactive SSH shell
;;; skips.  Either way the probe fails on every attach, so the client keeps
;;; re-seeding.  After installing this service, delete any
;;; That is measured, not inferred -- the 2026-11-03 attach printed
;;; 'already installed but this connection cannot use it', which proves
;;; 'command -v herdr' DID find a copy in that probe's environment, so the
;;; PATH then reached ~/.local/bin even though today's does not.  The
;;; conclusion is unchanged: a ~/.local/bin seed is PATH-fragile.
;;; ~/.local/bin/herdr the earlier failures left behind.
;;;
;;; PATH, and why this service sets it by hand.  shepherd starts services with
;;; a minimal environment, and herdr's pane shells are NOT login shells on
;;; Linux (src/pane.rs: the "auto" shell mode uses login shells only on macOS)
;;; — so a pane inherits this process's environment and nothing else.  Without
;;; the home profile's bin dir on PATH, `pi' is simply missing inside every
;;; pane on a box where pi lives in ~/.guix-home/profile (which is the case for
;;; `agents', see peteches/home/configs/agents-peteches.scm).  HOME, SHELL and
;;; USER are set too because herdr's default pane shell is whatever $SHELL
;;; names.

;;; ── Configuration record ─────────────────────────────────────────────────

(define-record-type* <herdr-server-configuration> herdr-server-configuration
                     make-herdr-server-configuration
  herdr-server-configuration?
  ;; The package providing bin/herdr.  It must be the SAME package the
  ;; attaching client uses — see the parity note above.
  (package herdr-server-configuration-package
           (default herdr-mx))
  ;; The account the server runs as.  Its ~/.config/herdr is the session the
  ;; client attaches to, and its home profile is where the PATH below looks
  ;; for the agent CLIs the panes run.
  (user herdr-server-configuration-user
        (default "peteches"))
  (group herdr-server-configuration-group
         (default "users"))
  ;; Directories prepended to PATH, in order, before the account's own Guix
  ;; profiles.  The store bin of PACKAGE is always first, so `herdr' stays
  ;; resolvable inside panes even on a box whose home profile is not yet
  ;; activated.  Extra entries are for the rare account that keeps a tool in
  ;; somewhere else.
  (extra-path-directories herdr-server-configuration-extra-path-directories
                          (default '()))
  ;; Require the guix-home-<user> one-shot before starting, so the account's
  ;; ~/.guix-home/profile really exists (and really is on PATH) when the
  ;; first pane is spawned.  Turn this off only for an account whose home
  ;; environment is not managed by Guix System.
  (require-guix-home? herdr-server-configuration-require-guix-home?
                      (default #t))
  ;; Shepherd's own log: herdr's server logging goes to ~/.config/herdr
  ;; (its own logging::startup), so this file mostly catches what is printed
  ;; before that takes over — plus the respawn churn, which is the thing you
  ;; actually want in the syslog-visible place.
  (log-file herdr-server-configuration-log-file
            (default "/var/log/herdr-server.log"))
  (respawn? herdr-server-configuration-respawn?
            (default #t)))

;;; ── Helpers ──────────────────────────────────────────────────────────────

(define (join-colon dirs)
  "Join DIRS with ':' and a trailing ':', or return \"\" when DIRS is empty.
Done at CONFIGURE time, so the resulting string can be spliced into the
service gexp as plain data."
  (if (null? dirs)
      ""
      (fold (lambda (dir acc) (string-append dir ":" acc))
            ""
            dirs)))

;;; ── Shepherd service ─────────────────────────────────────────────────────

(define (herdr-shepherd-service config)
  (let* ((pkg        (herdr-server-configuration-package config))
         (user       (herdr-server-configuration-user config))
         (group      (herdr-server-configuration-group config))
         (log-file   (herdr-server-configuration-log-file config))
         (respawn?   (herdr-server-configuration-respawn? config))
         (extra      (herdr-server-configuration-extra-path-directories config))
         (require-h? (herdr-server-configuration-require-guix-home? config))
         (home       (string-append "/home/" user))
         ;; One provisioning name per account, the way (gnu services guix)
         ;; names its guix-home one-shots: two accounts on one box must not
         ;; collide on a single 'herdr-server symbol.
         (name        (symbol-append 'herdr-server- (string->symbol user)))
         (guix-home-name (symbol-append 'guix-home- (string->symbol user)))
         (extra-path-colon (join-colon extra)))
    ;; A LIST of service records, not a bare record: shepherd-root-service-type
    ;; composes its dependents' outputs with concatenate, so an extension
    ;; function returning #<shepherd-service> instead of (#<shepherd-service>)
    ;; fails the OS build with "Wrong type argument in position N (expecting
    ;; empty list)".  Same reason (peteches services rules-lawyer)'s
    ;; rules-lawyer-shepherd-service wraps its single record in a list.
    (list (shepherd-service
     (provision (list name))
     (documentation (string-append "Headless herdr agent-session server for "
                                   user "."))
     (requirement (if require-h? (list guix-home-name) '()))
     ;; A LAMBDA, not a bare #~(make-forkexec-constructor ...): see gotcha #1
     ;; in (peteches services rules-lawyer)'s commentary — that constructor
     ;; evaluates its keyword arguments eagerly, at service-file load time.
     ;; Nothing here is read from a secret file, so nothing throws, but keep
     ;; the shape anyway: the service must be able to start long after load.
     (start #~(lambda _
                ((make-forkexec-constructor
                  (list #$(file-append pkg "/bin/herdr") "server")
                  #:user #$user
                  #:group #$group
                  #:log-file #$log-file
                  #:environment-variables
                  ;; All of these are gexp string-appends, so the whole env
                  ;; list is built INSIDE the gexp (at service start), not
                  ;; outside it.  The '#$ splices below carry configure-time
                  ;; values -- strings, and one gexp for each file-append --
                  ;; into the generated service file; none of them needs the
                  ;; '#$(...) quoted-splice form, because none of them is a
                  ;; whole list in argument position (gotcha #3 in the
                  ;; rules-lawyer commentary is about exactly that case).
                  (list (string-append "HOME=" #$home)
                        (string-append "USER=" #$user)
                        (string-append "SHELL=" #$(file-append bash "/bin/bash"))
                        (string-append
                         "PATH="
                         #$(file-append pkg "/bin") ":"
                         #$extra-path-colon
                         #$home "/.guix-home/profile/bin:"
                         #$home "/.guix-home/profile/sbin:"
                         #$home "/.guix-profile/bin:"
                         "/run/current-system/profile/bin:"
                         "/run/current-system/profile/sbin:"
                         "/run/privileged/bin"))))))
     (stop #~(make-kill-destructor))
     ;; The server's own state lives in ~/.config/herdr and survives its
     ;; death, so a respawn is safe: workspaces already tracked by herdr
     ;; resume against the restarted server.  Plain configure-time boolean,
     ;; NOT '#$respawn?': shepherd-service is a define-record-type* macro
     ;; whose arguments here are evaluated in the HOST process, outside any
     ;; gexp, so an ungexp there fails with "ungexp: unbound variable"
     ;; (only start/stop below are inside #~, which is why the splices there
     ;; are legal).
     (respawn? respawn?)))))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public herdr-server-service-type
  (service-type
   (name 'herdr-server)
   (description
    "Headless herdr agent-session server for one account, supervised by the
system shepherd so it survives reboots without ever needing a login shell.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             herdr-shepherd-service)))
   ;; No compose/extend, matching (peteches services rules-lawyer) and
   ;; (peteches services alloy): those service types are read-only from the
   ;; extension side, so Guix never calls compose on them (gnu services
   ;; calls it only when extend is present), and several instances of this
   ;; type — one per account — coalesce inside shepherd-root's own compose.
   (default-value
    (herdr-server-configuration
     (user "peteches")))))
