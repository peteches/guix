;; peteches/services/sillytavern.scm — SillyTavern LLM frontend service.
;;
;; Deliberately not packaged as a Guix package — same reasoning as
;; peteches/services/comfyui.scm: Guix owns the service boundary (account,
;; paths, environment, ports, supervision); npm owns the Node.js dependency
;; tree (829 packages as of 1.18.0). Unlike ComfyUI, no FHS-emulated
;; container is needed here — verified directly: a plain `npm install` on
;; this codebase pulls no native (.node) addons requiring compilation, and a
;; `node server.js` run against the result serves HTTP with no ELF-
;; interpreter/dynamic-linker complaints, the exact class of problem that
;; forced ComfyUI into `guix shell --container --emulate-fhs` in the first
;; place. So this is a plain forkexec'd daemon, closer to colibri.scm.
;;
;; Auto-start defaults to #t here, unlike colibri.scm's #f: colibri needs a
;; multi-hundred-GB model acquired out of band before it can do anything,
;; where SillyTavern is fully self-contained after `npm install` (a few
;; seconds) — there is nothing to wait on a human for.
;;
;; SillyTavern reads every config.yaml key as an environment variable too,
;; via a generic SILLYTAVERN_<DOTTED_PATH_UPPERCASED_WITH_UNDERSCORES>
;; mechanism (src/util.js: keyToEnv) — object/array values get JSON-parsed
;; from the env var string. That covers basicAuthUser.{username,password}
;; and hostWhitelist.{enabled,hosts}, neither of which has a CLI flag, so
;; this service never needs to render a config.yaml file at all: CLI flags
;; for the plain scalars SillyTavern does expose a flag for, environment
;; variables for the rest.
;;
;; Host-header guard: like colibri, reachable via Caddy over Tailscale, not
;; just loopback. whitelistMode is IP-based and would only ever see Caddy's
;; own address, not the real client's, so it's disabled here in favour of
;; hostWhitelist (Host-header based) + basic auth, the same two-layer
;; approach colibri.scm uses for the identical reason.

(define-module (peteches services sillytavern)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages node)
  #:use-module (gnu packages version-control)
  #:use-module (peteches services firewall)
  #:export (sillytavern-configuration sillytavern-configuration?
                                      sillytavern-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (bool->js b)
  (if b "true" "false"))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <sillytavern-configuration> sillytavern-configuration
                     make-sillytavern-configuration
  sillytavern-configuration?
  (service-name sillytavern-configuration-service-name
                (default "sillytavern"))
  (documentation sillytavern-configuration-documentation
                 (default "SillyTavern LLM frontend."))
  (auto-start? sillytavern-configuration-auto-start?
               (default #t))

  (create-account? sillytavern-configuration-create-account?
                   (default #t))
  (user sillytavern-configuration-user
        (default "sillytavern"))
  (group sillytavern-configuration-group
         (default "sillytavern"))

  ;; git checkout + node_modules. Kept separate from state-dir (user data)
  ;; so an update (git pull + npm install) never touches characters/chats.
  (repo-dir sillytavern-configuration-repo-dir
           (default "/srv/sillytavern"))
  (git-repo-url sillytavern-configuration-git-repo-url
                (default "https://github.com/SillyTavern/SillyTavern"))
  (git-branch sillytavern-configuration-git-branch
              (default "release"))
  (git-command sillytavern-configuration-git-command
               (default (file-append git "/bin/git")))
  (node-package sillytavern-configuration-node-package
                (default node))

  (host sillytavern-configuration-host
        (default "127.0.0.1"))
  (port sillytavern-configuration-port
        ;; Not upstream's own default (8000) — that collides with colibri
        ;; on the same box (peteches/services/colibri.scm). Deployments
        ;; running both must pick non-clashing ports explicitly either way;
        ;; this default just isn't a landmine on first use.
        (default 8001))

  ;; Host-header allowlist (hostWhitelist.hosts) — see the module comment.
  (allowed-hosts sillytavern-configuration-allowed-hosts
                (default '()))

  ;; Basic auth. Username is not secret; password is read from a file at
  ;; service start (like colibri's api-key-file), never baked into the
  ;; store.
  (basic-auth? sillytavern-configuration-basic-auth?
               (default #f))
  (basic-auth-username sillytavern-configuration-basic-auth-username
                       (default "peteches"))
  (basic-auth-password-file sillytavern-configuration-basic-auth-password-file
                            (default #f))

  (extra-args sillytavern-configuration-extra-args
              (default '()))
  (extra-environment-variables
   sillytavern-configuration-extra-environment-variables
   (default '()))

  (state-dir sillytavern-configuration-state-dir
             (default #f))
  (cache-dir sillytavern-configuration-cache-dir
             (default #f))
  (log-file sillytavern-configuration-log-file
            (default #f))

  (open-firewall? sillytavern-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (sillytavern-resolved-state-dir config)
  (or (sillytavern-configuration-state-dir config)
      (string-append "/var/lib/sillytavern/"
                     (sillytavern-configuration-service-name config))))

(define (sillytavern-resolved-cache-dir config)
  (or (sillytavern-configuration-cache-dir config)
      (string-append "/var/cache/sillytavern/"
                     (sillytavern-configuration-service-name config))))

(define (sillytavern-resolved-data-root config)
  (string-append (sillytavern-resolved-state-dir config) "/data"))

(define (sillytavern-resolved-config-path config)
  (string-append (sillytavern-resolved-state-dir config) "/config.yaml"))

(define (sillytavern-resolved-log-file config)
  (or (sillytavern-configuration-log-file config)
      (string-append "/var/log/sillytavern/"
                     (sillytavern-configuration-service-name config) ".log")))

(define (sillytavern-resolved-sync-log-file config)
  (string-append "/var/log/sillytavern/"
                (sillytavern-configuration-service-name config) "-sync.log"))

(define (sillytavern-resolved-update-log-file config)
  (string-append "/var/log/sillytavern/"
                (sillytavern-configuration-service-name config) "-update.log"))

;;; ── server.js invocation ─────────────────────────────────────────────────

(define (sillytavern-server-args config)
  (list "--port" (number->string (sillytavern-configuration-port config))
        "--listen" (bool->js (not (string=? (sillytavern-configuration-host
                                             config) "127.0.0.1")))
        "--listenAddressIPv4" (sillytavern-configuration-host config)
        "--dataRoot" (sillytavern-resolved-data-root config)
        "--configPath" (sillytavern-resolved-config-path config)
        "--whitelist" "false"
        "--basicAuthMode" (bool->js (sillytavern-configuration-basic-auth?
                                     config))
        "--browserLaunchEnabled" "false"))

(define (sillytavern-environment config)
  (let ((state-dir (sillytavern-resolved-state-dir config))
        (cache-dir (sillytavern-resolved-cache-dir config))
        (allowed-hosts (sillytavern-configuration-allowed-hosts config))
        (basic-auth? (sillytavern-configuration-basic-auth? config))
        (username (sillytavern-configuration-basic-auth-username config)))
    (append (list (string-append "HOME=" state-dir)
                  (string-append "npm_config_cache=" cache-dir)
                  "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
                  ;; #:environment-variables replaces the process
                  ;; environment outright rather than extending it, and
                  ;; git/npm have no built-in fallback CA path on Guix —
                  ;; same lesson as colibri's missing ldd, same fix
                  ;; comfyui.scm already established for this exact need.
                  "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
            (if (not (null? allowed-hosts))
                (list "SILLYTAVERN_HOSTWHITELIST_ENABLED=true"
                      (string-append
                       "SILLYTAVERN_HOSTWHITELIST_HOSTS=["
                       (string-join (map (lambda (h)
                                          (string-append "\"" h "\""))
                                        allowed-hosts)
                                   ",")
                       "]"))
                '())
            (if basic-auth?
                (list (string-append "SILLYTAVERN_BASICAUTHUSER_USERNAME="
                                     username))
                '())
            (sillytavern-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (sillytavern-accounts config)
  (if (sillytavern-configuration-create-account? config)
      (list (user-group (name (sillytavern-configuration-group config))
                        (system? #t))
            (user-account (name (sillytavern-configuration-user config))
                          (group (sillytavern-configuration-group config))
                          (system? #t)
                          (comment "SillyTavern LLM frontend daemon")
                          (home-directory (sillytavern-resolved-state-dir
                                           config))
                          (shell (file-append shadow "/sbin/nologin"))))
      '()))

(define (sillytavern-activation config)
  (let* ((user (sillytavern-configuration-user config))
         (repo-dir (sillytavern-configuration-repo-dir config))
         (state-dir (sillytavern-resolved-state-dir config))
         (cache-dir (sillytavern-resolved-cache-dir config))
         (data-root (sillytavern-resolved-data-root config))
         (log-dir (string-append "/var/log/sillytavern")))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$repo-dir #$state-dir #$cache-dir #$data-root
                          #$log-dir))))))

(define (sillytavern-sync-shepherd-service config)
  (let* ((service-name (sillytavern-configuration-service-name config))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (repo-dir (sillytavern-configuration-repo-dir config))
         (git-repo-url (sillytavern-configuration-git-repo-url config))
         (git-branch (sillytavern-configuration-git-branch config))
         (git-command (sillytavern-configuration-git-command config))
         (node-pkg (sillytavern-configuration-node-package config))
         (npm-command (file-append node-pkg "/bin/npm"))
         (user (sillytavern-configuration-user config))
         (group (sillytavern-configuration-group config))
         (log-file (sillytavern-resolved-sync-log-file config))
         (env (sillytavern-environment config))
         (runner (program-file
                  (string-append service-name "-sync-runner")
                  #~(begin
                      (unless (file-exists? (string-append #$repo-dir "/.git"))
                        (let ((ret (system* #$git-command "clone"
                                            "--branch" #$git-branch
                                            #$git-repo-url #$repo-dir)))
                          (unless (zero? ret)
                            (error "git clone failed with exit code" ret))))
                      (let ((ret (system* #$npm-command "install"
                                         "--no-audit" "--no-fund"
                                         "--prefix" #$repo-dir)))
                        (unless (zero? ret)
                          (error "npm install failed with exit code" ret)))))))
    (shepherd-service (provision (list sync-name))
                      (documentation
                       (string-append "Clone/install the npm environment for "
                                      service-name "."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$runner)
                                                          #:directory #$repo-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(lambda _
                                #t)))))

;; Manual-only version bump, mirrors comfyui.scm's update service: never
;; auto-starts, never a dependency of anything else, only reachable via
;; `herd start sillytavern-update`, then `herd restart sillytavern`.
(define (sillytavern-update-shepherd-service config)
  (let* ((service-name (sillytavern-configuration-service-name config))
         (update-name (string->symbol (string-append service-name "-update")))
         (repo-dir (sillytavern-configuration-repo-dir config))
         (git-command (sillytavern-configuration-git-command config))
         (node-pkg (sillytavern-configuration-node-package config))
         (npm-command (file-append node-pkg "/bin/npm"))
         (user (sillytavern-configuration-user config))
         (group (sillytavern-configuration-group config))
         (log-file (sillytavern-resolved-update-log-file config))
         (env (sillytavern-environment config))
         (runner (program-file
                  (string-append service-name "-update-runner")
                  #~(begin
                      (unless (file-exists? (string-append #$repo-dir "/.git"))
                        (error "sillytavern-update: no git checkout found at"
                               #$repo-dir))
                      (let ((ret (system* #$git-command "-C" #$repo-dir
                                         "pull" "--ff-only")))
                        (unless (zero? ret)
                          (error "git pull failed with exit code" ret)))
                      (let ((ret (system* #$npm-command "install"
                                         "--no-audit" "--no-fund"
                                         "--prefix" #$repo-dir)))
                        (unless (zero? ret)
                          (error "npm install failed with exit code" ret)))))))
    (shepherd-service (provision (list update-name))
                      (documentation
                       (string-append "Pull the latest commit and re-run npm "
                                      "install for " service-name
                                      ". Never starts on its own."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$runner)
                                                          #:directory #$repo-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(lambda _
                                #t)))))

(define (sillytavern-daemon-shepherd-service config)
  (let* ((service-name (sillytavern-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (repo-dir (sillytavern-configuration-repo-dir config))
         (node-pkg (sillytavern-configuration-node-package config))
         (node-command (file-append node-pkg "/bin/node"))
         (user (sillytavern-configuration-user config))
         (group (sillytavern-configuration-group config))
         (log-file (sillytavern-resolved-log-file config))
         (password-file (sillytavern-configuration-basic-auth-password-file
                         config))
         (env (sillytavern-environment config))
         (args (sillytavern-server-args config)))
    (shepherd-service (provision (list provision-name))
                      (documentation (sillytavern-configuration-documentation
                                      config))
                      ;; Requiring the sync one-shot here (rather than just
                      ;; networking/file-systems) is what makes this self-
                      ;; bootstrapping: shepherd starts a one-shot
                      ;; requirement on demand regardless of its own
                      ;; auto-start?, satisfied permanently once it exits 0.
                      (requirement (list sync-name))
                      (auto-start? (sillytavern-configuration-auto-start?
                                    config))
                      (start
                       #~(lambda _
                           (let* ((pw-file #$password-file)
                                  (pw-env
                                   (if pw-file
                                       (list
                                        (string-append
                                         "SILLYTAVERN_BASICAUTHUSER_PASSWORD="
                                         (string-trim-right
                                          (call-with-input-file pw-file
                                            (@ (ice-9 rdelim) read-line)))))
                                       '())))
                             ((make-forkexec-constructor
                               (list #$node-command
                                    (string-append #$repo-dir "/server.js")
                                    #$@args
                                    #$@(sillytavern-configuration-extra-args
                                        config))
                               #:directory #$repo-dir
                               #:user #$user
                               #:group #$group
                               #:log-file #$log-file
                               #:environment-variables
                               (append (list #$@env) pw-env))))))
                      (stop #~(make-kill-destructor)))))

(define (sillytavern-shepherd-services config)
  (list (sillytavern-sync-shepherd-service config)
        (sillytavern-update-shepherd-service config)
        (sillytavern-daemon-shepherd-service config)))

(define (sillytavern-firewall-rules config)
  (if (sillytavern-configuration-open-firewall? config)
      (nftables-rules
       (input (list (string-append "tcp dport "
                                   (number->string (sillytavern-configuration-port
                                                    config))
                                   " accept comment \""
                                   (sillytavern-configuration-service-name config)
                                   "\""))))
      (nftables-rules (input '()))))

(define (sillytavern-profile config)
  (list (sillytavern-configuration-node-package config) git))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public sillytavern-service-type
  (service-type (name 'sillytavern)
                (description
                 "Run the SillyTavern LLM frontend, cloned via git and installed via npm.")
                (extensions (list (service-extension account-service-type
                                                     sillytavern-accounts)
                                  (service-extension activation-service-type
                                                     sillytavern-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   sillytavern-shepherd-services)
                                  (service-extension firewall-service-type
                                                     sillytavern-firewall-rules)
                                  (service-extension profile-service-type
                                                     sillytavern-profile)))))
