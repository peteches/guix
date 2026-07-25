(define-module (peteches services concourse)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (peteches packages concourse)
  #:use-module (peteches services firewall)
  #:use-module (srfi srfi-13)
  #:export (concourse-web-configuration concourse-web-configuration?
                                        concourse-web-service-type
                                        concourse-worker-configuration
                                        concourse-worker-configuration?
                                        concourse-worker-service-type))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <concourse-web-configuration> concourse-web-configuration
                     make-concourse-web-configuration
  concourse-web-configuration?
  (package
    concourse-web-configuration-package
    (default concourse))
  (bind-ip concourse-web-configuration-bind-ip
           (default "0.0.0.0"))
  (bind-port concourse-web-configuration-bind-port
             (default 8080))
  (external-url concourse-web-configuration-external-url
                (default "http://localhost:8080"))
  (postgres-host concourse-web-configuration-postgres-host
                 (default "localhost"))
  (postgres-port concourse-web-configuration-postgres-port
                 (default 5432))
  (postgres-user concourse-web-configuration-postgres-user
                 (default "concourse"))
  (postgres-database concourse-web-configuration-postgres-database
                     (default "concourse"))
  (postgres-password-file concourse-web-configuration-postgres-password-file
                          (default "/run/secrets/concourse-db-password"))
  (session-signing-key concourse-web-configuration-session-signing-key
                       (default "/run/secrets/concourse-session-signing-key"))
  (tsa-host-key concourse-web-configuration-tsa-host-key
                (default "/run/secrets/concourse-tsa-host-key"))
  (tsa-authorized-keys concourse-web-configuration-tsa-authorized-keys
                       (default
                        "/run/secrets/concourse-authorized-worker-keys"))
  (local-users-file concourse-web-configuration-local-users-file
                    (default "/run/secrets/concourse-local-users"))
  (main-team-local-users concourse-web-configuration-main-team-local-users
                         (default '()))
  (data-dir concourse-web-configuration-data-dir
            (default "/var/lib/concourse-web"))
  (log-file concourse-web-configuration-log-file
            (default "/var/log/concourse-web.log"))
  (log-level concourse-web-configuration-log-level
             (default "info"))
  (vault-url concourse-web-configuration-vault-url
             (default #f))
  (vault-auth-backend concourse-web-configuration-vault-auth-backend
                      (default "approle"))
  (vault-role-id-file concourse-web-configuration-vault-role-id-file
                      (default "/run/secrets/concourse-vault-role-id"))
  (vault-secret-id-file concourse-web-configuration-vault-secret-id-file
                        (default "/run/secrets/concourse-vault-secret-id"))
  (vault-enable-kv-cache concourse-web-configuration-vault-enable-kv-cache
                         (default #t)))

(define-record-type* <concourse-worker-configuration>
                     concourse-worker-configuration
                     make-concourse-worker-configuration
  concourse-worker-configuration?
  (package
    concourse-worker-configuration-package
    (default concourse))
  (work-dir concourse-worker-configuration-work-dir
            (default "/var/lib/concourse-worker"))
  (tsa-host concourse-worker-configuration-tsa-host
            (default "127.0.0.1:2222"))
  (tsa-public-key concourse-worker-configuration-tsa-public-key
                  (default "/run/secrets/concourse-tsa-host-key.pub"))
  (worker-private-key concourse-worker-configuration-worker-private-key
                      (default "/run/secrets/concourse-worker-key"))
  (runtime concourse-worker-configuration-runtime
           (default "containerd"))
  (log-file concourse-worker-configuration-log-file
            (default "/var/log/concourse-worker.log"))
  (log-level concourse-worker-configuration-log-level
             (default "info")))

;;; ── Web service extension helpers ────────────────────────────────────────

(define (concourse-web-accounts config)
  (list (user-group
          (name "concourse")
          (system? #t))
        (user-account
          (name "concourse-web")
          (group "concourse")
          (system? #t)
          (comment "Concourse CI web daemon")
          (home-directory (concourse-web-configuration-data-dir config))
          (shell (file-append shadow "/sbin/nologin")))))

(define (concourse-web-activation config)
  (let ((data-dir (concourse-web-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam "concourse-web"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-dir)
          (chown #$data-dir uid gid)))))

(define (concourse-web-shepherd-service config)
  (let* ((pkg (concourse-web-configuration-package config))
         (bind-ip (concourse-web-configuration-bind-ip config))
         (bind-port (concourse-web-configuration-bind-port config))
         (external-url (concourse-web-configuration-external-url config))
         (session-signing-key (concourse-web-configuration-session-signing-key
                               config))
         (tsa-host-key (concourse-web-configuration-tsa-host-key config))
         (tsa-authorized-keys (concourse-web-configuration-tsa-authorized-keys
                               config))
         (local-users-file (concourse-web-configuration-local-users-file
                            config))
         (main-team-local-users (concourse-web-configuration-main-team-local-users
                                 config))
         (postgres-host (concourse-web-configuration-postgres-host config))
         (postgres-port (concourse-web-configuration-postgres-port config))
         (postgres-user (concourse-web-configuration-postgres-user config))
         (postgres-database (concourse-web-configuration-postgres-database
                             config))
         (postgres-password-file (concourse-web-configuration-postgres-password-file
                                  config))
         (log-file (concourse-web-configuration-log-file config))
         (log-level (concourse-web-configuration-log-level config))
         (vault-url (concourse-web-configuration-vault-url config))
         (vault-auth-backend (concourse-web-configuration-vault-auth-backend
                              config))
         (vault-role-id-file (concourse-web-configuration-vault-role-id-file
                              config))
         (vault-secret-id-file (concourse-web-configuration-vault-secret-id-file
                                config))
         (vault-enable-kv-cache (concourse-web-configuration-vault-enable-kv-cache
                                 config)))
    (list (shepherd-service (provision '(concourse-web))
                            (documentation
                             "Concourse CI web node (ATC + TSA).")
                            (requirement '(networking file-systems
                                                      sops-secrets))
                            (start #~(make-forkexec-constructor (list #$(file-append
                                                                         pkg
                                                                         "/bin/concourse")
                                                                 "web"
                                                                 "--bind-ip"
                                                                 #$bind-ip
                                                                 "--bind-port"
                                                                 #$(number->string
                                                                    bind-port)
                                                                 "--external-url"
                                                                 #$external-url
                                                                 "--session-signing-key"
                                                                 #$session-signing-key
                                                                 "--tsa-host-key"
                                                                 #$tsa-host-key
                                                                 "--tsa-authorized-keys"
                                                                 #$tsa-authorized-keys
                                                                 "--log-level"
                                                                 #$log-level)
                                      #:environment-variables (append (list (string-append
                                                                             "CONCOURSE_POSTGRES_HOST="
                                                                             #$postgres-host)
                                                                            (string-append
                                                                             "CONCOURSE_POSTGRES_PORT="
                                                                             #$
                                                                             (number->string
                                                                              postgres-port))
                                                                            (string-append
                                                                             "CONCOURSE_POSTGRES_USER="
                                                                             #$postgres-user)
                                                                            (string-append
                                                                             "CONCOURSE_POSTGRES_DATABASE="
                                                                             #$postgres-database)
                                                                            (string-append
                                                                             "CONCOURSE_POSTGRES_PASSWORD_FILE="
                                                                             #$postgres-password-file))
                                                                      (if #$local-users-file
                                                                          (begin
                                                                            (use-modules
                                                                             (ice-9
                                                                              textual-ports)
                                                                             (srfi
                                                                              srfi-13))
                                                                            (list
                                                                             (string-append
                                                                              "CONCOURSE_ADD_LOCAL_USER="

                                                                              
                                                                              (string-trim-right (call-with-input-file #$local-users-file
                                                                                                   get-string-all)))))
                                                                          '())
                                                                      (if (null? '#$main-team-local-users)
                                                                          '()
                                                                          (list
                                                                           (string-append
                                                                            "CONCOURSE_MAIN_TEAM_LOCAL_USER="
                                                                            #$
                                                                            (string-join
                                                                             main-team-local-users
                                                                             ","))))
                                                                      (if #$vault-url
                                                                          (begin
                                                                            (use-modules
                                                                             (ice-9
                                                                              rdelim)
                                                                             (srfi
                                                                              srfi-13))
                                                                            (let 
                                                                                 (
                                                                                  (role-id
                                                                                   (string-trim-right (call-with-input-file #$vault-role-id-file
                                                                                                        read-line)))
                                                                                  
                                                                                  
                                                                                  (secret-id
                                                                                   (string-trim-right (call-with-input-file #$vault-secret-id-file
                                                                                                        read-line))))
                                                                              
                                                                              
                                                                              (list
                                                                               (string-append
                                                                                "CONCOURSE_VAULT_URL="
                                                                                #$vault-url)

                                                                               
                                                                               (string-append
                                                                                "CONCOURSE_VAULT_AUTH_BACKEND="
                                                                                #$vault-auth-backend)

                                                                               
                                                                               (string-append
                                                                                "CONCOURSE_VAULT_AUTH_PARAM=role_id:"
                                                                                role-id
                                                                                ",secret_id:"
                                                                                secret-id)

                                                                               
                                                                               (if #$vault-enable-kv-cache
                                                                                "CONCOURSE_VAULT_ENABLE_KV_MOUNT_CACHE=true"
                                                                                "CONCOURSE_VAULT_ENABLE_KV_MOUNT_CACHE=false"))))
                                                                          '()))
                                      #:user "concourse-web"
                                      #:group "concourse"
                                      #:log-file #$log-file))
                            (stop #~(make-kill-destructor))))))

(define (concourse-web-firewall-rules config)
  (nftables-rules (input (list (string-append "tcp dport "
                                (number->string (concourse-web-configuration-bind-port
                                                 config))
                                " accept comment \"concourse-web\"")
                          "tcp dport 2222 accept comment \"concourse-tsa\""))))

(define (concourse-web-profile config)
  (list (concourse-web-configuration-package config)))

;;; ── Worker service extension helpers ─────────────────────────────────────

(define (concourse-worker-activation config)
  (let ((work-dir (concourse-worker-configuration-work-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$work-dir))))

(define (concourse-worker-shepherd-service config)
  (let* ((pkg (concourse-worker-configuration-package config))
         (work-dir (concourse-worker-configuration-work-dir config))
         (tsa-host (concourse-worker-configuration-tsa-host config))
         (tsa-public-key (concourse-worker-configuration-tsa-public-key config))
         (worker-private-key (concourse-worker-configuration-worker-private-key
                              config))
         (runtime (concourse-worker-configuration-runtime config))
         (log-file (concourse-worker-configuration-log-file config))
         (log-level (concourse-worker-configuration-log-level config)))
    (list (shepherd-service (provision '(concourse-worker))
                            (documentation "Concourse CI worker node.")
                            (requirement '(networking file-systems
                                           file-system-/sys/fs/cgroup
                                           sops-secrets))
                            (start #~(make-forkexec-constructor (list #$(file-append
                                                                         pkg
                                                                         "/bin/concourse")
                                                                 "worker"
                                                                 "--work-dir"
                                                                 #$work-dir
                                                                 "--tsa-host"
                                                                 #$tsa-host
                                                                 "--tsa-public-key"
                                                                 #$tsa-public-key
                                                                 "--tsa-worker-private-key"
                                                                 #$worker-private-key
                                                                 "--runtime"
                                                                 #$runtime
                                                                 "--containerd-bin"
                                                                 #$(file-append
                                                                    pkg
                                                                    "/bin/containerd")
                                                                 "--containerd-init-bin"
                                                                 #$(file-append
                                                                    pkg
                                                                    "/bin/init")
                                                                 "--containerd-cni-plugins-dir"
                                                                 #$(file-append
                                                                    pkg "/bin")
                                                                 "--containerd-log-level"
                                                                 "debug"
                                                                 "--log-level"
                                                                 #$log-level)
                                                                #:environment-variables
                                                                (list (string-append
                                                                       "PATH="
                                                                       #$(file-append
                                                                          pkg
                                                                          "/bin")
                                                                       ":/run/setuid-programs"
                                                                       ":/run/current-system/profile/bin"
                                                                       ":/run/current-system/profile/sbin")
                                                                 "IPTABLES_MODE=nft")
                                                                #:log-file #$log-file))
                            (stop #~(make-kill-destructor))))))

(define (concourse-worker-firewall-rules config)
  (nftables-rules
   ;; Mostly not needed for normal forwarded container egress, but harmless and
   ;; useful if Quad100 traffic is handled as local input by tailscaled.
   (input (list
           "iifname \"concourse0\" ip daddr 100.100.100.100 udp dport 53 accept comment \"concourse containers to tailscale dns udp\""
           "iifname \"concourse0\" ip daddr 100.100.100.100 tcp dport 53 accept comment \"concourse containers to tailscale dns tcp\""))

   ;; Concourse build/resource containers live behind concourse0.
   ;; Allow egress to LAN/internet via eth0 and tailnet via ts-peteches.
   (forward (list
             "iifname \"concourse0\" oifname \"eth0\" accept comment \"concourse containers to lan/internet\""
             "iifname \"eth0\" oifname \"concourse0\" ct state established,related accept comment \"lan/internet return to concourse containers\""

             "iifname \"concourse0\" oifname \"ts-peteches\" accept comment \"concourse containers to tailscale\""
             "iifname \"ts-peteches\" oifname \"concourse0\" ct state established,related accept comment \"tailscale return to concourse containers\""))

   ;; NAT containers both through normal LAN/internet and through Tailscale.
   (nat-postrouting (list
                     "ip saddr 10.80.0.0/16 oifname \"eth0\" masquerade comment \"nat concourse containers to lan/internet\""
                     "ip saddr 10.80.0.0/16 oifname \"ts-peteches\" masquerade comment \"nat concourse containers to tailscale\""))))

(define (concourse-worker-profile config)
  (list (concourse-worker-configuration-package config) iptables))

;;; ── Service types ─────────────────────────────────────────────────────────

(define-public concourse-web-service-type
  (service-type (name 'concourse-web)
                (description
                 "Concourse CI web node — ATC (scheduler/API) and TSA (worker gateway).")
                (extensions (list (service-extension account-service-type
                                                     concourse-web-accounts)
                                  (service-extension activation-service-type
                                                     concourse-web-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   concourse-web-shepherd-service)
                                  (service-extension firewall-service-type
                                   concourse-web-firewall-rules)
                                  (service-extension profile-service-type
                                                     concourse-web-profile)))
                (default-value (concourse-web-configuration))))

(define-public concourse-worker-service-type
  (service-type (name 'concourse-worker)
                (description
                 "Concourse CI worker node — runs builds inside containers (requires root).")
                (extensions (list (service-extension activation-service-type
                                   concourse-worker-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   concourse-worker-shepherd-service)
                                  (service-extension profile-service-type
                                                     concourse-worker-profile)))
                (default-value (concourse-worker-configuration))))
