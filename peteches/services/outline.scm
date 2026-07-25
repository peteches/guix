;; peteches/services/outline.scm — Guix service type for Outline wiki.
;;
;; Runs the official outlinewiki/outline Docker image as a Shepherd service
;; via Guix's oci-service-type.  No package derivation is needed.

(define-module (peteches services outline)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches services firewall)
  #:export (outline-configuration outline-configuration? outline-service-type))

;;; ── Configuration record ──────────────────────────────────────────────────

(define-record-type* <outline-configuration> outline-configuration
                     make-outline-configuration
  outline-configuration?

  ;; Container image reference.
  (image outline-configuration-image
         (default "docker.io/outlinewiki/outline:1.8.1"))

  ;; Publicly accessible URL Outline is served at, e.g. "https://wiki.example.com".
  ;; Required — Outline embeds this in generated links and CSP headers.
  (url outline-configuration-url)

  ;; Host port to forward to the container's port 3000.
  (port outline-configuration-port
        (default 3000))

  ;; Host directory bind-mounted into the container at /var/lib/outline/data.
  ;; Owned by UID/GID 1001 (the container's nodejs user).
  (data-dir outline-configuration-data-dir
            (default "/var/lib/outline"))

  (log-file outline-configuration-log-file
            (default "/var/log/outline.log"))

  ;; ── Core secrets ─────────────────────────────────────────────────────────
  ;; Paths to SOPS-decrypted files in /run/secrets/.  Each file is mounted
  ;; read-only into the container and referenced via Outline's VAR_FILE
  ;; convention: Outline reads the file at startup and uses its trimmed
  ;; content as the variable value.
  
  ;; → SECRET_KEY_FILE  File content: 64-char hex string (openssl rand -hex 32).
  (secret-key-file outline-configuration-secret-key-file
                   (default "/run/secrets/outline_secret_key"))

  ;; → UTILS_SECRET_FILE  File content: any unique random string (openssl rand -hex 32).
  (utils-secret-file outline-configuration-utils-secret-file
                     (default "/run/secrets/outline_utils_secret"))

  ;; → DATABASE_URL_FILE  File content: full PostgreSQL connection URL,
  ;; e.g. postgres://outline:password@localhost:5432/outline
  (database-url-file outline-configuration-database-url-file
                     (default "/run/secrets/outline_database_url"))

  ;; ── Redis ────────────────────────────────────────────────────────────────
  ;; Plain connection URL used when Redis requires no auth or auth is embedded.
  (redis-url outline-configuration-redis-url
             (default "redis://localhost:6379"))

  ;; Path to a SOPS-decrypted env-file containing REDIS_URL=redis://:pw@host:port.
  ;; When set the file is bind-mounted read-only and passed via --env-file, which
  ;; takes precedence over (redis-url ...).  Use this when Redis has a password.
  (redis-url-file outline-configuration-redis-url-file
                  (default ""))

  ;; ── OIDC auth ─────────────────────────────────────────────────────────────
  ;; At least one auth block (OIDC or SMTP) must be configured for users to
  ;; be able to log in.  Set oidc-auth-uri to a non-empty string to enable
  ;; OIDC; leave all oidc-* fields as "" to skip this provider.
  
  ;; → OIDC_CLIENT_ID_FILE  File content: OAuth client ID from your OIDC provider.
  (oidc-client-id-file outline-configuration-oidc-client-id-file
                       (default ""))

  ;; → OIDC_CLIENT_SECRET_FILE  File content: OAuth client secret from your provider.
  (oidc-client-secret-file outline-configuration-oidc-client-secret-file
                           (default ""))

  (oidc-auth-uri outline-configuration-oidc-auth-uri
                 (default ""))
  (oidc-token-uri outline-configuration-oidc-token-uri
                  (default ""))
  (oidc-userinfo-uri outline-configuration-oidc-userinfo-uri
                     (default ""))
  (oidc-logout-uri outline-configuration-oidc-logout-uri
                   (default ""))
  (oidc-display-name outline-configuration-oidc-display-name
                     (default "OpenID Connect"))
  (oidc-scopes outline-configuration-oidc-scopes
               (default "openid profile email"))

  ;; ── SMTP magic-link auth ──────────────────────────────────────────────────
  ;; Set smtp-host to a non-empty string to enable email magic-link login.
  ;; Leave smtp-host as "" to skip SMTP entirely.
  
  (smtp-host outline-configuration-smtp-host
             (default ""))
  (smtp-port outline-configuration-smtp-port
             (default 587))
  (smtp-username outline-configuration-smtp-username
                 (default ""))

  ;; → SMTP_PASSWORD_FILE  File content: password for the SMTP account.
  (smtp-password-file outline-configuration-smtp-password-file
                      (default ""))

  (smtp-from-email outline-configuration-smtp-from-email
                   (default ""))
  (smtp-reply-email outline-configuration-smtp-reply-email
                    (default ""))
  ;; Whether to use TLS for SMTP (sets SMTP_SECURE=true).
  (smtp-secure outline-configuration-smtp-secure
               (default #f))

  ;; ── Escape hatch ─────────────────────────────────────────────────────────
  ;; Extra environment variables passed directly to the container, as a list
  ;; of "KEY=value" strings.  Use this for optional integrations (GitHub,
  ;; GitLab, S3, Sentry, iFramely, etc.) and for VAR_FILE= forms of any
  ;; secret not covered by the fields above.
  (extra-environment outline-configuration-extra-environment
                     (default '())))

;;; ── Private helpers ───────────────────────────────────────────────────────

(define (maybe-env key value)
  "Return (list \"KEY=VALUE\") when VALUE is a non-empty string, else '()."
  (if (and (string? value)
           (not (string-null? value)))
      (list (string-append key "=" value))
      '()))

(define (secret-volume path)
  "Return a read-only bind-mount string for a SOPS secret file, or '()."
  (if (and (string? path)
           (not (string-null? path)))
      (list (string-append path ":" path ":ro"))
      '()))

;;; ── OCI container ─────────────────────────────────────────────────────────

(define (outline->oci-container config)
  (let* ((port (outline-configuration-port config))
         (data-dir (outline-configuration-data-dir config))
         (sk-file (outline-configuration-secret-key-file config))
         (us-file (outline-configuration-utils-secret-file config))
         (db-file (outline-configuration-database-url-file config))
         (oidc-id (outline-configuration-oidc-client-id-file config))
         (oidc-sec (outline-configuration-oidc-client-secret-file config))
         (oidc-on? (not (string-null? (outline-configuration-oidc-auth-uri
                                       config))))
         (smtp-pw (outline-configuration-smtp-password-file config))
         (smtp-on? (not (string-null? (outline-configuration-smtp-host config))))
         (ru-file (outline-configuration-redis-url-file config))
         (redis-file? (and (string? ru-file)
                           (not (string-null? ru-file)))))
    (oci-container-configuration (image (outline-configuration-image config))
                                 (provision "outline")
                                 (requirement '(networking file-systems
                                                           sops-secrets))
                                 (log-file (outline-configuration-log-file
                                            config))
                                 ;; --env-file (when redis-url-file is set) overrides the plain REDIS_URL= entry
                                 ;; because Podman applies --env-file after --env flags on the command line.
                                 (extra-arguments (append '("--user=root")
                                                          (if redis-file?
                                                              (list (string-append
                                                                     "--env-file="
                                                                     ru-file))
                                                              '())))
                                 (ports (list (string-append (number->string
                                                              port) ":3000")))
                                 (volumes (append (list (string-append
                                                         data-dir
                                                         ":/var/lib/outline/data"))
                                                  ;; Core secret files — always mounted
                                                  (secret-volume sk-file)
                                                  (secret-volume us-file)
                                                  (secret-volume db-file)
                                                  ;; Redis URL env-file (when using password auth)
                                                  (if redis-file?
                                                      (secret-volume ru-file)
                                                      '())
                                                  ;; OIDC secret files
                                                  (if oidc-on?
                                                      (secret-volume oidc-id)
                                                      '())
                                                  (if oidc-on?
                                                      (secret-volume oidc-sec)
                                                      '())
                                                  ;; SMTP password file
                                                  (if smtp-on?
                                                      (secret-volume smtp-pw)
                                                      '())))
                                 (environment (append
                                               ;; Core always-present vars
                                               (list "NODE_ENV=production"
                                                     (string-append "URL="
                                                                    (outline-configuration-url
                                                                     config)))
                                               ;; Omit REDIS_URL when redis-url-file is set: --env takes priority over
                                               ;; --env-file in Podman, so we must not emit it at all in that case.
                                               (if redis-file?
                                                   '()
                                                   (list (string-append
                                                          "REDIS_URL="
                                                          (outline-configuration-redis-url
                                                           config))))
                                               (list "FILE_STORAGE=local"
                                                "FILE_STORAGE_LOCAL_ROOT_DIR=/var/lib/outline/data"
                                                (string-append
                                                 "SECRET_KEY_FILE=" sk-file)
                                                (string-append
                                                 "UTILS_SECRET_FILE=" us-file)
                                                (string-append
                                                 "DATABASE_URL_FILE=" db-file))
                                               ;; OIDC
                                               (if oidc-on?
                                                   (append (maybe-env
                                                            "OIDC_CLIENT_ID_FILE"
                                                            oidc-id)
                                                           (maybe-env
                                                            "OIDC_CLIENT_SECRET_FILE"
                                                            oidc-sec)
                                                           (maybe-env
                                                            "OIDC_AUTH_URI"
                                                            (outline-configuration-oidc-auth-uri
                                                             config))
                                                           (maybe-env
                                                            "OIDC_TOKEN_URI"
                                                            (outline-configuration-oidc-token-uri
                                                             config))
                                                           (maybe-env
                                                            "OIDC_USERINFO_URI"
                                                            (outline-configuration-oidc-userinfo-uri
                                                             config))
                                                           (maybe-env
                                                            "OIDC_LOGOUT_URI"
                                                            (outline-configuration-oidc-logout-uri
                                                             config))
                                                           (maybe-env
                                                            "OIDC_DISPLAY_NAME"
                                                            (outline-configuration-oidc-display-name
                                                             config))
                                                           (maybe-env
                                                            "OIDC_SCOPES"
                                                            (outline-configuration-oidc-scopes
                                                             config)))
                                                   '())
                                               ;; SMTP
                                               (if smtp-on?
                                                   (append (maybe-env
                                                            "SMTP_HOST"
                                                            (outline-configuration-smtp-host
                                                             config))
                                                           (maybe-env
                                                            "SMTP_PORT"
                                                            (number->string (outline-configuration-smtp-port
                                                                             config)))
                                                           (maybe-env
                                                            "SMTP_USERNAME"
                                                            (outline-configuration-smtp-username
                                                             config))
                                                           (maybe-env
                                                            "SMTP_PASSWORD_FILE"
                                                            smtp-pw)
                                                           (maybe-env
                                                            "SMTP_FROM_EMAIL"
                                                            (outline-configuration-smtp-from-email
                                                             config))
                                                           (maybe-env
                                                            "SMTP_REPLY_EMAIL"
                                                            (outline-configuration-smtp-reply-email
                                                             config))
                                                           (if (outline-configuration-smtp-secure
                                                                config)
                                                               '("SMTP_SECURE=true")
                                                               '()))
                                                   '())
                                               (outline-configuration-extra-environment
                                                config))))))

(define (outline->oci-extension config)
  (oci-extension (containers (list (outline->oci-container config)))))

;;; ── Accounts ──────────────────────────────────────────────────────────────

(define (outline-accounts config)
  (list (user-group
          (name "outline")
          (system? #t))
        (user-account
          (name "outline")
          (group "outline")
          (system? #t)
          (comment "Outline wiki daemon")
          (home-directory (outline-configuration-data-dir config))
          (shell (file-append shadow "/sbin/nologin")))))

;;; ── Activation ────────────────────────────────────────────────────────────

(define (outline-activation config)
  (let ((data-dir (outline-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        ;; The container's nodejs user runs as UID/GID 1001.  Chown the data
        ;; directory so it can write attachments and state files.
        (mkdir-p #$data-dir)
        (chown #$data-dir 1001 1001))))

;;; ── Firewall ──────────────────────────────────────────────────────────────

(define (outline-firewall-rules config)
  (nftables-rules (input (list (string-append "tcp dport "
                                              (number->string (outline-configuration-port
                                                               config))
                                              " accept comment \"outline\"")))))

;;; ── Service type ──────────────────────────────────────────────────────────

(define-public outline-service-type
  (service-type (name 'outline)
                (description "Outline knowledge base server (OCI container).")
                (extensions (list (service-extension oci-service-type
                                                     outline->oci-extension)
                                  (service-extension account-service-type
                                                     outline-accounts)
                                  (service-extension activation-service-type
                                                     outline-activation)
                                  (service-extension firewall-service-type
                                                     outline-firewall-rules)))
                (default-value (outline-configuration (url
                                                       "http://localhost:3000")))))
