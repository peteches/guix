;; peteches/services/plane.scm — Guix service type for Plane project management.
;;
;; Runs Plane via official makeplane/* Docker images as Shepherd services
;; via Guix's oci-service-type.  Eight containers run on a custom Podman
;; network named "plane" so the proxy can route to the other services by
;; hostname.  All external services (PostgreSQL, Redis, RabbitMQ, S3/MinIO)
;; are assumed to be managed elsewhere; connection details are supplied via
;; the secrets-env-file field.
;;
;; IMPORTANT — network addressing:
;;   Containers on the "plane" network cannot reach the host via "localhost".
;;   For services that run on the host (postgres, redis, rabbitmq), use the
;;   Podman-provided alias "host.containers.internal" in your connection URLs:
;;     DATABASE_URL=postgresql://user:pass@host.containers.internal:5432/plane
;;     REDIS_URL=redis://host.containers.internal:6379
;;     AMQP_URL=amqp://user:pass@host.containers.internal:5672/vhost
;;
;; NOTE — Shepherd provision names for the five app containers ("api", "web",
;;   "space", "admin", "live") deliberately match the hostnames that the
;;   makeplane/plane-proxy Caddyfile routes to.  Podman uses the container
;;   name (= provision name) as the DNS hostname within the "plane" network.

(define-module (peteches services plane)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages containers)
  #:use-module (peteches services firewall)
  #:export (plane-configuration plane-configuration? plane-service-type))

;;; ── Configuration record ──────────────────────────────────────────────────

(define-record-type* <plane-configuration> plane-configuration
                     make-plane-configuration
  plane-configuration?

  ;; Docker image tag applied to all makeplane/* images.
  (image-tag plane-configuration-image-tag
             (default "stable"))

  ;; Publicly accessible URL, e.g. "https://plane.example.com".
  ;; Required — used for CORS, WEB_URL, and as the proxy's SITE_ADDRESS domain.
  (web-url plane-configuration-web-url)

  ;; Host port forwarded to the proxy container's internal port 80.
  (port plane-configuration-port
        (default 80))

  ;; Directory for per-container log files.
  (log-dir plane-configuration-log-dir
           (default "/var/log/plane"))

  ;; ── Secrets ───────────────────────────────────────────────────────────────
  ;; Path to a SOPS-decrypted env-file mounted read-only into all backend
  ;; containers (api, worker, beat, live) via --env-file.
  ;; Plane reads secrets directly from environment variables (no *_FILE
  ;; convention), so this file is the secure injection point.
  ;;
  ;; Required keys in the env-file:
  ;; SECRET_KEY              — Django secret key (openssl rand -hex 32)
  ;; DATABASE_URL            — postgresql://user:pass@host:5432/plane
  ;; REDIS_URL               — redis://[:password@]host:6379
  ;; AMQP_URL                — amqp://user:pass@host:5672/vhost
  ;; AWS_ACCESS_KEY_ID       — S3 or MinIO access key
  ;; AWS_SECRET_ACCESS_KEY   — S3 or MinIO secret key
  ;;
  ;; Optional in the env-file (if SMTP is enabled):
  ;; EMAIL_PASSWORD          — SMTP account password
  (secrets-env-file plane-configuration-secrets-env-file
                    (default "/run/secrets/plane.env"))

  ;; ── Backend tuning ────────────────────────────────────────────────────────
  ;; Number of Gunicorn worker processes in the API container.
  (gunicorn-workers plane-configuration-gunicorn-workers
                    (default 2))

  ;; ── S3 / MinIO storage ───────────────────────────────────────────────────
  ;; Endpoint URL for MinIO or any S3-compatible service.
  ;; Leave empty ("") to use AWS S3 (default AWS SDK endpoint resolution).
  (s3-endpoint-url plane-configuration-s3-endpoint-url
                   (default ""))

  ;; S3 bucket name for file uploads (also used by the proxy's bucket route).
  (s3-bucket-name plane-configuration-s3-bucket-name
                  (default "uploads"))

  ;; AWS region for S3 / MinIO.
  (s3-region plane-configuration-s3-region
             (default "us-east-1"))

  ;; Set to #t when pointing at MinIO instead of AWS S3.
  ;; Enables USE_MINIO=1 and sets MINIO_ENDPOINT_SSL=0.
  (use-minio? plane-configuration-use-minio?
              (default #f))

  ;; Maximum upload file size in bytes.  Enforced by both the backend and proxy.
  (file-size-limit plane-configuration-file-size-limit
                   (default 5242880)) ;5 MB
  
  ;; Signed URL expiration in seconds for file downloads.
  (signed-url-expiration plane-configuration-signed-url-expiration
                         (default 3600))

  ;; ── SMTP notifications (optional) ────────────────────────────────────────
  ;; Leave smtp-host as "" to disable email notifications entirely.
  ;; The SMTP password must be placed in secrets-env-file as EMAIL_PASSWORD=...
  (smtp-host plane-configuration-smtp-host
             (default ""))
  (smtp-port plane-configuration-smtp-port
             (default 587))
  (smtp-username plane-configuration-smtp-username
                 (default ""))
  (smtp-from-email plane-configuration-smtp-from-email
                   (default ""))
  ;; Whether to use TLS for SMTP.
  (smtp-secure plane-configuration-smtp-secure
               (default #f))

  ;; ── Escape hatch ─────────────────────────────────────────────────────────
  ;; Extra "KEY=value" strings appended to the environment of all backend
  ;; containers (api, worker, beat, live).  Use this for optional integrations
  ;; (Sentry, GitHub OAuth, SAML, etc.) not covered by the fields above.
  (extra-environment plane-configuration-extra-environment
                     (default '())))

;;; ── Private helpers ───────────────────────────────────────────────────────

(define (maybe-env key value)
  "Return (list \"KEY=VALUE\") when VALUE is a non-empty string, else '()."
  (if (and (string? value)
           (not (string-null? value)))
      (list (string-append key "=" value))
      '()))

(define (plane-image config suffix)
  "Return the full docker.io image reference for a Plane service named SUFFIX."
  (string-append "docker.io/makeplane/" suffix ":"
                 (plane-configuration-image-tag config)))

(define (plane-backend-env config)
  "Build the shared non-secret environment list for all backend containers.
Secrets (SECRET_KEY, DATABASE_URL, etc.) come from the secrets-env-file."
  (append
   ;; Core always-present settings
   (list "DEBUG=0"
         "DOCKERIZED=1"
         (string-append "WEB_URL="
                        (plane-configuration-web-url config))
         (string-append "CORS_ALLOWED_ORIGINS="
                        (plane-configuration-web-url config))
         (string-append "GUNICORN_WORKERS="
                        (number->string (plane-configuration-gunicorn-workers
                                         config)))
         (string-append "AWS_REGION="
                        (plane-configuration-s3-region config))
         (string-append "AWS_S3_BUCKET_NAME="
                        (plane-configuration-s3-bucket-name config))
         (string-append "FILE_SIZE_LIMIT="
                        (number->string (plane-configuration-file-size-limit
                                         config)))
         (string-append "SIGNED_URL_EXPIRATION="
                        (number->string (plane-configuration-signed-url-expiration
                                         config)))
         (if (plane-configuration-use-minio? config) "USE_MINIO=1"
             "USE_MINIO=0")
         (if (plane-configuration-use-minio? config) "MINIO_ENDPOINT_SSL=0"
             "MINIO_ENDPOINT_SSL=1"))
   ;; Optional S3/MinIO endpoint override
   (maybe-env "AWS_S3_ENDPOINT_URL"
              (plane-configuration-s3-endpoint-url config))
   ;; Optional SMTP
   (if (and (string? (plane-configuration-smtp-host config))
            (not (string-null? (plane-configuration-smtp-host config))))
       (append (list (string-append "EMAIL_HOST="
                                    (plane-configuration-smtp-host config))
                     (string-append "EMAIL_PORT="
                                    (number->string (plane-configuration-smtp-port
                                                     config))))
               (maybe-env "EMAIL_HOST_USER"
                          (plane-configuration-smtp-username config))
               (maybe-env "EMAIL_FROM"
                          (plane-configuration-smtp-from-email config))
               (list (if (plane-configuration-smtp-secure config)
                         "EMAIL_USE_TLS=1" "EMAIL_USE_TLS=0")))
       '())
   ;; User-supplied extras
   (plane-configuration-extra-environment config)))

(define (plane-backend-extra-args config)
  "Podman extra-arguments common to all backend containers."
  (list "--user=root"
        (string-append "--env-file="
                       (plane-configuration-secrets-env-file config))))

;;; ── OCI containers ────────────────────────────────────────────────────────
;;
;; Shepherd provision names are prefixed with "plane-" for all containers.
;; Each container also carries --network-alias=<unprefixed-name> so that
;; aardvark-dns keeps resolving the short hostnames that the makeplane/plane-proxy
;; Caddyfile hardcodes (e.g. "api:8000", "web:3000").

(define (plane->migrator-container config)
  ;; Runs Django migrations once and exits.  All backend containers that call
  ;; wait_for_migrations require this so Shepherd starts it before them.
  (oci-container-configuration (image (plane-image config "plane-backend"))
                               (provision "plane-migrator")
                               (requirement '(networking file-systems
                                                         sops-secrets))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config)
                                                        "/migrator.log"))
                               (network "plane")
                               (extra-arguments (append (plane-backend-extra-args
                                                         config)
                                                        '("--entrypoint"
                                                          "./bin/docker-entrypoint-migrator.sh")))
                               (environment (plane-backend-env config))))

(define (plane->api-container config)
  (oci-container-configuration (image (plane-image config "plane-backend"))
                               (provision "plane-api")
                               (requirement '(networking file-systems
                                                         sops-secrets
                                                         plane-migrator))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/api.log"))
                               (network "plane")
                               (extra-arguments (append (plane-backend-extra-args
                                                         config)
                                                        '("--network-alias=api")))
                               (environment (plane-backend-env config))))

(define (plane->worker-container config)
  ;; Same image as api; --entrypoint selects the worker startup script.
  ;; The image's default CMD ("./bin/docker-entrypoint-api.sh") is passed as
  ;; $1 but ignored by the worker script which does not use positional args.
  (oci-container-configuration (image (plane-image config "plane-backend"))
                               (provision "plane-worker")
                               (requirement '(networking file-systems
                                                         sops-secrets
                                                         plane-migrator
                                                         plane-api))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/worker.log"))
                               (network "plane")
                               (extra-arguments (append (plane-backend-extra-args
                                                         config)
                                                        '("--entrypoint"
                                                          "./bin/docker-entrypoint-worker.sh")))
                               (environment (plane-backend-env config))))

(define (plane->beat-container config)
  ;; Same image as api; --entrypoint selects the Celery beat startup script.
  (oci-container-configuration (image (plane-image config "plane-backend"))
                               (provision "plane-beat")
                               (requirement '(networking file-systems
                                                         sops-secrets
                                                         plane-migrator
                                                         plane-api))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/beat.log"))
                               (network "plane")
                               (extra-arguments (append (plane-backend-extra-args
                                                         config)
                                                        '("--entrypoint"
                                                          "./bin/docker-entrypoint-beat.sh")))
                               (environment (plane-backend-env config))))

(define (plane->web-container config)
  (oci-container-configuration (image (plane-image config "plane-frontend"))
                               (provision "plane-web")
                               (requirement '(networking file-systems
                                                         plane-api))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/web.log"))
                               (network "plane")
                               (extra-arguments '("--network-alias=web"))
                               (environment (list (string-append
                                                   "NEXT_PUBLIC_API_BASE_URL="
                                                   (plane-configuration-web-url
                                                    config))
                                                  (string-append "WEB_URL="
                                                                 (plane-configuration-web-url
                                                                  config))))))

(define (plane->admin-container config)
  (oci-container-configuration (image (plane-image config "plane-admin"))
                               (provision "plane-admin")
                               (requirement '(networking file-systems
                                                         plane-api))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/admin.log"))
                               (network "plane")
                               (extra-arguments '("--network-alias=admin"))
                               (environment (list (string-append
                                                   "NEXT_PUBLIC_API_BASE_URL="
                                                   (plane-configuration-web-url
                                                    config))
                                                  (string-append "WEB_URL="
                                                                 (plane-configuration-web-url
                                                                  config))))))

(define (plane->space-container config)
  (oci-container-configuration (image (plane-image config "plane-space"))
                               (provision "plane-space")
                               (requirement '(networking file-systems
                                                         plane-api))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/space.log"))
                               (network "plane")
                               (extra-arguments '("--network-alias=space"))
                               (environment (list (string-append
                                                   "NEXT_PUBLIC_API_BASE_URL="
                                                   (plane-configuration-web-url
                                                    config))
                                                  (string-append "WEB_URL="
                                                                 (plane-configuration-web-url
                                                                  config))))))

(define (plane->live-container config)
  ;; The live service needs the same secrets as the backend (SECRET_KEY, DB, Redis).
  ;; API_BASE_URL must point to the api container on the plane network.
  ;; LIVE_SERVER_SECRET_KEY must be present in the secrets env-file (same value as SECRET_KEY).
  (oci-container-configuration (image (plane-image config "plane-live"))
                               (provision "plane-live")
                               (requirement '(networking file-systems
                                                         sops-secrets
                                                         plane-api))
                               (log-file (string-append (plane-configuration-log-dir
                                                         config) "/live.log"))
                               (network "plane")
                               (extra-arguments (append (plane-backend-extra-args
                                                         config)
                                                        '("--network-alias=live")))
                               (environment (append (plane-backend-env config)
                                                    (list
                                                     "API_BASE_URL=http://api:8000")))))

(define (plane->proxy-container config)
  ;; The Caddy proxy is the only container with a host port binding.
  ;; It reads SITE_ADDRESS, BUCKET_NAME, and FILE_SIZE_LIMIT from its env.
  ;; Upstream hostnames ("api", "web", "space", "admin", "live") are
  ;; hardcoded in the makeplane/plane-proxy Caddyfile and resolve via the
  ;; "plane" Podman network DNS.
  (let ((port (plane-configuration-port config)))
    (oci-container-configuration (image (plane-image config "plane-proxy"))
                                 (provision "plane-proxy")
                                 (requirement '(networking file-systems
                                                           plane-api
                                                           plane-web
                                                           plane-admin
                                                           plane-space
                                                           plane-live))
                                 (log-file (string-append (plane-configuration-log-dir
                                                           config)
                                                          "/proxy.log"))
                                 (network "plane")
                                 (ports (list (string-append (number->string
                                                              port) ":80")))
                                 (environment (list "SITE_ADDRESS=:80"
                                               (string-append "BUCKET_NAME="
                                                              (plane-configuration-s3-bucket-name
                                                               config))
                                               (string-append
                                                "FILE_SIZE_LIMIT="
                                                (number->string (plane-configuration-file-size-limit
                                                                 config)))
                                               "TRUSTED_PROXIES=0.0.0.0/0")))))

;;; ── OCI extension ─────────────────────────────────────────────────────────

(define (plane->oci-extension config)
  (oci-extension (containers (list (plane->migrator-container config)
                                   (plane->api-container config)
                                   (plane->worker-container config)
                                   (plane->beat-container config)
                                   (plane->web-container config)
                                   (plane->admin-container config)
                                   (plane->space-container config)
                                   (plane->live-container config)
                                   (plane->proxy-container config)))))

;;; ── Accounts ──────────────────────────────────────────────────────────────

(define (plane-accounts config)
  (list (user-group
          (name "plane")
          (system? #t))
        (user-account
          (name "plane")
          (group "plane")
          (system? #t)
          (comment "Plane project management daemon")
          (home-directory "/var/lib/plane")
          (shell (file-append shadow "/sbin/nologin")))))

;;; ── Activation ────────────────────────────────────────────────────────────

(define (plane-activation config)
  (let ((log-dir (plane-configuration-log-dir config))
        (podman-bin (file-append podman "/bin/podman")))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$log-dir)
        ;; /etc/containers/ is a read-only Guix store symlink; Podman cannot
        ;; create a networks/ subdir inside it.  containers.conf redirects
        ;; network_config_dir to here, so the directory must exist first.
        (mkdir-p "/var/lib/containers/networks")
        ;; Create the inter-container Podman network.  --ignore makes this
        ;; idempotent: the command succeeds even if "plane" already exists.
        ;; podman-bin is a store path so it is available during activation
        ;; before /run/current-system/profile/bin is on PATH.
        ;; Fixed subnet so firewall rules can match a deterministic range.
        ;; --ignore is idempotent; if the network already exists with a
        ;; different subnet, remove it first: podman network rm plane.
        (system* #$podman-bin
                 "network"
                 "create"
                 "--ignore"
                 "--subnet"
                 "10.91.0.0/24"
                 "--gateway"
                 "10.91.0.1"
                 "plane"))))

;;; ── Firewall ──────────────────────────────────────────────────────────────

(define (plane-firewall-rules config)
  (nftables-rules (input (list (string-append "tcp dport "
                                              (number->string (plane-configuration-port
                                                               config))
                                              " accept comment \"plane\"")))))

;;; ── Service type ──────────────────────────────────────────────────────────

(define-public plane-service-type
  (service-type (name 'plane)
                (description
                 "Plane project management server (OCI containers).")
                (extensions (list (service-extension oci-service-type
                                                     plane->oci-extension)
                                  (service-extension account-service-type
                                                     plane-accounts)
                                  (service-extension activation-service-type
                                                     plane-activation)
                                  (service-extension firewall-service-type
                                                     plane-firewall-rules)))
                (default-value (plane-configuration (web-url
                                                     "http://localhost")))))
