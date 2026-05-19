;; peteches/system-services/loki.scm — Guix service type for Grafana Loki.

(define-module (peteches system-services loki)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (srfi srfi-1)
  #:use-module (peteches packages loki)
  #:use-module (peteches system-services firewall)
  #:export (loki-schema-config-entry
            loki-schema-config-entry?
            loki-limits-config
            loki-limits-config?
            loki-configuration
            loki-configuration?
            render-loki-yaml
            loki-service-type))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <loki-schema-config-entry>
  loki-schema-config-entry make-loki-schema-config-entry
  loki-schema-config-entry?
  ;; Start date for this schema period (string "YYYY-MM-DD").
  (from          loki-schema-config-entry-from)
  (store         loki-schema-config-entry-store         (default "tsdb"))
  (object-store  loki-schema-config-entry-object-store  (default "filesystem"))
  (schema        loki-schema-config-entry-schema        (default "v13"))
  (index-prefix  loki-schema-config-entry-index-prefix  (default "loki_index_"))
  (index-period  loki-schema-config-entry-index-period  (default "24h")))

(define-record-type* <loki-limits-config>
  loki-limits-config make-loki-limits-config
  loki-limits-config?
  (ingestion-rate-mb             loki-limits-config-ingestion-rate-mb             (default 8))
  (ingestion-burst-size-mb       loki-limits-config-ingestion-burst-size-mb       (default 16))
  (max-cache-freshness-per-query loki-limits-config-max-cache-freshness-per-query (default "10m"))
  (reject-old-samples?           loki-limits-config-reject-old-samples?           (default #t))
  (reject-old-samples-max-age    loki-limits-config-reject-old-samples-max-age    (default "168h"))
  (split-queries-by-interval     loki-limits-config-split-queries-by-interval     (default "15m")))

(define-record-type* <loki-configuration>
  loki-configuration make-loki-configuration
  loki-configuration?
  (package              loki-configuration-package              (default loki))
  (http-listen-address  loki-configuration-http-listen-address  (default "0.0.0.0"))
  (http-listen-port     loki-configuration-http-listen-port     (default 3100))
  (grpc-listen-port     loki-configuration-grpc-listen-port     (default 9096))
  (storage-path         loki-configuration-storage-path         (default "/var/lib/loki"))
  (log-file             loki-configuration-log-file             (default "/var/log/loki.log"))
  (auth-enabled?        loki-configuration-auth-enabled?        (default #f))
  ;; List of <loki-schema-config-entry> records.
  (schema-configs       loki-configuration-schema-configs
                        (default (list (loki-schema-config-entry (from "2020-10-24")))))
  (replication-factor   loki-configuration-replication-factor   (default 1))
  ;; kvstore backend for the ring: "inmemory", "consul", "etcd", "memberlist".
  (ring-kvstore         loki-configuration-ring-kvstore         (default "inmemory"))
  ;; Optional <loki-limits-config> record; #f uses Loki built-in defaults.
  (limits-config        loki-configuration-limits-config        (default #f))
  (analytics-reporting? loki-configuration-analytics-reporting? (default #f))
  (retention-period     loki-configuration-retention-period     (default "720h"))
  (extra-args           loki-configuration-extra-args           (default '())))

;;; ── YAML rendering ───────────────────────────────────────────────────────
;;
;; All rendering is pure Scheme → string at Guix evaluation time.
;; No gexps here.

(define (render-schema-config-entry e)
  (string-append
   "    - from: "         (loki-schema-config-entry-from e)         "\n"
   "      store: "        (loki-schema-config-entry-store e)        "\n"
   "      object_store: " (loki-schema-config-entry-object-store e) "\n"
   "      schema: "       (loki-schema-config-entry-schema e)       "\n"
   "      index:\n"
   "        prefix: "     (loki-schema-config-entry-index-prefix e) "\n"
   "        period: "     (loki-schema-config-entry-index-period e) "\n"))


(define (render-loki-yaml config)
  "Render a complete loki.yaml string from a <loki-configuration> record."
  (let* ((storage   (loki-configuration-storage-path config))
         (lc        (loki-configuration-limits-config config))
         (retention (loki-configuration-retention-period config)))
    (string-append
     "auth_enabled: "
     (if (loki-configuration-auth-enabled? config) "true" "false") "\n"
     "\n"
     "server:\n"
     "  http_listen_port: "
     (number->string (loki-configuration-http-listen-port config)) "\n"
     "  http_listen_address: "
     (loki-configuration-http-listen-address config) "\n"
     "  grpc_listen_port: "
     (number->string (loki-configuration-grpc-listen-port config)) "\n"
     "\n"
     "common:\n"
     "  instance_addr: 127.0.0.1\n"
     "  path_prefix: " storage "\n"
     "  storage:\n"
     "    filesystem:\n"
     "      chunks_directory: " storage "/chunks\n"
     "      rules_directory: "  storage "/rules\n"
     "  replication_factor: "
     (number->string (loki-configuration-replication-factor config)) "\n"
     "  ring:\n"
     "    kvstore:\n"
     "      store: " (loki-configuration-ring-kvstore config) "\n"
     "\n"
     "query_range:\n"
     "  results_cache:\n"
     "    cache:\n"
     "      embedded_cache:\n"
     "        enabled: true\n"
     "        max_size_mb: 100\n"
     "\n"
     "schema_config:\n"
     "  configs:\n"
     (apply string-append
            (map render-schema-config-entry
                 (loki-configuration-schema-configs config)))
     "\n"
     "compactor:\n"
     "  working_directory: " storage "/compactor\n"
     "  retention_enabled: true\n"
     "  delete_request_store: filesystem\n"
     "\n"
     "limits_config:\n"
     "  retention_period: " retention "\n"
     "  volume_enabled: true\n"
     (if lc
         (string-append
          "  ingestion_rate_mb: "
          (number->string (loki-limits-config-ingestion-rate-mb lc)) "\n"
          "  ingestion_burst_size_mb: "
          (number->string (loki-limits-config-ingestion-burst-size-mb lc)) "\n"
          "  max_cache_freshness_per_query: "
          (loki-limits-config-max-cache-freshness-per-query lc) "\n"
          "  reject_old_samples: "
          (if (loki-limits-config-reject-old-samples? lc) "true" "false") "\n"
          "  reject_old_samples_max_age: "
          (loki-limits-config-reject-old-samples-max-age lc) "\n"
          "  split_queries_by_interval: "
          (loki-limits-config-split-queries-by-interval lc) "\n")
         "")
     "\n"
     "analytics:\n"
     "  reporting_enabled: "
     (if (loki-configuration-analytics-reporting? config) "true" "false") "\n")))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (loki-accounts config)
  (list
   (user-group
    (name "loki")
    (system? #t))
   (user-account
    (name "loki")
    (group "loki")
    (system? #t)
    (comment "Grafana Loki log aggregation daemon")
    (home-directory (loki-configuration-storage-path config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (loki-activation config)
  (let ((storage-path (loki-configuration-storage-path config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "loki"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (d)
                      (mkdir-p d)
                      (chown d uid gid))
                    (list #$storage-path
                          (string-append #$storage-path "/chunks")
                          (string-append #$storage-path "/rules")
                          (string-append #$storage-path "/compactor"))))
        (system* #$(file-append shepherd "/bin/herd")
                 "restart" "loki"))))

(define (loki-etc-files config)
  (list `("loki/loki.yaml"
          ,(plain-file "loki.yaml" (render-loki-yaml config)))))

(define (loki-shepherd-service config)
  (let* ((pkg       (loki-configuration-package config))
         (log-file  (loki-configuration-log-file config))
         (extra-args (loki-configuration-extra-args config)))
    (list
     (shepherd-service
      (provision '(loki))
      (documentation "Grafana Loki log aggregation service.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (append
                 (list #$(file-append pkg "/bin/loki")
                       "--config.file=/etc/loki/loki.yaml")
                 '#$extra-args)
                #:user "loki"
                #:group "loki"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (loki-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (loki-configuration-http-listen-port config))
                 " accept comment \"loki\"")))))

(define (loki-profile config)
  (list (loki-configuration-package config)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public loki-service-type
  (service-type
   (name 'loki)
   (description "Grafana Loki log aggregation service.")
   (extensions
    (list
     (service-extension account-service-type       loki-accounts)
     (service-extension activation-service-type    loki-activation)
     (service-extension etc-service-type           loki-etc-files)
     (service-extension shepherd-root-service-type loki-shepherd-service)
     (service-extension firewall-service-type      loki-firewall-rules)
     (service-extension profile-service-type       loki-profile)))
   (default-value (loki-configuration))))
