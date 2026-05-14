;; peteches/system-services/prometheus.scm — Guix service type for Prometheus.

(define-module (peteches system-services prometheus)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (srfi srfi-1)
  #:use-module (peteches packages prometheus)
  #:use-module (peteches system-services firewall)
  #:export (prometheus-global-config
            prometheus-global-config?
            prometheus-relabel-config
            prometheus-relabel-config?
            prometheus-static-config
            prometheus-static-config?
            prometheus-scrape-config
            prometheus-scrape-config?
            prometheus-configuration
            prometheus-configuration?
            render-prometheus-yaml
            prometheus-service-type))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <prometheus-global-config>
  prometheus-global-config make-prometheus-global-config
  prometheus-global-config?
  (scrape-interval     prometheus-global-config-scrape-interval     (default "15s"))
  (evaluation-interval prometheus-global-config-evaluation-interval (default "15s"))
  (scrape-timeout      prometheus-global-config-scrape-timeout      (default #f))
  (external-labels     prometheus-global-config-external-labels     (default '())))

(define-record-type* <prometheus-relabel-config>
  prometheus-relabel-config make-prometheus-relabel-config
  prometheus-relabel-config?
  (source-labels prometheus-relabel-config-source-labels (default '()))
  (separator     prometheus-relabel-config-separator     (default #f))
  (target-label  prometheus-relabel-config-target-label  (default #f))
  (regex         prometheus-relabel-config-regex         (default #f))
  (modulus       prometheus-relabel-config-modulus       (default #f))
  (replacement   prometheus-relabel-config-replacement   (default #f))
  (action        prometheus-relabel-config-action        (default #f)))

(define-record-type* <prometheus-static-config>
  prometheus-static-config make-prometheus-static-config
  prometheus-static-config?
  (targets prometheus-static-config-targets (default '()))
  (labels  prometheus-static-config-labels  (default '())))

(define-record-type* <prometheus-scrape-config>
  prometheus-scrape-config make-prometheus-scrape-config
  prometheus-scrape-config?
  (job-name               prometheus-scrape-config-job-name)
  (scrape-interval        prometheus-scrape-config-scrape-interval        (default #f))
  (scrape-timeout         prometheus-scrape-config-scrape-timeout         (default #f))
  (metrics-path           prometheus-scrape-config-metrics-path           (default #f))
  (scheme                 prometheus-scrape-config-scheme                 (default #f))
  (params                 prometheus-scrape-config-params                 (default '()))
  (static-configs         prometheus-scrape-config-static-configs         (default '()))
  (relabel-configs        prometheus-scrape-config-relabel-configs        (default '()))
  (metric-relabel-configs prometheus-scrape-config-metric-relabel-configs (default '())))

(define-record-type* <prometheus-configuration>
  prometheus-configuration make-prometheus-configuration
  prometheus-configuration?
  (package            prometheus-configuration-package            (default prometheus))
  (global             prometheus-configuration-global             (default (prometheus-global-config)))
  (rule-files         prometheus-configuration-rule-files         (default '()))
  (scrape-configs     prometheus-configuration-scrape-configs     (default '()))
  (web-listen-address prometheus-configuration-web-listen-address (default "0.0.0.0:9090"))
  (storage-path       prometheus-configuration-storage-path       (default "/var/lib/prometheus"))
  (log-file           prometheus-configuration-log-file           (default "/var/log/prometheus.log"))
  (extra-args         prometheus-configuration-extra-args         (default '())))

;;; ── YAML serialisation ───────────────────────────────────────────────────
;;
;; All rendering happens at Guix evaluation time (pure Scheme → string),
;; then passed to plain-file.  No gexps here.

(define (yaml-quote s)
  (string-append "\"" s "\""))

(define (yaml-block-seq items indent)
  "Render a list of strings as a YAML block sequence."
  (apply string-append
         (map (lambda (s) (string-append indent "- " (yaml-quote s) "\n"))
              items)))

(define (render-global g)
  (string-append
   "global:\n"
   "  scrape_interval: " (prometheus-global-config-scrape-interval g) "\n"
   "  evaluation_interval: " (prometheus-global-config-evaluation-interval g) "\n"
   (let ((t (prometheus-global-config-scrape-timeout g)))
     (if t (string-append "  scrape_timeout: " t "\n") ""))
   (let ((el (prometheus-global-config-external-labels g)))
     (if (null? el)
         ""
         (string-append
          "  external_labels:\n"
          (apply string-append
                 (map (lambda (kv)
                        (string-append "    " (car kv) ": " (yaml-quote (cdr kv)) "\n"))
                      el)))))))

(define (render-params params)
  "Render the params block within a scrape job.
params is an alist of (key . (val ...)) pairs."
  (if (null? params)
      ""
      (string-append
       "    params:\n"
       (apply string-append
              (map (lambda (kv)
                     (string-append
                      "      " (car kv) ":\n"
                      (yaml-block-seq (cdr kv) "        ")))
                   params)))))

(define (render-static-config-item sc)
  "Render one static_config list item (with its '      - ' marker)."
  (string-append
   "      - targets:\n"
   (yaml-block-seq (prometheus-static-config-targets sc) "          ")
   (let ((lbls (prometheus-static-config-labels sc)))
     (if (null? lbls)
         ""
         (string-append
          "        labels:\n"
          (apply string-append
                 (map (lambda (kv)
                        (string-append "          " (car kv) ": "
                                       (yaml-quote (cdr kv)) "\n"))
                      lbls)))))))

(define (render-relabel-config-item rc)
  "Render one relabel_config list item (with its '      - ' marker).
The first present field gets '      - ' prefix; subsequent get '        '."
  (let ((src (prometheus-relabel-config-source-labels rc))
        (sep (prometheus-relabel-config-separator rc))
        (tgt (prometheus-relabel-config-target-label rc))
        (rex (prometheus-relabel-config-regex rc))
        (mod (prometheus-relabel-config-modulus rc))
        (rep (prometheus-relabel-config-replacement rc))
        (act (prometheus-relabel-config-action rc)))
    (let* ((fields
            (filter (lambda (x) x)
                    (list
                     (and (not (null? src))
                          (lambda (m)
                            (string-append m "source_labels:\n"
                                           (yaml-block-seq src "          "))))
                     (and sep (lambda (m) (string-append m "separator: "    (yaml-quote sep) "\n")))
                     (and tgt (lambda (m) (string-append m "target_label: " (yaml-quote tgt) "\n")))
                     (and rex (lambda (m) (string-append m "regex: "        (yaml-quote rex) "\n")))
                     (and mod (lambda (m) (string-append m "modulus: "      (number->string mod) "\n")))
                     (and rep (lambda (m) (string-append m "replacement: "  (yaml-quote rep) "\n")))
                     (and act (lambda (m) (string-append m "action: "       act "\n"))))))
           (first-fn (and (not (null? fields)) (car fields)))
           (rest-fns (if (null? fields) '() (cdr fields))))
      (if (not first-fn)
          ""
          (string-append
           (first-fn "      - ")
           (apply string-append (map (lambda (fn) (fn "        ")) rest-fns)))))))

(define (render-relabel-list key rcs)
  "Render a named relabel list block (relabel_configs or metric_relabel_configs)."
  (if (null? rcs)
      ""
      (string-append
       "    " key ":\n"
       (apply string-append (map render-relabel-config-item rcs)))))

(define (render-scrape-config job)
  "Render one scrape_configs list item (with its '  - ' marker)."
  (string-append
   "  - job_name: " (yaml-quote (prometheus-scrape-config-job-name job)) "\n"
   (let ((si (prometheus-scrape-config-scrape-interval job)))
     (if si (string-append "    scrape_interval: " si "\n") ""))
   (let ((st (prometheus-scrape-config-scrape-timeout job)))
     (if st (string-append "    scrape_timeout: " st "\n") ""))
   (let ((mp (prometheus-scrape-config-metrics-path job)))
     (if mp (string-append "    metrics_path: " (yaml-quote mp) "\n") ""))
   (let ((sc (prometheus-scrape-config-scheme job)))
     (if sc (string-append "    scheme: " sc "\n") ""))
   (render-params (prometheus-scrape-config-params job))
   (let ((scs (prometheus-scrape-config-static-configs job)))
     (if (null? scs)
         ""
         (string-append
          "    static_configs:\n"
          (apply string-append (map render-static-config-item scs)))))
   (render-relabel-list "relabel_configs"
                        (prometheus-scrape-config-relabel-configs job))
   (render-relabel-list "metric_relabel_configs"
                        (prometheus-scrape-config-metric-relabel-configs job))))

(define (render-prometheus-yaml config)
  "Render a complete prometheus.yml string from a prometheus-configuration record."
  (string-append
   (render-global (prometheus-configuration-global config))
   "\n"
   (let ((rf (prometheus-configuration-rule-files config)))
     (if (null? rf)
         ""
         (string-append "rule_files:\n" (yaml-block-seq rf "  ") "\n")))
   (let ((jobs (prometheus-configuration-scrape-configs config)))
     (if (null? jobs)
         ""
         (string-append
          "scrape_configs:\n"
          (apply string-append (map render-scrape-config jobs)))))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (web-address->port addr)
  "Extract the port from a 'host:port' or '[::]:port' string."
  (let loop ((i (- (string-length addr) 1)))
    (cond
     ((< i 0) "9090")
     ((char=? (string-ref addr i) #\:) (substring addr (+ i 1)))
     (else (loop (- i 1))))))

(define (prometheus-accounts config)
  (list
   (user-group
    (name "prometheus")
    (system? #t))
   (user-account
    (name "prometheus")
    (group "prometheus")
    (system? #t)
    (comment "Prometheus monitoring daemon")
    (home-directory (prometheus-configuration-storage-path config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (prometheus-activation config)
  (let ((storage-path (prometheus-configuration-storage-path config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "prometheus"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$storage-path)
          (chown #$storage-path uid gid))
        (when (file-exists? "/run/shepherd/socket")
          (system* #$(file-append shepherd "/bin/herd")
                   "restart" "prometheus")))))

(define (prometheus-etc-files config)
  (list `("prometheus/prometheus.yml"
          ,(plain-file "prometheus.yml"
                       (render-prometheus-yaml config)))))

(define (prometheus-shepherd-service config)
  (let* ((pkg          (prometheus-configuration-package config))
         (storage-path (prometheus-configuration-storage-path config))
         (listen-addr  (prometheus-configuration-web-listen-address config))
         (log-file     (prometheus-configuration-log-file config))
         (extra-args   (prometheus-configuration-extra-args config)))
    (list
     (shepherd-service
      (provision '(prometheus))
      (documentation "Prometheus monitoring server")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (append
                 (list #$(file-append pkg "/bin/prometheus")
                       "--config.file=/etc/prometheus/prometheus.yml"
                       (string-append "--storage.tsdb.path=" #$storage-path)
                       (string-append "--web.listen-address=" #$listen-addr))
                 '#$extra-args)
                #:user "prometheus"
                #:group "prometheus"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (prometheus-firewall-rules config)
  (let ((port (web-address->port
               (prometheus-configuration-web-listen-address config))))
    (nftables-rules
     (input (list (string-append "tcp dport " port
                                 " accept comment \"prometheus\""))))))

(define (prometheus-profile config)
  (list (prometheus-configuration-package config)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public prometheus-service-type
  (service-type
   (name 'prometheus)
   (description "Prometheus monitoring server.")
   (extensions
    (list
     (service-extension account-service-type      prometheus-accounts)
     (service-extension activation-service-type   prometheus-activation)
     (service-extension etc-service-type          prometheus-etc-files)
     (service-extension shepherd-root-service-type prometheus-shepherd-service)
     (service-extension firewall-service-type     prometheus-firewall-rules)
     (service-extension profile-service-type      prometheus-profile)))
   (default-value (prometheus-configuration))))
