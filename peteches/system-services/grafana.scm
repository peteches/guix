;; peteches/system-services/grafana.scm — Guix service type for Grafana.

(define-module (peteches system-services grafana)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (srfi srfi-1)
  #:use-module (peteches packages grafana)
  #:use-module (peteches system-services firewall)
  #:export (grafana-datasource
            grafana-datasource?
            grafana-dashboard
            grafana-dashboard?
            grafana-configuration
            grafana-configuration?
            grafana-service-type))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <grafana-datasource>
  grafana-datasource make-grafana-datasource
  grafana-datasource?
  (name        grafana-datasource-name)
  (type        grafana-datasource-type)
  (url         grafana-datasource-url)
  (uid         grafana-datasource-uid         (default ""))
  (access      grafana-datasource-access      (default "proxy"))
  (is-default? grafana-datasource-is-default? (default #f))
  (org-id      grafana-datasource-org-id      (default 1)))

(define-record-type* <grafana-dashboard>
  grafana-dashboard make-grafana-dashboard
  grafana-dashboard?
  ;; Name is used as the installed filename: /etc/grafana/dashboards/<name>.json
  (name      grafana-dashboard-name)
  ;; Any file-like object: (local-file "..."), (plain-file "..." "..."), etc.
  (json-file grafana-dashboard-json-file))

(define-record-type* <grafana-configuration>
  grafana-configuration make-grafana-configuration
  grafana-configuration?
  (package        grafana-configuration-package        (default grafana))
  (http-addr      grafana-configuration-http-addr      (default "0.0.0.0"))
  (http-port      grafana-configuration-http-port      (default 3000))
  (admin-user     grafana-configuration-admin-user     (default "admin"))
  (admin-password grafana-configuration-admin-password (default "admin"))
  (data-path      grafana-configuration-data-path      (default "/var/lib/grafana"))
  (log-path       grafana-configuration-log-path       (default "/var/log/grafana"))
  (datasources    grafana-configuration-datasources    (default '()))
  (dashboards     grafana-configuration-dashboards     (default '()))
  ;; Extra INI sections: alist of (section-name . ((key . value) ...))
  (extra-ini      grafana-configuration-extra-ini      (default '())))

;;; ── INI / YAML rendering ─────────────────────────────────────────────────
;;
;; All rendering is pure Scheme → string at Guix evaluation time.
;; No gexps here.

(define (render-ini-section name pairs)
  (string-append
   "[" name "]\n"
   (apply string-append
          (map (lambda (kv) (string-append (car kv) " = " (cdr kv) "\n"))
               pairs))
   "\n"))

(define (render-grafana-ini config)
  (string-append
   (render-ini-section "server"
     `(("http_addr" . ,(grafana-configuration-http-addr config))
       ("http_port" . ,(number->string (grafana-configuration-http-port config)))))
   (render-ini-section "paths"
     `(("data"         . ,(grafana-configuration-data-path config))
       ("logs"         . ,(grafana-configuration-log-path config))
       ("plugins"      . ,(string-append (grafana-configuration-data-path config) "/plugins"))
       ("provisioning" . "/etc/grafana/provisioning")))
   (render-ini-section "security"
     `(("admin_user"     . ,(grafana-configuration-admin-user config))
       ("admin_password" . ,(grafana-configuration-admin-password config))))
   (apply string-append
          (map (lambda (section)
                 (render-ini-section (car section) (cdr section)))
               (grafana-configuration-extra-ini config)))))

(define (render-datasources-yaml config)
  (let ((ds (grafana-configuration-datasources config)))
    (string-append
     "apiVersion: 1\n"
     (if (null? ds)
         ""
         (string-append
          "datasources:\n"
          (apply string-append
                 (map (lambda (d)
                        (string-append
                         "  - name: "     (grafana-datasource-name d)   "\n"
                         "    type: "     (grafana-datasource-type d)   "\n"
                         (let ((uid (grafana-datasource-uid d)))
                           (if (string-null? uid)
                               ""
                               (string-append "    uid: " uid "\n")))
                         "    access: "   (grafana-datasource-access d) "\n"
                         "    url: "      (grafana-datasource-url d)    "\n"
                         "    orgId: "    (number->string (grafana-datasource-org-id d)) "\n"
                         "    isDefault: " (if (grafana-datasource-is-default? d)
                                               "true" "false") "\n"
                         "    editable: true\n"))
                      ds)))))))

(define (render-dashboard-provider-yaml)
  (string-append
   "apiVersion: 1\n"
   "providers:\n"
   "  - name: default\n"
   "    orgId: 1\n"
   "    type: file\n"
   "    disableDeletion: false\n"
   "    updateIntervalSeconds: 10\n"
   "    options:\n"
   "      path: /etc/grafana/dashboards\n"))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (grafana-accounts config)
  (list
   (user-group
    (name "grafana")
    (system? #t))
   (user-account
    (name "grafana")
    (group "grafana")
    (system? #t)
    (comment "Grafana dashboard daemon")
    (home-directory (grafana-configuration-data-path config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (grafana-activation config)
  (let ((data-path (grafana-configuration-data-path config))
        (log-path  (grafana-configuration-log-path config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "grafana"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-path)
          (chown #$data-path uid gid)
          (mkdir-p #$log-path)
          (chown #$log-path uid gid)))))

(define (grafana-etc-files config)
  (append
   (list
    `("grafana/grafana.ini"
      ,(plain-file "grafana.ini" (render-grafana-ini config)))
    `("grafana/provisioning/datasources/datasources.yaml"
      ,(plain-file "datasources.yaml" (render-datasources-yaml config)))
    `("grafana/provisioning/dashboards/default.yaml"
      ,(plain-file "dashboards.yaml" (render-dashboard-provider-yaml))))
   (map (lambda (d)
          `(,(string-append "grafana/dashboards/" (grafana-dashboard-name d) ".json")
            ,(grafana-dashboard-json-file d)))
        (grafana-configuration-dashboards config))))

(define (grafana-shepherd-service config)
  (let* ((pkg      (grafana-configuration-package config))
         (log-file (string-append (grafana-configuration-log-path config) "/grafana.log")))
    (list
     (shepherd-service
      (provision '(grafana))
      (documentation "Grafana analytics and monitoring dashboard server.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/grafana")
                      "server"
                      "--config=/etc/grafana/grafana.ini"
                      (string-append "--homepath=" #$pkg))
                #:user "grafana"
                #:group "grafana"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (grafana-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (grafana-configuration-http-port config))
                 " accept comment \"grafana\"")))))

(define (grafana-profile config)
  (list (grafana-configuration-package config)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public grafana-service-type
  (service-type
   (name 'grafana)
   (description "Grafana analytics and monitoring dashboard server.")
   (extensions
    (list
     (service-extension account-service-type       grafana-accounts)
     (service-extension activation-service-type    grafana-activation)
     (service-extension etc-service-type           grafana-etc-files)
     (service-extension shepherd-root-service-type grafana-shepherd-service)
     (service-extension firewall-service-type      grafana-firewall-rules)
     (service-extension profile-service-type       grafana-profile)))
   (default-value (grafana-configuration))))
