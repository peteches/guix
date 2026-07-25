(define-module (peteches services vault)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base) ;etc-service-type
  #:use-module (peteches packages vault)
  #:use-module (peteches services firewall)
  #:export (vault-listener vault-listener?
                           vault-listener-address
                           vault-listener-tls-cert-file
                           vault-listener-tls-key-file
                           vault-listener-tls-disable
                           vault-configuration
                           vault-configuration?
                           vault-configuration-package
                           vault-configuration-data-dir
                           vault-configuration-listener
                           vault-configuration-api-addr
                           vault-configuration-cluster-addr
                           vault-configuration-ui
                           vault-configuration-log-level
                           vault-configuration-log-file
                           render-vault-hcl
                           vault-service-type))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <vault-listener> vault-listener make-vault-listener
  vault-listener?
  (address vault-listener-address
           (default "127.0.0.1:8200"))
  (tls-cert-file vault-listener-tls-cert-file
                 (default #f))
  (tls-key-file vault-listener-tls-key-file
                (default #f))
  (tls-disable vault-listener-tls-disable
               (default #t)))

(define-record-type* <vault-configuration> vault-configuration
                     make-vault-configuration
  vault-configuration?
  (package
    vault-configuration-package
    (default vault))
  (data-dir vault-configuration-data-dir
            (default "/var/lib/vault"))
  (listener vault-configuration-listener
            (default (vault-listener)))
  (api-addr vault-configuration-api-addr
            (default "http://127.0.0.1:8200"))
  (cluster-addr vault-configuration-cluster-addr
                (default "https://127.0.0.1:8201"))
  (ui vault-configuration-ui
      (default #t))
  (log-level vault-configuration-log-level
             (default "info"))
  (log-file vault-configuration-log-file
            (default "/var/log/vault.log")))

;;; ── HCL config renderer ──────────────────────────────────────────────────
;;
;; Pure Scheme, evaluated at Guix eval time.  No gexps here.

(define (render-vault-hcl config)
  "Return a Vault HCL config string for CONFIG."
  (let* ((data-dir (vault-configuration-data-dir config))
         (listener (vault-configuration-listener config))
         (address (vault-listener-address listener))
         (tls-disable (vault-listener-tls-disable listener))
         (tls-cert (vault-listener-tls-cert-file listener))
         (tls-key (vault-listener-tls-key-file listener))
         (api-addr (vault-configuration-api-addr config))
         (cluster-addr (vault-configuration-cluster-addr config))
         (ui (vault-configuration-ui config))
         (log-level (vault-configuration-log-level config)))
    (string-append "storage \"file\" {\n"
                   "  path = \""
                   data-dir
                   "/data\"\n"
                   "}\n\n"
                   "listener \"tcp\" {\n"
                   "  address = \""
                   address
                   "\"\n"
                   (if tls-disable "  tls_disable = \"true\"\n"
                       (string-append (if tls-cert
                                          (string-append
                                           "  tls_cert_file = \"" tls-cert
                                           "\"\n") "")
                                      (if tls-key
                                          (string-append
                                           "  tls_key_file  = \"" tls-key
                                           "\"\n") "")))
                   "}\n\n"
                   "api_addr     = \""
                   api-addr
                   "\"\n"
                   "cluster_addr = \""
                   cluster-addr
                   "\"\n"
                   "ui           = "
                   (if ui "true" "false")
                   "\n"
                   "log_level    = \""
                   log-level
                   "\"\n")))

;;; ── Service extension helpers ─────────────────────────────────────────────

(define (vault-address->port addr)
  (let loop
    ((i (- (string-length addr) 1)))
    (cond
      ((< i 0)
       "8200")
      ((char=? (string-ref addr i) #\:)
       (substring addr
                  (+ i 1)))
      (else (loop (- i 1))))))

(define (vault-firewall-rules config)
  (let ((port (vault-address->port (vault-listener-address (vault-configuration-listener
                                                            config)))))
    (nftables-rules (input (list (string-append "tcp dport " port
                                                " accept comment \"vault\""))))))

(define (vault-activation config)
  (let ((data-dir (vault-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p (string-append #$data-dir "/data"))
        (chmod #$data-dir #o700)
        (mkdir-p "/var/log"))))

(define (vault-etc-files config)
  (list `("vault/vault.hcl" ,(plain-file "vault.hcl"
                                         (render-vault-hcl config)))))

(define (vault-shepherd-service config)
  (let* ((pkg (vault-configuration-package config))
         (log-file (vault-configuration-log-file config)))
    (list (shepherd-service (provision '(vault))
                            (documentation "HashiCorp Vault secrets manager.")
                            (requirement '(networking file-systems))
                            (start #~(make-forkexec-constructor (list #$(file-append
                                                                         pkg
                                                                         "/bin/vault")
                                                                 "server"
                                                                 "-config=/etc/vault/vault.hcl")
                                                                #:log-file #$log-file
                                                                #:environment-variables
                                                                (list
                                                                 "HOME=/var/lib/vault"
                                                                 "PATH=/run/current-system/profile/bin")))
                            (stop #~(make-kill-destructor))))))

(define (vault-profile config)
  (list (vault-configuration-package config)))

;;; ── Service type ──────────────────────────────────────────────────────────

(define-public vault-service-type
  (service-type (name 'vault)
                (description "HashiCorp Vault secrets management daemon.")
                (extensions (list (service-extension activation-service-type
                                                     vault-activation)
                                  (service-extension etc-service-type
                                                     vault-etc-files)
                                  (service-extension
                                   shepherd-root-service-type
                                   vault-shepherd-service)
                                  (service-extension profile-service-type
                                                     vault-profile)
                                  (service-extension firewall-service-type
                                                     vault-firewall-rules)))
                (default-value (vault-configuration))))
