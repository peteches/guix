;; peteches/system-services/caddy.scm — Guix service type for Caddy web server.
;;
;; Generates a Caddy JSON config from Guile record types and runs Caddy via
;; Shepherd.  TLS certificates are obtained via the deSEC DNS-01 challenge;
;; the API token is read at start-up from a SOPS-managed secrets file.

(define-module (peteches system-services caddy)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (peteches packages caddy)
  #:use-module (peteches system-services firewall)
  #:export (caddy-reverse-proxy
            caddy-reverse-proxy?
            caddy-reverse-proxy-domain
            caddy-reverse-proxy-upstream
            caddy-configuration
            caddy-configuration?
            render-caddy-json
            caddy-service-type))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <caddy-reverse-proxy>
  caddy-reverse-proxy make-caddy-reverse-proxy
  caddy-reverse-proxy?
  (domain   caddy-reverse-proxy-domain)    ; "prometheus.peteches.co.uk"
  (upstream caddy-reverse-proxy-upstream)) ; "192.168.51.187:9090"

(define-record-type* <caddy-configuration>
  caddy-configuration make-caddy-configuration
  caddy-configuration?
  (package          caddy-configuration-package          (default caddy))
  (email            caddy-configuration-email            (default "pete@peteches.co.uk"))
  (desec-token-file caddy-configuration-desec-token-file (default "/run/secrets/desec-token"))
  (virtual-hosts    caddy-configuration-virtual-hosts    (default '()))
  (data-path        caddy-configuration-data-path        (default "/var/lib/caddy"))
  (log-file         caddy-configuration-log-file         (default "/var/log/caddy.log"))
  (http-port        caddy-configuration-http-port        (default 80))
  (https-port       caddy-configuration-https-port       (default 443)))

;;; ── JSON config rendering ─────────────────────────────────────────────────
;;
;; Pure Scheme → string at Guix evaluation time.  No gexps here.

(define (jq s)
  "Return a JSON-quoted string literal."
  (string-append "\"" s "\""))

(define (render-route vhost)
  "Render one Caddy JSON route object for a <caddy-reverse-proxy>."
  (let ((domain   (caddy-reverse-proxy-domain vhost))
        (upstream (caddy-reverse-proxy-upstream vhost)))
    (string-append
     "        {\n"
     "          \"match\": [{\"host\": [" (jq domain) "]}],\n"
     "          \"handle\": [{\n"
     "            \"handler\": \"reverse_proxy\",\n"
     "            \"upstreams\": [{\"dial\": " (jq upstream) "}]\n"
     "          }]\n"
     "        }")))

(define (render-caddy-json config)
  "Render a complete Caddy JSON config string from a <caddy-configuration>."
  (let* ((email            (caddy-configuration-email config))
         (data-path        (caddy-configuration-data-path config))
         (log-file         (caddy-configuration-log-file config))
         (http-port        (number->string (caddy-configuration-http-port config)))
         (https-port       (number->string (caddy-configuration-https-port config)))
         (virtual-hosts    (caddy-configuration-virtual-hosts config))
         (domains          (map caddy-reverse-proxy-domain virtual-hosts))
         (domain-list      (string-join (map jq domains) ", "))
         (routes           (map render-route virtual-hosts))
         (routes-str       (string-join routes ",\n")))
    (string-append
     "{\n"
     "  \"admin\": {\"disabled\": false, \"listen\": \"localhost:2019\"},\n"
     "  \"logging\": {\n"
     "    \"logs\": {\n"
     "      \"default\": {\n"
     "        \"writer\": {\"output\": \"file\", \"filename\": " (jq log-file) "}\n"
     "      }\n"
     "    }\n"
     "  },\n"
     "  \"storage\": {\n"
     "    \"module\": \"file_system\",\n"
     "    \"root\": " (jq data-path) "\n"
     "  },\n"
     "  \"apps\": {\n"
     "    \"tls\": {\n"
     "      \"automation\": {\n"
     "        \"policies\": [{\n"
     "          \"subjects\": [" domain-list "],\n"
     "          \"issuers\": [{\n"
     "            \"module\": \"acme\",\n"
     "            \"email\": " (jq email) ",\n"
     "            \"challenges\": {\n"
     "              \"dns\": {\n"
     "                \"provider\": {\n"
     "                  \"name\": \"desec\",\n"
     "                  \"token\": \"{env.DESEC_TOKEN}\"\n"
     "                }\n"
     "              }\n"
     "            }\n"
     "          }]\n"
     "        }]\n"
     "      }\n"
     "    },\n"
     "    \"http\": {\n"
     "      \"servers\": {\n"
     "        \"main\": {\n"
     "          \"listen\": [\":" https-port "\"],\n"
     "          \"tls_connection_policies\": [{}],\n"
     "          \"routes\": [\n"
     routes-str "\n"
     "          ]\n"
     "        },\n"
     "        \"redirect\": {\n"
     "          \"listen\": [\":" http-port "\"],\n"
     "          \"routes\": [{\n"
     "            \"handle\": [{\n"
     "              \"handler\": \"static_response\",\n"
     "              \"status_code\": 301,\n"
     "              \"headers\": {\n"
     "                \"Location\": [\"https://{http.request.host}{http.request.uri}\"]\n"
     "              }\n"
     "            }]\n"
     "          }]\n"
     "        }\n"
     "      }\n"
     "    }\n"
     "  }\n"
     "}\n")))

;;; ── Service extension helpers ─────────────────────────────────────────────

(define (caddy-activation config)
  (let ((data-path (caddy-configuration-data-path config)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$data-path)
        (chmod #$data-path #o700))))

(define (caddy-etc-files config)
  (list `("caddy/caddy.json"
          ,(plain-file "caddy.json" (render-caddy-json config)))))

(define (caddy-shepherd-service config)
  (let* ((pkg              (caddy-configuration-package config))
         (log-file         (caddy-configuration-log-file config))
         (desec-token-file (caddy-configuration-desec-token-file config)))
    (list
     (shepherd-service
      (provision '(caddy))
      (documentation "Caddy web server with automatic HTTPS via deSEC DNS-01.")
      (requirement '(networking file-systems sops-secrets))
      (start
       ;; Read the DESEC_TOKEN from the SOPS-decrypted secret file at start-up
       ;; and inject it into Caddy's environment via a shell subshell expansion.
       ;; The token never appears in process arguments or log output.
       #~(make-forkexec-constructor
          (list "/bin/sh" "-c"
                (string-append
                 "DESEC_TOKEN=$(cat " #$desec-token-file ") "
                 "exec " #$(file-append pkg "/bin/caddy")
                 " run --config /etc/caddy/caddy.json"))
          #:log-file #$log-file))
      (stop #~(make-kill-destructor))
      (actions
       (list
        (shepherd-action
         (name 'reload)
         (documentation "Reload Caddy config without dropping connections.")
         (procedure
          #~(lambda _
              (system* #$(file-append pkg "/bin/caddy")
                       "reload" "--config" "/etc/caddy/caddy.json")
              #t)))))))))

(define (caddy-firewall-rules config)
  (nftables-rules
   (input
    (list
     (string-append "tcp dport "
                    (number->string (caddy-configuration-http-port config))
                    " accept comment \"caddy-http\"")
     (string-append "tcp dport "
                    (number->string (caddy-configuration-https-port config))
                    " accept comment \"caddy-https\"")))))

(define (caddy-profile config)
  (list (caddy-configuration-package config)))

;;; ── Service type ──────────────────────────────────────────────────────────

(define-public caddy-service-type
  (service-type
   (name 'caddy)
   (description "Caddy web server with automatic HTTPS via deSEC DNS-01 challenge.")
   (extensions
    (list
     (service-extension activation-service-type    caddy-activation)
     (service-extension etc-service-type           caddy-etc-files)
     (service-extension shepherd-root-service-type caddy-shepherd-service)
     (service-extension firewall-service-type      caddy-firewall-rules)
     (service-extension profile-service-type       caddy-profile)))
   (default-value (caddy-configuration))))
