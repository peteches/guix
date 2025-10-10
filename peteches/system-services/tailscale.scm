;; please create a service-type for tailscale:
;; it should:
;;   install tailscale
;;   create a shepherd service
;; follow direction for use from https://tailscale.com/kb/1053/install-static
(define-module (peteches system-services tailscale)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages networking)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (peteches packages tailscale)
  #:use-module (ice-9 match))

(define-record-type* <tailscale-configuration>
  tailscale-configuration make-tailscale-configuration
  tailscale-configuration?
  (package tailscale-configuration-package
           (default tailscale))
  (log-file tailscale-configuration-log-file
            (default "/var/log/tailscaled.log")))

(define tailscale-shepherd-service
  (match-lambda
    (($ <tailscale-configuration> package log-file)
     (list (shepherd-service
            (provision '(tailscaled))
            (documentation "Tailscale VPN daemon")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append package "/bin/tailscaled")
                            "--state=/var/lib/tailscale/tailscaled.state"
                            "--socket=/run/tailscale/tailscaled.sock")
                      #:log-file #$log-file
                      #:environment-variables
                      (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
            (stop #~(make-kill-destructor)))))))

(define tailscale-activation
  (match-lambda
    (($ <tailscale-configuration> package log-file)
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p "/var/lib/tailscale")
         (mkdir-p "/run/tailscale")
         (mkdir-p "/var/log")))))

(define-public tailscale-service-type
  (service-type
   (name 'tailscale)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-shepherd-service)
          (service-extension activation-service-type
                             tailscale-activation)
          (service-extension profile-service-type
                             (compose list tailscale-configuration-package))))
   (default-value (tailscale-configuration))
   (description "Run the Tailscale VPN daemon.")))
