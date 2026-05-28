;; peteches/system-services/cifs.scm — CIFS client support service.
;;
;; Wires up the kernel's hostname-resolution upcall chain so CIFS mounts
;; using hostnames (e.g. //nas.example.com/share) work without addr= pinning:
;;
;;   kernel CIFS module
;;     → /sbin/request-key  (keyutils)
;;       → /etc/request-key.conf
;;         → cifs.upcall %k  (cifs-utils)
;;           → DNS resolved, result returned to kernel via keyring

(define-module (peteches system-services cifs)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages samba)   ; cifs-utils
  #:use-module (gnu packages crypto)  ; keyutils
  #:export (cifs-client-service-type))

(define (cifs-etc-files _)
  (list `("request-key.conf"
          ,(mixed-text-file "request-key.conf"
             "create dns_resolver * * "
             (file-append cifs-utils "/sbin/cifs.upcall") " %k\n"
             "create cifs.spnego  * * "
             (file-append cifs-utils "/sbin/cifs.upcall") " %k\n"
             "create cifs.idmap   * * "
             (file-append cifs-utils "/sbin/cifs.upcall") " -t %k\n"))))

(define (cifs-activation _)
  ;; The kernel hardcodes /sbin/request-key for the keyring upcall mechanism.
  ;; Guix VMs have no /sbin; create it and symlink the binary from the store.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/sbin")
      (let ((target "/sbin/request-key")
            (source #$(file-append keyutils "/sbin/request-key")))
        (when (file-exists? target) (delete-file target))
        (symlink source target))))

(define (cifs-profile _)
  (list cifs-utils keyutils))

(define-public cifs-client-service-type
  (service-type
   (name 'cifs-client)
   (description
    "CIFS client support: installs cifs-utils and keyutils, writes
/etc/request-key.conf, and symlinks /sbin/request-key so the kernel
can resolve SMB server hostnames via cifs.upcall.")
   (extensions
    (list (service-extension profile-service-type    cifs-profile)
          (service-extension etc-service-type        cifs-etc-files)
          (service-extension activation-service-type cifs-activation)))
   (default-value #f)))
