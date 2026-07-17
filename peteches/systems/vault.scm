;; vault.scm — HashiCorp Vault secrets manager on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.
;; Vault service enabled after first successful base deploy.
;;
;; API on :8200 with TLS disabled — it is fronted by the caddy VM
;; (vault.ts.peteches.co.uk) over Tailscale, which terminates TLS.  Never
;; expose :8200 to the LAN directly.  Prometheus scrapes /v1/sys/metrics.
;; concourse-web01 authenticates to it with the AppRole credentials in
;; secrets/groups/concourse/vault.yaml.
;;
;; Vault does not auto-unseal: it comes up sealed after every reboot or
;; deploy that restarts the service, and needs `vault operator unseal' by
;; hand before anything depending on it works.
;;
;; NOTE: this VM does not declare a `guix-offload-key' sops-secret, but
;; make-vm-os's #:with-nug-offload? defaults to #t and no VM overrides it —
;; so it is configured to offload builds to nug using a private key at
;; /run/secrets/guix-offload-key that is never created.  Offload then fails
;; and builds fall back to local, which is why it has gone unnoticed.
;; Either add the secret (as loki.scm does, from
;; secrets/hosts/<name>/guix-build.yaml, plus the matching public key in
;; nug.scm's authorized-keys) or pass #:with-nug-offload? #f.
;;
;; Build offload is half-wired fleet-wide; both halves are needed:
;;   no guix-offload-key secret:  vault, critical-grind-outline, plane
;;   secret present, but no matching public key in nug.scm's
;;   authorized-keys:            concourse-db, concourse-web01,
;;                               concourse-worker01

(define-module (peteches systems vault)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (peteches services vault)
  #:use-module (sops secrets)
  #:export (vault-os))

(define-public vault-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "vault.peteches.co.uk"
     #:ipv4-address "192.168.51.201/23"
     #:ipv6-address "2a10:d582:ef59::108/64"
     #:bootloader
     (bootloader-configuration
      (bootloader grub-efi-removable-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "us")))
     #:file-systems
     (list
      (file-system
        (mount-point "/boot/efi")
        (device (file-system-label "GNU-ESP"))
        (type "vfat"))
      (file-system
        (mount-point "/")
        (device "/dev/vda2")
        (type "ext4")))
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "vault")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/vault"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("restic-password"))
       (file (local-file "../../secrets/hosts/vault/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/vault/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("auth-key"))
       (file (local-file "../../secrets/shared/tailscale.yaml"))
       (path "/run/secrets/tailscale-auth-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "vault.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/vault.log" "vault")))))
      (service vault-service-type
               (vault-configuration
                (listener
                 (vault-listener
                  (address "0.0.0.0:8200")
                  (tls-disable #t)))
                (api-addr "http://192.168.51.201:8200")
                (cluster-addr "https://192.168.51.201:8201"))))))))

vault-os
