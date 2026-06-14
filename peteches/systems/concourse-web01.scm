;; concourse-web01.scm — Concourse CI web (ATC + TSA) node on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.
;;
;; Firewall note: concourse-web-service-type from (peteches services concourse)
;; extends (peteches services firewall)'s firewall-service-type (the channel's).
;; make-vm-os installs the main repo's (peteches system-services firewall) instead.
;; We therefore swap the two: delete the main-repo firewall from the base OS and
;; add the channel's firewall service so that the concourse extension can target it.

(define-module (peteches systems concourse-web01)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  ;; Main-repo firewall imported under vm-fw: prefix so we can delete it
  #:use-module ((peteches system-services firewall) #:prefix vm-fw:)
  ;; Channel modules: firewall-service-type, nftables-rules, concourse service
  #:use-module (peteches services firewall)
  #:use-module (peteches services concourse)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (sops secrets)
  #:export (concourse-web01-os))

;; Base VM firewall rules mirroring %vm-base-firewall in vm-base.scm.
;; Uses the channel's nftables-rules constructor so concourse-web-service-type
;; can extend this firewall service instance.
(define %ch-vm-base-firewall
  (nftables-rules
   (input (list
           "iifname \"lo\" accept comment \"loopback\""
           "ct state { established, related } accept comment \"established/related\""
           "tcp dport 22 accept comment \"ssh\""
           "tcp dport 9100 accept comment \"prometheus node-exporter\""
           "ip protocol icmp accept comment \"icmpv4\""
           "ip6 nexthdr ipv6-icmp accept comment \"icmpv6\""))))

(define %base-os
  (make-vm-os
   #:host-name "concourse-web01.peteches.co.uk"
   #:ipv4-address "192.168.51.199/23"
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
    (vm-name "concourse-web01")
    (synology-host "nas.peteches.co.uk")
    (backup-paths '("/var/lib/concourse-web"))
    (password-file "/run/secrets/restic-password")
    (ssh-key-file "/run/secrets/restic-ssh-key"))
   #:sops-secrets
   (list
    (sops-secret
     (key '("password"))
     (file (local-file "../../secrets/hosts/concourse-web01/restic.yaml"))
     (path "/run/secrets/restic-password"))
    (sops-secret
     (key '("ssh-key"))
     (file (local-file "../../secrets/hosts/concourse-web01/restic.yaml"))
     (path "/run/secrets/restic-ssh-key")
     (permissions #o400))
    (sops-secret
     (key '("ssh-private-key"))
     (file (local-file "../../secrets/hosts/concourse-web01/guix-build.yaml"))
     (path "/run/secrets/guix-offload-key")
     (permissions #o400))
    (sops-secret
     (key '("db-password"))
     (file (local-file "../../secrets/hosts/concourse-web01/concourse.yaml"))
     (path "/run/secrets/concourse-db-password")
     (permissions #o400))
    (sops-secret
     (key '("session-signing-key"))
     (file (local-file "../../secrets/hosts/concourse-web01/concourse.yaml"))
     (path "/run/secrets/concourse-session-signing-key")
     (permissions #o400))
    (sops-secret
     (key '("tsa-host-key"))
     (file (local-file "../../secrets/hosts/concourse-web01/concourse.yaml"))
     (path "/run/secrets/concourse-tsa-host-key")
     (permissions #o400))
    (sops-secret
     (key '("authorized-worker-keys"))
     (file (local-file "../../secrets/hosts/concourse-web01/concourse.yaml"))
     (path "/run/secrets/concourse-authorized-worker-keys")
     (permissions #o400)))
   #:extra-services
   (list
    (service alloy-service-type
             (alloy-configuration
              (hostname "concourse-web01.peteches.co.uk")
              (log-files (list (cons "/var/log/messages" "syslog")
                               (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                               (cons "/var/log/concourse-web.log" "concourse")
                               (cons "/var/log/ntpd.log" "ntpd")
                               (cons "/var/log/alloy.log" "alloy")
                               (cons "/var/log/tailscaled-*.log" "tailscale")))))
    (service tailscale-service-type
             (list (tailscale-instance-configuration
                    (name "peteches")))))))

(define-public concourse-web01-os
  (operating-system
   (inherit %base-os)
   (services
    (cons* (service firewall-service-type %ch-vm-base-firewall)
           (service concourse-web-service-type
                    (concourse-web-configuration
                     (postgres-host "192.168.51.198")
                     (external-url "https://concourse.ts.peteches.co.uk")))
           (modify-services (operating-system-services %base-os)
             (delete vm-fw:firewall-service-type))))))

concourse-web01-os
