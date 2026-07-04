;; concourse-worker01.scm — Concourse CI worker node on a Proxmox QEMU/KVM VM.
;; Connects to concourse-web01 (192.168.51.199) via TSA on port 2222.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems concourse-worker01)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services tailscale)
  #:use-module (peteches services concourse)
  #:use-module (gnu services linux)
  #:use-module (sops secrets)
  #:export (concourse-worker01-os))

(define concourse-worker-firewall-rules
  (nftables-rules
   ;; Mostly not needed for normal forwarded container egress, but useful if
   ;; Quad100 traffic is handled as local input by tailscaled.
   (input
    (list
     "iifname \"concourse0\" ip daddr 100.100.100.100 udp dport 53 accept comment \"concourse containers to tailscale dns udp\""
     "iifname \"concourse0\" ip daddr 100.100.100.100 tcp dport 53 accept comment \"concourse containers to tailscale dns tcp\""))

   ;; Concourse build/resource containers live behind concourse0.
   ;; Allow egress to LAN/internet via eth0 and tailnet via ts-peteches.
   (forward
    (list
     "iifname \"concourse0\" oifname \"eth0\" accept comment \"concourse containers to lan/internet\""
     "iifname \"eth0\" oifname \"concourse0\" ct state established,related accept comment \"lan/internet return to concourse containers\""

     "iifname \"concourse0\" oifname \"ts-peteches\" accept comment \"concourse containers to tailscale\""
     "iifname \"ts-peteches\" oifname \"concourse0\" ct state established,related accept comment \"tailscale return to concourse containers\""))

   ;; NAT containers through normal LAN/internet and through Tailscale.
   (nat-postrouting
    (list
     "ip saddr 10.80.0.0/16 oifname \"eth0\" masquerade comment \"nat concourse containers to lan/internet\""
     "ip saddr 10.80.0.0/16 oifname \"ts-peteches\" masquerade comment \"nat concourse containers to tailscale\""))))

(define-public concourse-worker01-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "concourse-worker01.peteches.co.uk"
     #:ipv4-address "192.168.51.200/23"
     #:bootloader
     (bootloader-configuration
      (bootloader grub-efi-removable-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "us")))
     #:file-systems
     (list
      (file-system
       (device "none")
       (mount-point "/sys/fs/cgroup")
       (type "cgroup2")
       (check? #f)
       (create-mount-point? #f))
      (file-system
       (mount-point "/boot/efi")
       (device (file-system-label "GNU-ESP"))
       (type "vfat"))
      (file-system
       (mount-point "/")
       (device "/dev/vda2")
       (type "ext4"))
      (file-system
       (device (uuid "7dc9f5e9-4563-4e68-b4f6-97027cc86bb7"))
       (mount-point "/var/lib/concourse-worker")
       (type "ext4")))
     #:sops-secrets
     (list
      (sops-secret
       (key '("worker-private-key"))
       (file (local-file "../../secrets/hosts/concourse-worker01/concourse.yaml"))
       (path "/run/secrets/concourse-worker-key")
       (permissions #o400))
      (sops-secret
       (key '("tsa-host-key-pub"))
       (file (local-file "../../secrets/hosts/concourse-worker01/concourse.yaml"))
       (path "/run/secrets/concourse-tsa-host-key.pub")
       (permissions #o400))
      (sops-secret
       (key '("ssh-private-key"))
       (file (local-file "../../secrets/hosts/concourse-worker01/guix-build.yaml"))
       (path "/run/secrets/guix-offload-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service concourse-worker-service-type
               (concourse-worker-configuration
                (tsa-host "192.168.51.199:2222")
                (tsa-public-key "/run/secrets/concourse-tsa-host-key.pub")
                (worker-private-key "/run/secrets/concourse-worker-key")))
      (service alloy-service-type
               (alloy-configuration
                (hostname "concourse-worker01.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/concourse-worker.log" "concourse-worker")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      (simple-service 'concourse-worker-firewall
                firewall-service-type
                concourse-worker-firewall-rules)
      (simple-service 'nbd-modprobe-options
                etc-service-type
                (list `("modprobe.d/nbd.conf"
                        ,(plain-file "nbd.conf" "options nbd max_part=8\n"))))
      (simple-service 'load-nbd-module
                kernel-module-loader-service-type
                '("nbd"))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")))))))))

concourse-worker01-os
