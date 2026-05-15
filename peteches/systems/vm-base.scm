;; vm-base.scm — stripped-down base OS for Proxmox QEMU/KVM virtual machines.
;; No desktop services, no display manager, no libvirt, no GPU drivers.
;; Intended to be composed via make-vm-os, analogous to make-base-os.

(define-module (peteches systems vm-base)
  #:use-module (guix gexp)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system nss)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages linux)
  #:use-module (peteches system-services firewall)
  #:export (make-vm-os
            %vm-peteches-user))

(define %vm-peteches-user
  (user-account
   (name "peteches")
   (comment "Pete McCabe")
   (group "users")
   (home-directory "/home/peteches")
   (supplementary-groups '("wheel" "netdev"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

(define (nonguix-substitute-service)
  (simple-service 'add-nonguix-substitutes
                  guix-service-type
                  (guix-extension
                   (substitute-urls
                    (append (list "https://substitutes.nonguix.org")
                            %default-substitute-urls))
                   (authorized-keys
                    (append (list (plain-file "non-guix.pub"
                                             "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                            %default-authorized-guix-keys)))))

;; Authorize nug (the deploy coordinator) to push store items to all VMs.
(define %authorize-coordinator-key
  (simple-service 'authorize-coordinator-key
                  guix-service-type
                  (guix-extension
                   (authorized-keys
                    (list (plain-file "nug-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #89306B461D55FBB9F6A60C75463BA2AEE181FB3E8FA5F46CB2E1C29157ACA88A#)))"))))))

(define %vm-interface   "eth0")
(define %vm-ipv4-gw    "192.168.50.1")
(define %vm-ipv6-gw    "2a10:d582:ef59::1")
(define %vm-nameservers '("192.168.50.1"))

(define %vm-base-firewall
  (nftables-rules
   (input (list
           "iifname \"lo\" accept comment \"loopback\""
           "ct state { established, related } accept comment \"established/related\""
           "tcp dport 22 accept comment \"ssh\""
           "tcp dport 9100 accept comment \"prometheus node-exporter\""
           "ip protocol icmp accept comment \"icmpv4\""
           "ip6 nexthdr ipv6-icmp accept comment \"icmpv6\""))))

(define* (make-vm-os
          #:key host-name bootloader file-systems
          (mapped-devices '())
          (kernel linux-libre)
          (firmware '())
          (users-extra '())
          (extra-services '())
          (extra-packages '())
          (with-nonguix? #f)
          (ipv4-address #f)
          (ipv6-address #f))
  (let* ((nonguix-services (if with-nonguix? (list (nonguix-substitute-service)) '()))
         (final-services
          (append
           %base-services
           (list
            (service openssh-service-type
                     (openssh-configuration
                      (authorized-keys
                       `(("peteches"
                          ,(plain-file "peteches-nug.pub"
                                       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMddPKUs7sbjMj8GtmzytHhGx7JOoCikqPEBuwE50qa7 peteches@nug\n")
                          ,(plain-file "peteches-nyarlothotep.pub"
                                       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM28x2V8tgwfzjyhapMayamDFwviOTHfU4W9BMnmc70w peteches@nyarlothotep.peteches.co.uk\n"))))))
            (if ipv4-address
                (service static-networking-service-type
                         (list (static-networking
                                (addresses
                                 (append
                                  (list (network-address (device %vm-interface)
                                                         (value ipv4-address)))
                                  (if ipv6-address
                                      (list (network-address (device %vm-interface)
                                                             (value ipv6-address)))
                                      '())))
                                (routes
                                 (append
                                  (list (network-route (destination "default")
                                                       (gateway %vm-ipv4-gw)))
                                  (if ipv6-address
                                      (list (network-route (destination "default")
                                                           (gateway %vm-ipv6-gw)
                                                           (ipv6? #t)))
                                      '())))
                                (name-servers %vm-nameservers))))
                (service dhcpcd-service-type))
            (service ntp-service-type)
            (service qemu-guest-agent-service-type)
            (service firewall-service-type %vm-base-firewall)
            (service prometheus-node-exporter-service-type)
            %authorize-coordinator-key)
           nonguix-services
           extra-services)))
    (operating-system
      (kernel kernel)
      (kernel-arguments %default-kernel-arguments)
      (firmware firmware)
      (locale "en_GB.utf8")
      (timezone "Europe/London")
      (keyboard-layout (keyboard-layout "us"))
      (host-name host-name)
      (sudoers-file (plain-file "sudoers"
                                (string-append
                                 "root ALL=(ALL) ALL\n"
                                 "%wheel ALL=(ALL) NOPASSWD: ALL\n"
                                 "#includedir /run/sudoers.d\n")))
      (users (append (list %vm-peteches-user)
                     %base-user-accounts
                     users-extra))
      (name-service-switch
       (name-service-switch
        (inherit %mdns-host-lookup-nss)
        (hosts (list %files %dns))))
      (packages (append extra-packages %base-packages))
      (services final-services)
      (mapped-devices mapped-devices)
      (file-systems (append file-systems %base-file-systems))
      (bootloader bootloader))))
