;; vm-base.scm — stripped-down base OS for Proxmox QEMU/KVM virtual machines.
;; No desktop services, no display manager, no libvirt, no GPU drivers.
;; Intended to be composed via make-vm-os, analogous to make-base-os in
;; (peteches systems base), which serves the desktop/laptop machines.
;;
;; Every VM system config in peteches/systems/ is a thin wrapper around
;; make-vm-os.  The house style is:
;;
;;   (define-public <name>-os
;;     (operating-system (inherit (make-vm-os …))))
;;   <name>-os          ; ← bare final expression, see below
;;
;; The trailing bare `<name>-os' matters: `guix system build FILE' evaluates
;; the file and uses its last value, while `guix deploy' imports the module
;; and reads the exported variable.  Omit it and `guix system build' fails.
;;
;; make-vm-os keyword arguments
;; ----------------------------
;;   #:host-name          (required) FQDN, e.g. "loki.peteches.co.uk".
;;   #:bootloader         (required) always grub-efi-removable-bootloader here.
;;   #:file-systems       (required) machine-specific mounts; %base-file-systems
;;                        is appended internally, so list only your own.
;;   #:mapped-devices     LUKS/LVM mappings.  Unused by the VMs.
;;   #:kernel             defaults to linux-libre.  Note: linux-libre has no
;;                        overlayfs/FUSE here, which is why the Podman hosts
;;                        (plane) pin the vfs storage driver.
;;   #:firmware           defaults to '() — VMs need no blobs.
;;   #:users-extra        extra user-accounts appended to %vm-peteches-user.
;;   #:extra-services     the main composition point.
;;   #:extra-packages     prepended to %vm-base-packages.
;;   #:ipv4-address       CIDR string, e.g. "192.168.51.190/23".  When set,
;;                        static-networking-service-type is used; when #f the
;;                        VM falls back to dhcpcd (only bootstrap.scm does).
;;   #:ipv6-address       CIDR string; only added when ipv4-address is also set.
;;   #:nameservers        defaults to %vm-nameservers (the pihole VM).
;;   #:restic-config      a restic-vm-backup-configuration, or #f for no backups.
;;   #:sops-secrets       list of sops-secret records decrypted at boot into
;;                        /run/secrets/ using the VM's own age key (baked into
;;                        the image at /etc/age/keys.txt by CI; see note below).
;;   #:with-nonguix?      register the nonguix substitute server.
;;   #:with-nug-offload?  DEFAULT #t — adds nug as a build machine.  This
;;                        requires a private key at /run/secrets/guix-offload-key,
;;                        so a VM enabling it must also declare a sops-secret
;;                        writing that path (conventionally from
;;                        secrets/hosts/<name>/guix-build.yaml), AND its
;;                        guix-offload public key must be listed in
;;                        peteches/systems/nug.scm's authorized_keys service.
;;   #:with-nvidia?       nonguix NVIDIA driver + CUDA (jellyfin, for NVENC).
;;
;; Baseline every VM gets: openssh (key-only, nug + nyarlothotep enrolled),
;; ntpd, qemu-guest-agent, nftables firewall (%vm-base-firewall: ssh + 9100
;; + icmp only), cifs-client, prometheus-node-exporter, and
;; %authorize-coordinator-key (trusts nug/nyarlothotep to push store items,
;; and registers nug's guix-publish as a substitute server).
;;
;; The age key sops uses to decrypt #:sops-secrets is baked into each VM's
;; image by the CI build-vm-image pipeline (debugfs-written to
;; /etc/age/keys.txt), so the base no longer runs sops-key-generator to make
;; one on first boot -- doing so would fail, since age-keygen refuses to
;; overwrite the injected key.  The two non-pipeline paths that build
;; UN-injected images still add that service themselves: bootstrap.scm (the
;; legacy Proxmox template) and nyarlothotep.scm (a bare-metal desktop).
;;
;; Opening a port means extending firewall-service-type from #:extra-services;
;; see git.scm or rustdesk.scm for the `simple-service' pattern.
;;
;; See CLAUDE.md "Adding a New VM" for the other files that must be updated
;; in the same change (pihole hosts, ssh config, machines.scm, monitored-hosts,
;; terraform, age-keys).

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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (peteches systems common)
  #:use-module (peteches services firewall)
  #:use-module (peteches services restic)
  #:use-module (peteches services cifs)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (gnu services linux)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix-science-nonfree packages cuda-modules)
  #:export (make-vm-os
            %vm-peteches-user
            %vm-peteches-authorized-keys))

;; Console-login user for the VMs.  `password' is a pre-hashed crypt(3)
;; string, needed because these are headless clones with no interactive
;; install step — it exists for Proxmox console recovery only.  Day-to-day
;; access is SSH key-only (see the openssh-configuration below), and this
;; user gets passwordless sudo via the sudoers-file set in make-vm-os.
(define %vm-peteches-user
  (user-account
   (name "peteches")
   (comment "Pete McCabe")
   (group "users")
   (home-directory "/home/peteches")
   (supplementary-groups '("wheel" "netdev"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

;; The admin SSH keys enrolled on every VM: nug + nyarlothotep.  Kept as a
;; named, exported list so other system configs can grant the same access to
;; an additional login user (see peteches/systems/claude-workstation.scm,
;; which authorizes these for the criticalgrind account too).
(define %vm-peteches-authorized-keys
  (list (plain-file "peteches-nug.pub"
                    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMddPKUs7sbjMj8GtmzytHhGx7JOoCikqPEBuwE50qa7 peteches@nug\n")
        (plain-file "peteches-nyarlothotep.pub"
                    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM28x2V8tgwfzjyhapMayamDFwviOTHfU4W9BMnmc70w peteches@nyarlothotep.peteches.co.uk\n")))

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

;; nscd-cache-database is not exported by (gnu services base) in current Guix.
;; @@ reaches into the module's private bindings; it is the only way to read
;; the field off the stock %nscd-default-caches records.  If a Guix update
;; renames this binding, every VM build breaks here with an unbound-variable
;; error — that is the expected failure mode, not something subtler.
(define nscd-cache-database*
  (@@ (gnu services base) nscd-cache-database))

;; Drop nscd's `hosts' cache.  nscd caches NXDOMAIN and DNS answers past
;; their TTL, which makes pihole's custom-hosts edits (and Tailscale's
;; *.ts.net names) take unpredictable effect.  The other caches are kept.
(define (nscd-without-hosts-cache cfg)
  (nscd-configuration
   (inherit cfg)
   (caches (filter (lambda (cache)
                     (let ((db (nscd-cache-database* cache)))
                       (not (or (eq? db 'hosts)
                                (equal? db "hosts")))))
                   %nscd-default-caches))))

(define %vm-interface   "eth0")
;; The LAN is a /23 (192.168.50.0/23), so the .50.x gateway and the .51.x
;; VM addresses are on the same subnet.  Every VM passes a /23 prefix in
;; #:ipv4-address for this reason — a /24 would make the gateway unreachable.
(define %vm-ipv4-gw    "192.168.50.1")
(define %vm-ipv6-gw    "2a10:d582:ef59::1")
;; DNS is the pihole VM (192.168.51.189).  Note the circularity: pihole
;; resolves through itself.  Deploys therefore address VMs by IP, never name.
(define %vm-nameservers '("192.168.51.189"))
(define %vm-base-packages (cons btop %base-packages))

;; Baseline input policy for every VM.  firewall-service-type's default
;; input policy is drop, so anything not listed here (or added by a VM's own
;; `simple-service … firewall-service-type' extension) is silently blocked.
;; A service that "starts fine but is unreachable" is almost always a
;; missing rule here or in the VM's own extension.
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
          (ipv6-address #f)
          (restic-config #f)
          (nameservers %vm-nameservers)
          (sops-secrets '())
          (with-nug-offload? #t)
          (with-nvidia? #f))
  (let* ((nonguix-services (if with-nonguix? (list (nonguix-substitute-service)) '()))
         (restic-services
          (if restic-config
              (list (service restic-vm-backup-service-type restic-config))
              '()))
         (sops-services
          (if (not (null? sops-secrets))
              (list (service sops-secrets-service-type
                             (sops-service-configuration
                              (age-key-file "/etc/age/keys.txt")
                              (secrets sops-secrets))))
              '()))
         (nvidia-packages
          (if with-nvidia?
              (list nvidia-firmware nvidia-driver cuda-nvcc)
              '()))
         (nvidia-services
          (if with-nvidia?
              (list
               (service nvidia-service-type)
               (simple-service 'nvidia-runtime-state
                               activation-service-type
                               #~(begin
                                   (use-modules (guix build utils))
                                   (mkdir-p "/run/nvidia")))
               (simple-service 'custom-udev-rules udev-service-type
                               (list nvidia-driver))
               (service kernel-module-loader-service-type
                        '("nvidia" "nvidia_uvm")))
              '()))
         (final-services
          (append
           %base-services
           (list
            (service openssh-service-type
                     (openssh-configuration
                      (authorized-keys
                       `(("peteches" ,@%vm-peteches-authorized-keys)))))
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
                                ;; Only the IPv4 default route is static. The IPv6
                                ;; default comes from the network's Router
                                ;; Advertisements (`default proto ra`). Adding a
                                ;; static IPv6 default here races the kernel's
                                ;; RA-installed default route — both land at metric
                                ;; 1024, so whichever loses the race gets netlink
                                ;; EEXIST (17), which kills the one-shot networking
                                ;; service and everything that Requires: networking
                                ;; (ssh, tailscale, node-exporter…). The static IPv6
                                ;; address is still assigned above.
                                (routes
                                 (list (network-route (destination "default")
                                                      (gateway %vm-ipv4-gw))))
                                (name-servers nameservers))))
                (service dhcpcd-service-type))
            (service ntp-service-type)
            (service qemu-guest-agent-service-type)
            (service firewall-service-type %vm-base-firewall)
            (service cifs-client-service-type)
            (service prometheus-node-exporter-service-type)
            %authorize-coordinator-key)
           nonguix-services
           restic-services
           sops-services
           nvidia-services
           extra-services)))
    (operating-system
      (kernel kernel)
      (kernel-arguments (append (if with-nvidia?
                                    (list "modprobe.blacklist=nouveau")
                                    '())
                                %default-kernel-arguments))
      (kernel-loadable-modules (if with-nvidia?
                                   (list nvidia-module)
                                   '()))
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
      (packages (append extra-packages nvidia-packages %vm-base-packages))
      (services
       (modify-services final-services
         (nscd-service-type cfg => (nscd-without-hosts-cache cfg))
         (guix-service-type cfg =>
           (guix-configuration
            (inherit cfg)
            (build-accounts 20)
            (build-machines (if with-nug-offload? (list %nug-build-machine) '()))))))
      (mapped-devices mapped-devices)
      (file-systems (append file-systems %base-file-systems))
      (bootloader bootloader))))
