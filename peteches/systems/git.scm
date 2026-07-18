;; git.scm — Personal git server on a Proxmox QEMU/KVM VM.
;; gitolite for SSH access control, cgit for web UI, and smart-HTTP clone
;; access via git-http-backend (all served by the same nginx on :80).
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems git)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services cgit)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (peteches services firewall)
  #:use-module (sops secrets)
  #:export (git-os))

;;; Smart-HTTP clone access, so consumers that cannot use SSH can still fetch.
;;;
;;; The motivating case is `guix pull': guix/git.scm authenticates git
;;; fetches with (%make-auth-ssh-agent) and nothing else -- it never reads
;;; ~/.ssh/config or a key file -- so an ssh:// channel URL requires the key
;;; to be loaded in an agent on every machine that pulls, including CI
;;; containers.  An https:// URL needs no agent at all.
;;;
;;; SECURITY.  git-http-backend does NOT consult gitolite's ACLs; gitolite
;;; enforces those in its SSH command wrapper, which this path bypasses
;;; entirely.  `export-all?' is therefore left at its #f default, which makes
;;; git-http-backend refuse any repository that does not contain a
;;; `git-daemon-export-ok' file.  Publishing a repo is thus an explicit,
;;; per-repo opt-in -- preferably via gitolite's own `option daemon = 1' in
;;; gitolite-admin, which creates that file, so the decision stays recorded
;;; in gitolite.conf rather than as untracked state on disk.
;;;
;;; Note this does not make the box meaningfully more exposed than it already
;;; was: cgit has always served every repository under repository-directory
;;; over this same unauthenticated port 80.  Read the firewall comment below.
(define %git-http-config
  (git-http-configuration
   (git-root "/var/lib/gitolite/repositories")
   ;; Default-deny.  See the SECURITY note above before changing this.
   (export-all? #f)
   ;; Keeps clone URLs clear of cgit's UI namespace:
   ;;   https://git.ts.peteches.co.uk/git/<repo>.git
   ;; The .git suffix is required -- that is the real directory name under
   ;; gitolite, and git-http-backend does not guess it the way SSH access does.
   (uri-path "/git/")
   ;; The socket cgit-service-type already stands up; both CGIs share it.
   (fcgiwrap-socket "127.0.0.1:9000")))

;;; cgit ships a ready-made nginx server block, but only extends nginx with
;;; that one block.  Rather than declare a second server competing for :80,
;;; add the git-http location to cgit's own block.  Ordering is irrelevant:
;;; git-http is a regex location, which nginx matches ahead of the try-files
;;; fallback into the named @cgit location.
(define %cgit-nginx-with-git-http
  (nginx-server-configuration
   (inherit %cgit-configuration-nginx)
   (locations
    (cons (git-http-nginx-location-configuration %git-http-config)
          (nginx-server-configuration-locations %cgit-configuration-nginx)))))

(define-public git-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "git.peteches.co.uk"
     #:ipv4-address "192.168.51.191/23"
     #:ipv6-address "2a10:d582:ef59::103/64"
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
      (vm-name "git")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/gitolite"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/git/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/git/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("ssh-private-key"))
       (file (local-file "../../secrets/hosts/git/guix-build.yaml"))
       (path "/run/secrets/guix-offload-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service gitolite-service-type
               (gitolite-configuration
                (rc-file (gitolite-rc-file
                          (umask #o0022)))
                (admin-pubkey
                 (plain-file "admin.pub"
                             "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM28x2V8tgwfzjyhapMayamDFwviOTHfU4W9BMnmc70w peteches@nyarlothotep.peteches.co.uk"))
                (admin-name "peteches@nyarlothotep")))
      (service cgit-service-type
               (cgit-configuration
                (repository-directory "/var/lib/gitolite/repositories")
                (nginx (list %cgit-nginx-with-git-http))))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "git.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      ;; This rule is interface-agnostic: :80 is reachable from the whole
      ;; 192.168.50.0/23 LAN, not only over the tailnet.  The file header used
      ;; to say "Tailscale only", which was never true of the firewall -- only
      ;; of how the service is *published* (Caddy fronts it at
      ;; git.ts.peteches.co.uk).  Both cgit and git-http are unauthenticated,
      ;; so anything readable here is readable by anything on the LAN.
      ;;
      ;; To make the header's original claim real, restrict this to the
      ;; tailscale interface:
      ;;   "iifname \"tailscale0\" tcp dport 80 accept comment \"cgit + git-http\""
      ;; Deliberately left as-is for now: that is a change in existing cgit
      ;; behaviour and belongs in its own commit, not smuggled in with this one.
      (simple-service 'cgit-firewall
                       firewall-service-type
                       (nftables-rules
                        (input (list "tcp dport 80 accept comment \"cgit + git-http\"")))))))))

git-os
