;; dagon.scm — dagon.peteches.co.uk, new workstation (similar to nug).
;;
;;   sudo guix system -L . reconfigure peteches/systems/dagon.scm
;;
;; Reconfigured locally, not deployed — desktops are absent from
;; (peteches machines) by design.  Built on make-base-os from
;; (peteches systems base); see that module for what each flag does.
;;
;; Host specifics:
;;   - Intel CPU (#:intel-cpu? #t, the default) and an NVIDIA GPU
;;     (#:with-nvidia? #t), like nug.
;;   - Disk layout is simpler than nug/nyarlothotep: just an EFI System
;;     Partition and a single LUKS container holding the ext4 root
;;     directly — no separate /boot partition, no LVM.  GRUB's
;;     cryptodisk support is enabled automatically because
;;     `mapped-devices' includes a luks-device-mapping.
;;   - SOPS was bootstrapped in two passes because dagon had no age identity
;;     at install time (nyarlothotep already ran sops-key-generator).
;;     Pass 1 — sops-key-generator-service-type, wired below — wrote
;;     /etc/age/keys.txt on first boot (2026-10-05); the public half was
;;     committed to age-keys/dagon.pub, dagon was added to the
;;     `secrets/shared/deepgram\.yaml$' recipient list in .sops.yaml (plus a
;;     `secrets/hosts/dagon/.*\.yaml$' creation rule), and
;;     `sops updatekeys secrets/shared/deepgram.yaml' was run.  Pass 2 —
;;     wired below — decrypts the Deepgram key to
;;     /run/secrets/deepgram-api-key for the DMS Dictate plugin.  Pass 2
;;     could only land once pass 1's key existed: a secret dagon cannot
;;     decrypt would fail activation.
;;   - #:offload-builds? is #f for this initial install.  Wiring up
;;     offload to guix-build (nug's build-offload/publish successor,
;;     peteches/systems/guix-build.scm) needs a guix-offload SSH keypair
;;     delivered via SOPS (secrets/hosts/dagon/guix-build.yaml — the
;;     `secrets/hosts/dagon/.*\.yaml$' creation rule for it is already in
;;     .sops.yaml).  Once that secret exists, add its sops-secret entry to
;;     the sops-secrets-service-type block below (nyarlothotep.scm's
;;     ssh-private-key entry is the pattern), flip this to #t, and add
;;     dagon's offload pubkey to guix-build.scm's guix-offload-authorized-keys.
;;
;; The file evaluates to a bare `operating-system' record as its last
;; expression, which is what `guix system' consumes.

(define-module (peteches systems dagon)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages base)           ; glibc-locales
  #:use-module (gnu packages admin)          ; solaar
  #:use-module (nongnu packages linux)
  #:use-module (peteches systems base)
  #:use-module (peteches systems network-mounts)
  #:use-module (peteches services sops-key-generator)
  #:use-module (sops secrets)
  #:use-module (sops services sops))

(use-service-modules base linux cups desktop networking ssh xorg)

(define mapped-devices
  (list
   (mapped-device
    (source (uuid "cf8737a4-d6c9-44aa-88e1-14758a845d5f"))
    (target "cryptroot")
    (type luks-device-mapping))))

(make-base-os
 #:host-name "dagon"
 #:kernel linux
 #:firmware (list linux-firmware)

 #:bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (keyboard-layout (keyboard-layout "us")))

 #:mapped-devices
 mapped-devices

 #:file-systems
 (list
  (file-system
   (mount-point "/")
   (device "/dev/mapper/cryptroot")
   (type "ext4")
   (dependencies mapped-devices))
  (file-system
   (mount-point "/boot/efi")
   (device (uuid "CC71-F02D" 'fat32))
   (type "vfat"))
  scoreplay-cifs-mount)

 #:extra-packages (list glibc-locales)

 #:extra-services
 (list (udev-rules-service 'solaar solaar)
       ;; SOPS pass 1: give dagon an age identity (/etc/age/keys.txt) on
       ;; first boot; the sops-secrets service below (pass 2) decrypts the
       ;; shared secrets with it — see the header comment.
       (service sops-key-generator-service-type)
       (service sops-secrets-service-type
                (sops-service-configuration
                 (age-key-file "/etc/age/keys.txt")
                 (secrets
                  (list
                   ;; Deepgram Nova-3 API key, consumed by the DMS Dictate
                   ;; plugin (configs/dms/plugins/dictate/dictate-stream.py
                   ;; reads /run/secrets/deepgram-api-key when DEEPGRAM_API_KEY
                   ;; is not in the shell's environment -- it is not: DMS is
                   ;; started from Hyprland with a bare session environment).
                   ;; Mode 0400 peteches-only, same as nyarlothotep's and
                   ;; claude-workstation's copies; the file is shared, see
                   ;; the secrets/shared/deepgram\.yaml$ creation rule in
                   ;; .sops.yaml.
                   (sops-secret
                    (key '("deepgram-api-key"))
                    (file (local-file "../../secrets/shared/deepgram.yaml"))
                    (path "/run/secrets/deepgram-api-key")
                    (user "peteches")
                    (group "users")
                    (permissions #o400)))))))

 ;; Feature flags
 #:laptop? #f
 #:intel-cpu? #t
 #:with-bluetooth? #t
 #:with-printing? #f
 #:with-nonguix? #t
 #:with-docker? #t
 #:with-nvidia? #t
 #:offload-builds? #f)
