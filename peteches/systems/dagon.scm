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
;;   - SOPS is bootstrapped in two passes because dagon had no age identity
;;     at all (nyarlothotep already ran sops-key-generator).  Pass 1 — what
;;     is wired below — only starts sops-key-generator-service-type, which
;;     writes /etc/age/keys.txt on first boot; no sops-secret is declared
;;     yet, because a secret dagon cannot decrypt would fail activation.
;;     After reconfiguring:
;;       1. `ssh dagon cat /etc/age/keys.pub > age-keys/dagon.pub'
;;          (the generator also writes the public half, mode 0644; never
;;          print or commit the private /etc/age/keys.txt),
;;       2. add a `secrets/hosts/dagon/.*\.yaml$' creation rule to .sops.yaml
;;          and add dagon to the `secrets/shared/deepgram\.yaml$' rule,
;;       3. `sops updatekeys secrets/shared/deepgram.yaml',
;;       4. pass 2: add the sops-secrets-service-type block below (copy
;;          nyarlothotep.scm's deepgram-api-key entry verbatim) and
;;          reconfigure again.
;;   - #:offload-builds? is #f for this initial install.  Wiring up
;;     offload to guix-build (nug's build-offload/publish successor,
;;     peteches/systems/guix-build.scm) needs a guix-offload SSH keypair
;;     delivered via SOPS (secrets/hosts/dagon/guix-build.yaml), which in
;;     turn needs dagon's age public key — only available after the pass 1
;;     reconfigure above.  Once that exists, follow nyarlothotep.scm's pattern
;;     (sops-key-generator-service-type + sops-secrets-service-type),
;;     flip this to #t, and add dagon's offload pubkey to guix-build.scm's
;;     guix-offload-authorized-keys.
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
       ;; SOPS pass 1: give dagon an age identity (/etc/age/keys.txt).
       ;; No sops-secret yet — see the header comment for pass 2, which adds
       ;; the Deepgram key the DMS Dictate plugin reads from
       ;; /run/secrets/deepgram-api-key.
       (service sops-key-generator-service-type))

 ;; Feature flags
 #:laptop? #f
 #:intel-cpu? #t
 #:with-bluetooth? #t
 #:with-printing? #f
 #:with-nonguix? #t
 #:with-docker? #t
 #:with-nvidia? #t
 #:offload-builds? #f)
