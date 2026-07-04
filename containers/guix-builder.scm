(define-module (containers guix-builder))

(use-modules (gnu)
             (gnu services guix)
             (gnu packages))

(use-package-modules virtualization  ; qemu (qemu-img, qemu-nbd)
                     linux           ; kmod, util-linux
                     ssh             ; openssh
                     gnupg)          ; gnupg

;; age and awscli resolved via specification->package to avoid needing exact
;; module paths (e.g. golang-xyz, python-xyz) that vary across Guix versions.
(define %extra-packages
  (map specification->package '("age" "awscli" "vault" "tailscale" "nss-certs" "jq")))

(operating-system
  (host-name "guix-builder")
  (timezone "UTC")
  (locale "en_US.utf8")

  ;; Bootloader and file-systems are required by operating-system but are
  ;; ignored when exporting as a Docker image via guix system docker-image.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))))

  (file-systems (cons (file-system
                        (device (file-system-label "guix-builder"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (packages
   (append (list qemu-minimal ; provides qemu-nbd and qemu-img (no LLVM)
                 kmod         ; provides modprobe
                 util-linux   ; provides mount / umount
                 openssh      ; ssh client
                 gnupg)       ; gpg for guix pull signature verification
           %extra-packages  ; age, awscli
           %base-packages))

  ;; guix-service-type creates the guixbuild group and guixbuilder accounts
  ;; during image activation so the daemon can be started in Concourse tasks
  ;; with --build-users-group=guixbuild even though shepherd is not PID 1.
  (services
   ;; %base-services includes guix-service-type (guixbuild users) and
   ;; special-files-service-type which creates /bin/sh in the image.
   %base-services))
