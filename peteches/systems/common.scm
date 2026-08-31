;;; peteches/systems/common.scm — bindings shared by both OS constructors.
;;;
;;; Imported by (peteches systems base) and (peteches systems vm-base):
;;;
;;;   %nug-build-machine        build-machine record for offloading to nug.
;;;                             It is a *gexp* (#~(build-machine …)), not a
;;;                             record, because guix-configuration's
;;;                             build-machines field is staged into
;;;                             /etc/guix/machines.scm on the target host —
;;;                             (guix scripts offload) is not available at
;;;                             config-evaluation time.
;;;   %authorize-coordinator-key  trusts nug + nyarlothotep to push signed
;;;                             store items to a host, and registers nug's
;;;                             guix-publish (port 3000) as a substitute
;;;                             server.  Every VM gets this via make-vm-os;
;;;                             without it `guix deploy' has to rebuild
;;;                             everything on the target.
;;;
;;; `common-home-services' is a leftover: nothing imports it.  The live home
;;; configuration is assembled in (peteches home modules base) instead, and
;;; the two have drifted (this one still sets a pinentry-emacs gpg-agent,
;;; whereas the home module uses the dispatching wrapper in
;;; (peteches home modules gpg)).  Prefer the home module; do not add here.

(define-module (peteches systems common)
   #:use-module (peteches utils)
   #:use-module (peteches home services desktop)
   #:use-module (gnu services)
   #:use-module (gnu services base)
   #:use-module (gnu packages gnupg)
   #:use-module (gnu home)
   #:use-module (gnu home services)
   #:use-module (gnu home services pm)
   #:use-module (gnu home services gnupg)
   #:use-module (gnu home services mcron)
   #:use-module (gnu home services shells)
   #:use-module (gnu home services desktop)
   #:use-module (gnu home services syncthing)
   #:use-module (guix gexp))

;; Build machine record for offloading to the guix-build VM.
;; Used by vm-base.scm and base.scm via (guix-configuration (build-machines ...)).
;;
;; Still named %nug-build-machine (and with-nug-offload? in vm-base.scm) for
;; minimal diff churn -- nug itself was reinstalled as the Proxmox host
;; (proxmox3) this offload/publish role used to run on directly; the role
;; moved to the guix-build VM (peteches/systems/guix-build.scm), the name
;; didn't follow. host-key was filled in via ssh-keyscan after guix-build's
;; first boot.
;;
;; parallel-builds was copied verbatim from nug's original build-machine
;; record (32 cores/94GB) and never adjusted for guix-build's actual specs
;; (8 cores/12GB) -- 20 parallel slots on this box appears to OOM-kill
;; individual offloaded builds silently (confirmed live: several small,
;; unrelated derivations -- an NVIDIA .run download, nvda-595.91, then
;; nvidia-firmware-595.91.07 -- each failed only when offloaded here, then
;; built successfully seconds later with --no-offload on the same inputs).
;; 6 leaves the daemon's own overhead some headroom against 8 real cores.
(define-public %nug-build-machine
  #~(build-machine
     (name "guix-build.spaniel-cordylus.ts.net")
     (systems '("x86_64-linux"))
     (user "guix-offload")
     (private-key "/run/secrets/guix-offload-key")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKSOGKH6AeVlj1WQhSuzT6ni0cpzqcdPjUaFVufYOCqt")
     (parallel-builds 6)))

;; Authorize deploy coordinators (nug, nyarlothotep, and claude-workstation)
;; to push store items to all VMs, and register nug's guix-publish as a
;; substitute server.
;;
;; claude-workstation added 2026-08-22: deploys run from there (via the
;; automation SSH key) hit `guix deploy: error: unauthorized public key'
;; while sending store items to pihole -- claude-workstation's own
;; /etc/guix/signing-key.pub was never added here, unlike nug/nyarlothotep,
;; because it's a newer deploy origin than the original two desktop
;; coordinators. Every VM gets this service via make-vm-os, so fixing it
;; once here (rather than per-VM) covers the same latent gap fleet-wide,
;; not just on pihole.
(define-public %authorize-coordinator-key
  (simple-service 'authorize-coordinator-key
                  guix-service-type
                  (guix-extension
                   (substitute-urls
                    (append (list "http://nug.spaniel-cordylus.ts.net:3000")
                            %default-substitute-urls))
                   (authorized-keys
                    (list (plain-file "nug-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #89306B461D55FBB9F6A60C75463BA2AEE181FB3E8FA5F46CB2E1C29157ACA88A#)))")
                          (plain-file "nyarlothotep-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #C41C4703766F019CF43C8FBA3C7E284610799FBBF9875AB561AD7D8A74075AFE#)))")
                          (plain-file "claude-workstation-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #EFED7FDADFFF4E2559977AFD10310E21C4EEF7685C6297595D5333CBEF037EDE#)))"))))))

(define-public common-home-services
  (list
   ;; Set environment variables for every session
   (simple-service 'profile-env-vars-service
                   home-environment-variables-service-type
                   '( ;; Sort hidden (dot) files first in `ls` listings
                     ("LC_COLLATE" . "C")

                     ;; Emacs is our editor
                     ("VISUAL" . "emacsclient")
                     ("EDITOR" . "emacsclient")

                     ;; Add some things to $PATH (maybe integrate into other services?)
                     ("PATH" . "$HOME/.bin:$HOME/.npm-global/bin:$PATH")

                     ;; Make sure Flatpak apps are visible
                     ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")))

   ;; Set up the shell environment
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              `(,(plain-file "bash-profile-extras"
                             (string-append
                              ;; Load the Nix profile
                              "if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then\n"
                              "  . /run/current-system/profile/etc/profile.d/nix.sh\n"
                              "fi\n"))))
             (bashrc
              `(,(local-file "../files/bash-prompt")))))

   ;; Place other files
   (simple-service 'profile-files-service
                   home-files-service-type
                   (list `(".inputrc" ,(local-file "../files/inputrc"))))

   ;; GnuPG configuration
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)))

   ;; Emacs configuration
   ;; (service home-emacs-config-service-type)

   ;; Run user dbus session
   (service home-dbus-service-type)

   ;; Set up desktop environment
   (service home-desktop-service-type)

   ;; File synchronization
   (service home-syncthing-service-type)

   ;; Monitor battery levels
   (service home-batsignal-service-type)))

   ;; Udiskie for auto-mounting devices
   ;; (service home-udiskie-service-type)))
