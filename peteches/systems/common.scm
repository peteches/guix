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

;; Build machine record for offloading to nug.
;; Used by vm-base.scm and base.scm via (guix-configuration (build-machines ...)).
(define-public %nug-build-machine
  #~(build-machine
     (name "nug.peteches.co.uk")
     (systems '("x86_64-linux"))
     (user "guix-offload")
     (private-key "/run/secrets/guix-offload-key")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFV+RLdfvLSuCoedNwepqbEEHnHu363OQj6U0diCX+SR")))

;; Authorize deploy coordinators (nug and nyarlothotep) to push store items to all VMs,
;; and register nug's guix-publish as a substitute server.
(define-public %authorize-coordinator-key
  (simple-service 'authorize-coordinator-key
                  guix-service-type
                  (guix-extension
                   (substitute-urls
                    (append (list "http://nug.peteches.co.uk:3000")
                            %default-substitute-urls))
                   (authorized-keys
                    (list (plain-file "nug-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #89306B461D55FBB9F6A60C75463BA2AEE181FB3E8FA5F46CB2E1C29157ACA88A#)))")
                          (plain-file "nyarlothotep-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #C41C4703766F019CF43C8FBA3C7E284610799FBBF9875AB561AD7D8A74075AFE#)))"))))))

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
