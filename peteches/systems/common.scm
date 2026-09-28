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
;;;   %authorize-coordinator-key  trusts nyarlothotep + claude-workstation to
;;;                             push signed store items to a host, and
;;;                             registers guix-build's guix-publish (port
;;;                             3000) as a substitute server.  Every VM gets
;;;                             this via make-vm-os; without it `guix deploy'
;;;                             has to rebuild everything on the target.
;;;                             dagon (nug's successor desktop) isn't listed
;;;                             yet -- add it once dagon's own
;;;                             /etc/guix/signing-key.pub is known.

(define-module (peteches systems common)
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

;; Authorize deploy coordinators (nyarlothotep and claude-workstation) to
;; push store items to all VMs, and register guix-build's guix-publish
;; (nug's build-offload/publish successor) as a substitute server.
;;
;; nug-coordinator.pub was dropped following nug's decommission (reinstalled
;; as the bare proxmox3 host -- no coordinator key survives it).
;;
;; dagon added 2026-09-27: deploying from dagon to claude-workstation hit the
;; same `guix deploy: error: unauthorized public key' as claude-workstation's
;; own gap below did against pihole -- dagon's /etc/guix/signing-key.pub was
;; never added here. A manual `guix archive --authorize' on the target only
;; "fixes" it until the next deploy, since that deploy's own /etc-populate
;; step regenerates /etc/guix/acl from this declarative list and silently
;; reverts the manual edit -- this list is the only fix that sticks.
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
                    (append (list "http://guix-build.spaniel-cordylus.ts.net:3000")
                            %default-substitute-urls))
                   (authorized-keys
                    (list (local-file "./guix-build-substitute-key.pub")
                          (plain-file "nyarlothotep-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #C41C4703766F019CF43C8FBA3C7E284610799FBBF9875AB561AD7D8A74075AFE#)))")
                          (plain-file "claude-workstation-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #EFED7FDADFFF4E2559977AFD10310E21C4EEF7685C6297595D5333CBEF037EDE#)))")
                          (plain-file "dagon-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #0762DB77028F0E513B7E4CE6CBCAC22E9E49D80AC092072EB2F959D99B6B6437#)))"))))))
