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
;;;   %authorize-coordinator-key  trusts the deploy coordinators (dagon,
;;;                             nyarlothotep, agents) to push signed store
;;;                             items to a host, and registers guix-build's
;;;                             guix-publish (port 3000) as a substitute
;;;                             server.  Every VM gets this via make-vm-os.
;;;                             It is NOT about rebuilding -- `guix deploy'
;;;                             sends the closure either way -- it is what
;;;                             makes the LAST deploy step, the live
;;;                             upgrade-shepherd-services archive send, stop
;;;                             failing with "unauthorized public key"; see
;;;                             the long comment on the definition below and
;;;                             docs/fleet-deployment.org §4.

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

;; Authorize the deploy coordinators' guix-daemon signing keys -- dagon,
;; nyarlothotep and agents -- so they can push store items to every VM, and
;; register guix-build's guix-publish (nug's build-offload/publish successor)
;; as a substitute server.
;;
;; WHY a declarative list at all: `guix deploy' does authorize the
;; coordinator's key on the target by itself ((gnu machine ssh)'s `authorize?'
;; field, default #t, runs remote-authorize-signing-key before anything else),
;; so this list is not what lets a deploy connect or transfer. What it fixes
;; is the ORDER: the switch-to-system that follows that authorization rebuilds
;; /etc/guix/acl from THIS declarative list, so any key not listed here is
;; revoked again before the final upgrade-shepherd-services archive send --
;; which is the `guix deploy: error: unauthorized public key' failure mode
;; documented in docs/fleet-deployment.org §4, always on the last step, with
;; the generation already switched correctly. A hand-run `guix archive
;; --authorize' on the target only "fixes" it until the next deploy wipes it.
;;
;; History of the entries below:
;;   * nug -- dropped after nug's decommission (reinstalled as the bare
;;     proxmox3 host; no coordinator key survives it).
;;   * claude-workstation -- added 2026-08-22, because deploys run from there
;;     (via the automation SSH key) hit exactly that gap against pihole.
;;     REMOVED 2026-11-05 in the same change that moved the private half of
;;     the peteches automation keypair off that box to `agents': with no
;;     outbound identity left (its ~/.ssh has no id_* file, so %deploy-identity
;;     in peteches/machines.scm falls back to a path that does not exist
;;     there), it is no longer a deploy origin, so trusting its key fleet-wide
;;     is dead weight. It was stale as well: that VM was rebuilt from the
;;     MinIO image on 2026-10-27, and /etc/guix/signing-key.pub is baked into
;;     the image -- the committed q no longer matches the live file (checked
;;     by hashing both). The identical stale copy is still in
;;     peteches/systems/guix-build.scm's guix-offload-signing-keys, so any
;;     archive that box pushes to guix-build during offload is refused there
;;     too -- offload from claude-workstation has to fall back to local builds
;;     (that box's /var/log/messages carries no offload activity at all,
;;     which is consistent, not proof).
;;   * dagon -- added 2026-09-27 for the same gap seen from dagon.
;;   * agents -- added 2026-11-05, now that it holds that automation key and
;;     is therefore a deploy origin: its own /etc/guix/signing-key.pub (fetched
;;     live from the box) has to be trusted everywhere or every deploy it runs
;;     ends in the §4 failure.  Re-verify this key against `agents' after any
;;     image rebuild or re-provision -- the same 2026-10-27 lesson above.
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
                          (plain-file "agents-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #F4004CE8FDDF38E595E3E3F729AAA8A859A9668426D9A97625D9BB06F3E92505#)))")
                          (plain-file "dagon-coordinator.pub"
                                      "(public-key (ecc (curve Ed25519) (q #0762DB77028F0E513B7E4CE6CBCAC22E9E49D80AC092072EB2F959D99B6B6437#)))"))))))
