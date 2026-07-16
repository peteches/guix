;;; peteches/deploy.scm — LEGACY.  Do not edit; do not deploy with this.
;;;
;;; This is a superseded `guix deploy' manifest.  It predates
;;; (peteches machines) and lists only 5 of the machines that exist today
;;; (prometheus, grafana, loki, pihole, git), with the machine records
;;; duplicated verbatim.  Deploying it would leave every other VM
;;; untouched while silently succeeding, which is why it is kept inert
;;; rather than wired into anything.
;;;
;;; The supported entry points are:
;;;   scripts/deploy.scm            — wrapper with --hosts filtering
;;;   (peteches machines)           — %all-machines, the single source of truth
;;;
;;; NOTE: docs/backups.org and proxmox-vms.org still tell you to run
;;; `guix deploy -L . peteches/deploy.scm'.  That guidance is stale —
;;; use scripts/deploy.scm instead.  This file is a deletion candidate
;;; once those docs are corrected.

(define-module (peteches deploy))

(use-modules (gnu machine)
             (gnu machine ssh)
             (peteches systems prometheus)
             (peteches systems grafana)
             (peteches systems loki)
             (peteches systems pihole)
             (peteches systems git))

(list
 (machine
  (operating-system prometheus-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.187")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINxQwnyL7Fm08s8UwzXXuSwbahwySM//Jv2jxpfmryHj")
    (system "x86_64-linux")
    (user "peteches")       ; must have passwordless sudo in sudoers
    (identity "/home/peteches/.ssh/id_ed25519"))))
 (machine
  (operating-system grafana-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.188")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA3sVMg8QH+g6Xtj2NmIzV90gbkSPMiCnlaaAJx+a7tG")               ; fill in after first boot: ssh-keyscan 192.168.51.188
    (system "x86_64-linux")
    (user "peteches")
    (identity "/home/peteches/.ssh/id_ed25519"))))
 (machine
  (operating-system loki-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.190")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII2AxOh6ksCO2dnP+S92mNnOR76J/ewMW1QrhkSvN/Xx")            ; fill in after first boot: ssh-keyscan 192.168.51.190
    (system "x86_64-linux")
    (user "peteches")
    (identity "/home/peteches/.ssh/id_ed25519"))))
 (machine
  (operating-system pihole-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.189")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5+ZCQgM0b8HJjRmzN2bpDkbtwqdbgop+g4ZiB4ZqjH")    ; fill in after first boot: ssh-keyscan 192.168.51.189
    (system "x86_64-linux")
    (user "peteches")
    (identity "/home/peteches/.ssh/id_ed25519"))))
 (machine
  (operating-system git-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.191")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII/nIMuSdo5NHolPHogjR+xrudcnpLFROLYc6fpL+fkp")                   ; fill in after first boot: ssh-keyscan 192.168.51.191
    (system "x86_64-linux")
    (user "peteches")
    (identity "/home/peteches/.ssh/id_ed25519")))))
