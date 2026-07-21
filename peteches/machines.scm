;;; peteches/machines.scm — `guix deploy' targets for every managed VM.
;;;
;;; Single source of truth for deployment: each VM's operating-system
;;; record (from (peteches systems …)) is paired with the SSH details
;;; needed to reach it.  %all-machines is what `scripts/deploy.scm' filters
;;; and what `guix deploy -e' ultimately evaluates.
;;;
;;; Conventions:
;;;   - One `define-public <name>-machine' per VM, added to %all-machines.
;;;   - `host-name' is the static LAN IP (see proxmox-vms.org for the
;;;     authoritative inventory).  VMs are addressed by IP, not DNS, so a
;;;     pihole outage cannot break deploys.
;;;   - `user' is always "peteches", who has passwordless sudo via the
;;;     sudoers-file set in (peteches systems vm-base).
;;;   - `host-key' can only be filled in after the VM's first boot:
;;;         ssh-keyscan <ip>
;;;     Use a TODO placeholder until then; `guix deploy' verifies it and
;;;     refuses to connect on a mismatch.
;;;
;;; Desktops (nug, nyarlothotep) are deliberately absent — they are
;;; reconfigured locally with `guix system reconfigure', not deployed.
;;;
;;; scripts/deploy.scm keeps its own %machine-names alist mapping these
;;; records back to their variable names.  Adding a machine here means
;;; adding it there too, or --hosts filtering will error with
;;; "Unknown machine".

(define-module (peteches machines)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (peteches systems prometheus)
  #:use-module (peteches systems grafana)
  #:use-module (peteches systems loki)
  #:use-module (peteches systems pihole)
  #:use-module (peteches systems git)
  #:use-module (peteches systems jellyfin)
  #:use-module (peteches systems caddy)
  #:use-module (peteches systems prowlarr)
  #:use-module (peteches systems arr)
  #:use-module (peteches systems downloads)
  #:use-module (peteches systems rustdesk)
  #:use-module (peteches systems concourse-db)
  #:use-module (peteches systems concourse-web01)
  #:use-module (peteches systems concourse-worker01)
  #:use-module (peteches systems vault)
  #:use-module (peteches systems critical-grind-outline)
  #:use-module (peteches systems plane)
  #:use-module (peteches systems critical-grind-campaign)
  #:use-module (peteches systems claude-workstation)
  )

(define-public prometheus-machine
  (machine
   (operating-system prometheus-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.187")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINxQwnyL7Fm08s8UwzXXuSwbahwySM//Jv2jxpfmryHj")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public grafana-machine
  (machine
   (operating-system grafana-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.188")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA3sVMg8QH+g6Xtj2NmIzV90gbkSPMiCnlaaAJx+a7tG")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public loki-machine
  (machine
   (operating-system loki-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.190")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII2AxOh6ksCO2dnP+S92mNnOR76J/ewMW1QrhkSvN/Xx")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public pihole-machine
  (machine
   (operating-system pihole-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.189")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5+ZCQgM0b8HJjRmzN2bpDkbtwqdbgop+g4ZiB4ZqjH")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public git-machine
  (machine
   (operating-system git-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.191")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII/nIMuSdo5NHolPHogjR+xrudcnpLFROLYc6fpL+fkp")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public jellyfin-machine
  (machine
   (operating-system jellyfin-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.192")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO0K7C2Fom+JtznRuvkCn1oIrjMy5ASD9tE5Ag8buO2Q")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public caddy-machine
  (machine
   (operating-system caddy-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.193")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFIxCigw2WFpZwTOWa075uT1IdGMbdCFGs4tCsNzNEz") ; fill in after new-vm Phase 6: ssh-keyscan 192.168.51.193
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public prowlarr-machine
  (machine
   (operating-system prowlarr-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.194")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHiYZARBCM47v7xWtaEhYRQNwfHK0ch6UnzOlaqnhIyA")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public arr-machine
  (machine
   (operating-system arr-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.195")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILv1EPh7kKTpPwuOBQPvyPiJ1XZ5Nd7SRzYMNEBewtNv")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public downloads-machine
  (machine
   (operating-system downloads-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.196")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF1K5LlMCN5seUpx5CRZOmZHvi7JR0NbijQtACHbBGaC")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))


(define-public rustdesk-machine
  (machine
   (operating-system rustdesk-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.197")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKcfUmsDVPcwbgd52PCaDDKyTMW/usAXACJHGg9cu2Wu")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public concourse-db-machine
  (machine
   (operating-system concourse-db-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.198")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdDypH5/tQh+ZXp5vNk8bnADKgSJ03GglQRc4mWfkMt")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public concourse-web01-machine
  (machine
   (operating-system concourse-web01-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.199")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBi4OydXz+VS2GhJ3tG8SEbI8MtY9C62iGYD3DBjYGsq")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public concourse-worker01-machine
  (machine
   (operating-system concourse-worker01-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.200")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFaB9+BPAly8+5hdeufEvQFzr+XhJSND9LxMHRqVbE7B")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public vault-machine
  (machine
   (operating-system vault-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.201")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIInM3jubj/B/ghMJumiBPKtY3AFAj4NCzzoFa57RXdWc")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public critical-grind-outline-machine
  (machine
   (operating-system critical-grind-outline-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.203")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJMkUD28mvqVRXcx+uvbPdahm+DEfhl6EvbIudm6KhyM")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public plane-machine
  (machine
   (operating-system plane-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.204")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHB4zbOzy5fOWq/js8TgZGi1CIXPOAHMkR4imLhKBYYt")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public critical-grind-campaign-machine
  (machine
   (operating-system critical-grind-campaign-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.202")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIZyL51uII6JZ9C75dHa0cWYHWJp5qgdddbWp+E1UnME")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public claude-workstation-machine
  (machine
   (operating-system claude-workstation-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.205")
     ;; TODO: ssh-keyscan 192.168.51.205 after first boot and replace this.
     ;; The VM pipeline provisions the host; guix deploy refuses on mismatch.
     (host-key "ssh-ed25519 AAAA-REPLACE-AFTER-FIRST-BOOT")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public %all-machines
  (list prometheus-machine
        grafana-machine
        loki-machine
        pihole-machine
        git-machine
        jellyfin-machine
        caddy-machine
        prowlarr-machine
        arr-machine
        downloads-machine
        rustdesk-machine
        concourse-db-machine
        concourse-web01-machine
        vault-machine
        concourse-worker01-machine
        critical-grind-outline-machine
        plane-machine
        critical-grind-campaign-machine
        claude-workstation-machine
        ))
