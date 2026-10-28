;;; peteches/machines.scm — `guix deploy' targets for every managed VM.
;;;
;;; Single source of truth for deployment: each VM's operating-system
;;; record (from (peteches systems …)) is paired with the SSH details
;;; needed to reach it.  %all-machines is what `scripts/deploy.scm' filters
;;; and what `guix deploy -e' ultimately evaluates.
;;;
;;; Conventions:
;;;   - One `define-public <name>-machine' per VM, added to %all-machines.
;;;   - `host-name' is the VM's Tailscale MagicDNS name
;;;     (<short-hostname>.spaniel-cordylus.ts.net, matching the ".ts" aliases
;;;     in (peteches home modules ssh)).  Every deployed VM runs
;;;     tailscale-service-type with no explicit --hostname override, so its
;;;     tailnet name is always its OS host-name's leading label.  Deploys
;;;     therefore work from anywhere the deploying machine's tailscale is up,
;;;     not just the home LAN; see proxmox-vms.org for the LAN IPs if you
;;;     need to fall back to them (e.g. tailscale itself is down).
;;;   - `user' is always "peteches", who has passwordless sudo via the
;;;     sudoers-file set in (peteches systems vm-base).
;;;   - `host-key' can only be filled in after the VM's first boot:
;;;         ssh-keyscan <ip>
;;;     Use a TODO placeholder until then; `guix deploy' verifies it and
;;;     refuses to connect on a mismatch.
;;;
;;; Desktops (dagon, nyarlothotep) are deliberately absent — they are
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
  #:use-module (peteches systems plane)
  #:use-module (peteches systems claude-workstation)
  #:use-module (peteches systems comfyui)
  #:use-module (peteches systems guix-build)
  #:use-module (peteches systems agents)
  )

;; SSH private key used to connect out to every machine below. Dagon and
;; nyarlothotep (the desktop deploy coordinators) each have their own
;; personal keypair at the conventional path, already enrolled in every
;; VM's %vm-peteches-authorized-keys (vm-base.scm). claude-workstation has
;; no such personal key, but it does decrypt the private half of the
;; fleet-wide "peteches automation" keypair at boot (see
;; %automation-authorized-key-secret in peteches/systems/vm-base.scm) --
;; every VM already trusts its public half for the peteches login. Use it
;; when present so `guix deploy'/scripts/deploy.scm works unmodified from
;; whichever host runs it.
(define %deploy-identity
  (if (file-exists? "/run/secrets/peteches-automation-ssh-key")
      "/run/secrets/peteches-automation-ssh-key"
      "/home/peteches/.ssh/id_ed25519"))

(define-public prometheus-machine
  (machine
   (operating-system prometheus-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "prometheus.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINxQwnyL7Fm08s8UwzXXuSwbahwySM//Jv2jxpfmryHj")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public grafana-machine
  (machine
   (operating-system grafana-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "grafana.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA3sVMg8QH+g6Xtj2NmIzV90gbkSPMiCnlaaAJx+a7tG")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public loki-machine
  (machine
   (operating-system loki-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "loki.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII2AxOh6ksCO2dnP+S92mNnOR76J/ewMW1QrhkSvN/Xx")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public pihole-machine
  (machine
   (operating-system pihole-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "pihole.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5+ZCQgM0b8HJjRmzN2bpDkbtwqdbgop+g4ZiB4ZqjH")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public git-machine
  (machine
   (operating-system git-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "git.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII/nIMuSdo5NHolPHogjR+xrudcnpLFROLYc6fpL+fkp")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public jellyfin-machine
  (machine
   (operating-system jellyfin-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "jellyfin.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO0K7C2Fom+JtznRuvkCn1oIrjMy5ASD9tE5Ag8buO2Q")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public caddy-machine
  (machine
   (operating-system caddy-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "caddy.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFIxCigw2WFpZwTOWa075uT1IdGMbdCFGs4tCsNzNEz") ; fill in after new-vm Phase 6: ssh-keyscan 192.168.51.193
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public prowlarr-machine
  (machine
   (operating-system prowlarr-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "prowlarr.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHiYZARBCM47v7xWtaEhYRQNwfHK0ch6UnzOlaqnhIyA")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public arr-machine
  (machine
   (operating-system arr-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "arr.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILv1EPh7kKTpPwuOBQPvyPiJ1XZ5Nd7SRzYMNEBewtNv")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public downloads-machine
  (machine
   (operating-system downloads-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "downloads.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF1K5LlMCN5seUpx5CRZOmZHvi7JR0NbijQtACHbBGaC")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))


(define-public rustdesk-machine
  (machine
   (operating-system rustdesk-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "rustdesk.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKcfUmsDVPcwbgd52PCaDDKyTMW/usAXACJHGg9cu2Wu")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public concourse-db-machine
  (machine
   (operating-system concourse-db-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "concourse-db.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdDypH5/tQh+ZXp5vNk8bnADKgSJ03GglQRc4mWfkMt")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public concourse-web01-machine
  (machine
   (operating-system concourse-web01-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "concourse-web01.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBi4OydXz+VS2GhJ3tG8SEbI8MtY9C62iGYD3DBjYGsq")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public concourse-worker01-machine
  (machine
   (operating-system concourse-worker01-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "concourse-worker01.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFaB9+BPAly8+5hdeufEvQFzr+XhJSND9LxMHRqVbE7B")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public vault-machine
  (machine
   (operating-system vault-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "vault.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIInM3jubj/B/ghMJumiBPKtY3AFAj4NCzzoFa57RXdWc")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

;; The critical-grind VMs depend on the `critical-grind' channel -- a private
;; GitHub repo fetched over SSH that is only pulled on machines that load the
;; read-only deploy key into an ssh-agent (see peteches/channels/).  Their
;; system modules therefore cannot load on a host without the channel, so the
;; machines are defined conditionally: #f when the channel is unavailable, and
;; excluded from %all-machines.  This keeps (peteches machines) -- and hence
;; scripts/deploy.scm -- loadable on every host, not just the ones that can
;; reach the channel.
(define (maybe-critical-grind-machine system-module os-symbol host-name host-key)
  "Return a managed-host machine for the OS that SYSTEM-MODULE exports under
OS-SYMBOL, or #f when that module (i.e. the critical-grind channel) cannot be
loaded on this host."
  (let ((os (false-if-exception
             (and=> (resolve-interface system-module)
                   (lambda (mod) (module-ref mod os-symbol))))))
    (and os
         (machine
          (operating-system os)
          (environment managed-host-environment-type)
          (configuration
           (machine-ssh-configuration
            (host-name host-name)
            (host-key host-key)
            (system "x86_64-linux")
            (user "peteches")
            (identity %deploy-identity)))))))

(define-public critical-grind-outline-machine
  (maybe-critical-grind-machine
   '(peteches systems critical-grind-outline)
   'critical-grind-outline-os
   "critical-grind-outline.spaniel-cordylus.ts.net"
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJMkUD28mvqVRXcx+uvbPdahm+DEfhl6EvbIudm6KhyM"))

(define-public plane-machine
  (machine
   (operating-system plane-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "plane.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHB4zbOzy5fOWq/js8TgZGi1CIXPOAHMkR4imLhKBYYt")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public critical-grind-campaign-machine
  (maybe-critical-grind-machine
   '(peteches systems critical-grind-campaign)
   'critical-grind-campaign-os
   "critical-grind-campaign.spaniel-cordylus.ts.net"
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIZyL51uII6JZ9C75dHa0cWYHWJp5qgdddbWp+E1UnME"))

(define-public claude-workstation-machine
  (machine
   (operating-system claude-workstation-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     ;; TEMPORARY (2026-10-27): LAN IP until the tailnet name is reclaimed
     ;; from the destroyed node's stale entry (the new VM is
     ;; `claude-workstation-1` until then). Revert to
     ;; claude-workstation.spaniel-cordylus.ts.net once the old node is
     ;; removed and the hostname is set back.
     (host-name "192.168.51.205")
     ;; Re-keyed 2026-10-27: the VM was rebuilt from the MinIO image after
     ;; the 2026-10-27 terraform incident; this is the new VM's host key
     ;; (ssh-keyscan 192.168.51.205).
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAPxQTffohUYNtxPBwcpGvIhm/niIQ5478vfzZzL0o5m")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

;; Both booted post proxmox3 reinstall (the reinstalled nug); host-keys below
;; are real (ssh-keyscan'd), not placeholders.
(define-public comfyui-machine
  (machine
   (operating-system comfyui-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "comfyui.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBHNiC2tYA/0tDZxVmDLUrYJcQNqDOv84wNexSRI7zP")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public guix-build-machine
  (machine
   (operating-system guix-build-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "guix-build.spaniel-cordylus.ts.net")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKSOGKH6AeVlj1WQhSuzT6ni0cpzqcdPjUaFVufYOCqt")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public agents-machine
  (machine
   (operating-system agents-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "agents.spaniel-cordylus.ts.net")
     ;; TODO: ssh-keyscan 192.168.51.208 after first boot and replace this.
     ;; The VM pipeline provisions the host; guix deploy refuses on mismatch.
     (host-key "TODO-REPLACE-WITH-SSH-KEYSCAN-OUTPUT")
     (system "x86_64-linux")
     (user "peteches")
     (identity %deploy-identity)))))

(define-public %all-machines
  ;; filter identity drops the critical-grind machines when they are #f
  ;; (their channel is unavailable on this host).
  (filter identity
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
                comfyui-machine
                guix-build-machine
                agents-machine
                )))
