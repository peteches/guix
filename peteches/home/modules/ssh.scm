;;; peteches/home/modules/ssh.scm — ~/.ssh/config for every managed host.
;;;
;;; Two entries per VM by convention:
;;;   "<name>"     → the static LAN IP, with user + identity-file.  Works
;;;                  on the LAN and is what `guix deploy' effectively uses.
;;;   "<name>.ts"  → <name>.spaniel-cordylus.ts.net, the Tailscale name.
;;;                  Works from anywhere; no user/identity needed because
;;;                  the "*" block and the agent cover it.
;;;
;;; Add both when provisioning a VM (see CLAUDE.md "Adding a New VM").
;;; The IPs here duplicate peteches/machines.scm and proxmox-vms.org —
;;; proxmox-vms.org is the authoritative inventory.
;;;
;;; Known gaps: `critical-grind-outline' has no .ts alias, and `pihole' has
;;; no .ts alias, though both VMs run Tailscale.
;;;
;;; `authorized-keys' here populates ~/.ssh/authorized_keys on the *desktops*
;;; from the co-located ssh-authorized-keys file (kept next to this module so
;;; `local-file' can find it).  The VMs' authorized keys are set separately,
;;; inline in (peteches systems vm-base).

(define-module (peteches home modules ssh)
  #:use-module (gnu home services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (base-ssh-service))

(define-public base-ssh-service
  (service home-openssh-service-type
	   (home-openssh-configuration
	    (add-keys-to-agent "yes")
	    (authorized-keys (list (local-file "ssh-authorized-keys")))
	    (hosts (list
		    (openssh-host
		     (name "*")
		     (extra-content (string-append
				     "    ControlMaster auto\n"
				     "    ControlPath ~/.ssh/ctrl-%C\n"
				     "    ControlPersist 10m\n"
				     "    CanonicalizeHostname always\n")))
		    (openssh-host
		     (name "proxmox1")
		     (host-name "proxmox1.spaniel-cordylus.ts.net")
		     (user "root"))
		    (openssh-host
		     (name "nyarlothotep.ts")
		     (host-name "nyarlothotep.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "nug.ts")
		     (host-name "nug.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "prometheus.ts")
		     (host-name "prometheus.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "grafana.ts")
		     (host-name "grafana.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "prometheus")
		     (host-name "192.168.51.187")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "grafana")
		     (host-name "192.168.51.188")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "pihole")
		     (host-name "192.168.51.189")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "loki.ts")
		     (host-name "loki.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "loki")
		     (host-name "192.168.51.190")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "git.ts")
		     (host-name "git.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "git")
		     (host-name "192.168.51.191")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "jellyfin.ts")
		     (host-name "jellyfin.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "jellyfin")
		     (host-name "192.168.51.192")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "caddy.ts")
		     (host-name "caddy.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "caddy")
		     (host-name "192.168.51.193")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "prowlarr.ts")
		     (host-name "prowlarr.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "prowlarr")
		     (host-name "192.168.51.194")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "arr.ts")
		     (host-name "arr.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "arr")
		     (host-name "192.168.51.195")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "downloads.ts")
		     (host-name "downloads.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "downloads")
		     (host-name "192.168.51.196")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "rustdesk.ts")
		     (host-name "rustdesk.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "rustdesk")
		     (host-name "192.168.51.197")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "concourse-db.ts")
		     (host-name "concourse-db.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "concourse-db")
		     (host-name "192.168.51.198")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "concourse-web01.ts")
		     (host-name "concourse-web01.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "concourse-web01")
		     (host-name "192.168.51.199")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "concourse-worker01.ts")
		     (host-name "concourse-worker01.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "concourse-worker01")
		     (host-name "192.168.51.200")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "vault")
		     (host-name "192.168.51.201")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "vault.ts")
		     (host-name "vault.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "critical-grind-campaign")
		     (host-name "192.168.51.202")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "critical-grind-outline")
		     (host-name "192.168.51.203")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519"))
		    (openssh-host
		     (name "plane.ts")
		     (host-name "plane.spaniel-cordylus.ts.net"))
		    (openssh-host
		     (name "plane")
		     (host-name "192.168.51.204")
		     (user "peteches")
		     (identity-file "~/.ssh/id_ed25519")))))))
