;;; Home config for the `peteches' account on claude-workstation.
;;;
;;; NOT applied via a standalone `guix home reconfigure' on the VM --
;;; claude-workstation-os wires this value straight into its own
;;; `guix-home-service-type' instance (see peteches/systems/claude-
;;; workstation.scm), so a system-level `guix system reconfigure'/`guix
;;; deploy' (scripts/deploy.scm) activates it as the guix-home-peteches
;;; shepherd service in the same run. Editing this file just needs a
;;; redeploy of claude-workstation, same as any other change to that system.
;;;
;;; (Manual fallback, e.g. to test a change before a full redeploy: run ON
;;; THE VM as the peteches user --
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-peteches.scm
;;; -- which needs the guix repo cloned by hand into ~/area_51/guix first,
;;; since this config clones the OTHER repos but not the one it lives in.)
;;;
;;; Evaluates to a bare `home-environment' as its final expression, which is
;;; what `guix home' consumes.

(define-module (peteches home configs claude-workstation-peteches)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module ((peteches packages comfyui-mcp) #:select (node-comfyui-mcp))
  #:use-module ((peteches packages pi-dictate) #:select (pi-dictate))
  #:use-module ((peteches packages claude-workstation-scripts) #:select (claude-workstation-peteches-scripts))
  #:use-module ((gnu packages audio) #:select (sox))
  #:use-module (peteches home modules claude-workstation)
  #:use-module (peteches home modules claude))

;; Repos pre-cloned into ~/area_51/<name>.  Each entry is (NAME URL).
;; EDIT ME: add the repos this account works on, with real clone URLs.
;; SSH URLs need this user's key + known_hosts on the VM at reconfigure time;
;; a clone that can't authenticate just warns and retries next reconfigure.
(define %peteches-repos
  '(("guix" "git@github.com:peteches/guix")
    ("bas"  "git@git.peteches.co.uk:bas")))

;; The 4 standing herdr spaces for this account's `herdr --remote' session
;; (see configs/hypr/peteches/apps/herdr.lua, which now only autostarts
;; this one account rather than one alacritty window per account). Each
;; entry is (NAME RELATIVE-PATH REMOTE-USER) -- see
;; herdr-spaces-bootstrap-script in (peteches home modules
;; claude-workstation) for the exact semantics; REMOTE-USER spaces are
;; tracked on THAT account's own herdr server, reached over loopback SSH
;; using %automation-ssh-key below.
(define %peteches-herdr-spaces
  '(("guix" "area_51/guix" #f)
    ("bas"  "area_51/bas"  #f)
    ("critical-grind-campaign" "area_51/critical-grind-campaign" "criticalgrind")
    ("ygocloud" "area_51/ygocloud" "ygo")))

;; Decrypted at system boot from secrets/hosts/claude-workstation/
;; peteches-automation-ssh.yaml (see peteches/systems/claude-workstation.scm
;; and docs/secrets-management.org) -- the private half of the automation
;; keypair whose public half every VM authorizes for peteches (and, here,
;; also criticalgrind/ygo -- see #:automation-key-extra-users in
;; peteches/systems/claude-workstation.scm).
(define %automation-ssh-key "/run/secrets/peteches-automation-ssh-key")

(define-public claude-workstation-peteches-home
  (make-claude-workstation-home
 #:git-name "Pete 'Peteches' McCabe"
 #:git-email "claude@peteches.co.uk"
 #:repos %peteches-repos
 ;; The comfyui VM's always-on ComfyUI instance (nug's successor, following
 ;; nug's decommission), reached via the Caddy reverse proxy
 ;; (peteches/systems/caddy.scm) rather than its Tailscale host directly.
 #:mcp-env '(("COMFYUI_URL" . "https://comfyui.ts.peteches.co.uk"))
 #:mcp-servers
 (list (home-claude-mcp-server
        (name "comfyui")
        (command (file-append node-comfyui-mcp "/bin/comfyui-mcp"))))
 ;; Voice dictation in pi (alt+m): the pi-dictate extension, packaged in
 ;; peteches/packages/pi-dictate.scm.  Its two non-npm runtime needs are
 ;; wired here: sox supplies the `rec` binary it spawns for audio capture
 ;; (EXTRA-PACKAGES), and DEEPGRAM_API_KEY is exported into the shell at
 ;; startup from the sops secret the system decrypts to
 ;; /run/secrets/deepgram-api-key (SECRET-ENV-VARS -- see the
 ;; #:sops-secrets entry in peteches/systems/claude-workstation.scm and
 ;; docs/secrets-management.org for creating the encrypted file).  The
 ;; key is never baked into the world-readable store.
 #:pi-extensions (list pi-dictate)
 #:extra-packages (list sox claude-workstation-peteches-scripts)
 #:secret-env-vars '(("DEEPGRAM_API_KEY" . "/run/secrets/deepgram-api-key"))
 ;; This VM has no local audio input, so pi-dictate's `rec` is shadowed
 ;; by a wrapper that captures a desktop's PulseAudio session instead --
 ;; the desktops expose their session servers over TCP at Hyprland
 ;; session start, see configs/hypr/peteches/autostart.lua.  The wrapper
 ;; (packaged in claude-workstation-scripts.scm with an explicit chmod
 ;; phase, since the daemon's add-to-store strips exec bits) picks WHICH
 ;; desktop dynamically per dictation session: it inspects the VM's
 ;; established SSH connections (herdr --remote or plain ssh) and
 ;; captures the mic of whichever desktop is driving the VM right now,
 ;; so dagon and nyarlothotep alternate without any reconfigure (see
 ;; the wrapper's own header for the both/none tie-breaks).  It is
 ;; exposed as ~/.local/bin/rec -- first on PATH -- as a symlink to the
 ;; executable store file (a plain local-file here would land in the
 ;; store mode 444 and be unrunnable) and execs the profile's rec (sox,
 ;; EXTRA-PACKAGES) with -d pulse.  The PULSE_SERVER below is only the
 ;; fallback for when NEITHER desktop has a live session.
 #:extra-services
 (list (simple-service 'pi-dictate-rec-wrapper
                       home-files-service-type
                       (list (list ".local/bin/rec"
                                   (file-append claude-workstation-peteches-scripts
                                                "/bin/rec-mic"))))
       (simple-service 'pi-dictate-pulse-server
                       home-environment-variables-service-type
                       '(("PULSE_SERVER" . "nyarlothotep.spaniel-cordylus.ts.net"))))
 #:herdr-spaces %peteches-herdr-spaces
 #:automation-ssh-identity %automation-ssh-key))

claude-workstation-peteches-home

