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
 ;; wired here: the profile's `rec` is a wrapper that captures a
 ;; desktop's mic (EXTRA-PACKAGES, see the note below), and DEEPGRAM_API_KEY is exported into the shell at
 ;; startup from the sops secret the system decrypts to
 ;; /run/secrets/deepgram-api-key (SECRET-ENV-VARS -- see the
 ;; #:sops-secrets entry in peteches/systems/claude-workstation.scm and
 ;; docs/secrets-management.org for creating the encrypted file).  The
 ;; key is never baked into the world-readable store.
 #:pi-extensions (list pi-dictate)
 #:extra-packages (list claude-workstation-peteches-scripts)
 #:secret-env-vars '(("DEEPGRAM_API_KEY" . "/run/secrets/deepgram-api-key"))
 ;; This VM has no local audio input, so the profile's `rec` (which
 ;; pi-dictate spawns for audio capture) is a wrapper that points
 ;; PULSE_SERVER at a desktop's PulseAudio session instead -- the
 ;; desktops expose their session servers over TCP at Hyprland session
 ;; start, see configs/hypr/peteches/autostart.lua.  The wrapper is the
 ;; claude-workstation-peteches-scripts package, which provides bin/rec
 ;; (so it IS the profile's rec -- a plain guix store path, no
 ;; ~/.local/bin) and bakes the real sox rec's store path into its
 ;; final exec line so it does not exec itself.  It does NOT
 ;; auto-detect which desktop: both can be connected at once and
 ;; guessing is confusing, so the choice is EXPLICIT.  The /dictate pi
 ;; slash command (a prompt template, installed below at
 ;; ~/.pi/agent/prompts/dictate.md) writes the choice to
 ;; ~/.config/dictate/host, which the wrapper reads per invocation.  The
 ;; PULSE_SERVER below is only the fallback for when no choice has been
 ;; made yet -- a static default (nyarlothotep), not a switch.
 #:extra-services
 (list (simple-service 'pi-dictate-prompt
                       home-files-service-type
                       (list (list ".pi/agent/prompts/dictate.md"
                                   (local-file "claude-workstation-peteches-dictate-prompt"))))
       (simple-service 'pi-dictate-pulse-server
                       home-environment-variables-service-type
                       '(("PULSE_SERVER" . "nyarlothotep.spaniel-cordylus.ts.net"))))
 #:herdr-spaces %peteches-herdr-spaces
 #:automation-ssh-identity %automation-ssh-key))

claude-workstation-peteches-home

