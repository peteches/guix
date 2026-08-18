;;; Home config for the `peteches' account on claude-workstation.
;;;
;;; Run ON THE VM, as the peteches user:
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-peteches.scm
;;;
;;; Bootstrap (first time only — this config clones the OTHER repos, but the
;;; guix checkout it lives in must already exist): clone the guix repo by hand
;;; into ~/area_51/guix, then run the command above.
;;;
;;; Evaluates to a bare `home-environment' as its final expression, which is
;;; what `guix home' consumes.

(define-module (peteches home configs claude-workstation-peteches)
  #:use-module (guix gexp)
  #:use-module ((peteches packages comfyui-mcp) #:select (node-comfyui-mcp))
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
 ;; nug's always-on ComfyUI instance, reached via the Caddy reverse proxy
 ;; (peteches/systems/caddy.scm) rather than nug's Tailscale host directly.
 #:mcp-env '(("COMFYUI_URL" . "https://comfyui.ts.peteches.co.uk"))
 #:mcp-servers
 (list (home-claude-mcp-server
        (name "comfyui")
        (command (file-append node-comfyui-mcp "/bin/comfyui-mcp"))))
 #:herdr-spaces %peteches-herdr-spaces
 #:automation-ssh-identity %automation-ssh-key))

claude-workstation-peteches-home

