;;; peteches/home/modules/pi.scm — home service type for the `pi` coding
;;; agent (https://pi.dev, packaged in peteches/packages/pi-coding-agent.scm).
;;;
;;; Mirrors (peteches home modules claude)'s config-directory mechanism:
;;; symlinks each child of CONFIG-DIRECTORY into ~/.pi/agent/ -- pi's
;;; getAgentDir() (dist/config.js) resolves to join(homedir(), ".pi",
;;; "agent"), NOT ~/.pi/ itself; auth.json, models-store.json and
;;; models.json all live there. Currently that's just models.json
;;; (configs/pi/defaults/models.json), which declares
;;; custom OpenAI-completions-compatible providers/models per pi's
;;; core/model-config.js schema — pi loads this file itself at startup, so
;;; unlike Claude Code's ~/.claude.json there is no runtime-mutated state
;;; here to protect and no activation script is needed.
;;;
;;; The default models.json wires up nug's koboldcpp server as a custom
;;; provider. Its cert (Let's Encrypt, CN nug.peteches.co.uk) doesn't cover
;;; the Tailscale IPv6 literal the baseUrl connects through, so pi's own
;;; HTTPS calls need certificate verification disabled — see the
;;; `pi-koboldcpp' shell function this module installs, which sets
;;; NODE_TLS_REJECT_UNAUTHORIZED=0 only for that wrapped invocation rather
;;; than exporting it into the whole shell environment.

(define-module (peteches home modules pi)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:export (home-pi-service-type
            home-pi-configuration))

(define-record-type* <home-pi-configuration>
  home-pi-configuration make-home-pi-configuration
  home-pi-configuration?
  (config-directory home-pi-configuration-config-directory
                    (default #f)))

(define (directory-children directory)
  "Return the non-special immediate children of DIRECTORY."
  (filter (lambda (e) (not (member e '("." ".."))))
          (scandir directory)))

(define (home-pi-entry dir entry)
  (let ((src (string-append dir "/" entry)))
    (list (string-append ".pi/agent/" entry)
          (local-file src #:recursive? (file-is-directory? src)))))

(define (home-pi-files-service config)
  (let ((dir (home-pi-configuration-config-directory config)))
    (if dir
        (map (lambda (entry) (home-pi-entry dir entry))
             (directory-children dir))
        '())))

(define %pi-koboldcpp-bashrc "\
# koboldcpp's cert (CN nug.peteches.co.uk) doesn't cover the Tailscale
# IPv6 literal models.json points pi at, so wrap invocations that need it
# rather than disabling TLS verification for the whole shell.
pi-koboldcpp() {
  NODE_TLS_REJECT_UNAUTHORIZED=0 command pi --provider koboldcpp \"$@\"
}
")

(define-public home-pi-koboldcpp-bashrc
  (plain-file "pi-koboldcpp.bash" %pi-koboldcpp-bashrc))

(define-public home-pi-service-type
  (service-type
   (name 'home-pi)
   (description "Manage the pi coding-agent CLI's ~/.pi config, e.g. models.json.")
   (extensions
    (list (service-extension home-files-service-type
                             home-pi-files-service)))
   (default-value (home-pi-configuration))))
