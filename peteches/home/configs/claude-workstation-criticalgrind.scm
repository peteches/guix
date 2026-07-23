;;; Home config for the `criticalgrind' account on claude-workstation.
;;;
;;; Run ON THE VM, as the criticalgrind user:
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-criticalgrind.scm
;;;
;;; Bootstrap: clone the guix repo by hand into ~/area_51/guix first, then run
;;; the command above.
;;;
;;; Adds the Critical Grind tooling on top of the shared constructor: the Go
;;; toolchain plus the Plane and Outline MCP servers.  The non-secret MCP env
;;; is set here; PLANE_API_KEY / OUTLINE_API_KEY are secrets and must arrive
;;; via sops once the VM's age key exists (see the module header).
;;;
;;; Evaluates to a bare `home-environment' as its final expression.

(define-module (peteches home configs claude-workstation-criticalgrind)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; Import packages directly instead of `specification->package'.  A top-level
  ;; `specification->package' fires `fold-packages' over `%package-module-path',
  ;; which `guix deploy'/`guix system' populate with the repo (`-L .'); the scan
  ;; then re-enters this half-loaded module tree and every module fails to bind.
  ;; See the note in (peteches home modules claude-workstation).
  #:use-module ((peteches packages mcp) #:select (plane-mcp-server mcp-outline))
  #:use-module ((gnu packages golang) #:select (go))
  #:use-module ((gnu packages golang-apps) #:select (gopls))
  #:use-module (peteches home modules claude-workstation)
  #:use-module (peteches home modules claude))

;; EDIT ME: the repos this account works on.  critical-grind-campaign is a
;; channel served over smart HTTP (no key needed); see CLAUDE.md.
(define %criticalgrind-repos
  '(("critical-grind-campaign"
     "git@git.peteches.co.uk:critical-grind-campaign")))

;; TODO: set the git identity you want commits from this account to carry.
(define-public claude-workstation-criticalgrind-home
  (make-claude-workstation-home
 #:git-name "Critical Grind"
 #:git-email "criticalgrind@peteches.co.uk"
 #:repos %criticalgrind-repos
 ;; plane-mcp-server and mcp-outline are NOT in the profile: they propagate
 ;; conflicting python-pyjwt versions (2.13.0 vs 2.10.1) and guix refuses to
 ;; union them.  They don't need to be on PATH — the MCP servers below launch
 ;; them by absolute store path via file-append, so the profile only needs the
 ;; Go toolchain the account actually uses interactively.
 #:extra-packages (list go gopls)
 ;; Non-secret only.  The Plane SDK appends /api/v1 to PLANE_BASE_URL itself.
 #:mcp-env '(("PLANE_BASE_URL"       . "https://plane.ts.peteches.co.uk")
             ("PLANE_WORKSPACE_SLUG" . "critical-grind")
             ("OUTLINE_API_URL"      . "https://outline.ts.peteches.co.uk/api"))
 #:mcp-servers
 (list (home-claude-mcp-server
        (name "plane")
        (command (file-append plane-mcp-server "/bin/plane-mcp-server"))
        (args (list "stdio")))
       (home-claude-mcp-server
        (name "outline")
        (command (file-append mcp-outline "/bin/mcp-outline"))))))

claude-workstation-criticalgrind-home

