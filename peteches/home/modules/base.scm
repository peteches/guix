;;; peteches/home/modules/base.scm — the home-environment orchestrator.
;;;
;;; Exports `base-packages' and `base-services', which the two host configs
;;; in peteches/home/configs/ append their own extras onto.  Everything
;;; shared between nug and nyarlothotep belongs here or in one of the
;;; focused sibling modules (ssh, gpg, theming, ai, claude, mako, …), which
;;; this module composes.
;;;
;;; Three distinct sources of code, easy to confuse:
;;;   (gnu home services …)      — upstream Guix.
;;;   (peteches home services …) — the external `peteches' guix channel
;;;                                (pinned in peteches/channels/base.scm).
;;;                                NOT in this repo; to change a service
;;;                                type you must edit the channel and
;;;                                re-pin its commit.
;;;   (peteches home modules …)  — this directory: configuration *values*
;;;                                for those service types.
;;;
;;; Non-Scheme assets (configs/emacs, configs/hypr, configs/bin, …) are
;;; located through (peteches repository)'s `repo-directory' / `source-path'
;;; rather than relative paths.  See that module for why.
;;;
;;; Reconfigure with the `grhome' alias defined below, or:
;;;   guix home -L . reconfigure peteches/home/configs/$(hostname).scm

(define-module (peteches home modules base)
  ;; Guix / Home
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  ;; Packages
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages image)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages mozilla)
  #:use-module (peteches packages go-tools)
  #:use-module (peteches packages terraform)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Your feature modules
  #:use-module (peteches home services aws)
  #:use-module (peteches home services desktop)
  #:use-module (peteches home services emacs)
  #:use-module (peteches home services git)
  #:use-module (peteches home services password-store)
  #:use-module (peteches home services firefox)
  #:use-module (peteches home services hyprland)
  #:use-module (peteches home services wofi)
  #:use-module (peteches home services nyxt)
  ;; Shared config modules
  #:use-module (peteches home modules mako)
  #:use-module (peteches home modules firefox)
  #:use-module (peteches home modules git)
  #:use-module (peteches home modules ssh)
  #:use-module (peteches home modules gpg)
  #:use-module (peteches home modules syncthing)
  #:use-module (peteches home modules theming)
  #:use-module (peteches home modules ai)
  #:use-module (peteches home modules claude)
  ;; Your config fragments
  #:use-module (peteches packages gurps)
  #:use-module (peteches packages claude-code)
  #:use-module (containers claude)
  #:use-module (peteches packages rustdesk)
  #:use-module (peteches packages concourse)
  #:use-module (peteches packages proxmox-scripts)
  #:use-module (peteches packages desktop-scripts)

  #:use-module (peteches repository)
  ;; utilities
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  ;; Export
  #:export (base-packages
            base-services))

;; guile-lsp-server@0.4.8 propagates guile-3.0 (3.0.9) but the Guix system
;; profile is compiled with guile-3.0.11.  When both site-ccaches appear in
;; GUILE_LOAD_COMPILED_PATH, loading any (gnu …) module fails with
;; "incompatible bytecode version" causing a full recompile cascade.
;; Override the full dependency chain so all .go files in the home profile
;; are compiled with guile-3.0.11, matching the system profile.
(define guile-srfi-145/3.0.11
  (package/inherit guile-srfi-145
    (native-inputs (modify-inputs (package-native-inputs guile-srfi-145)
                    (replace "guile" guile-3.0.11)))))

(define guile-srfi-180/3.0.11
  (package/inherit guile-srfi-180
    (native-inputs (modify-inputs (package-native-inputs guile-srfi-180)
                    (replace "guile" guile-3.0.11)))
    (propagated-inputs (modify-inputs (package-propagated-inputs guile-srfi-180)
                         (replace "guile-srfi-145" guile-srfi-145/3.0.11)))))

(define guile-irregex/3.0.11
  (package/inherit guile-irregex
    (native-inputs (modify-inputs (package-native-inputs guile-irregex)
                    (replace "guile" guile-3.0.11)))
    (arguments (substitute-keyword-arguments (package-arguments guile-irregex)
                 ((#:phases phases)
                  #~(modify-phases #$phases
                      (delete 'check)
                      (delete 'check-installed)))))))

(define guile-scheme-json-rpc/3.0.11
  (package/inherit guile-scheme-json-rpc
    (inputs (modify-inputs (package-inputs guile-scheme-json-rpc)
              (replace "guile" guile-3.0.11)))
    (propagated-inputs (modify-inputs (package-propagated-inputs guile-scheme-json-rpc)
                         (replace "guile-srfi-145" guile-srfi-145/3.0.11)
                         (replace "guile-srfi-180" guile-srfi-180/3.0.11)))))

(define guile-lsp-server/3.0.11
  (package/inherit guile-lsp-server
    (native-inputs (modify-inputs (package-native-inputs guile-lsp-server)
                    (replace "guile" guile-3.0.11)))
    (propagated-inputs (modify-inputs (package-propagated-inputs guile-lsp-server)
                         (replace "guile" guile-3.0.11)
                         (replace "guile-scheme-json-rpc" guile-scheme-json-rpc/3.0.11)
                         (replace "guile-srfi-145" guile-srfi-145/3.0.11)
                         (replace "guile-srfi-180" guile-srfi-180/3.0.11)
                         (replace "guile-irregex" guile-irregex/3.0.11)))))

;; 1) Shared package set for all machines.
(define-public base-packages
  (list
   peteches-desktop-scripts
   alacritty
   claude-container
   emacs-anvil
   rustdesk
   dank-material-shell
   mako
   (specification->package "hyprlock")
   (specification->package "gsettings-desktop-schemas")
   (specification->package "beeper-bin")
   (specification->package "matugen")
   (specification->package "hyprcursor")
   (specification->package "xcur2png")
   (specification->package "curl")
   (specification->package "bzip2")
   (specification->package "dconf")
   (specification->package "qt5ct")
   (specification->package "qt6ct")
   (specification->package "adw-gtk3-theme")
   (specification->package "adwaita-icon-theme")
   (specification->package "font-nerd-symbols")
   (specification->package "font-nerd-jetbrains-mono")
   (specification->package "wl-clipboard")
   (specification->package "playerctl")
   (specification->package "ddcutil")
   (specification->package "pavucontrol-qt")
   recutils
   eza
   fly
   tcpdump
   netcat
   ncdu
   (list isc-bind "utils")
   bluez
   git
   peteches-gpg-for-git
   gurpscharactersheet
   gnupg
   jq
   slurp
   wf-recorder
   terragrunt
   go-golangci-lint
   pgcli
   sops
   ripgrep
   pinentry-qt
   pinentry-tty
   bibata-cursor-theme
   qtwayland
   unzip
   btop
   proxmox-scripts
   (specification->package "imagemagick")))

;; Enumerate configs/claude/<session>/ contents so home-files-service
;; can symlink them under ~/.claude-sessions/<session>/.  Called at
;; module-load time; adding new files in the repo is picked up on the
;; next `guix home reconfigure'.  `canonicalize-path' turns the
;; %load-path-resolved dir into an absolute path so `local-file'
;; doesn't warn about relative resolution.  Leaf filenames must not
;; start with '.' — home-files-service-type rejects those.
(define (claude-session-files sessions)
  (apply append
         (map (lambda (session)
                (let ((session-dir
                       (canonicalize-path
                        (repo-directory
                         (string-append "configs/claude/" session)))))
                  (map (lambda (entry)
                         (let* ((src (string-append session-dir "/" entry))
                                (dst (string-append ".claude-sessions/"
                                                    session "/" entry))
                                (dir? (eq? 'directory (stat:type (stat src)))))
                           `(,dst ,(local-file src #:recursive? dir?))))
                       (filter (lambda (e) (not (member e '("." ".."))))
                               (scandir session-dir)))))
              sessions)))

;; 2) Shared services (with your existing configs).
(define-public base-services
  (append
   (list

   (simple-service 'general-env  home-environment-variables-service-type
		   `(("PATH" . "$HOME/.local/bin:$PATH")
		     ("GOMODCACHE" . "$HOME/.cache/go/mod")
		     ("GOCACHE" . "$HOME/.cache/go/build")
		     ("GOPATH" . "$HOME/state/go")
		     ("GOBIN" . "$HOME/.local/bin")
		     ("HYPRCURSOR_THEME" . "phinger-matugen")
		     ("HYPRCURSOR_SIZE" . "32")
		     ("XCURSOR_THEME" . "phinger-matugen")
		     ("XCURSOR_SIZE" . "32")
		     ("NUG_HOST" . "nug.spaniel-cordylus.ts.net")
		     ("COMFYUI_URL" . "http://$NUG_HOST:8188")))
   (simple-service 'peteches-guile-load-path home-environment-variables-service-type
		   `(("GUILE_LOAD_PATH" . "$HOME/area_51/guix:${HOME}/area_51/codeberg.org/peteches/guix-channel.git_main:${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}")))

   ;; Dbus
   (service home-dbus-service-type)

   (service home-aws-service-type)

   base-syncthing-service

   ;; Notifications
   base-mako-service

   ;; Git config
   (service home-git-service-type
	    (home-git-configuration
	     (pre-commit-hook (list (local-file "./git-hooks/pre-commit")))
	     (config git-config)
	     (global-ignore (map gitignore-file
				 `("Global/Backup"
				   "Global/Diff"
				   "Global/Emacs"
				   "Global/Linux"
				   "Go"
				   "CommonLisp"
				   "Elisp"
				   "Scheme")))))

   ;; Password store
   (service home-password-store-service-type
	    (home-password-store-configuration
	     (repo-uri "git@github.com:peteches/password-store.git")
	     (password-store-dir "${HOME}/.local/share/password-store")))

   ;; Browsers (Firefox profiles + Nyxt)
   (service firefox-service-type
	    (firefox-configuration (profiles base-firefox-profiles)))
   (service nyxt-service-type
	    (home-nyxt-base-configuration
	     (config-directory (repo-directory "configs/nyxt"))))

   base-ssh-service

   base-gpg-service

   ;; HyprLand
   (service home-hyprland-service-type
	    (home-hyprland-configuration
	     (extra-packages '())
	     (config-directory (repo-directory "configs/hypr"))))

   (service wofi-service-type
	    (wofi-config
	     (matching "multi-contains")
	     (insensitive "true")
	     (close_on_focus_loss "true")
	     (key_up "Ctrl-p")
	     (key_down "Ctrl-n")))

   (service home-emacs-base-service-type
	    (home-emacs-base-configuration
	     (emacs-package emacs-pgtk)
	     (config-directory (repo-directory "configs/emacs"))
	     (extra-packages (cons guile-lsp-server/3.0.11 (map specification->package
				  '("emacs-forge"
				    "emacs-olivetti"
				    "emacs-string-inflection"
				    "emacs-ob-go"
				    "emacs-modus-themes"
				    "emacs-gruvbox-theme"
				    "emacs-paredit"
				    "emacs-geiser"
				    "emacs-emojify"
				    "emacs-geiser-guile"
				    "emacs-org-present"
				    "gcc-toolchain" ; to compile vterm / tree-sitter grammars
				    "emacs-vterm"
				    "emacs-eat"

				    "yarn" ; for mcp servers
				    "curl"
				    "emacs-compat"
				    "postgresql"
					;	emacs-go-playground

				    "emacs-orderless"

				    "emacs-show-font"
				    "emacs-projectile"
				    "ripgrep"

				    "emacs-gnus-desktop-notify"

				    "emacs-tramp"

				    "emacs-password-store"
				    "emacs-password-store-otp"
				    "emacs-auth-source-pass"

				    "emacs-marginalia"
				    "emacs-vertico"

				    "emacs-slack"

				    "emacs-which-key"

				    "emacs-magit"

				    "emacs-rainbow-delimiters"

				    ;; Language support.  Keep native tree-sitter grammars
				    ;; Guix-managed; use straight.el for faster-moving
				    ;; Emacs Lisp packages.
				    "tree-sitter"

				    "tree-sitter-yaml"

				    "tree-sitter-go"
				    "tree-sitter-gomod"

				    "tree-sitter-lua"
				    "tree-sitter-luadoc"

				    "tree-sitter-scheme"

				    "tree-sitter-python"

				    "tree-sitter-org"

				    "tree-sitter-markdown"

				    "tree-sitter-json"

				    "tree-sitter-html"

				    "tree-sitter-hcl"

				    "tree-sitter-dockerfile"

				    "tree-sitter-c"

				    "tree-sitter-bash"
				    "tree-sitter-awk"


				    "emacs-go-mode"

				    "emacs-pinentry"

				    "emacs-atom-one-dark-theme"

				    "emacs-company"
				    "emacs-company-quickhelp"
				    "emacs-company-org-block"
				    "emacs-company-emoji"

				    ;; Language server binaries.  Keep these Guix-managed;
				    ;; Emacs only starts servers already present on exec-path.
				    "lua-language-server"
				    "go"
				    "gopls"
				    "sqls"

				    "emacs-yaml-mode"

				    ;; mcp stuff
				    "mcp-server-filesystem-go"
				    "go-github-com-sonirico-mcp-shell"

				    "emacs-mcp"

				    "emacs-all-the-icons"
				    "emacs-all-the-icons-dired"
				    "emacs-all-the-icons-ibuffer"
				    "emacs-all-the-icons-completion"

				    "font-google-noto"
				    "font-google-noto-emoji"
				    "font-iosevka@33.3.0"))))))


   (service home-bash-service-type
	    (home-bash-configuration
	     (aliases `(("grhome" . "guix home -L ~/area_51/guix reconfigure ~/area_51/guix/peteches/home/configs/$(hostname).scm")
			("grsys" . "sudo guix system -L ~/area_51/guix reconfigure ~/area_51/guix/peteches/systems/$(hostname).scm")))
	     (guix-defaults? #t)
	     (environment-variables '(("CGO_ENABLED" . "1")))))

   ;; Desktop conveniences (terminals/aliases/env, etc.).
   (service home-desktop-service-type)

   base-ai-service

   (service home-claude-service-type
	    (home-claude-configuration
	     (config-directory (repo-directory "configs/claude/defaults"))
	     (mcp-servers
	      (let* ((bash-path  (file-append bash "/bin/bash"))
		     ;; Point at the packaged emacs-anvil launcher.  Old
		     ;; setup used a straight.el checkout under
		     ;; ~/.config/emacs which no longer exists.
		     (script     (file-append emacs-anvil "/bin/anvil-stdio.sh")))
		(list
		 (home-claude-mcp-server
		  (name "anvil")
		  (command bash-path)
		  (args (list script
			      "--server-id=anvil"
			      "--init-function=anvil-enable"
			      "--stop-function=anvil-disable")))
		 (home-claude-mcp-server
		  (name "anvil-emacs-eval")
		  (command bash-path)
		  (args (list script "--server-id=emacs-eval")))
		 (home-claude-mcp-server
		  (name "comfyui")
		  (command "npx")
		  (args (list "-y" "comfyui-mcp")))))))))

   (list
    (simple-service 'claude-sessions
		    home-files-service-type
		    (claude-session-files
		     '("guix" "critical-grind" "peteches"))))

   base-theming-services))
