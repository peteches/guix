;;; peteches/home/modules/base.scm — the home-environment orchestrator.
;;;
;;; Exports `base-packages' and `base-services', which the two host configs
;;; in peteches/home/configs/ append their own extras onto.  Everything
;;; shared between dagon and nyarlothotep belongs here or in one of the
;;; focused sibling modules (ssh, gpg, theming, ai, pi, …), which
;;; this module composes.
;;;
;;; Three distinct sources of code, easy to confuse:
;;;   (gnu home services …)      — upstream Guix.
;;;   (peteches home services …) — peteches/home/services/ in this repo:
;;;                                reusable home service *types*. (Formerly
;;;                                the external `peteches' guix channel;
;;;                                folded back in once Codeberg's ToS
;;;                                barred AI-generated code there.)
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
  #:use-module ((gnu packages gnupg) #:hide (gnupg))
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
  #:use-module ((gnu packages wm) #:hide (dank-material-shell))
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
  #:use-module (peteches home modules firefox)
  #:use-module (peteches home modules git)
  #:use-module (peteches home modules ssh)
  #:use-module (peteches home modules gpg)
  #:use-module (peteches home modules syncthing)
  #:use-module (peteches home modules theming)
  #:use-module (peteches home modules ai)
  ;; Claude Code is gone from the desktops too -- this import now exists
  ;; only for the <home-claude-mcp-server> record that
  ;; %desktop-mcp-servers below builds and (peteches home modules pi)
  ;; consumes for pi's mcp.json.  Narrowed with #:select for exactly that
  ;; reason: home-claude-service-type / home-claude-configuration are no
  ;; longer referenced anywhere in this file.
  #:use-module ((peteches home modules claude)
               #:select (home-claude-mcp-server))
  #:use-module (peteches home modules pi)
  ;; Your config fragments
  #:use-module (peteches packages gurps)
  #:use-module ((peteches packages pi-coding-agent) #:select (pi-coding-agent pi-mcp-adapter))
  #:use-module ((peteches packages pi-interactive-subagents) #:select (pi-interactive-subagents))
  #:use-module (peteches packages emacs-anvil)
  #:use-module (peteches packages graphify)
  #:use-module (peteches packages herdr-mx)
  ;; These three used to be `(specification->package "...")' calls in
  ;; base-packages below.  They are now direct imports, for two reasons:
  ;; specification->package only consults the PULLED CHANNEL package sets plus
  ;; %package-module-path, so beeper-bin / feishin-bin / apidog (local modules
  ;; in peteches/packages/{beeper,feishin,apidog}.scm, loaded through -L rather
  ;; than through a channel since the peteches channel was folded back in)
  ;; resolve fine under `guix home build' -- which puts the repo on
  ;; %package-module-path -- but are "unknown package" in a bare `guix repl',
  ;; where that path is never set.  That asymmetry is documented at length in
  ;; (peteches home modules vm-jumpbox)'s header, which is why this module
  ;; follows the same direct-#:select discipline; measured 2026-11-05,
  ;; `guix home build -L . --dry-run peteches/home/configs/dagon.scm' returned
  ;; 0 with the old specifications in place, so nothing here was actually
  ;; broken for the real build path -- this is consistency, not a fix.  The
  ;; remaining specification->package calls below (matugen, hyprcursor,
  ;; xcur2png, curl, dconf, qt5ct, ...) are genuine upstream names and stay.
  #:use-module (peteches packages beeper)
  #:use-module (peteches packages feishin)
  #:use-module (peteches packages apidog)
  #:use-module ((peteches packages comfyui-mcp) #:select (node-comfyui-mcp))
  #:use-module (peteches packages rustdesk)
  #:use-module (peteches packages concourse)
  #:use-module (peteches packages proxmox-scripts)
  #:use-module (peteches packages desktop-scripts)
  ;; Guix proper's `dank-material-shell' (gnu packages window-management,
  ;; via `(gnu packages wm)') is pinned to v0.5.1 and predates DMS's fix for
  ;; Hyprland's Lua config migration -- see peteches/packages/
  ;; dank-material-shell.scm for why this local v1.5.0 override exists.
  #:use-module ((peteches packages dank-material-shell)
                #:select ((dank-material-shell-1.5.0 . dank-material-shell)))
  ;; Upstream `gnupg' is pinned to an explicit dev/test snapshot (2.5.20) at
  ;; the current channel commit -- see peteches/packages/gnupg.scm.
  #:use-module (peteches packages gnupg)

  #:use-module (peteches repository)
  ;; utilities
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
   pi-coding-agent
   emacs-anvil
   graphify
   herdr-mx
   rustdesk
   dank-material-shell
   (specification->package "hyprlock")
   (specification->package "gsettings-desktop-schemas")
   beeper-bin
   feishin-bin
   apidog
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
   (specification->package "python-minimal")
   (specification->package "python-websockets")
   ;; sox provides `rec` for audio capture (used by DMS Dictate plugin
   ;; and any other local audio recording).
   (specification->package "sox")
   ;; espeak-ng for offline TTS (used by DMS Dictate plugin's Read Aloud).
   (specification->package "espeak-ng")
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

;; MCP server set for the pi coding-agent config: the same anvil (Emacs) /
;; comfyui / graphify stdio servers -- declared once here and passed to
;; home-pi-service-type, because pi is the only harness left on these boxes.
;; They used to be handed to Claude Code as well, from a second inline copy of
;; this same list; that copy is gone (see the note at the end of base-services).
;; The <home-claude-mcp-server> record name survives because pi.scm reads the
;; same record type -- it is a shared shape, not a Claude-only one.
(define %desktop-mcp-servers
  (let* ((bash-path (file-append bash "/bin/bash"))
         ;; Point at the packaged emacs-anvil launcher.  Old setup used a
         ;; straight.el checkout under ~/.config/emacs which no longer exists.
         (script    (file-append emacs-anvil "/bin/anvil-stdio.sh")))
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
     ;; The packaged server, not `npx -y comfyui-mcp': comfyui-mcp depends on
     ;; two native addons (better-sqlite3 and sharp), which npx builds from
     ;; source on first use.  That needs a C toolchain that isn't on PATH
     ;; here, and npx swallowed the failure, so the server only ever showed
     ;; as "failed to connect".
     (home-claude-mcp-server
      (name "comfyui")
      (command (file-append node-comfyui-mcp "/bin/comfyui-mcp")))
     ;; Stdio only (see graphify.scm on why the HTTP transport isn't
     ;; available). No args: it defaults to reading graphify-out/graph.json
     ;; relative to the launch cwd, which Claude Code sets to the current
     ;; project directory.
     (home-claude-mcp-server
      (name "graphify")
      (command (file-append graphify "/bin/graphify-mcp"))))))

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
		     ("COMFYUI_URL" . "https://comfyui.ts.peteches.co.uk")
		     ;; Reuse the running Emacs daemon and open a new frame
		     ;; for it; if no daemon is reachable, emacsclient's
		     ;; empty alternate-editor tells it to start one
		     ;; (`emacs --daemon') and connect rather than falling
		     ;; back to a disposable standalone editor.
		     ("EDITOR" . "emacsclient --alternate-editor='' --create-frame")
		     ("VISUAL" . "emacsclient --alternate-editor='' --create-frame")))
   (simple-service 'peteches-guile-load-path home-environment-variables-service-type
		   `(("GUILE_LOAD_PATH" . "$HOME/area_51/guix:${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}")))

   ;; Dbus
   (service home-dbus-service-type)

   (service home-aws-service-type)

   base-syncthing-service

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
	    (firefox-configuration (profiles base-firefox-profiles)
				   ;; nvidia VAAPI force-enables — must be
				   ;; global so every profile gets them.
				   (global-prefs %nvidia-vaapi-prefs)))
   (service nyxt-service-type
	    (home-nyxt-base-configuration
	     (config-directory (repo-directory "configs/nyxt"))))

   base-ssh-service
   base-ssh-control-dir-service

   base-gpg-service
   base-log-dir-service

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
	     (extra-packages (cons* guile-lsp-server/3.0.11
				     ;; Local override -- see peteches/packages/gnupg.scm:
				     ;; upstream's propagates gnupg 2.5.20, conflicting
				     ;; with the 2.4.8 pin elsewhere in this profile.
				     emacs-pinentry
				     (map specification->package
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


   ;; pi coding-agent (https://pi.dev) -- the `pi' binary
   ;; (pi-coding-agent, in base-packages) plus its ~/.pi config:
   ;; models.json (the remote koboldcpp model) and the MCP servers from
   ;; %desktop-mcp-servers above.
   ;;
   ;; pi-mcp-adapter renders the MCP servers above into
   ;; ~/.pi/agent/mcp.json; pi-interactive-subagents supplies the
   ;; `subagent' tool, which runs scout/researcher/worker in herdr panes --
   ;; herdr-mx is in base-packages above, a desktop session runs inside
   ;; herdr, and tmux is not installed, so the fork's herdr pane path is
   ;; the only one that works on these boxes. That extension is also the
   ;; consumer for configs/pi/defaults/agents/ -- CONFIG-DIRECTORY already
   ;; symlinks those three .md files into ~/.pi/agent/agents/, but pi core
   ;; has no agents/ directory of its own (docs/configuration.md's agent-dir
   ;; table lists extensions/, skills/, prompts/ and themes/, not agents/),
   ;; so until now nothing read them. The extension was wired into
   ;; (peteches home modules claude-workstation) in b5e6b29 and lost with
   ;; that module in 125cd4f; the desktops were left out in c2be992
   ;; ("deliberately not installed here", no reason recorded). This is the
   ;; one place both desktop home configs compose, so it lands on dagon and
   ;; nyarlothotep together.
   (service home-pi-service-type
	    (home-pi-configuration
	     (config-directory (repo-directory "configs/pi/defaults"))
	     (extensions (list pi-mcp-adapter pi-interactive-subagents))
	     (mcp-servers %desktop-mcp-servers)))
   ;; What used to sit between the pi service above and this line was
   ;; `(service home-claude-service-type ...)
   ;; plus its own inline copy of the %desktop-mcp-servers list above.
   ;; Claude Code came off every machine -- the harness is `pi' now,
   ;; with strata on comfyui and koboldcpp on the desktop as its local
   ;; backends -- so this box gets no ~/.claude and no claude binary.
   ;; The service type itself is unwired rather than deleted:
   ;; (peteches home modules claude) and configs/claude/ still exist,
   ;; and this module still imports that one for the
   ;; <home-claude-mcp-server> record pi needs.  Deleting the record
   ;; would be the next step once nothing builds it any more --
   ;; (peteches home modules pi) imports it the same way.

   )
   base-theming-services))
