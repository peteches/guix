(define-module (peteches home-services emacs base)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages node)
  #:use-module (peteches packages emacs)
  #:use-module (peteches packages mcp)
  #:export (home-emacs-base-service-type))

(define (home-emacs-base-profile-service config)
  (map specification->package '("emacs"
				"emacs-ob-go"
				"emacs-modus-themes"
				"emacs-gruvbox-theme"
				"emacs-paredit"
				"emacs-geiser"
				"emacs-emojify"
				"emacs-geiser-guile"
				"emacs-combobulate"
				"gcc-toolchain" ; to compile vterm / treesitter grammers

				"yarn" ; for mcp servers
				"curl"
				"emacs-compat"

					;	emacs-go-playground

				"emacs-orderless"

				"emacs-show-font"
				"emacs-projectile"
				"ripgrep"

				"emacs-gnus-desktop-notify"
				
				"emacs-tramp"

				"emacs-org"
				"emacs-org-superstar"
				"emacs-org-roam"
				
				"emacs-password-store"
				"emacs-password-store-otp"
				"emacs-auth-source-pass"

				"emacs-marginalia"
				"emacs-vertico"
				
				"emacs-slack"
				
				"emacs-which-key"

				"emacs-magit"

				"emacs-rainbow-delimiters"

				;; language support
				"tree-sitter"

				"tree-sitter-yaml"
				
				"tree-sitter-go"
				"tree-sitter-gomod"

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
				
				"emacs-lsp-mode"

				"emacs-pinentry"

				"emacs-atom-one-dark-theme"

				"emacs-yasnippet"
				"emacs-yasnippet-snippets"
				"emacs-company"
				"emacs-company-quickhelp"
				"emacs-company-org-block"
				"emacs-company-emoji"
				
				
				;; language servers
				"guile-lsp-server"
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
				"font-iosevka")))

(define (home-emacs-base-files-service config)
  (list
   `("emacs/lisp" ,(local-file "./configs/lisp" #:recursive? #t))
   `("emacs/early-init.el" ,(local-file "./configs/early-init.el"))
   `("emacs/init.el" ,(local-file "./configs/init.el"))))

(define (home-emacs-base-shepherd-service-type config)
  (list
   (shepherd-service
    (documentation "Manage my emacs deamon")
    (provision '(emacs))
    (respawn? #t)
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
	      (list #$(file-append emacs "/bin/emacs") "--fg-daemon")
	      #:log-file (string-append
			  (or (getenv "XDG_LOG_HOME")
			      (format #f "~a/.local/var/log"
				      (getenv "HOME")))
			  "/emacs-dameon.log"))))))

(define home-emacs-base-service-type
  (service-type (name 'home-emacs-base-config)
		(extensions
		 (list
		  (service-extension
		   home-activation-service-type
		   (const
		    #~(when (zero? (system* #$(file-append emacs "/bin/emacsclient") "-e" "t"))
			(system* #$(file-append emacs "/bin/emacsclient")
				 "-e"
				 "(progn
                  (when (fboundp 'guix-refresh-emacs-load-path)
                    (guix-refresh-emacs-load-path))
                  (let* ((init (or user-init-file
                                   (expand-file-name \"init.el\" user-emacs-directory))))
                    (when (and init (file-readable-p init))
                      (load init nil 'nomessage))))"))))
		  (service-extension
		   home-shepherd-service-type
		   home-emacs-base-shepherd-service-type)
		  (service-extension
		   home-profile-service-type
		   home-emacs-base-profile-service)
		  (service-extension
		   home-xdg-configuration-files-service-type
		   home-emacs-base-files-service)))
		(default-value 'nil)
		(description "Applies my personal Emacs base configuration")))
