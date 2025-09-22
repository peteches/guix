(define-module (peteches home-services emacs base)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages node)
  #:use-module (peteches packages emacs)
  #:export (home-emacs-base-service-type))

(define (home-emacs-base-profile-service config)
  (list emacs
	emacs-modus-themes
	emacs-gruvbox-theme
	emacs-paredit
	emacs-geiser
	emacs-geiser-guile
	emacs-combobulate
	gcc-toolchain ; to compile vterm / treesitter grammers

	yarn ; for mcp servers
	
	emacs-compat

					;	emacs-go-playground

	emacs-orderless

	emacs-show-font
	emacs-projectile
	ripgrep
	
	emacs-tramp

	emacs-org

	emacs-password-store
	emacs-password-store-otp
	emacs-auth-source-pass

	emacs-marginalia
	emacs-vertico
	
	emacs-slack
	
	emacs-which-key

	emacs-magit

	emacs-rainbow-delimiters

	;; language support
	emacs-go-mode
	
	emacs-lsp-mode

	emacs-pinentry

	emacs-atom-one-dark-theme

	emacs-yasnippet
	emacs-yasnippet-snippets
	emacs-company
	emacs-company-quickhelp
	emacs-company-org-block
	emacs-company-emoji
	
	
	;; language servers
	guile-lsp-server
	go
	gopls
	sqls

	emacs-gptel
	emacs-mcp
	
	emacs-all-the-icons
	emacs-all-the-icons-dired
	emacs-all-the-icons-ibuffer
	emacs-all-the-icons-completion

	font-google-noto
	font-google-noto-emoji
	font-iosevka))

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
