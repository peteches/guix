;;; AGiXT backend + Telegram bots as Guix Home services
;;; - Scripts are taken from ./agixt-scripts (relative to invocation CWD),
;;;   built into a package `agixt-scripts`, added to profile, and staged into $HOME.
;;; - Every field uses a custom type + serializer.
;;; - Files are installed using the home-files alist pattern (DEST . FILE-LIKE).

(define-module (peteches home-services agixt)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)            ; local-file, mixed-text-file, file-append
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages curl)
  #:use-module (srfi srfi-1)          ; every
  #:use-module (srfi srfi-13)         ; string-join
  #:export (home-agixt-backend-service-type
            home-agixt-telegram-bots-service-type
	    home-agixt-webui-service-type
	    home-agixt-chatui-service-type
            agixt-backend-configuration
	    agixt-webui-configuration
	    agixt-chatui-configuration
            agixt-telegram-bot-configuration
            agixt-telegram-bots-configuration))

;; -----------------------------------------------------------------------------
;; Package that installs the helper scripts from ./agixt-scripts
;; -----------------------------------------------------------------------------
(define agixt-scripts
  (let ((src (local-file "./agixt-scripts" #:recursive? #t)))
    (package
      (name "agixt-scripts")
      (version "0.1")
      (source src)
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             #~'(
                 ("agixt-backend"       "bin/")            ; executable
		 ("agixt-webui"         "bin/")            ; executable
		 ("agixt-chatui"        "bin/")            ; executable
		 ("agixt-telegram-bot"  "bin/")            ; executable
                 ("telegram-bot.py"     "libexec/agixt/")  ; helper
                 )))
      (home-page "https://example.invalid/agixt-home-scripts")
      (synopsis "Helper scripts for AGiXT Guix Home services")
      (description "Start scripts and bot helper used by the AGiXT Home services.")
      (license license:expat))))

;; -----------------------------------------------------------------------------
;; Custom types: one predicate + serializer per field
;; -----------------------------------------------------------------------------
;; Backend fields
(define (agixt-instance-name? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-instance-name x) x)

(define (agixt-git-url? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-git-url x) x)

(define (agixt-git-branch? x) (or (not x) (string? x)))
(define (serialize-agixt-git-branch x) x)

(define (agixt-base-uri? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-base-uri x) x)

(define (agixt-credentials-file? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-credentials-file x) x)

(define (agixt-workdir? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-workdir x) x)

(define (agixt-backend-respawn? x) (boolean? x))
(define (serialize-agixt-backend-respawn x) x)

;; Bot fields
(define (agixt-bot-name? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-bot-name x) x)

(define (agixt-bot-credentials-file? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-bot-credentials-file x) x)

(define (agixt-bot-base-uri? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-bot-base-uri x) x)

(define (agixt-agent-name? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-agent-name x) x)

(define (agixt-chain-name? x) (string? x))  ; allow empty
(define (serialize-agixt-chain-name x) x)

(define (agixt-backend-instance-name? x) (or (not x) (string? x)))
(define (serialize-agixt-backend-instance-name x) x)

(define (agixt-bot-workdir? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-bot-workdir x) x)

;; Bot list (user IDs as decimal strings)
(define (agixt-user-id? x)
  (and (string? x)
       (not (string-null? x))
       (let loop ((cs (string->list x)))
         (or (null? cs) (and (char-numeric? (car cs)) (loop (cdr cs)))))))
(define (serialize-agixt-user-id x) x)

(define (agixt-user-id-list? x) (and (list? x) (every agixt-user-id? x)))
(define (serialize-agixt-user-id-list xs) (map serialize-agixt-user-id xs))

;; List of bot configs: forward-declare; fill after record exists
(define (agixt-bot-config-list? x) #f)
(define (serialize-agixt-bot-config-list x) x)

;; Bots service respawn flag
(define (agixt-bots-respawn? x) (boolean? x))
(define (serialize-agixt-bots-respawn x) x)

(define (agixt-port? x) (and (integer? x) (<= 1 x 65535)))
(define (serialize-agixt-port x) x)

(define (agixt-webui-repo-url? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-webui-repo-url x) x)

(define (agixt-chatui-repo-url? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-chatui-repo-url x) x)

(define (agixt-chatui-entry? x) (and (string? x) (not (string-null? x))))
(define (serialize-agixt-chatui-entry x) x)

;; -----------------------------------------------------------------------------
;; Backend configuration + services
;; -----------------------------------------------------------------------------
;; Web UI
(define-configuration agixt-webui-configuration
  (instance-name
    (agixt-instance-name "default")
    "Backend instance this UI is associated with. Provision: agixt-webui-<instance>."
     (serializer serialize-agixt-instance-name))
  (base-uri
    (agixt-base-uri "http://localhost:7437")
    "AGiXT API base URI."
     (serializer serialize-agixt-base-uri))
  (credentials-file
    (agixt-credentials-file ".config/agixt/cred-backend")
    "Shell file with AGIXT_API_KEY (NOT managed by Guix)."
     (serializer serialize-agixt-credentials-file))
  (workdir
    (agixt-workdir ".local/share")
    "Base working directory relative to $HOME."
     (serializer serialize-agixt-workdir))
  (port
    (agixt-port 8501)
    "Streamlit port for the Web UI."
     (serializer serialize-agixt-port))
  (webui-git-url
    (agixt-webui-repo-url "https://github.com/AGiXT/streamlit")
    "Repository URL for the Streamlit UI."
     (serializer serialize-agixt-webui-repo-url))
  (respawn?
    (agixt-backend-respawn #t)
    "Whether Shepherd auto-respawns."
     (serializer serialize-agixt-backend-respawn)))

;; Chat UI
(define-configuration agixt-chatui-configuration
  (instance-name
    (agixt-instance-name "default")
    "Backend instance this Chat UI is associated with. Provision: agixt-chatui-<instance>."
     (serializer serialize-agixt-instance-name))
  (base-uri
    (agixt-base-uri "http://localhost:7437")
    "AGiXT API base URI."
     (serializer serialize-agixt-base-uri))
  (credentials-file
    (agixt-credentials-file ".config/agixt/cred-backend")
    "Shell file with AGIXT_API_KEY (NOT managed by Guix)."
     (serializer serialize-agixt-credentials-file))
  (workdir
    (agixt-workdir ".local/share")
    "Base working directory relative to $HOME."
     (serializer serialize-agixt-workdir))
  (port
    (agixt-port 3437)
    "Port for the Chat UI."
     (serializer serialize-agixt-port))
  (chatui-git-url
    (agixt-chatui-repo-url "https://github.com/AGiXT/streamlit")
    "Repository URL for the Chat UI (defaults to Streamlit; you can change it later)."
     (serializer serialize-agixt-chatui-repo-url))
  (chat-entry
    (agixt-chatui-entry "Main.py")
    "Entry file within the Chat UI repository (e.g., Main.py)."
     (serializer serialize-agixt-chatui-entry))
  (respawn?
    (agixt-bots-respawn #t)
    "Whether Shepherd auto-respawns."
     (serializer serialize-agixt-bots-respawn)))

(define (agixt-webui-home-files cfg)
  (let* ((inst (agixt-webui-configuration-instance-name cfg))
         (wd   (agixt-webui-configuration-workdir cfg)))
    (list
     (list (string-append wd "/agixt-webui-" inst "/start.sh")
           (file-append agixt-scripts "/bin/agixt-webui")))))

(define (agixt-chatui-home-files cfg)
  (let* ((inst (agixt-chatui-configuration-instance-name cfg))
         (wd   (agixt-chatui-configuration-workdir cfg)))
    (list
     (list (string-append wd "/agixt-chatui-" inst "/start.sh")
           (file-append agixt-scripts "/bin/agixt-chatui")))))

(define (agixt-webui-packages _)
  (list agixt-scripts bash git curl python python-pip coreutils nss-certs grep sed findutils))

(define (agixt-chatui-packages _)
  (list agixt-scripts bash git curl python python-pip coreutils nss-certs grep sed findutils))


(define-configuration agixt-backend-configuration
  (instance-name
   (agixt-instance-name "default")
   "Shepherd provision will be agixt-<instance>."
   (serializer serialize-agixt-instance-name))
  (agixt-git-url
   (agixt-git-url "https://github.com/Josh-XT/AGiXT")
   "Git repository URL for AGiXT."
   (serializer serialize-agixt-git-url))
  (agixt-git-branch
   (agixt-git-branch #f)
   "Optional branch to checkout."
   (serializer serialize-agixt-git-branch))
  (base-uri
   (agixt-base-uri "http://localhost:7437")
   "AGiXT API base URI."
   (serializer serialize-agixt-base-uri))
  (credentials-file
   (agixt-credentials-file ".config/agixt/credentials")
   "Path to a POSIX shell file containing AGIXT_API_KEY (NOT managed by Guix)."
   (serializer serialize-agixt-credentials-file))
  (workdir
   (agixt-workdir ".local/share")
   "Base working directory relative to $HOME."
   (serializer serialize-agixt-workdir))
  (respawn?
   (agixt-backend-respawn #t)
   "Whether Shepherd auto-respawns the backend."
   (serializer serialize-agixt-backend-respawn)))

(define (agixt-backend-packages _cfg)
  ;; Add our helper package + runtime deps to the user profile.
  (list agixt-scripts bash git curl node coreutils grep sed findutils nss-certs python python-pip))

;; Files via home-files-service-type: list of ("DEST" FILE-LIKE)
(define (agixt-backend-home-files cfg)
  (let* ((inst (agixt-backend-configuration-instance-name cfg))
         (wd   (agixt-backend-configuration-workdir cfg)))
    (list
     (list (string-append wd "/agixt-" inst "/start.sh")
           (file-append agixt-scripts "/bin/agixt-backend")))))

(define (agixt-backend-shepherd-services cfg)
  (let* ((inst      (agixt-backend-configuration-instance-name cfg))
	 (git-url   (agixt-backend-configuration-agixt-git-url cfg))
	 (git-branch (or (agixt-backend-configuration-agixt-git-branch cfg) ""))
	 (base      (agixt-backend-configuration-base-uri cfg))
	 (cred      (agixt-backend-configuration-credentials-file cfg))
	 (resp      (agixt-backend-configuration-respawn? cfg))
	 (wd        (agixt-backend-configuration-workdir cfg))
	 (prov      (string->symbol (string-append "agixt-backend-" inst)))
	 ;; what we exec
	 (start-cmd (string-append "sh \"$HOME/" wd "/agixt-" inst "/start.sh\""))
	 ;; values passed into the child env
	 (dir-rel   (string-append wd "/agixt-" inst))
	 (env-vars  (list (string-append "AGIXT_DIR_REL=" dir-rel)
                          (string-append "AGIXT_GIT_URL=" git-url)
                          (string-append "AGIXT_GIT_BRANCH=" git-branch)
                          (string-append "AGIXT_BASE_URI=" base)
                          (string-append "AGIXT_CREDENTIALS_FILE=" cred))))
    (list
     (shepherd-service
      (provision (list prov))
      (documentation (string-append "AGiXT backend (" inst ")"))
      (start
       #~(let* ((home      (or (getenv "HOME") "."))
		(start-path (string-append home "/" #$dir-rel "/start.sh"))
		(log-path   (string-append home "/" #$dir-rel "/agixt.log"))
		;; Guix Home puts user packages here; include system as fallback.
		(path       (string-append home "/.guix-home/profile/bin:"
					   home "/.guix-home/profile/sbin:"
					   home "/.guix-profile/bin:"
					   home "/.guix-profile/sbin:"
					   "/run/current-system/profile/bin:"
					   "/run/current-system/profile/sbin"))
		(ca-home (string-append home "/.guix-home/profile/etc/ssl/certs/ca-certificates.crt"))
		(ca-user (string-append home "/.guix-profile/etc/ssl/certs/ca-certificates.crt"))
		(ca-sys  "/etc/ssl/certs/ca-certificates.crt")
		(caf     (cond ((access? ca-home R_OK) ca-home)
                               ((access? ca-user R_OK) ca-user)
                               (else ca-sys))))
	   (make-forkexec-constructor
	    ;; Run the script directly (shebang points to store bash).
	    (list start-path)
	    #:environment-variables
	    (list
	     (string-append "HOME=" home)
	     (string-append "PATH=" path)
	     (string-append "SSL_CERT_FILE=" caf)
	     (string-append "GIT_SSL_CAINFO=" caf)
	     (string-append "CURL_CA_BUNDLE=" caf)
	     (string-append "PIP_CERT=" caf)
	     (string-append "AGIXT_DIR_REL=" #$dir-rel)
	     (string-append "AGIXT_GIT_URL=" #$git-url)
	     (string-append "AGIXT_GIT_BRANCH=" #$git-branch)
	     (string-append "AGIXT_BASE_URI=" #$base)
	     (string-append "AGIXT_CREDENTIALS_FILE=" #$cred))
	    #:log-file log-path)))
      (stop  #~(make-kill-destructor))
      (respawn? resp)))))

(define home-agixt-backend-service-type
  (service-type
   (name 'home-agixt-backend)
   (extensions
    (list (service-extension home-profile-service-type agixt-backend-packages)
          (service-extension home-files-service-type   agixt-backend-home-files)
          (service-extension home-shepherd-service-type agixt-backend-shepherd-services)))
   (default-value (agixt-backend-configuration))
   (description "Run the AGiXT backend (FastAPI). Scripts come from the agixt-scripts package; files staged via home-files alist.")))

;; -----------------------------------------------------------------------------
;; Telegram bots (per-bot) + collection
;; -----------------------------------------------------------------------------
(define-configuration agixt-telegram-bot-configuration
  (name
    (agixt-bot-name "default")
    "Unique bot name; provision becomes agixt-telegram-<name>."
     (serializer serialize-agixt-bot-name))
  (credentials-file
    (agixt-bot-credentials-file ".config/agixt/credentials")
    "External credentials file (NOT managed by Guix): TELEGRAM_BOT_TOKEN, AGIXT_API_KEY."
     (serializer serialize-agixt-bot-credentials-file))
  (base-uri
    (agixt-bot-base-uri "http://localhost:7437")
    "AGiXT API base URI for this bot."
     (serializer serialize-agixt-bot-base-uri))
  (agent
    (agixt-agent-name "AGiXT")
    "Default agent name."
     (serializer serialize-agixt-agent-name))
  (chain
    (agixt-chain-name "")
    "Optional chain to run; empty for normal chat."
     (serializer serialize-agixt-chain-name))
  (allowed-user-ids
    (agixt-user-id-list '())
    "Allowed Telegram numeric user IDs; empty means allow all."
     (serializer serialize-agixt-user-id-list))
  (backend-instance-name
    (agixt-backend-instance-name #f)
    "Require/start after agixt-<instance> if set."
     (serializer serialize-agixt-backend-instance-name))
  (workdir
    (agixt-bot-workdir ".local/share")
    "Base working directory relative to $HOME."
     (serializer serialize-agixt-bot-workdir)))

;; Complete list-of-bot-config type now that the record exists
(set! agixt-bot-config-list?
      (lambda (x) (and (list? x) (every agixt-telegram-bot-configuration? x))))
(set! serialize-agixt-bot-config-list
      (lambda (xs) (map serialize-agixt-telegram-bot-configuration xs)))

(define-configuration agixt-telegram-bots-configuration
  (bots
    (agixt-bot-config-list '())
    "List of Telegram bot configurations."
     (serializer serialize-agixt-bot-config-list))
  (respawn?
    (agixt-bots-respawn #t)
    "Whether Shepherd auto-respawns bot services."
     (serializer serialize-agixt-bots-respawn)))

(define (agixt-telegram-bots-packages _cfg)
  ;; Add our helper package + runtime deps to the user profile.
  (list agixt-scripts bash git curl nss-certs grep sed findutils python python-pip))

;; Files via home-files-service-type (alist)
(define (agixt-telegram-bots-home-files cfg)
  (define (bot-files b)
    (let* ((name  (agixt-telegram-bot-configuration-name b))
           (wd    (agixt-telegram-bot-configuration-workdir b))
           (base  (agixt-telegram-bot-configuration-base-uri b))
           (agent (agixt-telegram-bot-configuration-agent b))
           (chain (agixt-telegram-bot-configuration-chain b))
           (uids  (or (agixt-telegram-bot-configuration-allowed-user-ids b) '()))
           (ids   (string-join uids ",")))
      (list
       ;; Non-secret per-bot env (managed by Guix)
       (list (string-append ".config/agixt/bots/" name "/config.env")
             (mixed-text-file (string-append "bot-" name "-config.env")
                              "# Non-secret bot config managed by Guix\n"
                              "AGIXT_BASE_URI=" base "\n"
                              "AGIXT_AGENT="    agent "\n"
                              "AGIXT_CHAIN="    chain "\n"
                              "TELEGRAM_ALLOWED_USER_IDS=" ids "\n"))
       ;; Scripts from the agixt-scripts package
       (list (string-append wd "/agixt-tg-" name "/bot.py")
             (file-append agixt-scripts "/libexec/agixt/telegram-bot.py"))
       (list (string-append wd "/agixt-tg-" name "/start.sh")
             (file-append agixt-scripts "/bin/agixt-telegram-bot")))))
  (apply append (map bot-files (agixt-telegram-bots-configuration-bots cfg))))

(define (agixt-telegram-bots-shepherd-services cfg)
  (let ((respawn? (agixt-telegram-bots-configuration-respawn? cfg)))
    (define (bot-svc b)
      (let* ((name    (agixt-telegram-bot-configuration-name b))
             (wd      (agixt-telegram-bot-configuration-workdir b))
             (prov    (string->symbol (string-append "agixt-telegram-" name)))
             (start-cmd (string-append "sh \"$HOME/" wd "/agixt-tg-" name "/start.sh\""))
             (backend (agixt-telegram-bot-configuration-backend-instance-name b))
             (req     (if backend (list (string->symbol (string-append "agixt-backend-" backend))) '()))
             (cfg-rel (string-append ".config/agixt/bots/" name "/config.env"))
             (dir-rel (string-append wd "/agixt-tg-" name))
             (cred    (agixt-telegram-bot-configuration-credentials-file b))
             ;; Values to pass into the child env (single place to maintain)
             (env-vars (list (string-append "AGIXT_TG_DIR_REL=" dir-rel)
                             (string-append "AGIXT_TG_CONFIG_PATH_REL=" cfg-rel)
                             (string-append "AGIXT_CREDENTIALS_FILE=" cred))))
	(shepherd-service
	 (provision (list prov))
	 (requirement req)
	 (documentation (string-append "Telegram → AGiXT trigger bot (" name ")"))
	 (start
	  #~(let* ((home       (or (getenv "HOME") "."))
		   (start-path (string-append home "/" #$dir-rel "/start.sh"))
		   (log-path   (string-append home "/" #$dir-rel "/bot.log"))
		   (path       (string-append home "/.guix-home/profile/bin:"
					      home "/.guix-home/profile/sbin:"
					      home "/.guix-profile/bin:"
					      home "/.guix-profile/sbin:"
					      "/run/current-system/profile/bin:"
					      "/run/current-system/profile/sbin"))
		   (ca-home (string-append home "/.guix-home/profile/etc/ssl/certs/ca-certificates.crt"))
		   (ca-user (string-append home "/.guix-profile/etc/ssl/certs/ca-certificates.crt"))
		   (ca-sys  "/etc/ssl/certs/ca-certificates.crt")
		   (caf     (cond ((access? ca-home R_OK) ca-home)
				  ((access? ca-user R_OK) ca-user)
				  (else ca-sys))))
	      (make-forkexec-constructor
	       (list start-path)
	       #:environment-variables
	       (list
		(string-append "HOME=" home)
		(string-append "PATH=" path)
		(string-append "SSL_CERT_FILE=" caf)
		(string-append "GIT_SSL_CAINFO=" caf)
		(string-append "CURL_CA_BUNDLE=" caf)
		(string-append "PIP_CERT=" caf)
		(string-append "AGIXT_TG_DIR_REL=" #$dir-rel)
		(string-append "AGIXT_TG_CONFIG_PATH_REL=" #$cfg-rel)
		(string-append "AGIXT_CREDENTIALS_FILE=" #$cred))
	       #:log-file log-path)))

	 (stop  #~(make-kill-destructor))
	 (respawn? respawn?))))
    (map bot-svc (agixt-telegram-bots-configuration-bots cfg))))

(define (agixt-webui-shepherd-services cfg)
  (let* ((inst   (agixt-webui-configuration-instance-name cfg))
         (wd     (agixt-webui-configuration-workdir cfg))
         (prov   (string->symbol (string-append "agixt-webui-" inst)))
         (req    (list (string->symbol (string-append "agixt-backend-" inst))))
         (dir-rel (string-append wd "/agixt-webui-" inst))
         (start-path (string-append "$HOME/" dir-rel "/start.sh"))
         (base   (agixt-webui-configuration-base-uri cfg))
         (cred   (agixt-webui-configuration-credentials-file cfg))
         (port   (number->string (agixt-webui-configuration-port cfg)))
         (repo   (agixt-webui-configuration-webui-git-url cfg))
         (resp   (agixt-webui-configuration-respawn? cfg)))
    (list
     (shepherd-service
      (provision (list prov))
      (requirement req)
      (documentation (string-append "AGiXT Web UI (Streamlit) for " inst))
      (start
       #~(let* ((home (or (getenv "HOME") "."))
                (log-path (string-append home "/" #$dir-rel "/webui.log"))
                (path (string-append home "/.guix-home/profile/bin:"
                                     home "/.guix-home/profile/sbin:"
                                     home "/.guix-profile/bin:"
                                     home "/.guix-profile/sbin:"
                                     "/run/current-system/profile/bin:"
                                     "/run/current-system/profile/sbin"))
                (ca-home (string-append home "/.guix-home/profile/etc/ssl/certs/ca-certificates.crt"))
                (ca-user (string-append home "/.guix-profile/etc/ssl/certs/ca-certificates.crt"))
                (ca-sys "/etc/ssl/certs/ca-certificates.crt")
                (caf (cond ((access? ca-home R_OK) ca-home)
                           ((access? ca-user R_OK) ca-user)
                           (else ca-sys))))
           (make-forkexec-constructor
            (list #$(file-append bash "/bin/sh") "-lc" #$start-path)
            #:environment-variables
            (list
             (string-append "HOME=" home)
             (string-append "PATH=" path)
             (string-append "SSL_CERT_FILE=" caf)
             (string-append "GIT_SSL_CAINFO=" caf)
             (string-append "CURL_CA_BUNDLE=" caf)
             (string-append "PIP_CERT=" caf)
             (string-append "AGIXT_DIR_REL=" #$dir-rel)
             (string-append "AGIXT_CREDENTIALS_FILE=" #$cred)
             (string-append "AGIXT_BASE_URI=" #$base)
             (string-append "AGIXT_WEBUI_GIT_URL=" #$repo)
             (string-append "AGIXT_WEBUI_PORT=" #$port))
            #:log-file log-path)))
      (stop  #~(make-kill-destructor))
      (respawn? resp)))))

(define (agixt-chatui-shepherd-services cfg)
  (let* ((inst   (agixt-chatui-configuration-instance-name cfg))
         (wd     (agixt-chatui-configuration-workdir cfg))
         (prov   (string->symbol (string-append "agixt-chatui-" inst)))
         (req    (list (string->symbol (string-append "agixt-backend-" inst))))
         (dir-rel (string-append wd "/agixt-chatui-" inst))
         (start-path (string-append "$HOME/" dir-rel "/start.sh"))
         (base   (agixt-chatui-configuration-base-uri cfg))
         (cred   (agixt-chatui-configuration-credentials-file cfg))
         (port   (number->string (agixt-chatui-configuration-port cfg)))
         (repo   (agixt-chatui-configuration-chatui-git-url cfg))
         (entry  (agixt-chatui-configuration-chat-entry cfg))
         (resp   (agixt-chatui-configuration-respawn? cfg)))
    (list
     (shepherd-service
      (provision (list prov))
      (requirement req)
      (documentation (string-append "AGiXT Chat UI for " inst))
      (start
       #~(let* ((home (or (getenv "HOME") "."))
                (log-path (string-append home "/" #$dir-rel "/chatui.log"))
                (path (string-append home "/.guix-home/profile/bin:"
                                     home "/.guix-home/profile/sbin:"
                                     home "/.guix-profile/bin:"
                                     home "/.guix-profile/sbin:"
                                     "/run/current-system/profile/bin:"
                                     "/run/current-system/profile/sbin"))
                (ca-home (string-append home "/.guix-home/profile/etc/ssl/certs/ca-certificates.crt"))
                (ca-user (string-append home "/.guix-profile/etc/ssl/certs/ca-certificates.crt"))
                (ca-sys "/etc/ssl/certs/ca-certificates.crt")
                (caf (cond ((access? ca-home R_OK) ca-home)
                           ((access? ca-user R_OK) ca-user)
                           (else ca-sys))))
           (make-forkexec-constructor
            (list #$(file-append bash "/bin/sh") "-lc" #$start-path)
            #:environment-variables
            (list
             (string-append "HOME=" home)
             (string-append "PATH=" path)
             (string-append "SSL_CERT_FILE=" caf)
             (string-append "GIT_SSL_CAINFO=" caf)
             (string-append "CURL_CA_BUNDLE=" caf)
             (string-append "PIP_CERT=" caf)
             (string-append "AGIXT_DIR_REL=" #$dir-rel)
             (string-append "AGIXT_CREDENTIALS_FILE=" #$cred)
             (string-append "AGIXT_BASE_URI=" #$base)
             (string-append "AGIXT_CHATUI_GIT_URL=" #$repo)
             (string-append "AGIXT_CHATUI_PORT=" #$port)
             (string-append "AGIXT_CHAT_ENTRY=" #$entry))
            #:log-file log-path)))
      (stop  #~(make-kill-destructor))
      (respawn? resp)))))

(define home-agixt-webui-service-type
  (service-type
   (name 'home-agixt-webui)
   (extensions
    (list (service-extension home-profile-service-type agixt-webui-packages)
          (service-extension home-files-service-type   agixt-webui-home-files)
          (service-extension home-shepherd-service-type agixt-webui-shepherd-services)))
   (default-value (agixt-webui-configuration))
   (description "Run the AGiXT Streamlit Web UI (management UI).")))

(define home-agixt-chatui-service-type
  (service-type
   (name 'home-agixt-chatui)
   (extensions
    (list (service-extension home-profile-service-type agixt-chatui-packages)
          (service-extension home-files-service-type   agixt-chatui-home-files)
          (service-extension home-shepherd-service-type agixt-chatui-shepherd-services)))
   (default-value (agixt-chatui-configuration))
   (description "Run the AGiXT chat UI (defaults to Streamlit on a separate port; configurable repo/entry).")))


(define home-agixt-telegram-bots-service-type
  (service-type
   (name 'home-agixt-telegram-bots)
   (extensions
    (list (service-extension home-profile-service-type agixt-telegram-bots-packages)
          (service-extension home-files-service-type   agixt-telegram-bots-home-files)
          (service-extension home-shepherd-service-type agixt-telegram-bots-shepherd-services)))
   (default-value (agixt-telegram-bots-configuration))
   (description "Run one or more Telegram bots for AGiXT. Scripts come from the agixt-scripts package; files staged via home-files alist; secrets live outside Guix.")))
