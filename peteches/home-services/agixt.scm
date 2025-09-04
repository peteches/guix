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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages curl)
  #:use-module (srfi srfi-1)          ; every
  #:use-module (srfi srfi-13)         ; string-join
  #:export (home-agixt-backend-service-type
            home-agixt-telegram-bots-service-type
            agixt-backend-configuration
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

;; -----------------------------------------------------------------------------
;; Backend configuration + services
;; -----------------------------------------------------------------------------
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
  (list agixt-scripts bash git curl python python-pip))

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
         (prov      (string->symbol (string-append "agixt-" inst)))
         (logf      (string-append "$HOME/" wd "/agixt-" inst "/agixt.log"))
         (start     (string-append "sh \"$HOME/" wd "/agixt-" inst "/start.sh\""))
         (dir-rel   (string-append wd "/agixt-" inst)))
    (list
     (shepherd-service
      (provision (list prov))
      (documentation (string-append "AGiXT backend (" inst ")"))
      (start #~(make-forkexec-constructor
                (list #$(file-append bash "/bin/sh") "-lc" #$start)
                #:environment-variables
                (list
                 (string-append "AGIXT_DIR_REL=" #$dir-rel)
                 (string-append "AGIXT_GIT_URL=" #$git-url)
                 (string-append "AGIXT_GIT_BRANCH=" #$git-branch)
                 (string-append "AGIXT_BASE_URI=" #$base)
                 (string-append "AGIXT_CREDENTIALS_FILE=" #$cred))
                #:log-file #$logf))
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
  (list agixt-scripts bash git curl python python-pip))

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
             (logf    (string-append "$HOME/" wd "/agixt-tg-" name "/bot.log"))
             (start   (string-append "sh \"$HOME/" wd "/agixt-tg-" name "/start.sh\""))
             (backend (agixt-telegram-bot-configuration-backend-instance-name b))
             (req     (if backend (list (string->symbol (string-append "agixt-" backend))) '()))
             (cfg-rel (string-append ".config/agixt/bots/" name "/config.env"))
             (dir-rel (string-append wd "/agixt-tg-" name))
             (cred    (agixt-telegram-bot-configuration-credentials-file b)))
        (shepherd-service
         (provision (list prov))
         (requirement req)
         (documentation (string-append "Telegram → AGiXT trigger bot (" name ")"))
         (start #~(make-forkexec-constructor
                   (list #$(file-append bash "/bin/sh") "-lc" #$start)
                   #:environment-variables
                   (list
                    (string-append "AGIXT_TG_DIR_REL=" #$dir-rel)
                    (string-append "AGIXT_TG_CONFIG_PATH_REL=" #$cfg-rel)
                    (string-append "AGIXT_CREDENTIALS_FILE=" #$cred))
                   #:log-file #$logf))
         (stop  #~(make-kill-destructor))
         (respawn? respawn?))))
    (map bot-svc (agixt-telegram-bots-configuration-bots cfg))))

(define home-agixt-telegram-bots-service-type
  (service-type
   (name 'home-agixt-telegram-bots)
   (extensions
    (list (service-extension home-profile-service-type agixt-telegram-bots-packages)
          (service-extension home-files-service-type   agixt-telegram-bots-home-files)
          (service-extension home-shepherd-service-type agixt-telegram-bots-shepherd-services)))
   (default-value (agixt-telegram-bots-configuration))
   (description "Run one or more Telegram bots for AGiXT. Scripts come from the agixt-scripts package; files staged via home-files alist; secrets live outside Guix.")))
