(define-module (peteches services ai-bots)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu packages node)
  #:use-module (gnu packages video)
  #:use-module (peteches packages ai-bots)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (ai-bots-service-type
            ai-bots-configuration
            ai-bots-configuration?
            ai-bots-configuration-bots
            <ai-bot>
            ai-bot ai-bot?
            ai-bot-name ai-bot-dir ai-bot-package ai-bot-user ai-bot-group
            ai-bot-data-dir ai-bot-log-file ai-bot-environment ai-bot-secrets
            ai-bot-allowed-chats ai-bot-command-args ai-bot-documentation
            ai-bot-requirement
            ;; Exported so a system config can pick apart the pieces — and so the modules can be
            ;; exercised in Guile without instantiating a whole operating system.
            ai-bots-accounts ai-bots-activation ai-bots-profile ai-bot-shepherd-service
            %tiffany-bot))

;;; Commentary:
;;;
;;; Runs the bots packaged by (peteches packages ai-bots) as shepherd services.  Each entry in
;;; the `bots' list is one bot: it gets a system user and group, a writable state directory, its
;;; own log file, its package in the system profile, and a service that execs bin/<bot> from the
;;; store under node.
;;;
;;; ── What the service hands each bot ────────────────────────────────────────────────────────
;;;
;;; Environment variables are derived from the bot's name uppercased, because a packaged bot's
;;; config.json lives in the read-only store and cannot be edited at deploy time:
;;;
;;;   TIFFANY_DATA_DIR       default /var/lib/tiffany.  The sqlite databases, transcripts,
;;;                         inbound voice notes, generated pictures and telegram-offset.json all
;;;                         go there.  bots/Tiffany/config.json spells every path as
;;;                         ${TIFFANY_DATA_DIR:-data}/… , which is what gets them out of the store.
;;;   TIFFANY_ALLOWED_CHATS  from `allowed-chats'.  Empty means she polls and answers nobody, so
;;;                         set it — chat ids are not secrets.
;;;   anything else          pass through in `environment' (STRATA_MODEL, COMFYUI_URL, ...).
;;;
;;; `secrets' entries are "VAR=/run/secrets/file" strings, read INSIDE the start lambda: reading
;;; them at configure time, or from an activation snippet, races the sops decryption one-shot that
;;; 'sops-secrets provides.  services/rules-lawyer.scm documents that trap in detail; this service
;;; takes the same shape on purpose.  A declared secret file that is missing prints a message
;;; naming the path (the thing to fix in the system's #:sops-secrets list) instead of pretending to
;;; work — but it does not stop the process, so she can still serve CLI commands from the shell.
;;;
;;; PATH leads with node and ffmpeg.  The launcher bakes in node's absolute path so it runs under
;;; any PATH, but the voice pipeline shells out to ffmpeg (Telegram accepts only ogg/opus for a
;;; voice note) and src/lib/system.mjs checks PATH before scanning /gnu/store; putting them first
;;; is what makes voice replies deterministic.
;;;
;;; ── Wiring it up on a box ──────────────────────────────────────────────────────────────────
;;;
;;;   (service ai-bots-service-type
;;;     (ai-bots-configuration
;;;       (bots (list (ai-bot (inherit %tiffany-bot)
;;;                 (allowed-chats '("1234567890")))))))
;;;
;;; plus the secret, in that box's #:sops-secrets list:
;;;
;;;   (sops-secret (key '("telegram_bot_token"))
;;;                (file (local-file "../../secrets/hosts/agents/tiffany-bot.yaml"))
;;;                (user "tiffany") (group "tiffany") (permissions #o400)
;;;                (path "/run/secrets/tiffany-telegram-token"))
;;;
;;; The secret's user/group has to match the bot's account, otherwise she cannot read her own
;;; token: every other secret on these boxes is root-owned mode 400 only because its consumer runs
;;; as root.

;; ── Bot definition ───────────────────────────────────────────────────────────────────────────

(define-record-type* <ai-bot>
  ai-bot make-ai-bot ai-bot?
  (name ai-bot-name                       ; "tiffany": launcher name, service tag, account stem
         (default "tiffany"))
  (dir ai-bot-dir                         ; bots/<Dir> in the store; defaults to capitalized name
        (default #f))
  (package ai-bot-package (default ai-bots))
  (user ai-bot-user (default #f))         ; defaults to the bot's name
  (group ai-bot-group (default #f))       ; defaults to the bot's name
  (data-dir ai-bot-data-dir (default #f)) ; defaults to /var/lib/<bot>
  (log-file ai-bot-log-file (default #f)) ; defaults to /var/log/<bot>.log
  (environment ai-bot-environment         ; plain "KEY=value" strings — NOT secrets
                (default '()))
  (secrets ai-bot-secrets                 ; "VAR=/run/secrets/file", read when the service starts
           (default '()))
  (allowed-chats ai-bot-allowed-chats     ; Telegram chat ids or @usernames she may talk to
                 (default '()))
  (command-args ai-bot-command-args       ; what the launcher is invoked with; 'run is the daemon
                (default '("run")))
  (requirement ai-bot-requirement         ; shepherd prerequisites.  Drop 'sops-secrets on a box
               (default '(networking file-systems sops-secrets))) ; that decrypts nothing at boot
                                        ; and uses `environment' for tokens instead.
  (documentation ai-bot-documentation (default "area_51 AI bot.")))

(define-record-type* <ai-bots-configuration>
  ai-bots-configuration make-ai-bots-configuration ai-bots-configuration?
  (bots ai-bots-configuration-bots (default '())))

;; ── Derived defaults ─────────────────────────────────────────────────────────────────────────
;;;
;;; Every one of these depends on the bot's name, and a record default cannot refer to another
;;; field — so they stay #f in the record and are resolved here.

(define (%bot-id b) (string-downcase (ai-bot-name b)))
(define (%bot-store-dir b) (or (ai-bot-dir b) (string-titlecase (ai-bot-name b))))
(define (%bot-user b) (or (ai-bot-user b) (%bot-id b)))
(define (%bot-group b) (or (ai-bot-group b) (%bot-id b)))
(define (%bot-data-dir b) (or (ai-bot-data-dir b) (string-append "/var/lib/" (%bot-id b))))
(define (%bot-log-file b) (or (ai-bot-log-file b) (string-append "/var/log/" (%bot-id b) ".log")))
(define (%bot-launcher b) (file-append (ai-bot-package b) "/bin/" (%bot-id b)))

;;; The env-var prefix a bot's config.json is written against: tiffany gives TIFFANY_DATA_DIR and
;;; TIFFANY_KEEP_ALLOWED_CHATS.  Keep bot names lowercase, single-word, or the names turn ugly.
(define (%bot-env-prefix b) (string-upcase (%bot-id b)))

;; ── Accounts ─────────────────────────────────────────────────────────────────────────────────

(define (%bot-users config)
  ;; Deduped by user name: several bots may deliberately share one account, in which case the
  ;; first one's home directory wins.
  (let loop ((bots (ai-bots-configuration-bots config)) (seen '()) (out '()))
    (if (null? bots)
        (reverse out)
        (let* ((b (car bots))
               (u (%bot-user b)))
          (if (member u seen string=)
              (loop (cdr bots) seen out)
              (loop (cdr bots) (cons u seen)
                    (cons (user-account
                           (name u)
                           (group (%bot-group b))
                           (system? #t)
                           (comment (string-append "ai-bots service account for " (%bot-id b)))
                           (home-directory (%bot-data-dir b))
                           (shell (file-append shadow "/sbin/nologin")))
                          out)))))))

(define (ai-bots-accounts config)
  (append (map (lambda (g) (group (name g) (system? #t) (comment "ai-bots service group")))
               (delete-duplicates (map %bot-group (ai-bots-configuration-bots config)) string=))
          (%bot-users config)))

;; ── Activation: the state directories, which are deliberately not in the store ───────────────

(define (ai-bots-activation config)
  ;; Plain (dir user) data computed at configure time; the gexp only iterates it.
  (let ((specs (delete-duplicates
                (map (lambda (b) (list (%bot-data-dir b) (%bot-user b)))
                     (ai-bots-configuration-bots config))
                (lambda (a b) (string= (car a) (car b))))))
    #~(begin
        (use-modules (guix build utils) (srfi srfi-1))
        ;; '#$specs emits (quote (("/var/lib/tiffany" "tiffany") ...)): data in code position.
        ;; A bare #$specs splices the list unquoted, which reads as a call of the string
        ;; "/var/lib/tiffany" on the strings after it and dies with "Wrong type to apply".
        (for-each (lambda (spec)
                    (let* ((dir (first spec))
                           (uname (second spec))
                           (pw (getpwnam uname)))
                      (mkdir-p dir)
                      ;; Accounts are activated before this: the shepherd services require
                      ;; file-systems, and account-service-type runs earlier in the order.
                      (if pw
                          (begin
                            (chown dir (passwd:uid pw) (passwd:gid pw))
                            (chmod dir #o750))
                          (format #t "ai-bots: user ~a is missing, leaving ~a root-owned~%"
                                  uname dir))))
                  '#$specs))))

;; ── Shepherd services ────────────────────────────────────────────────────────────────────────

(define (%parse-secret-spec spec)
  ;; "VAR=/run/secrets/file" -> ("VAR" "/run/secrets/file").  Split on the FIRST '=', since both
  ;; paths and values can contain more of them.
  (let ((i (string-index spec #\=)))
    (unless i
      (error "ai-bots: a secrets entry wants VAR=/run/secrets/file, got" spec))
    (list (string-take spec i) (string-drop spec (+ 1 i)))))

(define (%chat-id-string c)
  ;; Supergroup ids genuinely begin with a dash ("-1001234567890"), and numeric ids are usually
  ;; written as numbers in the system config; both have to arrive at her as text.
  (if (number? c) (number->string c) (string c)))

(define (ai-bot-shepherd-service b)
  (let* ((pkg (ai-bot-package b))
         (prefix (%bot-env-prefix b))
         (secret-pairs (map %parse-secret-spec (ai-bot-secrets b)))
         ;; Configure-time values go in as strings; the secrets are read at start instead.
         (static-env
          (append
           (list (string-append prefix "_DATA_DIR=" (%bot-data-dir b))
                 (string-append "PATH=" #$node "/bin:" #$ffmpeg "/bin"
                                ":/run/current-system/profile/bin"
                                ":/run/current-system/profile/sbin:/run/setuid-programs"))
           (let ((chats (ai-bot-allowed-chats b)))
             (if (null? chats)
                 '()
                 (list (string-append prefix "_ALLOWED_CHATS="
                                      (string-join (map %chat-id-string chats) " ")))))
           (map string (ai-bot-environment b)))))
    (shepherd-service
     (provision (list (symbol-append 'aibots- (string->symbol (%bot-id b)))))
     (documentation (ai-bot-documentation b))
     (requirement (ai-bot-requirement b))
     (start
      ;; A lambda and not a bare constructor, so the secret reads happen at start.  Note the two
      ;; quotes on '#$static-env: they emit (quote ("PATH=..." ...)), a list literal.  Unquoted,
      ;; those strings land in call position and blow up with "Wrong type to apply".
      #~(lambda _
          (let ((secret-env
                 (fold (lambda (pair acc)
                         (let ((var (first pair))
                               (file (second pair)))
                           (if (file-exists? file)
                               ;; Trim the trailing newline the sops one-shot leaves behind and
                               ;; nothing else: a token is exactly what the file holds.
                               (cons (string-append var "="
                                                    (string-trim-both
                                                     (call-with-input-file file
                                                       (@ (ice-9 textual-ports) get-string-all))))
                                                   acc)
                               (begin
                                 (format (current-error-port)
                                         "ai-bots: secret ~a not found at ~a~%" var file)
                                 acc))))
                       '()
                       '#$secret-pairs)))
            ((make-forkexec-constructor
              ;; cons* splices the quoted list of extra arguments onto the launcher path.
              (cons* #$(%bot-launcher b) '#$command-args)
              #:directory #$(file-append pkg "/share/aibots")
              ;; HOME comes from the account's home-directory, which activation just made and
              ;; chowned; node wants a writable HOME for its temp files.
              #:user #$(%bot-user b)
              #:group #$(%bot-group b)
              #:environment-variables (append secret-env '#$static-env)
              #:log-file #$(%bot-log-file b)))))
     (stop #~(make-kill-destructor)))))

;; ── Profile: enabling the service installs the bots ──────────────────────────────────────────

(define (ai-bots-profile config)
  (delete-duplicates (map ai-bot-package (ai-bots-configuration-bots config))))

;; ── Service type ─────────────────────────────────────────────────────────────────────────────

(define-public ai-bots-service-type
  (service-type
   (name 'ai-bots)
   (description
    "Run the area_51 AI bots (Tiffany and whoever joins her) as shepherd services, each with its
own unprivileged account, state directory under /var/lib, log file and package in the system
profile.")
   (extensions
    (list (service-extension account-service-type ai-bots-accounts)
          (service-extension activation-service-type ai-bots-activation)
          (service-extension shepherd-root-service-type
                             (lambda (config)
                               (map ai-bot-shepherd-service
                                    (ai-bots-configuration-bots config))))
          (service-extension profile-service-type ai-bots-profile)))
   ;; Empty by default: enabling the module on a box with no bots listed must create no accounts
   ;; and start nothing.
   (default-value (ai-bots-configuration))))

;; ── A ready-made definition for Tiffany ──────────────────────────────────────────────────────

(define-public %tiffany-bot
  (ai-bot
   (name "tiffany")
   (dir "Tiffany")
   (documentation
    "Tiffany — Peteches' Telegram assistant (@peteches_pa_bot): keeps his calendar with DST-safe
reminders and prep nudges, remembers facts about him, researches on the web, speaks through
AllTalk, draws herself in ComfyUI, and drives herdr and pi sessions on this box.")
   ;; Read at service start.  Decrypt it to a file this bot's account can read — see commentary.
   (secrets '("TIFFANY_TELEGRAM_BOT_TOKEN=/run/secrets/tiffany-telegram-token"))
   ;; Endpoints come from bots/Tiffany/config.json with ${VAR:-default} fallbacks; override the
   ;; ones you want pinned here.  Chat ids belong in `allowed-chats', not in this list.
   (environment '("STRATA_MODEL=qwen3.8-flash-next"))))
