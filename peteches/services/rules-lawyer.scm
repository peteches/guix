(define-module (peteches services rules-lawyer)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages node)
  #:use-module (peteches packages rules-lawyer)
  #:use-module (srfi srfi-13)
  #:export (rules-lawyer-configuration
            rules-lawyer-configuration?
            rules-lawyer-configuration-package
            rules-lawyer-configuration-discord-token-file
            rules-lawyer-configuration-llm-api-key-file
            rules-lawyer-configuration-llm-base-url
            rules-lawyer-configuration-llm-model
            rules-lawyer-configuration-llm-max-tokens
            rules-lawyer-configuration-llm-temperature
            rules-lawyer-configuration-listen-channel-ids
            rules-lawyer-configuration-idle-timeout-minutes
            rules-lawyer-configuration-outbox-dir
            rules-lawyer-configuration-log-file
            rules-lawyer-service-type))

;;; Commentary:
;;;
;;; Service type for the rules-lawyer Discord bot (see
;;; (peteches packages rules-lawyer)).  Runs the bot as a Shepherd service
;;; under the system user 'rules-lawyer', launching:
;;;
;;;   node bot.mjs          (working directory: <package>/discord)
;;;
;;; The bot's required configuration is supplied as environment variables:
;;;
;;;   LLM_BASE_URL, LLM_MODEL   plain values, set in the service configuration
;;;   DISCORD_TOKEN             read from a SOPS-decrypted file at service start
;;;   LLM_API_KEY               optional, read from a SOPS-decrypted file when set
;;;   LLM_MAX_TOKENS, LLM_TEMPERATURE, LISTEN_CHANNEL_IDS, IDLE_TIMEOUT_MINUTES
;;;                             optional plain values
;;;   OUTBOX_ROOT               a writable directory (the bot's default points
;;;                             into the read-only store, so it is overridden)
;;;
;;; LIBRARY_ROOT is left at the bot's default (the parent of the discord/ dir,
;;; i.e. the package root), which is where the reference library and AGENTS.md
;;; are installed.
;;;
;;; SOPS wiring (done in the VM system configuration, not here): the VM must
;;; decrypt the Discord token into the file referenced by discord-token-file,
;;; e.g.
;;;
;;;   (sops-secret
;;;    (key '("discord-token"))
;;;    (file (local-file "../../secrets/hosts/<vm>/rules-lawyer.yaml"))
;;;    (path "/run/secrets/rules-lawyer-discord-token")
;;;    (user "rules-lawyer")
;;;    (permissions #o400))
;;;
;;; The service requires the 'sops-secrets' provision so it only starts once
;;; the token file has been decrypted.

;;; ── Configuration record ─────────────────────────────────────────────────

(define-record-type* <rules-lawyer-configuration> rules-lawyer-configuration
                     make-rules-lawyer-configuration
  rules-lawyer-configuration?
  ;; The package providing the bot code, reference library and node_modules.
  (package rules-lawyer-configuration-package
           (default rules-lawyer))
  ;; Path to the SOPS-decrypted Discord bot token.  Its content is read at
  ;; service start and passed as DISCORD_TOKEN.
  (discord-token-file rules-lawyer-configuration-discord-token-file
                      (default "/run/secrets/rules-lawyer-discord-token"))
  ;; Optional path to a SOPS-decrypted LLM API key.  When non-empty its
  ;; content is read at service start and passed as LLM_API_KEY; when empty
  ;; no auth header is sent to the LLM.
  (llm-api-key-file rules-lawyer-configuration-llm-api-key-file
                    (default ""))
  ;; Base URL of the OpenAI-compatible LLM endpoint, e.g.
  ;; "https://api.openai.com/v1".  Required — the bot refuses to start without it.
  (llm-base-url rules-lawyer-configuration-llm-base-url)
  ;; Model name passed to the LLM, e.g. "gpt-4o".  Required.
  (llm-model rules-lawyer-configuration-llm-model)
  ;; Optional LLM max output tokens (number).
  (llm-max-tokens rules-lawyer-configuration-llm-max-tokens
                  (default #f))
  ;; Optional LLM temperature (number, default 0.2 in the bot).
  (llm-temperature rules-lawyer-configuration-llm-temperature
                  (default #f))
  ;; Optional comma-separated Discord channel IDs the bot listens in even
  ;; without an @mention.  Empty = respond to @mentions only.
  (listen-channel-ids rules-lawyer-configuration-listen-channel-ids
                      (default ""))
  ;; Optional conversation idle timeout in minutes (number, default 30 in the
  ;; bot).
  (idle-timeout-minutes rules-lawyer-configuration-idle-timeout-minutes
                        (default #f))
  ;; Writable directory for the bot's outbox (written answers).  The bot's
  ;; default points into the read-only store, so this is always overridden.
  (outbox-dir rules-lawyer-configuration-outbox-dir
              (default "/var/lib/rules-lawyer/outbox"))
  (log-file rules-lawyer-configuration-log-file
            (default "/var/log/rules-lawyer.log")))

;;; ── Accounts ─────────────────────────────────────────────────────────────

(define (rules-lawyer-accounts config)
  (list (user-group
          (name "rules-lawyer")
          (system? #t))
        (user-account
          (name "rules-lawyer")
          (group "rules-lawyer")
          (system? #t)
          (comment "rules-lawyer Discord bot")
          (home-directory (rules-lawyer-configuration-outbox-dir config))
          (shell (file-append shadow "/sbin/nologin")))))

;;; ── Activation ───────────────────────────────────────────────────────────

(define (rules-lawyer-activation config)
  (let ((outbox-dir (rules-lawyer-configuration-outbox-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam "rules-lawyer"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$outbox-dir)
          (chown #$outbox-dir uid gid)))))

;;; ── Shepherd service ─────────────────────────────────────────────────────

(define (rules-lawyer-shepherd-service config)
  (let* ((pkg (rules-lawyer-configuration-package config))
         (discord-token-file
          (rules-lawyer-configuration-discord-token-file config))
         (llm-api-key-file
          (rules-lawyer-configuration-llm-api-key-file config))
         (outbox-dir (rules-lawyer-configuration-outbox-dir config))
         (log-file (rules-lawyer-configuration-log-file config))
         ;; Plain (non-secret) environment variables, computed at configure
         ;; time and inlined.  Secrets are read from /run/secrets/ at service
         ;; start, below.
         (static-env
          (append
           (list (string-append "LLM_BASE_URL="
                                (rules-lawyer-configuration-llm-base-url
                                 config))
                 (string-append "LLM_MODEL="
                                (rules-lawyer-configuration-llm-model
                                 config))
                 (string-append "OUTBOX_ROOT=" outbox-dir)
                 "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs")
           (let ((mt (rules-lawyer-configuration-llm-max-tokens config)))
             (if mt (list (string-append "LLM_MAX_TOKENS="
                                         (number->string mt)))
                 '()))
           (let ((temp (rules-lawyer-configuration-llm-temperature config)))
             (if temp (list (string-append "LLM_TEMPERATURE="
                                           (number->string temp)))
                 '()))
           (let ((ids (rules-lawyer-configuration-listen-channel-ids config))
                 (idle (rules-lawyer-configuration-idle-timeout-minutes config)))
             (append (if (and ids (not (string-null? ids)))
                         (list (string-append "LISTEN_CHANNEL_IDS=" ids))
                         '())
                     (if idle (list (string-append "IDLE_TIMEOUT_MINUTES="
                                                   (number->string idle)))
                         '()))))))
    (list (shepherd-service
           (provision '(rules-lawyer))
           (documentation
            "rules-lawyer Discord bot — answers tabletop rules questions from a bundled rulebook library.")
           (requirement '(networking file-systems sops-secrets))
           (start
            #~(make-forkexec-constructor
               (list #$(file-append node "/bin/node") "bot.mjs")
               #:directory #$(file-append pkg "/discord")
               #:environment-variables
               (append
                ;; DISCORD_TOKEN is read from the SOPS-decrypted file at
                ;; service start (the file is guaranteed present by the
                ;; 'sops-secrets' requirement).
                (list (string-append
                       "DISCORD_TOKEN="
                       (string-trim-right
                        (call-with-input-file #$discord-token-file
                                              get-string-all))))
                ;; LLM_API_KEY, only when a secret file is configured.
                (if #$llm-api-key-file
                    (list (string-append
                           "LLM_API_KEY="
                           (string-trim-right
                            (call-with-input-file #$llm-api-key-file
                                                  get-string-all))))
                    '())
                ;; Plain configuration values.
                #$static-env)
               #:user "rules-lawyer"
               #:group "rules-lawyer"
               #:log-file #$log-file))
           (stop #~(make-kill-destructor))))))

;;; ── Profile ──────────────────────────────────────────────────────────────

(define (rules-lawyer-profile config)
  (list (rules-lawyer-configuration-package config)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public rules-lawyer-service-type
  (service-type
   (name 'rules-lawyer)
   (description
    "rules-lawyer Discord bot — answers tabletop rules questions (Warhammer 40,000 11e).")
   (extensions
    (list (service-extension account-service-type rules-lawyer-accounts)
          (service-extension activation-service-type rules-lawyer-activation)
          (service-extension shepherd-root-service-type
                             rules-lawyer-shepherd-service)
          (service-extension profile-service-type rules-lawyer-profile)))
   (default-value
    (rules-lawyer-configuration
     (llm-base-url "http://localhost:1234/v1")
     (llm-model "local-model")))))