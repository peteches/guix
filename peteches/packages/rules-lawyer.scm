(define-module (peteches packages rules-lawyer)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages node)
  #:use-module (gnu packages rust-apps)
  #:use-module (peteches packages rules-lawyer-deps))

;;; Commentary:
;;;
;;; The rules-lawyer Discord bot: a self-contained Node service that answers
;;; tabletop rules questions (currently Warhammer 40k 11e) by searching a
;;; bundled reference library of rulebook text extractions.
;;;
;;; The source is fetched over plain HTTPS from the private gitolite server
;;; (https://git.ts.peteches.co.uk/rules-lawyer.git/). The package installs:
;;;
;;;   $out/discord/        the bot code (bot.mjs, harness.mjs, llm.mjs, tools.mjs,
;;;                        smoke.mjs, package.json, ...)
;;;   $out/AGENTS.md       the library router doc (embedded as the LLM system prompt)
;;;   $out/systems/**/*.txt  the reference library (text extractions only; the
;;;                        canonical PDFs are deliberately not installed)
;;;   $out/discord/node_modules/  the exact Node dependency closure pinned by
;;;                        discord/package-lock.json, composed by copying the
;;;                        rules-lawyer-deps store items into the lockfile
;;;                        directory structure (including the nested second
;;;                        version of @discordjs/collection under @discordjs/rest
;;;                        and @discordjs/ws).
;;;
;;; node and ripgrep are propagated inputs: the bot runs under node and its
;;; grep tool shells out to ripgrep, so a service running it needs both on PATH.

(define-public rules-lawyer
  (package
    (name "rules-lawyer")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.ts.peteches.co.uk/rules-lawyer.git/")
             (commit "c8ed2b56f1b9c649de56beb5c307084322ed8d31")))
       (sha256
        (base32 "0jc1fz1plrcl209ngar6l1ly24k338yi8sazfbkxgp8s0ymik0kn"))))
    (build-system gnu-build-system)
    (inputs
     (list node-discordjs-builders-1.14.1
           node-discordjs-collection-1.5.3
           node-discordjs-formatters-0.6.2
           node-discordjs-rest-2.6.3
           node-discordjs-collection-2.1.1
           node-discordjs-util-1.2.0
           node-discordjs-ws-1.2.3
           node-sapphire-async-queue-1.5.5
           node-sapphire-shapeshift-4.0.0
           node-sapphire-snowflake-3.5.5
           node-types-node-26.6.5
           node-types-ws-8.18.2
           node-vladfrangu-async-event-emitter-2.4.7
           node-discord-api-types-0.38.57
           node-discordjs-14.27.0
           node-fast-deep-equal-3.1.3
           node-lodash-4.18.1
           node-lodash-snakecase-4.1.1
           node-magic-bytes-js-1.13.1
           node-ts-mixer-6.0.4
           node-tslib-2.8.1
           node-undici-6.29.1
           node-undici-types-8.9.0
           node-ws-8.22.0))
    (propagated-inputs
     (list node ripgrep))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (replace 'install
            (lambda _
              (define (copy-dep base store npm-name dest)
                (let* ((src (string-append store "/lib/node_modules/" npm-name))
                       (dst (string-append base "/" dest)))
                  (mkdir-p dst)
                  (copy-recursively src dst)))
              (let ((nm (string-append #$output "/discord/node_modules")))
                ;; Bot code, router doc, and reference library (text only).
                (copy-recursively "discord" (string-append #$output "/discord"))
                (copy-file "AGENTS.md" (string-append #$output "/AGENTS.md"))
                (for-each (lambda (f)
                            (let ((dst (string-append #$output "/" f)))
                              (mkdir-p (dirname dst))
                              (copy-file f dst)))
                          (find-files "systems" "\\.txt$"))
                ;; Compose node_modules from the pinned dependency store items.
                (copy-dep nm #$node-discordjs-builders-1.14.1
                          "@discordjs/builders" "@discordjs/builders")
                (copy-dep nm #$node-discordjs-collection-1.5.3
                          "@discordjs/collection" "@discordjs/collection")
                (copy-dep nm #$node-discordjs-formatters-0.6.2
                          "@discordjs/formatters" "@discordjs/formatters")
                (copy-dep nm #$node-discordjs-rest-2.6.3
                          "@discordjs/rest" "@discordjs/rest")
                (copy-dep nm #$node-discordjs-collection-2.1.1
                          "@discordjs/collection"
                          "@discordjs/rest/node_modules/@discordjs/collection")
                (copy-dep nm #$node-discordjs-util-1.2.0
                          "@discordjs/util" "@discordjs/util")
                (copy-dep nm #$node-discordjs-ws-1.2.3
                          "@discordjs/ws" "@discordjs/ws")
                (copy-dep nm #$node-discordjs-collection-2.1.1
                          "@discordjs/collection"
                          "@discordjs/ws/node_modules/@discordjs/collection")
                (copy-dep nm #$node-sapphire-async-queue-1.5.5
                          "@sapphire/async-queue" "@sapphire/async-queue")
                (copy-dep nm #$node-sapphire-shapeshift-4.0.0
                          "@sapphire/shapeshift" "@sapphire/shapeshift")
                (copy-dep nm #$node-sapphire-snowflake-3.5.5
                          "@sapphire/snowflake" "@sapphire/snowflake")
                (copy-dep nm #$node-types-node-26.6.5
                          "@types/node" "@types/node")
                (copy-dep nm #$node-types-ws-8.18.2
                          "@types/ws" "@types/ws")
                (copy-dep nm #$node-vladfrangu-async-event-emitter-2.4.7
                          "@vladfrangu/async_event_emitter"
                          "@vladfrangu/async_event_emitter")
                (copy-dep nm #$node-discord-api-types-0.38.57
                          "discord-api-types" "discord-api-types")
                (copy-dep nm #$node-discordjs-14.27.0
                          "discord.js" "discord.js")
                (copy-dep nm #$node-fast-deep-equal-3.1.3
                          "fast-deep-equal" "fast-deep-equal")
                (copy-dep nm #$node-lodash-4.18.1
                          "lodash" "lodash")
                (copy-dep nm #$node-lodash-snakecase-4.1.1
                          "lodash.snakecase" "lodash.snakecase")
                (copy-dep nm #$node-magic-bytes-js-1.13.1
                          "magic-bytes.js" "magic-bytes.js")
                (copy-dep nm #$node-ts-mixer-6.0.4
                          "ts-mixer" "ts-mixer")
                (copy-dep nm #$node-tslib-2.8.1
                          "tslib" "tslib")
                (copy-dep nm #$node-undici-6.29.1
                          "undici" "undici")
                (copy-dep nm #$node-undici-types-8.9.0
                          "undici-types" "undici-types")
                (copy-dep nm #$node-ws-8.22.0
                          "ws" "ws")))))))
    (home-page "https://git.ts.peteches.co.uk/rules-lawyer.git/")
    (synopsis "Discord bot that answers tabletop rules questions from a bundled rulebook library")
    (description
     "rules-lawyer is a self-contained Node service that answers tabletop
rules questions (currently Warhammer 40,000 11th edition) by searching a
bundled reference library of rulebook text extractions. It bridges the
Discord gateway and an OpenAI-compatible LLM, exposing the library to the
model through sandboxed read-only tools (list_dir, read_file, grep) and a
sandboxed outbox for written answers.")
    (license license:asl2.0)))