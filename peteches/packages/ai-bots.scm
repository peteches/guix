(define-module (peteches packages ai-bots)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages node)
  #:use-module (gnu packages video))

;;; Commentary:
;;;
;;; The area_51 AI bots — Tiffany first (@peteches_pa_bot on Telegram), and whatever else
;;; lands under bots/ in the aiBots repository.  Everything there is plain Node ESM with
;;; ZERO npm dependencies: src/lib reimplements what we need on top of node:sqlite,
;;; node:http and Intl.  There is no dependency closure to compose, nothing to compile and
;;; no package-lock.json to honour, which is exactly why this package can be a copy-build-
;;; system with one extra phase instead of a hand-built node_modules like rules-lawyer needs.
;;;
;;; Source comes from gitolite over plain HTTPS, the same way rules-lawyer does it:
;;;
;;;   https://git.ts.peteches.co.uk/ai-bots.git/     the form our packages already use
;;;   ssh://<you>@git.ts.peteches.co.uk/ai-bots.git  where you push from (gitolite ACLs)
;;;
;;; What gets installed:
;;;
;;;   $out/share/aibots/src/lib/*.mjs   the shared libraries every bot shares
;;;   $out/share/aibots/bots/<Bot>/     code, config.json, persona.json, prompt/, workflows/
;;;   $out/bin/<bot>                    exec node $out/share/aibots/bots/<Bot>/index.mjs "$@"
;;;
;;; bots/<Bot>/data/ is deliberately NOT installed.  A store path is read-only and a running
;;; bot writes sqlite databases, transcripts, inbound voice notes and generated pictures; the
;;; service sets TIFFANY_DATA_DIR to /var/lib/<bot> rather than letting it fall back to
;;; bots/<Bot>/data next to the code (see services/ai-bots.scm).  persona.json, prompt/system.md
;;; and config.json are read-only inputs and do belong in the store.
;;;
;;; node is propagated because the wrapper execs it.  ffmpeg (+ffprobe) is propagated because
;;; the voice pipeline shells out to it: Telegram only accepts ogg/opus for voice notes, so a
;;; synthesized .wav has to be transcoded before it can be sent.  src/lib/system.mjs looks on
;;; PATH first and then scans /gnu/store, so having ffmpeg in the profile is enough either way.
;;;
;;; Updating the pin: set AI-BOTS-COMMIT to the commit you pushed, run
;;;   guix build -f peteches/packages/ai-bots.scm
;;; Guix answers "hash mismatch in ... got SHA256: <new>" — put that into the sha256.  Commit
;;; and hash move together; a fresh commit with a stale hash is the half-update that eats a
;;; build before it errors.

(define-public ai-bots-repo-url "https://git.ts.peteches.co.uk/ai-bots.git/")

;; Pinned commit of the aiBots repository.  Replace the placeholder below together with this.
(define-public ai-bots-commit "98c58406d98b5bf3c5b185b3d64929c72717d84c")

(define-public ai-bots
  (package
    (name "ai-bots")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url ai-bots-repo-url) (commit ai-bots-commit)))
       (file-name (git-file-name name version))
       ;; Placeholder: replace with the hash Guix reports for the commit above.
       (sha256 (base32 "1jrynflgn0lnp9gr9jqyabg2bsk80bf87l0p7aqgi8imxyrvr61p"))))
    (build-system copy-build-system)
    ;; Nothing is compiled: node and ffmpeg are wanted at run time by the installed bots, so they
    ;; propagate instead of being build-only inputs.
    (propagated-inputs (list node ffmpeg))
    (arguments
     (list
      ;; tests/ is fast and network-free but assumes a writable checkout (it drops fake
      ;; executables into a temp dir and resolves ffmpeg through the store).  Run the suites in
      ;; the repo — see `npm test` — rather than inside this build.
      #:tests? #f
      #:install-plan
      #~'(("src/" "share/aibots/src/")
          ("bots/" "share/aibots/bots/")
          ("package.json" "share/aibots/")
          ("README.md" "share/aibots/")
          ("AGENTS.md" "share/aibots/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'strip-state-and-generate-launchers
            (lambda _
              ;; (ice-9 format) is not re-exported by (guix build utils), and this phase writes
              ;; the launchers with format; ftw was never used.
              (use-modules (guix build utils) (ice-9 format))
              (let ((share (string-append #$output "/share/aibots"))
                    (bindir (string-append #$output "/bin")))
                ;; Anything a running bot writes must not come out of the store.
                ;; Generated media is pruned by extension below, so bots/<Bot>/avatar/*.png goes
                ;; while the script that produced it stays in the store.
                (for-each delete-file-recursively
                          (find-files share "^data$" #:directories? #t))
                (for-each delete-file
                          (append (find-files share "\\.(sqlite3?|sqlite3?-wal|sqlite3?-shm|png|jpg|jpeg|wav|ogg|opus)$")
                                  (find-files share "telegram-offset\\.json$")))
                ;; Launchers: one per bot, i.e. per directory under bots/ that ships an index.mjs.
                ;; Scan bots/ itself rather than the whole store output — find-files matches its
                ;; pattern against the BASE name (guix/build/utils.scm's file-name-predicate), so a
                ;; path-shaped pattern silently matches nothing, and scanning from `share` also
                ;; picked up src/index.mjs, which is the shared library entry point and not a bot.
                ;; The store path of node is baked into each wrapper so it runs under any PATH.
                (mkdir-p bindir)
                (for-each
                  (lambda (entry)
                    (let* ((bot-dir-name (basename (dirname entry)))
                           (launcher (string-append bindir "/" (string-downcase bot-dir-name))))
                      (format #t "writing launcher ~a for bots/~a~%" launcher bot-dir-name)
                      (call-with-output-file launcher
                        (lambda (port)
                          (format port "#!/bin/sh\n# launcher for bots/~a — run as a service (see peteches/services/ai-bots.scm)\nexec ~a/bin/node \"~a/bots/~a/index.mjs\" \"$@\"\n"
                                  bot-dir-name #$node share bot-dir-name)))
                      (chmod launcher #o755)
                      ;; The index.mjs files carry shebangs; keep them executable so
                      ;; ./bots/Tiffany/index.mjs works straight out of the store too.
                      (for-each (lambda (mjs) (chmod mjs #o755))
                                (find-files (string-append share "/bots/" bot-dir-name) "\\.mjs$"))))
                  (sort (find-files (string-append share "/bots") "^index\\.mjs$") string<))))))))
    (synopsis "Telegram-connected AI assistants for the area_51 fleet")
    (description
     "Node.js assistant bots written for Peteches' area_51 machines.  Each bot is a directory
under @code{bots/} with its own persona, tool wiring and configuration, sharing the libraries in
@code{src/lib}: a Telegram Bot API client with HTML escaping and chunking, a calendar with RRULE
recurrence, DST-safe wall-clock reminders and ICS import/export, SQLite memory with FTS5 recall,
an OpenAI-compatible client for the strata inference gateway, ComfyUI workflow generation, AllTalk
text-to-speech, WhisperX transcription, and wrappers around herdr and pi.  Tiffany is the first of
them; this package installs one launcher per bot.")
    (home-page "https://git.ts.peteches.co.uk/ai-bots")
    (license license:asl2.0)))
