(define-module (peteches packages ai-bots)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages node)
  #:use-module (gnu packages video)
  #:use-module (peteches packages pi-coding-agent))

;;; Commentary:
;;;
;;; The area_51 AI bots — Tiffany first (@peteches_pa_bot on Telegram), and whatever else
;;; lands under bots/ in the aiBots repository.  Everything there is plain Node ESM: src/lib
;;; reimplements what we need on top of node:sqlite, node:http and Intl, so there is no npm install,
;;; nothing to compile and no package-lock.json to honour — which is why this package can be a
;;; copy-build-system with two extra phases instead of a hand-built dependency tree like rules-lawyer
;;; needs.
;;;
;;; One thing it does have since the pi-durable work (aiBots docs/rearchitecture-on-pi-durable.md):
;;; bots/Tiffany/host.mjs imports @earendil-works/pi-durable, /chord/context and pi-ai/models as bare
;;; specifiers, and ESM ignores NODE_PATH.  So the package installs a node_modules farm next to the
;;; code — symlinks into store paths of Guix-packaged packages from
;;; peteches/packages/pi-coding-agent.scm (the 1.5.2 line).  Symlinks are enough because each of those
;;; carries its whole npm closure inside itself as real directories (npm --install-links; checked
;;; against the built output), which is also why none of the realpath-vs-symlink trouble described at
;;; length in that file applies here: nothing resolves out of a symlink into a bare store path.
;;; openai and partial-json need no entry — they already live inside pi-ai's own tree.
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
;;;   $out/share/aibots/docs/           design docs and the dependency pin file (index.mjs status
;;;                                     reads docs/dependencies.json next to its own file)
;;;   $out/share/aibots/node_modules/   symlinks for the durable host's bare specifiers
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
(define-public ai-bots-commit "578842f1e136da921fb889c6cf5d908ffb056365")

(define-public ai-bots
  (package
    (name "ai-bots")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url ai-bots-repo-url) (commit ai-bots-commit)))
       (file-name (git-file-name name version))
      ;; Hash for the commit above, obtained by the ritual: poison it to all-zeros, build once, copy the
      ;; hash Guix reports.  A stale hash here reuses last week's checkout instead of failing.
      ;; This one is a trap in two layers: its first two characters are "0x", which reads exactly like a
      ;; hexadecimal prefix that Guix had added — and 'x' really is a digit in nix-base32 (the alphabet is
      ;; 0123456789abcdfghijklmnpqrsvwxyz).  Strip those two chars and you get a legal-looking 50-char
      ;; string; left-pad it back to the 52 Guix wants and the file loads fine, decodes fine, round-trips
      ;; fine — and describes a digest that differs from the real one only in its last byte (…d500 vs
      ;; …d574 measured here), so the build fails with "hash mismatch" instead of "invalid length".
      ;; Copy the reported string verbatim; `guix hash -rx --format=base16 <checkout>' plus a converter is
      ;; how I confirmed which one was right when Guix's own message misled me.  The value below came from the
      ;; same ritual for 0.3.2 (poison to all-zeros, build, copy `09zhqix…`), and this time Guix printed no
      ;; "0x" to mislead anyone: nix-base32 digits include '0', so a hash may simply start with one.
       (sha256 (base32 "09zhqix4k0rvrx04fbq9vvrinr2dp83664sl2sa43847a1sibv8z"))))
    (build-system copy-build-system)
    ;; Nothing is compiled: node and ffmpeg are wanted at run time by the installed bots, so they
    ;; propagate instead of being build-only inputs.  The @earendil-works packages propagate too:
    ;; they are what the installed node_modules farm points at, and naming them here keeps
    ;; `guix install ai-bots` from looking like it installs a bot that cannot import its own engine.
    (propagated-inputs (list node ffmpeg
                  node-earendil-works-pi-durable-1.5.2
                  node-earendil-works-chord-1.5.2
                  node-earendil-works-pi-ai-1.5.2
                  node-earendil-works-pi-schema-1.5.2
                  node-earendil-works-pi-telemetry-1.5.2))
    (arguments
     (list
      ;; tests/ is fast and network-free but assumes a writable checkout (it drops fake
      ;; executables into a temp dir and resolves ffmpeg through the store).  Run the suites in
      ;; the repo — see `npm test` — rather than inside this build.
      #:tests? #f
      #:install-plan
      #~'(("src/" "share/aibots/src/")
          ("bots/" "share/aibots/bots/")
          ("docs/" "share/aibots/docs/")
          ("package.json" "share/aibots/")
          ("README.md" "share/aibots/")
          ("AGENTS.md" "share/aibots/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'assert-node-floor
            (lambda _
              ;; pi-durable 1.5.2 declares engines.node >=22.19 and its SQLite storage leans on
              ;; node:sqlite APIs from that line, so check the version of the binary the launchers
              ;; actually exec instead of trusting a comment.  This phase's body compiles into the
              ;; builder script, not this file's module, so declare what it uses: (ice-9 popen) for
              ;; open-pipe*/close-pipe — Guile does not put them in default scope, which is how the first
              ;; attempt at this phase died with "Unbound variable: open-pipe*" — and (ice-9 rdelim) for
              ;; read-line.
              (use-modules (guix build utils) (ice-9 popen) (ice-9 rdelim))
              (let* ((pipe (open-pipe* OPEN_READ (string-append #$node "/bin/node") "--version"))
                     ;; read-line's answer, not close-pipe's: `begin' returns its LAST expression, so the
                     ;; first version of this line bound raw to close-pipe's exit status (a fixnum) and
                     ;; string-ref answered "Wrong type argument in position 1 (expecting string): 0".
                     (raw (let ((line (read-line pipe))) (close-pipe pipe) line))
                     (s (if (char=? (string-ref raw 0) #\v) (substring raw 1) raw))
                     (dot1 (string-index s #\.))
                     (major (string->number (substring s 0 dot1)))
                     (after (substring s (+ dot1 1)))
                     (dot2 (string-index after #\.))
                     (minor (string->number (if dot2 (substring after 0 dot2) after))))
                (format #t "node for the launchers: ~a (floor is >=22.19 for pi-durable)~%" raw)
                (when (not (or (> major 22) (and (= major 22) (>= minor 19))))
                  (error "node version does not satisfy pi-durable's engines floor of >=22.19" raw)))))
          (add-after 'assert-node-floor 'link-durable-node-modules
            (lambda _
              ;; Answer Tiffany's bare specifiers from a node_modules directory next to the code,
              ;; because ESM ignores NODE_PATH: symlink each packaged store package's own install dir.
              ;; Nothing else is needed — every one of them carries its full npm closure inside itself
              ;; as real directories (verified against the built output: pi-durable's inner
              ;; chord/pi-ai and pi-ai's inner openai/partial-json are directories, not symlinks), so
              ;; transitive imports resolve from wherever Node's realpath lands.  That is also why the
              ;; jiti/mlly "symlink realpaths out of its closure" problem documented in
              ;; peteches/packages/pi-coding-agent.scm does not bite here.
              (use-modules (guix build utils) (ice-9 format))
              (let* ((share (string-append #$output "/share/aibots"))
                     (scoped (string-append share "/node_modules/@earendil-works"))
                     (pkgs (list
                            (cons "pi-durable"
                                  (string-append #$node-earendil-works-pi-durable-1.5.2
                                                 "/lib/node_modules/@earendil-works/pi-durable"))
                            (cons "chord"
                                  (string-append #$node-earendil-works-chord-1.5.2
                                                 "/lib/node_modules/@earendil-works/chord"))
                            (cons "pi-ai"
                                  (string-append #$node-earendil-works-pi-ai-1.5.2
                                                 "/lib/node_modules/@earendil-works/pi-ai"))
                            (cons "pi-schema"
                                  (string-append #$node-earendil-works-pi-schema-1.5.2
                                                 "/lib/node_modules/@earendil-works/pi-schema"))
                            (cons "pi-telemetry"
                                  (string-append #$node-earendil-works-pi-telemetry-1.5.2
                                                 "/lib/node_modules/@earendil-works/pi-telemetry")))))
                (mkdir-p scoped)
                (for-each (lambda (entry)
                            (format #t "linking @earendil-works/~a -> ~a~%" (car entry) (cdr entry))
                            (symlink (cdr entry) (string-append scoped "/" (car entry))))
                          pkgs))))
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
