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
;;; peteches/packages/pi-coding-agent.scm (the 1.6.0 line from 0.4.0 on; the 1.5.2 line is still packaged
;;; beside it, but nothing in here points at it any more).  Symlinks are enough because each of those
;;; carries its whole npm closure inside itself as real directories (npm --install-links; checked
;;; against the built output), which is also why none of the realpath-vs-symlink trouble described at
;;; length in that file applies here: nothing resolves out of a symlink into a bare store path.
;;; openai (7.59.0 at this line) and partial-json need no entry: re-measured on the built 1.6.0 output, pi-ai's
;;; only remaining declared dependencies are absolute file:///gnu/store paths, and those trees sit inside pi-ai's
;;; own store path as real directories (find … -name openai answers type 'd' with no link target).
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
;;; Updating the pin: set ai-bots-commit to the commit you pushed — read it from `git log -1 --format=%H' rather
;;; than from memory, and confirm it is actually published, because guix fetches over plain HTTPS from
;;; ai-bots-repo-url and a commit that only exists in someone's working tree is unfetchable (the anonymous ref
;;; listing at <url>/info/refs?service=git-upload-pack answers without credentials here).  Then poison the
;;; sha256 below and build through a manifest, because two more convenient forms do not work in this tree:
;;;   printf '(use-modules (peteches packages ai-bots))\n(packages->manifest (list ai-bots))' >/tmp/manifest-ai-bots.scm
;;;   guix build -L . -m /tmp/manifest-ai-bots.scm
;;; Guix answers "expected hash: <poison> / actual hash: <new>" — copy that string verbatim, and re-measure it
;;; with `guix hash -rx <the …-ai-bots-<version>-checkout> path the message names' before trusting it.  What does
;;; NOT work here, each measured: `guix build -f peteches/packages/ai-bots.scm' (the file ends in define-public,
;;; which evaluates to #<unspecified>, so -f reports "not something we can build"), the spec form
;;; MODULE.scm:ai-bots:source ("unknown package"), and a digest poisoned to literal all-zeros (it fails to load).
;;; Commit and hash move together; a fresh commit with a stale hash is the half-update that eats a
;;; build before it errors.

(define-public ai-bots-repo-url "https://git.ts.peteches.co.uk/ai-bots.git/")

;; Pinned commit of the aiBots repository: 3788a3d, "deps: the durable line moves to 1.6.0", which re-pins
;; docs/dependencies.json — that JSON is read at RUNTIME by bots/Tiffany/index.mjs status, so the installed
;; source has to carry it or she reports the wrong pinned versions.  Bump it together with the sha256 below:
;; commit and hash move as one, and a fresh commit on a stale hash silently reuses the old checkout.
(define-public ai-bots-commit "3788a3d8c2cd991d00ad3bc2752ed4e45d8ca2d2")

(define-public ai-bots
  (package
    (name "ai-bots")
    ;; 0.4.0 because the engine line moves, not just the source: pi-durable 1.6.0 changes restart semantics
    ;; (a parked tool call is re-asked after a crash instead of answered with a synthetic error).  The version
    ;; names the store path — …-ai-bots-<version>/bin/tiffany — and that path is what shepherd compares, so
    ;; bumping here is also how the running service gets forced to notice.  Never goes backwards or stays put.
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url ai-bots-repo-url) (commit ai-bots-commit)))
       (file-name (git-file-name name version))
      ;; Hash for the commit above, obtained by the ritual: poison it (a literal all-zeros digest does not load
      ;; in this Guix — flip a character instead), build once, copy the
      ;; hash Guix reports.  A stale hash here reuses last week's checkout instead of failing.
      ;; The trap is nix-base32's alphabet, not hex: the digits are 0123456789abcdfghijklmnpqrsvwxyz, so a leading
      ;; '0' and any 'x' (this digest has one: …grf0xgg0r0lardjgz…) are ordinary characters, not a "0x" prefix Guix
      ;; added.  Read it as hex anyway, strip the apparent prefix, left-pad back to the 52 characters Guix wants,
      ;; and the file loads, decodes and round-trips while describing a digest differing from the real one only in
      ;; its last byte (measured once on this box: …d500 against …d574) — so it fails as "hash mismatch", which
      ;; looks nothing like the padding mistake it was.  Copy what Guix prints; never re-format a digest by hand.
      ;; Copy the reported string verbatim; `guix hash -rx --format=base16 <checkout>' plus a converter is
      ;; how I confirmed which one was right when Guix's own message misled me.  Every bump repeats the ritual
      ;; (poison to all-zeros, build once, copy Guix's "actual hash"): this value came from 0.4.0 at 3788a3d and
      ;; was measured twice rather than typed by hand — `guix build -L . -m /tmp/manifest-ai-bots.scm' answered
      ;;   expected hash: <the poison>   actual hash: 0112kp4i2ilz8qkmgfj1ldnjwzcrgrf0xgg0r0lardjgz10mbds0
      ;; and `guix hash -rx /gnu/store/4q5bslgim39vrclvm6hk0i5nqagxc8js-ai-bots-0.4.0-checkout', on the checkout
      ;; Guix had just fetched, re-derived exactly that string, so the two agree without a converter between them.
      ;; Two things do NOT work on this box, both learned doing it: a literal all-zeros digest fails to *load*
      ;; ("guix/base32.scm:237:27 … bytevector-u8-ref: Argument 2 out of range: 28", so poison the last character
      ;; instead — 51 zeros and a '1' loads fine), and the spec `peteches/packages/ai-bots.scm:ai-bots:source'
      ;; answers "unknown package" here, so build through a manifest holding (packages->manifest (list ai-bots))
      ;; and let the source derivation fail early; that is just as cheap, since nothing else builds.
      ;; Note a nix-base32 digest may legitimately start with '0' — it is a digit, not a "0x" prefix.
       (sha256 (base32 "0112kp4i2ilz8qkmgfj1ldnjwzcrgrf0xgg0r0lardjgz10mbds0"))))
    (build-system copy-build-system)
    ;; Nothing is compiled: node and ffmpeg are wanted at run time by the installed bots, so they
    ;; propagate instead of being build-only inputs.  The @earendil-works packages propagate too:
    ;; they are what the installed node_modules farm points at, and naming them here keeps
    ;; `guix install ai-bots` from looking like it installs a bot that cannot import its own engine.
    (propagated-inputs (list node ffmpeg
                  node-earendil-works-pi-durable-1.6.0
                  node-earendil-works-chord-1.6.0
                  node-earendil-works-pi-ai-1.6.0
                  node-earendil-works-pi-schema-1.6.0
                  node-earendil-works-pi-telemetry-1.6.0))
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
              ;; pi-durable 1.6.0 declares engines.node >=22.19 — re-read from the built package.json, same
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
              ;; chord/pi-ai and pi-ai's inner openai/partial-json are directories, not symlinks — re-checked on the
              ;; 1.6.0 build), so
              ;; transitive imports resolve from wherever Node's realpath lands.  That is also why the
              ;; jiti/mlly "symlink realpaths out of its closure" problem documented in
              ;; peteches/packages/pi-coding-agent.scm does not bite here.
              (use-modules (guix build utils) (ice-9 format))
              (let* ((share (string-append #$output "/share/aibots"))
                     (scoped (string-append share "/node_modules/@earendil-works"))
                     (pkgs (list
                            (cons "pi-durable"
                                  (string-append #$node-earendil-works-pi-durable-1.6.0
                                                 "/lib/node_modules/@earendil-works/pi-durable"))
                            (cons "chord"
                                  (string-append #$node-earendil-works-chord-1.6.0
                                                 "/lib/node_modules/@earendil-works/chord"))
                            (cons "pi-ai"
                                  (string-append #$node-earendil-works-pi-ai-1.6.0
                                                 "/lib/node_modules/@earendil-works/pi-ai"))
                            (cons "pi-schema"
                                  (string-append #$node-earendil-works-pi-schema-1.6.0
                                                 "/lib/node_modules/@earendil-works/pi-schema"))
                            (cons "pi-telemetry"
                                  (string-append #$node-earendil-works-pi-telemetry-1.6.0
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
