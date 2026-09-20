(define-module (peteches packages pi-dictate)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system copy))

;; pi-dictate (https://github.com/amosblomqvist/pi-dictate) -- minimal
;; voice dictation for pi: alt+m toggles Deepgram Nova-3 streaming
;; transcription and the finalized text is appended to whatever input
;; field is focused at stop time (main chat editor or any dialog).
;;
;; A single-file extension (index.ts) with NO runtime npm dependencies:
;; pi's extension loader transpiles the .ts entry with its bundled jiti
;; and resolves the pi APIs (@earendil-works/pi-coding-agent, pi-tui)
;; through its own loader aliases -- the same packaging shape as
;; pi-interactive-subagents.  The repo's package.json declares
;; "pi.extensions" ["./index.ts"], which the loader honors, so both
;; files are installed below.  Packaged under
;; lib/node_modules/pi-dictate so that (peteches home modules pi)'s
;; EXTENSIONS field can symlink it to ~/.pi/agent/extensions/pi-dictate
;; -- the loader's discoverExtensionsInDir follows directory symlinks
;; and resolveExtensionEntries picks up the manifest/index.ts there.
;;
;; Runtime requirements (NOT npm -- the profile must supply these):
;;   - sox, for the `rec` binary it spawns to capture 16kHz/16-bit mono
;;     PCM (see claude-workstation-peteches.scm's #:extra-packages).
;;   - DEEPGRAM_API_KEY in the environment, read at dictation start.
;;     On claude-workstation it arrives via #:secret-env-vars from the
;;     sops secret decrypted to /run/secrets/deepgram-api-key (see the
;;     #:sops-secrets entry in peteches/systems/claude-workstation.scm).
;;     Without it the extension just notifies "DEEPGRAM_API_KEY not
;;     set" -- it is inert, not broken.
;;
;; Uses Node's native WebSocket (no `ws` package): fine on this profile's
;; node 24.  The pbcopy fallback for "no input focused at stop" is
;; macOS-only and silently no-ops on this VM (caught in a try/catch).
;;
;; The commit pin is upstream main; bump it (plus the version, which
;; tracks the repo's package.json) to update, then redeploy -- the
;; home-pi-service-type instance on claude-workstation picks it up for
;; the account that passes it via #:pi-extensions.

(define-public pi-dictate
  (package
    (name "pi-dictate")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amosblomqvist/pi-dictate")
             (commit "3208b563e3adfd070ac7b256a09ba9fc7b869f50")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a9ll5jk354702z5y3z91gfmx9pprcb1kyf42kqcg66j9ps2n07g"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; The two files pi's extension loader needs: the package.json
      ;; manifest (declares "pi.extensions") and the extension source
      ;; itself.  assets/ (the README's demo thumbnail), tsconfig.json
      ;; and the dev-only TypeScript toolchain are not needed at runtime.
      #:install-plan
      #~(list (list "package.json" "lib/node_modules/pi-dictate/package.json")
              (list "index.ts" "lib/node_modules/pi-dictate/index.ts"))))
    (home-page "https://github.com/amosblomqvist/pi-dictate")
    (license expat)
    (synopsis "Voice dictation for the pi coding agent (Deepgram Nova-3)")
    (description
     "pi-dictate is a pi extension adding minimal voice dictation:
press @kbd{alt+m} to start, press it again to stop, and the finalized
transcript is appended to the focused input field -- the main chat
editor or any dialog's text field.  Audio is captured with sox's
@command{rec} (16kHz/16-bit mono PCM) and streamed in real time to
Deepgram's Nova-3 speech-to-text endpoint over a WebSocket; a live
level meter in the status row confirms the microphone is receiving
audio.  @kbd{alt+n} cancels and discards the in-flight transcript.
Requires the @env{DEEPGRAM_API_KEY} environment variable.")))
