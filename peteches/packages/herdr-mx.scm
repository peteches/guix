(define-module (peteches packages herdr-mx)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

;; herdr-mx (2lab-ai/herdr-mx) is a community downstream distribution of
;; herdr that adds a multi-remote client -- attaching to several herdr
;; servers from one sidebar -- ahead of upstream shipping it natively (see
;; https://github.com/herdrdev/herdr/discussions/515). Installed as
;; `bin/herdr' so it is a drop-in replacement for the upstream package.
;; Wired into the desktop configs (dagon/nyarlothotep) that act as
;; multi-remote clients, and into the claude-workstation home as the
;; server: the mx client is a protocol fork (PROTOCOL_VERSION 21, upstream
;; 20) that cannot attach to the upstream server, and its add-remote
;; worker self-provisions (copies its own exe to the remote
;; ~/.local/bin/herdr) unless `command -v herdr' on the remote finds a
;; binary matching the client exactly -- which the store build on PATH
;; does.  The pin must be identical on both machines.
;;
;; Bumped 2026-09-21 to the v0.8.0-mx.1 stable tag.  (Earlier this was
;; pinned to a preview build rather than the stable tag: the mx client
;; on dagon was reproducibly SIGSEGVing under concurrent pane/agent
;; load and the preview builds since v0.8.0-mx.1 landed fixes for the
;; wedged-remote and live-handoff/status paths that crash was hitting.
;; Preview tags don't share stable's "v<version>" naming, hence the
;; separate pkg-tag.)
;;
;; Reverted 2026-09-26 back to the mx-preview-2026-09-11 build -- the same
;; build the 2026-09-21 bump above moved away from. This is a deliberate,
;; risk-accepted choice, not an oversight: this exact preview is still the
;; one documented above as SIGSEGVing/wedging under concurrent pane/agent
;; load, and is also implicated in a same-day incident where a stale copy
;; self-provisioned to ~/.local/bin/herdr on claude-workstation kept
;; crash-looping (local API socket EAGAIN/empty-response on its own
;; supervisor-refresh poll, then self-exit) and cascaded into dropping
;; every attached remote. As of this revert, upstream has published no
;; preview newer than 2026-09-11 to move to instead. Watch
;; ~/.config/herdr/herdr-client.log on every machine for
;; "main supervisor refresh failed" / "app.shutdown" clusters; if a crash
;; cluster recurs, revert this pin to v0.8.0-mx.1.
(define-public herdr-mx
  (let* ((pkg-tag "mx-preview-2026-09-11-0302-79803d1841bb")
         (pkg-version "0.8.2-mx.preview-2026-09-11-0302-79803d1841bb"))
    (package
      (name "herdr-mx")
      (version pkg-version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/2lab-ai/herdr-mx/releases/download/"
               pkg-tag "/herdr-linux-x86_64"))
         (sha256
          (base32 "13r6g69kiymqarw0cl8hvn1k67zv88lx3w9xlrhxlrjp62p5s117"))))
      (build-system copy-build-system)
      (arguments
       (list
        ;; static-pie linked, like upstream herdr -- no patchelf needed.
        #:install-plan
        #~(list (list "herdr-linux-x86_64" "bin/herdr"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'make-herdr-executable
              (lambda* (#:key outputs #:allow-other-keys)
                (chmod (string-append (assoc-ref outputs "out") "/bin/herdr")
                       #o555))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/2lab-ai/herdr-mx")
      (synopsis "Herdr distribution with a multi-remote client")
      (description
       "herdr-mx is a community downstream distribution of herdr, the
persistent terminal runtime for coding agents, adding a multi-remote
client: attach to herdr servers on several machines from one sidebar
instead of nesting SSH sessions.  It tracks upstream herdr closely and
installs as a drop-in @command{herdr} binary.")
      (license asl2.0))))
