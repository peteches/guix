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
;; `bin/herdr' so it is a drop-in replacement for the upstream package;
;; only wired into the desktop configs (nug/nyarlothotep) that act as
;; multi-remote clients. The claude-workstation VM accounts keep the
;; standard `herdr' package from (peteches packages herdr) as their
;; server, since they are attached *to*, not multi-remote clients.
;;
;; Pinned to the 2026-08-22 preview build rather than the v0.8.0-mx.1
;; stable tag: the mx client on dagon was reproducibly SIGSEGVing
;; (confirmed via a captured core + gdb backtrace -- crash thread had a
;; unique stack against ~28 idle pool threads, i.e. a race under
;; concurrent pane/agent load) and the three preview builds since
;; v0.8.0-mx.1 land fixes for a "wedged remote server" and hardened
;; live-handoff/status paths -- exactly the multi-remote code paths this
;; crash was hitting. No matching issue was open upstream at the time.
;; Preview tags don't share stable's "v<version>" naming, hence the
;; separate pkg-tag.
(define-public herdr-mx
  (let* ((pkg-tag "mx-preview-2026-08-22-1600-be4d15051d3f")
         (pkg-version "0.8.0-mx-preview.2026.08.22"))
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
          (base32 "0ig0zvw108mwdzwnimkkdsvfgkbbwavd69zf3iwhlgn5qc87ajgi"))))
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
