;;; Docker Compose v2+, packaged as a docker CLI plugin.
;;;
;;; Guix packages only the standalone Python Compose v1 (1.29.2), and it
;;; is unusable: it pulls python-docker@5.0.3 against
;;; python-requests@2.32.5, and requests 2.32 removed the transport
;;; adapter docker-py used for unix:// sockets, so every command that
;;; talks to the daemon dies with "Not supported URL scheme
;;; http+docker".  docker-py did not fix that until 7.1.0.  Guix master
;;; still carried 1.29.2 as of 2026-07-11, so there is no pin bump that
;;; helps.
;;;
;;; This is the official upstream release binary rather than a source
;;; build -- a nonguix-style blob.  Building Compose from source would
;;; mean vendoring its entire Go dependency tree; upstream ships a
;;; static, dependency-free binary per arch, so there is nothing to
;;; patchelf.  The sha256 below was checked against upstream's published
;;; checksums.txt.
;;;
;;; It installs to libexec/docker/cli-plugins rather than bin: `docker'
;;; looks for subcommand plugins only in $DOCKER_CONFIG/cli-plugins and
;;; four hard-coded FHS directories, never on PATH.  Consumers must put
;;; it somewhere `docker' looks -- see the home-files symlink in
;;; (peteches home configs nug) and the --share mapping in
;;; (containers claude).

(define-module (peteches packages docker-compose)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (docker-compose-cli-plugin))

(define docker-compose-cli-plugin
  (package
    (name "docker-compose-cli-plugin")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/docker/compose/releases/download/v"
             version "/docker-compose-linux-x86_64"))
       (file-name (string-append "docker-compose-" version "-linux-x86_64"))
       (sha256
        (base32 "0ngrhx4kphb0qqjkpwc74p39icdfr8vafif2jfvnkmqrvgmwdszr"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((dir  (string-append #$output "/libexec/docker/cli-plugins"))
                 (dest (string-append dir "/docker-compose")))
            (mkdir-p dir)
            (copy-file #$source dest)
            (chmod dest #o555)))))
    (supported-systems '("x86_64-linux"))
    (synopsis "Define and run multi-container Docker applications")
    (description
     "Docker Compose reads a @file{compose.yaml} describing a set of
services and brings them up as a group.  This package installs the
upstream binary as a @command{docker} CLI plugin, providing the
@command{docker compose} subcommand.")
    (home-page "https://github.com/docker/compose")
    (license license:asl2.0)))
