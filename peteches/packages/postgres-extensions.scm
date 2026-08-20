;;; peteches/packages/postgres-extensions.scm — PostgreSQL extensions for
;;; claude-workstation's native postgresql-17 instance (see
;;; peteches/systems/claude-workstation.scm). Postgres extensions are
;;; ABI-locked to the major server version they're built against, so both
;;; packages here are pinned to postgresql-17 explicitly rather than the
;;; Guix default `postgresql' (= postgresql-14) -- mixing them means the
;;; server refuses to load the extension at all. Bumping the native
;;; server's major version again later (e.g. once Guix packages
;;; postgresql-18) means repeating this same rename-and-repoint dance for
;;; both packages below.
;;;
;;; pg_cron is a plain PGXS extension (like pgvector in upstream Guix's own
;;; (gnu packages databases)): `make' picks up pg_config from the
;;; postgresql input's PATH automatically, but PGXS's own `make install'
;;; target writes into that input's (read-only) store path rather than our
;;; own output, so the install phase is replaced with manual file copies,
;;; same as pgvector does.
;;;
;;; postgis (gnu packages geo) is already packaged upstream, but built
;;; against the bare `postgresql' (14) input -- postgis-17 below is a
;;; local rebuild against postgresql-17, otherwise identical.

(define-module (peteches packages postgres-extensions)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages geo)
  #:export (pg-cron postgis-17))

(define-public pg-cron
  (package
    (name "pg-cron")
    (version "1.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/citusdata/pg_cron")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1sv40h3sm94qckzwmhff41vwvdi3z3ag4n1alqwvhc84w90xl251"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The Makefile's own `check' target spins up a whole temp PostgreSQL
      ;; instance via --temp-instance; skip it here rather than carry that
      ;; weight in every rebuild.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          ;; Not `replace' -- that identifier collides with (guix
          ;; packages)'s modify-inputs guard macro of the same name once
          ;; both modules are imported into this file, so delete+add-after
          ;; stands in for it here.
          (delete 'install)
          (add-after 'build 'install
            (lambda _
              (let ((extension (string-append #$output "/share/extension"))
                    (lib (string-append #$output "/lib")))
                (mkdir-p extension)
                (mkdir-p lib)
                (install-file "pg_cron.so" lib)
                (chmod (string-append lib "/pg_cron.so") #o755)
                (install-file "pg_cron.control" extension)
                ;; pg_cron--1.0.sql (built by the Makefile from pg_cron.sql)
                ;; plus every pg_cron--X--Y.sql upgrade script already in
                ;; the source tree -- CREATE EXTENSION walks that chain up
                ;; to the control file's default_version.
                (for-each (lambda (file) (install-file file extension))
                          (find-files "." "^pg_cron--.*\\.sql$"))))))))
    (inputs (list postgresql-17))
    (home-page "https://github.com/citusdata/pg_cron")
    (synopsis "Job scheduler extension for PostgreSQL")
    (description
     "pg_cron is a cron-based job scheduler for PostgreSQL that runs inside
the database as an extension, using standard cron syntax to schedule
ordinary SQL commands directly from the database.  It requires
@code{shared_preload_libraries = 'pg_cron'} to be set in postgresql.conf,
since it registers a background worker at server start.")
    (license (license:x11-style
              "https://github.com/citusdata/pg_cron/blob/master/LICENSE"))))

;; Same package as upstream Guix's postgis, just built against
;; postgresql-17 instead of the bare `postgresql' (14) it normally links
;; against -- Postgres extensions are ABI-locked to their major version.
(define-public postgis-17
  (package
    (inherit postgis)
    (name "postgis-17")
    (inputs (modify-inputs (package-inputs postgis)
              (replace "postgresql" postgresql-17)))))
