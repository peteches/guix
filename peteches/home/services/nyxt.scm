(define-module (peteches home services nyxt)
  #:use-module (gnu home services)
  #:use-module (gnu packages gstreamer)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (peteches packages nyxt)
  #:export (home-nyxt-base-configuration home-nyxt-base-configuration?
            home-nyxt-base-configuration-config-directory
            home-nyxt-base-configuration-extra-packages))

(define (maybe-string? value)
  (or (eq? value
           '%unset-marker%)
      (not value)
      (string? value)))

(define (package-list? value)
  (and (list? value)
       (every package? value)))

(define-configuration/no-serialization home-nyxt-base-configuration
                                       (config-directory maybe-string
                                        "Path to a directory containing Nyxt configuration files.  Each top-level
entry in this directory is linked into ~/.config/nyxt, while ~/.config/nyxt
itself remains a real directory.

Warning: every top-level file or directory in this source directory is managed
by Guix Home and linked into the store.  Do not include directories that Nyxt
expects to mutate, such as data, history, cookies, cache, downloads, or similar
runtime/cache/state directories.  Including mutable Nyxt state here can cause
breakage because those paths will resolve to immutable store locations.")

                                       (extra-packages (package-list (map
                                                                      specification->package
                                                                      (list
                                                                       "gst-plugins-ugly"
                                                                       "gst-plugins-good")))
                                        "Additional packages to add to the home profile alongside the base Nyxt
packages.  This is useful for host-specific Nyxt extensions, helper tools,
fonts, themes, or browser integration utilities that should not be part of the
reusable base service."))

(define (nyxt-profile config)
  (append (list nyxt-webkit)
          (home-nyxt-base-configuration-extra-packages config)))

(define (nyxt-files-service config)
  (let ((config-dir (home-nyxt-base-configuration-config-directory config)))
    (if (string? config-dir)
        (list `("nyxt/config.lisp" ,(local-file (string-append config-dir
                                                               "/config.lisp")))
              `("nyxt/modules" ,(local-file (string-append config-dir
                                                           "/modules")
                                            "nyxt-modules"
                                            #:recursive? #t)))
        '())))

(define-public nyxt-service-type
  (service-type (name 'nyxt)
                (description "My nyxt configs")
                (default-value (home-nyxt-base-configuration))
                (extensions (list (service-extension
                                   home-xdg-configuration-files-service-type
                                   nyxt-files-service)
                                  (service-extension home-profile-service-type
                                   nyxt-profile)))))
