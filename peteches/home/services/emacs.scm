(define-module (peteches home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:export (home-emacs-base-service-type home-emacs-base-configuration))

(define (maybe-string? value)
  (or (not value)
      (string? value)))

(define (package-list? value)
  (and (list? value)
       (every package? value)))

(define-configuration/no-serialization home-emacs-base-configuration
                                       (emacs-package (package
                                                        emacs)
                                        "The Emacs package to install.  Defaults to @code{emacs}.  Use
@code{emacs-pgtk} for a pure-GTK Wayland-native build.")
                                       (extra-packages (package-list '())
                                        "Additional packages to add to the home profile alongside the base Emacs
packages.  This is useful for host-specific Emacs packages, language servers,
tree-sitter grammars, fonts, or tools that should not be part of the reusable
base service.")
                                       (config-directory (maybe-string #f)
                                        "Path to a directory containing Emacs configuration files.  Each top-level
entry in this directory is linked into ~/.config/emacs, while ~/.config/emacs
itself remains a real directory.

Warning: every top-level file or directory in this source directory is managed
by Guix Home and linked into the store.  Do not include directories that Emacs
or Emacs package managers expect to mutate, such as elpa, straight, eln-cache,
auto-save-list, transient, url, var, or similar runtime/cache/state
directories.  Including mutable Emacs state here can cause breakage because
those paths will resolve to immutable store locations."))

(define (home-emacs-base-profile-service config)
  (append (list (home-emacs-base-configuration-emacs-package config))
          (home-emacs-base-configuration-extra-packages config)))

(define (directory-children directory)
  "Return the non-special immediate children of DIRECTORY."
  (filter (lambda (entry)
            (not (member entry
                         '("." ".."))))
          (scandir directory)))

(define (home-emacs-base-files-service config)
  (let ((config-directory (home-emacs-base-configuration-config-directory
                           config)))
    (if config-directory
        (map (lambda (entry)
               (let ((source-path (string-append config-directory "/" entry))
                     (target-path (string-append "emacs/" entry)))
                 `(,target-path ,(local-file source-path
                                             #:recursive? (file-is-directory?
                                                           source-path)))))
             (directory-children config-directory))
        '())))

(define home-emacs-base-service-type
  (service-type (name 'home-emacs-base-config)
                (extensions (list (service-extension
                                   home-activation-service-type
                                   (lambda (config)
                                     (let ((pkg (home-emacs-base-configuration-emacs-package
                                                 config)))
                                       #~(when (zero? (system* #$(file-append
                                                                  pkg
                                                                  "/bin/emacsclient")
                                                               "-e" "t"))
                                           (system* #$(file-append pkg
                                                       "/bin/emacsclient")
                                            "-e"
                                            "(progn
  (when (fboundp 'guix-refresh-emacs-load-path)
    (guix-refresh-emacs-load-path))
  (let* ((init (or user-init-file
                   (expand-file-name \"init.el\" user-emacs-directory))))
    (when (and init (file-readable-p init))
      (load init nil 'nomessage))))")))))
                                  ;; (service-extension
                                  ;; home-shepherd-service-type
                                  ;; home-emacs-base-shepherd-service-type)
                                  (service-extension home-profile-service-type
                                   home-emacs-base-profile-service)
                                  (service-extension
                                   home-xdg-configuration-files-service-type
                                   home-emacs-base-files-service)))
                (default-value (home-emacs-base-configuration))
                (description "Applies my personal Emacs base configuration")))
