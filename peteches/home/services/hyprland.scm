(define-module (peteches home services hyprland)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib) ;dbus (dbus-update-activation-environment)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 binary-ports)
  #:export (home-hyprland-service-type <home-hyprland-configuration>
                                       home-hyprland-configuration
                                       home-hyprland-configuration?))

(define (maybe-string? value)
  (or (not value)
      (string? value)))

(define (package-list? value)
  (and (list? value)
       (every package? value)))

(define-configuration/no-serialization home-hyprland-configuration
                                       (extra-packages (package-list '())
                                        "Additional packages to add to the home profile alongside the base Hyprland
packages.  This is useful for host-specific Hyprland packages or tools that should not be part of the reusable
base service.")
                                       (config-directory (maybe-string #f)
                                        "Path to a directory containing Hyprland configuration files.  Each top-level
entry in this directory is linked into ~/.config/hypr, while ~/.config/hypr
itself remains a real directory.

Warning: every top-level file or directory in this source directory is managed
by Guix Home and linked into the store.  Do not include directories that Hyprland expects to mutate.  Including mutable Hyprland state here can cause breakage because
those paths will resolve to immutable store locations."))

(define (home-hyprland-profile-service config)
  (append (list hyprcursor xdg-desktop-portal xdg-desktop-portal-gtk
                xdg-desktop-portal-wlr xdg-desktop-portal-hyprland)
          (home-hyprland-configuration-extra-packages config)))

(define (directory-children directory)
  "Return the non-special immediate children of DIRECTORY."
  (filter (lambda (entry)
            (not (member entry
                         '("." ".."))))
          (scandir directory)))

(define (home-hyprland-files-service config)
  (let ((config-directory (home-hyprland-configuration-config-directory config)))
    (if config-directory
        (map (lambda (entry)
               (let ((source-path (string-append config-directory "/" entry))
                     (target-path (string-append "hypr/" entry)))
                 `(,target-path ,(local-file source-path
                                             #:recursive? (file-is-directory?
                                                           source-path)))))
             (directory-children config-directory))
        '())))

;; The lookups below must run *inside* the activation gexp, at activation
;; time -- not here at module load / derivation-build time.  `guix home
;; reconfigure` computes the activation script long before it runs it, and
;; often from a session where Hyprland isn't the one invoking it (e.g. an
;; SSH session), so evaluating them here would frequently bake in `#f` for
;; the socket path and crash `make-socket-address` when the script runs.
(define (home-hyprland-activation-service-type config)
  #~(begin
      (use-modules (ice-9 iconv) (ice-9 ftw))

      (define xdg-runtime-dir
        (or (getenv "XDG_RUNTIME_DIR")
            (string-append "/run/user/"
                           (number->string (getuid)))))

      (define hypr-root
        (string-append xdg-runtime-dir "/hypr"))

      (define (find-hypr-instance)
        (and (file-exists? hypr-root)
             (let ((entries (scandir hypr-root
                                     (lambda (name)
                                       (and (not (member name
                                                         '("." "..")))
                                            (file-is-directory? (string-append
                                                                 hypr-root "/" name)))))))
               (if (null? entries) #f
                   ;; usually only one instance, pick the newest
                   (car (sort entries
                              (lambda (a b)
                                (> (stat:mtime (stat (string-append hypr-root "/" a)))
                                   (stat:mtime (stat (string-append hypr-root "/" b)))))))))))

      (define (hypr-socket-path)
        (let ((sig (find-hypr-instance)))
          (and sig
               (string-append hypr-root "/" sig "/.socket.sock"))))

      (define (hypr-send socket-path command)
        ;; Create a Unix domain stream socket, like (socket PF_INET SOCK_STREAM 0)
        ;; but AF_UNIX instead of PF_INET.
        (let ((s (socket AF_UNIX SOCK_STREAM 0)))
          ;; Connect to Hyprland's control socket
          (connect s
                   (make-socket-address AF_UNIX socket-path))

          ;; Send the command. The Hyprland IPC accepts "reload" with no newline.
          ;; Using `display` matches the Guile manual style, which just writes to `s`
          ;; directly because `s` is a port once connected. :contentReference[oaicite:2]{index=2}
          (send s
                (string->bytevector command "utf8"))
          (force-output s)))

      ;; Skip silently if no Hyprland instance is running (e.g. reconfiguring
      ;; from an SSH session or before Hyprland has started).  A stale
      ;; instance directory/socket file can also be left behind after a
      ;; crash with nothing actually listening on it, which surfaces as
      ;; ECONNREFUSED from `connect' rather than an absent path -- that
      ;; must be swallowed the same way, or it aborts the whole `guix home
      ;; reconfigure'.
      ;;
      ;; Hyprland's IPC socket wraps whatever text follows "dispatch " into
      ;; `hl.dispatch(<text>)' verbatim and evaluates it as Lua once a Lua
      ;; config is active (`j/status' reports `configProvider: "lua"') --
      ;; the old plain "dispatch exec dms restart" string is not valid Lua
      ;; and errors out silently from this script's point of view (the
      ;; error only shows up in the socket reply, which this code never
      ;; reads).  That meant DMS silently never actually restarted on any
      ;; reconfigure since the Lua migration.  `hl.dsp.exec_cmd("...")' is
      ;; the confirmed-working replacement (tested directly against a live
      ;; socket on dagon).
      ;;
      ;; Also: `dms restart' itself was observed to kill the running
      ;; instance without reliably relaunching it when handing off between
      ;; mismatched DMS versions (e.g. the v0.5.1 -> v1.5.0 bump) -- do the
      ;; kill+relaunch explicitly instead of trusting its internal handoff,
      ;; mirroring exactly what autostart.lua uses at Hyprland startup.
      ;;
      ;; Send the kill and the relaunch as two separate dispatches rather
      ;; than one "pkill ... ; exec ..." shell one-liner: unlike
      ;; `hl.exec_cmd' (which autostart.lua wraps in `sh -lc "..."'
      ;; itself), `hl.dsp.exec_cmd' does not appear to run its argument
      ;; through a shell, so ';'/'exec' inside a single call are not
      ;; interpreted at all -- confirmed by testing directly against a
      ;; live socket on dagon, where the chained form silently spawned
      ;; nothing.  A real `sleep' here (not a shell one) gives Hyprland
      ;; time to actually terminate the old process before the relaunch
      ;; dispatch goes out.
      (let ((socket-path (hypr-socket-path))
            (dms-bin (string-append (getenv "HOME")
                                     "/.guix-home/profile/bin/dms"))
            (quickshell-dir (string-append (getenv "HOME")
                                            "/.guix-home/profile/share/quickshell")))
        (when socket-path
          (catch 'system-error
            (lambda ()
              (hypr-send socket-path "reload")
              (hypr-send
               socket-path
               (string-append "dispatch hl.dsp.exec_cmd(\"pkill -f "
                               dms-bin "\")"))
              (sleep 1)
              (hypr-send
               socket-path
               (string-append "dispatch hl.dsp.exec_cmd(\"" dms-bin
                               " run -c " quickshell-dir "\")")))
            (lambda (key . args)
              #t))))))

(define (home-hyprland-environment-variables-service-type config)
  `(("XDG_SESSION_TYPE" . "wayland") ("XDG_CURRENT_DESKTOP" . "Hyprland")
    ("XDG_SESSION_DESKTOP" . "Hyprland")
    ("MOZ_ENABLE_WAYLAND" . "1")
    ("QT_QPA_PLATFORM" . "wayland")
    ("GTK_USE_PORTAL" . "1")
    ("NIXOS_OZONE_WL" . "1")))

(define home-hyprland-service-type
  (service-type (name 'home-hyprland-config)
                (extensions (list (service-extension
                                   home-activation-service-type
                                   home-hyprland-activation-service-type)
                                  (service-extension
                                   home-environment-variables-service-type
                                   home-hyprland-environment-variables-service-type)
                                  (service-extension home-profile-service-type
                                   home-hyprland-profile-service)
                                  (service-extension
                                   home-xdg-configuration-files-service-type
                                   home-hyprland-files-service)))
                (default-value (home-hyprland-configuration))
                (description "Applies my personal Hyprland base configuration")))
