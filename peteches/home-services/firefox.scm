(define-module (peteches home-services firefox)
  ;; packages & home services
  #:use-module (gnu packages web-browsers)   ; firefox (or mozilla on some channels)
  #:use-module (gnu packages wm)             ; wofi
  #:use-module (gnu packages xorg)           ; xdg-utils
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  ;; guix utils
  #:use-module (guix gexp)                   ; #~, #$
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  ;; exports
  #:export (firefox-configuration
            firefox-profile
            firefox-files-service
            firefox-desktop-service
            firefox-mime-service
            firefox-service-type))

;; --------------------------
;; Records / configuration
;; --------------------------

(define-record-type* <firefox-profile-decl>
  firefox-profile-decl make-firefox-profile-decl firefox-profile-decl?
  (name          firefox-profile-decl-name)                       ; string
  (id            firefox-profile-decl-id)                         ; string (used for filenames)
  (path          firefox-profile-decl-path                        ; string under ~/.mozilla/firefox/
                 (default (lambda (p)
                            (string-append "profiles/" (firefox-profile-decl-id p)))))
  (prefs         firefox-profile-decl-prefs (default '()))        ; alist ((key . val) ...)
  (extensions    firefox-profile-decl-extensions (default '())))  ; alist ((addon-id . file-like) ...)

(define-record-type* <firefox-configuration>
  firefox-configuration make-firefox-configuration firefox-configuration?
  ;; package knobs (you probably won’t touch)
  (firefox-package   firefox-conf-firefox-package (default firefox))
  (wofi-package      firefox-conf-wofi-package    (default wofi))
  (xdg-utils-package firefox-conf-xdg-package     (default xdg-utils))
  ;; runtime commands used inside the launcher (if you ever swap to flatpak etc.)
  (firefox-cmd       firefox-conf-firefox-cmd     (default "firefox"))
  (wofi-cmd          firefox-conf-wofi-cmd        (default "wofi --dmenu --prompt 'Select Firefox Profile:'"))
  ;; profile data
  (profiles          firefox-conf-profiles)                       ; list of <firefox-profile-decl>
  (default-profile   firefox-conf-default-profile (default #f))   ; string or #f
  ;; global XPIs (same file is used for every profile)
  (global-extensions firefox-conf-global-extensions (default '()))) ; alist ((addon-id . file-like) ...)

;; --------------------------
;; Internals
;; --------------------------

(define %launcher-home-path "~/.local/bin/firefox-profile-launcher")
(define %launcher-target    ".local/bin/firefox-profile-launcher")

;; Your requirement: launcher must come from a local file in the repo.
(define (launcher-store-file _config)
  (local-file "./firefox-profile-launcher.sh" #:executable? #t))

(define (serialize-profiles.ini profiles default-name)
  ;; Build profiles.ini by hand to avoid extra deps
  (let loop ((ps profiles) (i 0) (acc '()))
    (if (null? ps)
        (string-append
         "[General]\nStartWithLastProfile=1\nVersion=2\n\n"
         (apply string-append (reverse acc)))
        (let* ((p (car ps))
               (s (string-append
                   "[Profile" (number->string i) "]\n"
                   "Name=" (firefox-profile-decl-name p) "\n"
                   "IsRelative=1\n"
                   "Path=" (firefox-profile-decl-path p) "\n"
                   (if (and default-name
                            (string=? (firefox-profile-decl-name p) default-name))
                       "Default=1\n\n" "\n"))))
          (loop (cdr ps) (+ i 1) (cons s acc))))))

(define (serialize-user.js prefs)
  (define (one kv)
    (let* ((k (car kv)) (v (cdr kv))
           (vv (cond ((boolean? v) (if v "true" "false"))
                     ((number? v)  (number->string v))
                     (else         (string-append "\"" v "\"")))))
      (string-append "user_pref(\"" k "\", " vv ");\n")))
  (apply string-append (map one prefs)))

;; --------------------------
;; Files payloads (like waybar-files-service)
;; --------------------------

(define (firefox-files-service config)
  (let* ((profiles      (firefox-conf-profiles config))
         (default       (firefox-conf-default-profile config))
         ;; base files
         (base-files
          (list
           ;; launcher to ~/.local/bin
           (cons %launcher-target (launcher-store-file config))
           ;; profiles.ini
           (cons ".mozilla/firefox/profiles.ini"
                 (plain-file "profiles.ini"
                             (serialize-profiles.ini profiles default)))))
         ;; global extensions
         (global-ext-dir ".mozilla/firefox/distribution/extensions/")
         (global-files
          (append
           (list
            (cons global-ext-dir
                  (computed-file "ensure-dist-ext-dir"
                    #~(begin (use-modules (guix build utils))
                             (mkdir-p #$output)))))
           (map (lambda (pair)
                  (let ((addon-id (car pair)) (xpi (cdr pair)))
                    (cons (string-append global-ext-dir addon-id ".xpi") xpi)))
                (firefox-conf-global-extensions config))))
         ;; per-profile files
         (per-profile-files
          (append-map
           (lambda (p)
             (let* ((ppath  (firefox-profile-decl-path p))
                    (dir    (string-append ".mozilla/firefox/" ppath "/"))
                    (extdir (string-append dir "extensions/"))
                    (userjs (plain-file (string-append (firefox-profile-decl-id p) "-user.js")
                                        (serialize-user.js (firefox-profile-decl-prefs p)))))
               (append
                (list
                 (cons dir
                       (computed-file "ensure-profile-dir"
                         #~(begin (use-modules (guix build utils))
                                  (mkdir-p #$output))))
                 (cons extdir
                       (computed-file "ensure-extensions-dir"
                         #~(begin (use-modules (guix build utils))
                                  (mkdir-p #$output))))
                 (cons (string-append dir "user.js") userjs))
                (map (lambda (pair)
                       (let ((addon-id (car pair)) (xpi (cdr pair)))
                         (cons (string-append extdir addon-id ".xpi") xpi)))
                     (firefox-profile-decl-extensions p)))))
           profiles))))
    ;; Return the alist the service expects.
    (append base-files global-files per-profile-files)))

(define (firefox-desktop-service config)
  (let* ((desktop-id   "firefox-profile-launcher.desktop")
         (desktop-file
          (mixed-text-file desktop-id
            (string-append
             "[Desktop Entry]\n"
             "Type=Application\n"
             "Name=Firefox (Profile Selector)\n"
             "Exec=" %launcher-home-path " %u\n"
             "Terminal=false\n"
             "MimeType=text/html;x-scheme-handler/http;x-scheme-handler/https;\n"
             "Categories=Network;WebBrowser;\n"))))
    ;; Alist-of-alists shape (like waybar’s xdg files service)
    `(("applications" . (("firefox-profile-launcher.desktop" . ,desktop-file))))))

(define (firefox-mime-service _config)
  ;; Set default handlers to our launcher desktop file
  (home-xdg-mime-applications-configuration
   (default-applications
    `(("x-scheme-handler/http"  . ("firefox-profile-launcher.desktop"))
      ("x-scheme-handler/https" . ("firefox-profile-launcher.desktop"))
      ("text/html"              . ("firefox-profile-launcher.desktop"))))))

;; --------------------------
;; Service type (like waybar-service-type)
;; --------------------------

(define-public firefox-service-type
  (service-type
   (name 'firefox-config)
   (extensions
    (list
     ;; install packages
     (service-extension
      home-profile-service-type
      (lambda (_cfg)
        (list firefox wofi xdg-utils)))
     ;; install files under $HOME
     (service-extension
      home-files-service-type
      firefox-files-service)
     ;; install .desktop under XDG data
     (service-extension
      home-xdg-data-files-service-type
      firefox-desktop-service)
     ;; set default handlers
     (service-extension
      home-xdg-mime-applications-service-type
      firefox-mime-service)))
   (default-value (firefox-configuration (profiles '())))
   (description "Install Firefox, wofi, xdg-utils. Provide a Wofi-powered
profile selector at ~/.local/bin/firefox-profile-launcher, generate profiles.ini and
per-profile user.js, and provision global/per-profile XPI extensions.")))
