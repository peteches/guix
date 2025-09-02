;;; firefox.scm — Guix Home service to manage Firefox profiles, prefs & addons.

(define-module (peteches home-services firefox)
  ;; packages & home services
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (nongnu packages mozilla)      ; firefox
  #:use-module (gnu packages xdisorg)         ; wofi
  #:use-module (gnu packages freedesktop)     ; xdg-utils
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  ;; guix utils
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((guix licenses) #:prefix license:)
  ;; lists
  #:use-module (srfi srfi-1)
  ;; exports
  #:export (firefox-configuration
            firefox-profile-decl
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
  (name          firefox-profile-decl-name)       ; string (shown in profiles.ini)
  (id            firefox-profile-decl-id)         ; string (used for file names)
  (path          firefox-profile-decl-path        ; optional subpath under ~/.mozilla/firefox/
                 (default #f))
  (prefs         firefox-profile-decl-prefs (default '()))        ; alist ((key . val) ...)
  (extensions    firefox-profile-decl-extensions (default '())))  ; alist ((addon-id . file-like|string) ...)

(define* (firefox-profile name id #:key (path #f) (prefs '()) (extensions '()))
  (firefox-profile-decl
    (name name) (id id) (path path) (prefs prefs) (extensions extensions)))

(define-record-type* <firefox-configuration>
  firefox-configuration make-firefox-configuration firefox-configuration?
  ;; packages
  (firefox-package   firefox-conf-firefox-package (default firefox))
  (wofi-package      firefox-conf-wofi-package    (default wofi))
  (xdg-utils-package firefox-conf-xdg-package     (default xdg-utils))
  ;; runtime commands (for launcher scripts)
  (firefox-cmd       firefox-conf-firefox-cmd     (default "firefox"))
  (wofi-cmd          firefox-conf-wofi-cmd        (default "wofi --dmenu --prompt 'Select Firefox Profile:'"))
  ;; profile data
  (profiles          firefox-conf-profiles)
  (default-profile   firefox-conf-default-profile (default #f))
  ;; global XPIs (same file is used for every profile)
  (global-extensions firefox-conf-global-extensions (default '()))
  ;; global prefs merged into each profile’s prefs (profile wins on conflicts)
  (global-prefs      firefox-conf-global-prefs (default '())))

;; --------------------------
;; Internals
;; --------------------------

;; Resolve on-disk profile path within ~/.mozilla/firefox/
(define (profile-path p)
  (let ((pth (firefox-profile-decl-path p)))
    (if (and pth (string? pth))
        pth
        (string-append "profiles/" (firefox-profile-decl-id p)))))

(define (serialize-profiles-ini profiles default-name)
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
                   "Path=" (profile-path p) "\n"
                   (if (and default-name
                            (string=? (firefox-profile-decl-name p) default-name))
                       "Default=1\n\n" "\n"))))
          (loop (cdr ps) (+ i 1) (cons s acc))))))

(define (serialize-user-js prefs)
  (define (one kv)
    (let* ((k (car kv)) (v (cdr kv))
           (vv (cond ((boolean? v) (if v "true" "false"))
                     ((number?  v) (number->string v))
                     (else          (string-append "\"" v "\"")))))
      (string-append "user_pref(\"" k "\", " vv ");\n")))
  (apply string-append (map one prefs)))

;; Merge global + per-profile prefs (per-profile wins on conflicts).
(define (merge-prefs global local)
  (let loop ((xs (append global local)) (acc '()))
    (if (null? xs)
        (reverse acc)
        (let* ((kv   (car xs))
               (key  (car kv))
               (acc2 (filter (lambda (p) (not (equal? (car p) key))) acc)))
          (loop (cdr xs) (cons kv acc2))))))

;; Allow XPI to be a file-like or a string path; if string, wrap with local-file.
(define (ensure-xpi-file v)
  (if (string? v) (local-file v) v))

;; Build 2-element list entries for XPIs.
(define (xpi-file-entries extdir addon-alist)
  (map (lambda (pair)
         (let* ((addon-id (car pair))
                (xpi      (ensure-xpi-file (cdr pair))))
           (list (string-append extdir addon-id ".xpi") xpi)))
       addon-alist))

;; --------------------------
;; Package: firefox-profile-launcher (goes into profile/bin)
;; --------------------------

(define (firefox-launcher-package _cfg)
  (let ((src (local-file "./firefox-profile-launcher.sh" #:recursive? #t)))
    (package
      (name "firefox-profile-launcher")
      (version "1.0")
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((out (assoc-ref %outputs "out"))
                   (bin (string-append out "/bin"))
                   (src (assoc-ref %build-inputs "src"))
                   (dst (string-append bin "/firefox-profile-launcher")))
              (mkdir-p bin)
              (copy-file src dst)
              (chmod dst #o555))
            #t)))
      (inputs (list (list "src" src)))
      (home-page #f)
      (synopsis "Launcher for Firefox profiles using wofi")
      (description "A small script that prompts for a Firefox profile via wofi and launches Firefox with it.")
      (license license:expat))))

;; --------------------------
;; Files payloads (2-element lists only)
;; --------------------------

(define (firefox-files-service config)
  (let* ((profiles        (firefox-conf-profiles config))
         (default-name    (firefox-conf-default-profile config))
         (global-ext-dir  ".mozilla/firefox/distribution/extensions/")
         (global-prefs    (firefox-conf-global-prefs config)))

    (define base-entries
      (list
       ;; NOTE: we no longer install the launcher into ~/.local/bin here.
       (list ".mozilla/firefox/profiles.ini"
             (plain-file "profiles.ini"
                         (serialize-profiles-ini profiles default-name)))))

    (define global-xpi-entries
      (xpi-file-entries global-ext-dir
                        (firefox-conf-global-extensions config)))

    (define per-profile-entries
      (concatenate
       (map (lambda (p)
              (let* ((pid     (firefox-profile-decl-id p))
                     (ppath   (profile-path p))
                     (dir     (string-append ".mozilla/firefox/" ppath "/"))
                     (extdir  (string-append dir "extensions/"))
                     (prefs   (merge-prefs global-prefs
                                           (firefox-profile-decl-prefs p)))
                     (userjs  (plain-file (string-append pid "-user.js")
                                          (serialize-user-js prefs))))
                (append
                 (list (list (string-append dir "user.js") userjs))
                 (xpi-file-entries extdir
                                   (firefox-profile-decl-extensions p)))))
            profiles)))

    (append base-entries global-xpi-entries per-profile-entries)))

(define (firefox-desktop-service _config)
  (let* ((desktop-id   "firefox-profile-launcher.desktop")
         (desktop-file
          (plain-file desktop-id
            (string-append
             "[Desktop Entry]\n"
             "Type=Application\n"
             "Name=Firefox (Profile Selector)\n"
             "Exec=firefox-profile-launcher %u\n"
             "Terminal=false\n"
             "MimeType=text/html;x-scheme-handler/http;x-scheme-handler/https;\n"
             "Categories=Network;WebBrowser;\n"))))
    (list (list (string-append "applications/" desktop-id) desktop-file))))

(define (firefox-mime-service _config)
  (home-xdg-mime-applications-configuration
   (default
    '(("x-scheme-handler/http"  . "firefox-profile-launcher.desktop")
      ("x-scheme-handler/https" . "firefox-profile-launcher.desktop")
      ("text/html"              . "firefox-profile-launcher.desktop")))))

;; --------------------------
;; Service type
;; --------------------------

(define-public firefox-service-type
  (service-type
   (name 'firefox-config)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (lambda (cfg)
        (list (firefox-conf-firefox-package cfg)
              (firefox-conf-wofi-package    cfg)
              (firefox-conf-xdg-package     cfg)
              (firefox-launcher-package     cfg))))
     (service-extension home-files-service-type firefox-files-service)
     (service-extension home-xdg-data-files-service-type firefox-desktop-service)
     (service-extension home-xdg-mime-applications-service-type firefox-mime-service)))
   (default-value (firefox-configuration (profiles '())))
   (description
    "Install Firefox, wofi, and xdg-utils; provide a Wofi-powered
profile selector as a packaged program added to your profile (and PATH); generate
profiles.ini, per-profile user.js (merging global and per-profile prefs), and
provision global/per-profile XPI extensions.")))
