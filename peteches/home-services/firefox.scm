;;; firefox.scm — Per-profile enforced add-ons using one wrapper per profile + external launcher.

(define-module (peteches home-services firefox)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1)
  #:export (firefox-configuration
            firefox-profile-decl
            firefox-profile
            firefox-files-service
            firefox-desktop-service
            firefox-mime-service
            firefox-service-type))

;; ------------------------------------------------------------------
;; Records
;; ------------------------------------------------------------------

(define-record-type* <firefox-profile-decl>
  firefox-profile-decl make-firefox-profile-decl firefox-profile-decl?
  (name       firefox-profile-decl-name)
  (id         firefox-profile-decl-id)
  (path       firefox-profile-decl-path (default #f))
  (prefs      firefox-profile-decl-prefs (default '()))
  (extensions firefox-profile-decl-extensions (default '())))

(define* (firefox-profile name id #:key (path #f) (prefs '()) (extensions '()))
  (firefox-profile-decl
   (name name) (id id) (path path) (prefs prefs) (extensions extensions)))

(define-record-type* <firefox-configuration>
  firefox-configuration make-firefox-configuration firefox-configuration?
  (firefox-package   firefox-conf-firefox-package (default firefox))
  (wofi-package      firefox-conf-wofi-package    (default wofi))
  (xdg-utils-package firefox-conf-xdg-package     (default xdg-utils))
  (wofi-cmd          firefox-conf-wofi-cmd        (default "wofi --dmenu --prompt 'Select Firefox Profile:'"))
  (profiles          firefox-conf-profiles)
  (default-profile   firefox-conf-default-profile (default #f))
  (global-extensions firefox-conf-global-extensions (default '()))
  (global-prefs      firefox-conf-global-prefs (default '())))

;; ------------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------------

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
                   (if (and default-name (string=? (firefox-profile-decl-name p) default-name))
                       "Default=1\n\n" "\n"))))
          (loop (cdr ps) (+ i 1) (cons s acc))))))

(define (serialize-user-js prefs)
  (define (one kv)
    (let* ((k (car kv)) (v (cdr kv))
           (vv (cond ((boolean? v) (if v "true" "false"))
                     ((number?  v) (number->string v))
                     (else (string-append "\"" v "\"")))))
      (string-append "user_pref(\"" k "\", " vv ");\n")))
  (apply string-append (map one prefs)))

(define (merge-prefs global local)
  (let loop ((xs (append global local)) (acc '()))
    (if (null? xs)
        (reverse acc)
        (let* ((kv (car xs)) (key (car kv))
               (acc2 (filter (lambda (p) (not (equal? (car p) key))) acc)))
          (loop (cdr xs) (cons kv acc2))))))

(define (ensure-xpi-file v)
  (if (string? v) (local-file v) v))

;; ------------------------------------------------------------------
;; Wrapper package (builder uses a gexp)
;; ------------------------------------------------------------------

(define* (firefox-with-policies #:key (base firefox) (extensions '()) (tag "p"))
  (let* ((ensure    (lambda (p) (cons (car p) (ensure-xpi-file (cdr p)))))
         (ext-pairs (map ensure extensions))
         (xpi-inputs (map (lambda (p) (list (string-append "xpi-" (car p)) (cdr p))) ext-pairs))
         (ids (map car ext-pairs)))
    (package
     (name (string-append (package-name base) "-with-" tag))
     (version (package-version base))
     (source #f)
     (build-system trivial-build-system)
     (inputs `(("firefox" ,base) ,@xpi-inputs))
     (arguments
      (list
       #:builder
       (with-imported-modules '((guix build utils))
			      #~(begin
				  (use-modules (guix build utils))
				  (letrec ((join
					    (lambda (xs)
					      (let loop ((xs xs) (acc ""))
						(cond ((null? xs) acc)
						      ((string-null? acc) (loop (cdr xs) (car xs)))
						      (else (loop (cdr xs) (string-append acc "," (car xs)))))))))
				    (let* ((out   (assoc-ref %outputs "out"))
					   (ff-in (assoc-ref %build-inputs "firefox"))
					   (ffdir (string-append out "/lib/firefox"))
					   (dist  (string-append ffdir "/distribution"))
					   (extd  (string-append dist "/extensions"))
					   (ids '#$ids))
				      (copy-recursively ff-in out)
				      
				      ;; Ensure our /bin/firefox runs from this output's libdir, not the input.
				      ;; KEEP the copy-recursively and your distribution/policies/exts writes.
				      ;; Then install a /bin/firefox shim that *chains to the base wrapper*,
				      ;; but forces this output's libdir as the app dir.
				      (let* ((bindir (string-append out "/bin"))
					     (exe    (string-append bindir "/firefox"))
					     (ffdir  (string-append out "/lib/firefox"))
					     (base-wrapper (string-append (assoc-ref %build-inputs "firefox") "/bin/firefox")))
					(when (file-exists? exe) (delete-file exe))
					(mkdir-p bindir)
					(call-with-output-file exe
					  (lambda (p)
					    ;; ~s safely quotes the absolute paths.
					    (format p "#! /bin/sh
export MOZ_APP_DIR=~s
exec -a \"$0\" ~s \"$@\"
" ffdir base-wrapper)))
					(chmod exe #o555))


				      (mkdir-p dist)
				      (mkdir-p extd)
				      ;; Copy XPIs
				      (for-each
				       (lambda (id)
					 (let* ((lab (string-append "xpi-" id))
						(xpi (assoc-ref %build-inputs lab))
						(dst (string-append extd "/" id ".xpi")))
					   (copy-file xpi dst)))
				       ids)
				      ;; Write policies.json
				      (let* ((entries
					      (map (lambda (id)
						     (let* ((path (string-append extd "/" id ".xpi")))
						       (string-append
							"\"" id "\":{"
							"\"installation_mode\":\"force_installed\"," 
							"\"install_file\":\"" path "\"}")))
						   ids))
					     (json (string-append
						    "{\"policies\":{\"ExtensionSettings\":{"
						    (join entries)
						    "}}}\n")))
					(call-with-output-file (string-append dist "/policies.json")
					  (lambda (p) (display json p))))
				      #t))))))
     (home-page (package-home-page base))
     (synopsis "Firefox with embedded enterprise policies and extensions")
     (description "Copies Firefox into a new store item and overlays distribution/policies.json and distribution/extensions/*.xpi so Firefox force-installs the selected extensions.")
     (license (package-license base)))))

;; ------------------------------------------------------------------
;; External launcher package (script via local-file, map generated here)
;; ------------------------------------------------------------------

(define (firefox-launcher-package cfg wrappers)
  ;; WRAPPERS: list of (profile . wrapper-package)
  (let* ((launcher-src (local-file "./firefox-profile-launcher.sh"))
         (wofi-cmd     (firefox-conf-wofi-cmd cfg))
         (labels+inputs
          (map (lambda (pw)
                 (let* ((p   (car pw))
                        (pkg (cdr pw))
                        (name (firefox-profile-decl-name p))
                        (lab  (string-append "ff-" (firefox-profile-decl-id p))))
                   (list name lab pkg)))
               wrappers)))
    (package
     (name "firefox-profile-launcher")
     (version "1.0")
     (source #f)
     (build-system trivial-build-system)
     (inputs (append (list (list "launcher-src" launcher-src))
                     (map (lambda (n-l-p) (list (cadr n-l-p) (caddr n-l-p))) labels+inputs)))
     (arguments
      (list
       #:builder
       (with-imported-modules '((guix build utils))
			      #~(begin
				  (use-modules (guix build utils))
				  (let* ((out (assoc-ref %outputs "out"))
					 (bin (string-append out "/bin"))
					 (share (string-append out "/share"))
					 (dst (string-append bin "/firefox-profile-launcher"))
					 (src (assoc-ref %build-inputs "launcher-src"))
					 (mapf (string-append share "/firefox-wrappers.txt"))
					 (name+labels '#$(map (lambda (nlp) (list (car nlp) (cadr nlp))) labels+inputs))
                     (pairs
                      (map (lambda (n-l)
                             (let* ((nm (car n-l))
                                    (lb (cadr n-l))
                                    (pp (assoc-ref %build-inputs lb)))
                               (cons nm (string-append pp "/bin/firefox"))))
                           name+labels)))
				    (mkdir-p bin)
				    (mkdir-p share)
				    (copy-file src dst)
				    (chmod dst #o555)
				    (call-with-output-file mapf
				      (lambda (port)
					(for-each (lambda (pr)
						    (format port "~a\t~a\n" (car pr) (cdr pr)))
						  pairs)))
				    (call-with-output-file (string-append share "/firefox-launcher.cfg")
				      (lambda (p) (format p "wofi=%s\n" #$wofi-cmd)))
				    #t)))))
     (home-page #f)
     (synopsis "Profile selector for per-profile Firefox wrappers")
     (description "Copies an external shell launcher and a generated map of profile names to wrapped Firefox binaries; the launcher uses wofi to select and exec the right binary with -P <Name>.")
     (license license:expat))))

;; ------------------------------------------------------------------
;; Files payloads
;; ------------------------------------------------------------------

(define (firefox-files-service config)
  (let* ((profiles     (firefox-conf-profiles config))
         (default-name (firefox-conf-default-profile config))
         (global-prefs (firefox-conf-global-prefs config)))
    (define per-profile-entries
      (concatenate
       (map (lambda (p)
              (let* ((pid   (firefox-profile-decl-id p))
                     (ppath (profile-path p))
                     (dir   (string-append ".mozilla/firefox/" ppath "/"))
                     (prefs (merge-prefs global-prefs (firefox-profile-decl-prefs p)))
                     (userjs (plain-file (string-append pid "-user.js")
                                         (serialize-user-js prefs))))
                (list (list (string-append dir "user.js") userjs))))
            profiles)))
    (append
     (list (list ".mozilla/firefox/profiles.ini"
                 (plain-file "profiles.ini"
                             (serialize-profiles-ini profiles default-name))))
     per-profile-entries)))

;; ------------------------------------------------------------------
;; Desktop & MIME
;; ------------------------------------------------------------------

(define (firefox-desktop-service _config)
  (let* ((desktop-id "firefox-profile-launcher.desktop")
         (desktop-file
          (plain-file desktop-id
		      (string-append
		       "[Desktop Entry]\n"
		       "Type=Application\n"
		       "Name=Firefox (Profile Selector)\n"
		       "Exec=firefox-profile-launcher\n"
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

;; ------------------------------------------------------------------
;; Service type
;; ------------------------------------------------------------------

(define-public firefox-service-type
  (service-type
   (name 'firefox-config)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (lambda (cfg)
        (let* ((base     (firefox-conf-firefox-package cfg))
               (globals  (firefox-conf-global-extensions cfg))
               (profiles (firefox-conf-profiles cfg))
               (wrappers
                (map (lambda (p)
                       (let* ((per (firefox-profile-decl-extensions p))
                              (all (append globals per))
                              (pkg (firefox-with-policies
                                    #:base base
                                    #:extensions all
                                    #:tag (firefox-profile-decl-id p))))
                         (cons p pkg)))
                     profiles))
               (launcher (firefox-launcher-package cfg wrappers)))
          (append
           (list (firefox-conf-wofi-package cfg)
                 (firefox-conf-xdg-package cfg)
                 launcher)
           (map cdr wrappers)))))
     (service-extension home-files-service-type firefox-files-service)
     (service-extension home-xdg-data-files-service-type firefox-desktop-service)
     (service-extension home-xdg-mime-applications-service-type firefox-mime-service)))
   (default-value (firefox-configuration (profiles '())))
   (description
    "Build one Firefox wrapper per profile with enterprise policies that force-install the selected extensions, copy an external selector launcher, and lay down profiles.ini and per-profile user.js files.")))
