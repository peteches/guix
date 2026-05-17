(define-module (peteches packages pihole)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages ncurses))

(define-public pihole-ftl
  (let* ((version "6.6.2")
         (url (string-append
               "https://github.com/pi-hole/FTL/releases/download/v"
               version "/pihole-FTL-amd64")))
    (package
      (name "pihole-ftl")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri url)
         (sha256 (base32 "1fswam72qz4f564zml8n2xi66d4rdlz6zdgvz3dnwx8qf58vvg91"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~(list (list "pihole-FTL-amd64" "bin/pihole-FTL"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'make-executable
              (lambda _
                (chmod "pihole-FTL-amd64" #o755))))))
      (home-page "https://github.com/pi-hole/FTL")
      (synopsis "Pi-hole FTL DNS ad-blocking daemon")
      (description "pihole-FTL is the core daemon of Pi-hole.  It is a
modified dnsmasq that provides DNS resolution with ad-blocking, a built-in
web server for the admin interface, and a SQLite-based query database.")
      (license (list gpl2+ lgpl2.1+)))))

(define-public pihole-scripts
  (package
    (name "pihole-scripts")
    (version "6.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pi-hole/pi-hole/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0qh67aqzgg1mya4v0a18l6k3h5zlcqk0zlwy9q4rib2rhr06d9s9"))))
    (build-system copy-build-system)
    (inputs (list ncurses))
    (arguments
     (list
      #:install-plan
      #~(list
         ;; copy-build-system installs directory source AS a subdirectory, so
         ;; "advanced/Scripts" ends up at share/pihole/Scripts/.  That is our
         ;; PI_HOLE_SCRIPT_DIR equivalent, matching /opt/pihole on a standard install.
         (list "advanced/Scripts" "share/pihole/")
         ;; gravity.sh, prestart/poststop go alongside the other scripts.
         (list "gravity.sh" "share/pihole/Scripts/gravity.sh")
         (list "advanced/Templates/pihole-FTL-prestart.sh"
               "share/pihole/Scripts/pihole-FTL-prestart.sh")
         (list "advanced/Templates/pihole-FTL-poststop.sh"
               "share/pihole/Scripts/pihole-FTL-poststop.sh")
         ;; SQL templates used by gravity.sh at runtime; gravity.sh sources these
         ;; relative to piholeGitDir which we patch below to point into the store.
         (list "advanced/Templates/gravity.db.sql"
               "share/pihole/advanced/Templates/gravity.db.sql")
         (list "advanced/Templates/gravity_copy.sql"
               "share/pihole/advanced/Templates/gravity_copy.sql")
         ;; Main pihole CLI sits one level above the Scripts/ dir.
         (list "pihole" "share/pihole/pihole"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-and-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out         (assoc-ref outputs "out"))
                     ;; All helper scripts land in Scripts/ due to copy-build-system
                     ;; directory-install semantics.
                     (scripts-dir (string-append out "/share/pihole/Scripts"))
                     (pihole-main (string-append out "/share/pihole/pihole"))
                     (bin-dir     (string-append out "/bin")))
                ;; Make all installed scripts executable.
                (for-each (lambda (f) (chmod f #o755))
                          (find-files (string-append out "/share/pihole")))
                ;; Patch PI_HOLE_SCRIPT_DIR in every .sh file so each script
                ;; finds its helpers in the Guix store (share/pihole/Scripts/)
                ;; rather than /opt/pihole.
                (for-each
                 (lambda (f)
                   (substitute* f
                     (("PI_HOLE_SCRIPT_DIR=\"/opt/pihole\"")
                      (string-append "PI_HOLE_SCRIPT_DIR=\"" scripts-dir "\""))))
                 (find-files scripts-dir "\\.sh$"))
                ;; The main pihole CLI has no .sh extension; patch it separately.
                (substitute* pihole-main
                  (("PI_HOLE_SCRIPT_DIR=\"/opt/pihole\"")
                   (string-append "PI_HOLE_SCRIPT_DIR=\"" scripts-dir "\""))
                  ;; Allow the wrapper's PI_HOLE_BIN_DIR export to survive.
                  (("^PI_HOLE_BIN_DIR=\"/usr/local/bin\"")
                   "PI_HOLE_BIN_DIR=\"${PI_HOLE_BIN_DIR:-/usr/local/bin}\""))
                ;; Fix upstream bug: pihole declares FTL_PID_FILE readonly at
                ;; global scope; getFTLPID() in utils.sh then tries to shadow it
                ;; with `local FTL_PID_FILE=...` which bash rejects.  Rename the
                ;; local parameter to avoid the conflict.
                (substitute* (string-append scripts-dir "/utils.sh")
                  (("local FTL_PID_FILE=\"\\$\\{1\\}\"")
                   "local _ftl_pid_file=\"${1}\"")
                  ;; Update the two uses of the local variable within the function.
                  (("\\$\\{FTL_PID_FILE\\}" all)
                   (if (string=? all "${FTL_PID_FILE}")
                       "${_ftl_pid_file}"
                       all)))
                ;; Patch gravity.sh: fix the three hardcoded paths that the
                ;; PI_HOLE_SCRIPT_DIR substitution above does not cover, and
                ;; replace the `getent hosts` DNS check (not in Guix) with wget.
                (let* ((gravity-sh  (string-append scripts-dir "/gravity.sh"))
                       (pihole-dir  (string-append out "/share/pihole")))
                  (substitute* gravity-sh
                    ;; gravity.sh sources gravity-db.sh from /etc/.pihole which
                    ;; doesn't exist on Guix; the file lives under Scripts/.
                    (("\"/etc/.pihole/advanced/Scripts/database_migration/gravity-db.sh\"")
                     (string-append "\"" scripts-dir "/database_migration/gravity-db.sh\""))
                    ;; piholeGitDir drives gravityDBschema and gravityDBcopy paths;
                    ;; point at our store location (advanced/Templates/ installed below).
                    (("piholeGitDir=\"/etc/.pihole\"")
                     (string-append "piholeGitDir=\"" pihole-dir "\""))
                    ;; PIHOLE_COMMAND is used for `pihole status` inside gravity.sh;
                    ;; honour PI_HOLE_BIN_DIR set by our wrapper.
                    (("PIHOLE_COMMAND=\"/usr/local/bin/\\$\\{basename\\}\"")
                     "PIHOLE_COMMAND=\"${PI_HOLE_BIN_DIR:-/run/current-system/profile/bin}/${basename}\"")
                    ;; gravity_CheckDNSResolutionAvailable uses `getent hosts` which
                    ;; is not available on a minimal Guix system; use wget instead.
                    (("timeout 4 getent hosts \"\\$\\{lookupDomain\\}\" &>/dev/null")
                     "wget -q --tries=1 --timeout=4 -O /dev/null \"https://${lookupDomain}/\" &>/dev/null")
                    (("if getent hosts github\\.com &> /dev/null")
                     "if wget -q --tries=1 --timeout=4 -O /dev/null https://github.com/ &> /dev/null")))
                ;; Create bin/pihole wrapper that sets PI_HOLE_BIN_DIR
                ;; dynamically and prepends ncurses to PATH so tput is found.
                (mkdir-p bin-dir)
                (let* ((ncurses-bin #$(file-append ncurses "/bin"))
                       (wrapper (string-append bin-dir "/pihole")))
                  (call-with-output-file wrapper
                    (lambda (port)
                      (display
                       (string-append
                        "#!/usr/bin/env bash\n"
                        "PATH=\"" ncurses-bin ":$PATH\"\n"
                        "PI_HOLE_BIN_DIR=\"$(dirname \"$(readlink -f"
                        " \"${BASH_SOURCE[0]}\")\")\"" "\n"
                        "export PATH PI_HOLE_BIN_DIR\n"
                        "exec \"" pihole-main "\" \"$@\"\n")
                       port)))
                  (chmod wrapper #o755))))))))
    (home-page "https://github.com/pi-hole/pi-hole")
    (synopsis "Pi-hole core administrative scripts and CLI")
    (description
     "Pi-hole core scripts providing the pihole command-line interface and
supporting utilities for DNS ad-blocking management.  Includes gravity.sh for
blocklist updates, api.sh for FTL API access, and helper scripts for common
Pi-hole administration tasks.")
    (license eupl1.2)))

(define-public pihole-web
  (package
    (name "pihole-web")
    (version "6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pi-hole/web/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "07wvcyfpzznwsvndv7yj0dj1aqs1q3gh1r9ammddd8y40djis9p7"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (dest (string-append out "/share/pihole-web/admin")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/pi-hole/web")
    (synopsis "Pi-hole web administration interface")
    (description
     "Pi-hole web interface providing the admin dashboard for DNS ad-blocking
management.  Includes Lua pages served by pihole-FTL's built-in web server,
along with JavaScript, CSS, and vendor libraries.")
    (license eupl1.2)))
