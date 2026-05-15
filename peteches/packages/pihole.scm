(define-module (peteches packages pihole)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

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
