;; peteches/system-services/sops-key-generator.scm
;;
;; One-shot shepherd service that generates an age keypair on first boot only.
;;
;; The existence of /etc/age/keys.txt is used as a sentinel — if it is already
;; present the service returns immediately without regenerating. This ensures
;; that every VM cloned from the same template gets its own unique keypair
;; while surviving subsequent reboots unchanged.
;;
;; Results:
;;   /etc/age/keys.txt  — private key (mode 0400, root only)
;;   /etc/age/keys.pub  — public key  (mode 0644)
;;
;; To use with sops: export SOPS_AGE_KEY_FILE=/etc/age/keys.txt

(define-module (peteches system-services sops-key-generator)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages golang-crypto)   ;; provides `age`
  #:use-module (guix gexp)
  #:export (sops-key-generator-service-type))

(define sops-key-generator-service-type
  (service-type
   (name 'sops-key-generator)
   (extensions
    (list
     (service-extension shepherd-root-service-type
       (lambda (_)
         (list
          (shepherd-service
           (provision '(sops-key-generator))
           (documentation "Generate age keypair for SOPS on first boot only.")
           (requirement '(file-systems))
           (one-shot? #t)
           (auto-start? #t)
           (start
            #~(lambda _
                (let* ((age-keygen #$(file-append age "/bin/age-keygen"))
                       (key-dir  "/etc/age")
                       (key-file "/etc/age/keys.txt")
                       (pub-file "/etc/age/keys.pub")
                       (read-line (@ (ice-9 rdelim) read-line)))
                  (if (and (file-exists? key-file) (file-exists? pub-file))
                      #t
                      (begin
                        (unless (file-exists? key-dir)
                          (mkdir key-dir))
                        (chmod key-dir #o700)
                        (unless (zero? (system* age-keygen "-o" key-file))
                          (error "sops-key-generator: age-keygen failed"))
                        (chmod key-file #o400)
                        ;; The generated file's second line is "# public key: age1..."
                        (let* ((port    (open-input-file key-file))
                               (dummy   (read-line port))
                               (pub-ln  (read-line port))
                               (_ (close-input-port port))
                               (pfx     "# public key: ")
                               (pub-key (if (string-prefix? pfx pub-ln)
                                            (substring pub-ln (string-length pfx))
                                            (error "sops-key-generator: unexpected key file format"))))
                          (call-with-output-file pub-file
                            (lambda (out) (display pub-key out) (newline out)))
                          (chmod pub-file #o644)
                          #t))))))
           (stop #~(lambda _ #t))))))))
   (default-value #f)
   (description "Generates an age keypair on first boot only.
Private key: /etc/age/keys.txt (0400). Public key: /etc/age/keys.pub (0644).
Set SOPS_AGE_KEY_FILE=/etc/age/keys.txt to use with sops.")))
