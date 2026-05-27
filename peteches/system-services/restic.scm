;; restic.scm — VM backup service using restic over SFTP to a Synology NAS.
;;
;; On first deploy the activation phase:
;;   1. Fetches the Synology SSH host key via ssh-keyscan (TOFU) if absent.
;;   2. Attempts restic init; fails gracefully with instructions if SSH not yet
;;      authorised on the NAS.
;;
;; Both the repository password and the SSH private key must be provided via
;; SOPS secrets (managed externally — no secrets are generated locally).
;;
;; Each scheduled backup job also attempts init if the repository does not yet
;; exist, making runs self-healing once SSH access is in place.
;;
;; Trigger a backup manually: herd trigger <vm-name>-backup

(define-module (peteches system-services restic)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages ssh)
  #:use-module ((gnu packages backup) #:select (restic))
  #:export (restic-vm-backup-configuration
            restic-vm-backup-configuration?
            restic-vm-backup-service-type))

(define-record-type* <restic-vm-backup-configuration>
  restic-vm-backup-configuration
  make-restic-vm-backup-configuration
  restic-vm-backup-configuration?
  (vm-name            restic-vm-backup-configuration-vm-name)
  (synology-host      restic-vm-backup-configuration-synology-host)
  (synology-user      restic-vm-backup-configuration-synology-user
                      (default "restic-backup"))
  (synology-base-path restic-vm-backup-configuration-synology-base-path
                      (default "/Backups"))
  (backup-paths       restic-vm-backup-configuration-backup-paths)
  (schedule           restic-vm-backup-configuration-schedule
                      (default "0 2 * * *"))
  (prune-schedule     restic-vm-backup-configuration-prune-schedule
                      (default "0 3 * * 0"))
  (password-file      restic-vm-backup-configuration-password-file
                      (default "/etc/restic/password"))
  (ssh-key-file       restic-vm-backup-configuration-ssh-key-file
                      (default "/etc/restic/id_ed25519")))

(define (restic-repository cfg)
  (string-append "sftp:"
                 (restic-vm-backup-configuration-synology-user cfg)
                 "@"
                 (restic-vm-backup-configuration-synology-host cfg)
                 ":"
                 (restic-vm-backup-configuration-synology-base-path cfg)
                 "/"
                 (restic-vm-backup-configuration-vm-name cfg)))

(define (restic-sftp-args cfg)
  (string-append "sftp.args=-i "
                 (restic-vm-backup-configuration-ssh-key-file cfg)
                 " -o BatchMode=yes"))

(define (restic-vm-backup-activation cfg)
  (let ((key-file (restic-vm-backup-configuration-ssh-key-file cfg))
        (pw-file  (restic-vm-backup-configuration-password-file cfg))
        (host     (restic-vm-backup-configuration-synology-host cfg))
        (repo     (restic-repository cfg))
        (sftp-o   (restic-sftp-args cfg)))
    #~(begin
        (use-modules (guix build utils))

        (let ((key-file #$key-file)
              (pw-file  #$pw-file)
              (kh-file  "/root/.ssh/known_hosts"))


          (mkdir-p "/root/.ssh")
          (chmod "/root/.ssh" #o700)
          (unless (zero? (system* #$(file-append openssh "/bin/ssh-keygen")
                                  "-F" #$host "-f" kh-file))
            (system (string-append #$(file-append openssh "/bin/ssh-keyscan")
                                   " -H " #$host " >> " kh-file)))

          (unless (zero? (system* #$(file-append restic "/bin/restic")
                                  "-r" #$repo "--password-file" pw-file
                                  "-o" #$sftp-o "snapshots" "--no-lock"))
            (let ((r (system* #$(file-append restic "/bin/restic")
                               "-r" #$repo "--password-file" pw-file
                               "-o" #$sftp-o "init")))
              (unless (zero? r)
                (display
                 (string-append
                  "restic: WARNING - init failed for " #$repo ".\n"
                  "Ensure " #$key-file " is authorised on the NAS,\n"
                  "then re-run: guix deploy -L . peteches/deploy.scm\n")))))))))

(define (restic-vm-shepherd-services cfg)
  (let* ((vm-name  (restic-vm-backup-configuration-vm-name cfg))
         (repo     (restic-repository cfg))
         (pw-file  (restic-vm-backup-configuration-password-file cfg))
         (sftp-o   (restic-sftp-args cfg))
         (paths    (restic-vm-backup-configuration-backup-paths cfg))
         (backup-s (restic-vm-backup-configuration-schedule cfg))
         (prune-s  (restic-vm-backup-configuration-prune-schedule cfg))
         (backup-script
          (program-file
           (string-append vm-name "-backup")
           #~(begin
               (setenv "PATH" (string-append #$(file-append openssh "/bin") ":"
                                             (or (getenv "PATH") "")))
               (unless (zero? (system* #$(file-append restic "/bin/restic")
                                       "-r" #$repo "--password-file" #$pw-file
                                       "-o" #$sftp-o "snapshots" "--no-lock"))
                 (system* #$(file-append restic "/bin/restic")
                          "-r" #$repo "--password-file" #$pw-file
                          "-o" #$sftp-o "init"))
               (system* #$(file-append restic "/bin/restic")
                        "backup"
                        "-r" #$repo "--password-file" #$pw-file
                        "-o" #$sftp-o
                        #$@paths))))
         (prune-script
          (program-file
           (string-append vm-name "-prune")
           #~(begin
               (setenv "PATH" (string-append #$(file-append openssh "/bin") ":"
                                             (or (getenv "PATH") "")))
               (system* #$(file-append restic "/bin/restic")
                        "-r" #$repo "--password-file" #$pw-file
                        "-o" #$sftp-o
                        "forget" "--prune"
                        "--keep-daily" "7"
                        "--keep-weekly" "4"
                        "--keep-monthly" "3")))))
    (list
     (shepherd-service
      (provision (list (string->symbol (string-append vm-name "-backup"))))
      (requirement '(networking))
      (modules '((shepherd service timer)))
      (start #~(make-timer-constructor
                (cron-string->calendar-event #$backup-s)
                (command (list #$backup-script))))
      (stop #~(make-timer-destructor))
      (actions
       (list (shepherd-action
              (name 'trigger)
              (documentation "Run the backup immediately.")
              (procedure #~(lambda (running . _)
                             (system* #$backup-script)
                             running)))))
      (documentation (string-append "Daily restic backup for " vm-name
                                    ". Trigger: herd trigger " vm-name "-backup")))
     (shepherd-service
      (provision (list (string->symbol (string-append vm-name "-prune"))))
      (requirement '(networking))
      (modules '((shepherd service timer)))
      (start #~(make-timer-constructor
                (cron-string->calendar-event #$prune-s)
                (command (list #$prune-script))))
      (stop #~(make-timer-destructor))
      (actions
       (list (shepherd-action
              (name 'trigger)
              (documentation "Run the prune immediately.")
              (procedure #~(lambda (running . _)
                             (system* #$prune-script)
                             running)))))
      (documentation (string-append "Weekly restic prune for " vm-name))))))

(define restic-vm-backup-service-type
  (service-type
   (name 'restic-vm-backup)
   (extensions
    (list
     (service-extension activation-service-type    restic-vm-backup-activation)
     (service-extension shepherd-root-service-type restic-vm-shepherd-services)))
   (description
    "VM backup service using restic over SFTP to a Synology NAS.  Fetches the \
NAS host key via ssh-keyscan (TOFU) and initializes the restic repository if \
missing.  Secrets (password and SSH key) are provided via SOPS.  Each \
scheduled backup also auto-inits if needed.  Trigger manually with \
`herd trigger <vm-name>-backup`.")))
