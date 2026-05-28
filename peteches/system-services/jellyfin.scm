;; peteches/system-services/jellyfin.scm — Guix service type for Jellyfin.

(define-module (peteches system-services jellyfin)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages jellyfin)
  #:use-module (gnu packages samba)
  #:use-module (peteches system-services firewall)
  #:export (jellyfin-configuration
            jellyfin-configuration?
            jellyfin-service-type))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <jellyfin-configuration>
  jellyfin-configuration make-jellyfin-configuration
  jellyfin-configuration?
  (package        jellyfin-configuration-package        (default jellyfin))
  (ffmpeg-package jellyfin-configuration-ffmpeg-package (default jellyfin-ffmpeg))
  (http-port      jellyfin-configuration-http-port      (default 8096))
  (data-path      jellyfin-configuration-data-path      (default "/var/lib/jellyfin"))
  (log-path       jellyfin-configuration-log-path       (default "/var/log/jellyfin"))
  (cache-path     jellyfin-configuration-cache-path     (default "/var/cache/jellyfin"))
  ;; Symbol: 'none, 'vaapi, or 'nvenc.
  ;; 'vaapi adds the jellyfin user to "video" and "render" groups.
  ;; 'nvenc adds it to "video" only.
  (hardware-accel jellyfin-configuration-hardware-accel (default 'none))
  ;; Optional CIFS media mount, handled as a shepherd service that depends
  ;; on sops-secrets so credentials exist before the mount is attempted.
  (media-device      jellyfin-configuration-media-device      (default #f))
  (media-mount-point jellyfin-configuration-media-mount-point (default "/media"))
  (media-options     jellyfin-configuration-media-options     (default "")))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (jellyfin-accounts config)
  (let ((hw        (jellyfin-configuration-hardware-accel config))
        (data-path (jellyfin-configuration-data-path config)))
    (list
     (user-group
      (name "jellyfin")
      (system? #t))
     (user-account
      (name "jellyfin")
      (group "jellyfin")
      (system? #t)
      (comment "Jellyfin media server daemon")
      (home-directory data-path)
      (supplementary-groups (cond
                              ((eq? hw 'vaapi) '("video" "render"))
                              ((eq? hw 'nvenc) '("video"))
                              (else '())))
      (shell (file-append shadow "/sbin/nologin"))))))

(define (jellyfin-activation config)
  (let ((data-path  (jellyfin-configuration-data-path config))
        (log-path   (jellyfin-configuration-log-path config))
        (cache-path (jellyfin-configuration-cache-path config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "jellyfin"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (d)
                      (mkdir-p d)
                      (chown d uid gid))
                    (list #$data-path #$log-path #$cache-path)))
        (system* #$(file-append shepherd "/bin/herd")
                 "restart" "jellyfin"))))

(define (jellyfin-shepherd-service config)
  (let* ((pkg        (jellyfin-configuration-package config))
         (ffmpeg-pkg (jellyfin-configuration-ffmpeg-package config))
         (data-path  (jellyfin-configuration-data-path config))
         (log-path   (jellyfin-configuration-log-path config))
         (cache-path (jellyfin-configuration-cache-path config))
         (log-file   (string-append log-path "/startup.log")))
    (list
     (shepherd-service
      (provision '(jellyfin))
      (documentation "Jellyfin free software media server.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/jellyfin")
                      (string-append "--ffmpeg="
                                     #$(file-append ffmpeg-pkg "/bin/jellyfin-ffmpeg"))
                      (string-append "--datadir="  #$data-path)
                      (string-append "--cachedir=" #$cache-path)
                      (string-append "--logdir="   #$log-path))
                #:user "jellyfin"
                #:group "jellyfin"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (jellyfin-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (jellyfin-configuration-http-port config))
                 " accept comment \"jellyfin\"")))))

(define (jellyfin-profile config)
  (list (jellyfin-configuration-package config)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public jellyfin-service-type
  (service-type
   (name 'jellyfin)
   (description "Jellyfin free software media server.")
   (extensions
    (list
     (service-extension account-service-type       jellyfin-accounts)
     (service-extension activation-service-type    jellyfin-activation)
     (service-extension shepherd-root-service-type jellyfin-shepherd-service)
     (service-extension firewall-service-type      jellyfin-firewall-rules)
     (service-extension profile-service-type       jellyfin-profile)))
   (default-value (jellyfin-configuration))))
