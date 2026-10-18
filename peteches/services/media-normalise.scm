;;; peteches/services/media-normalise.scm — nightly library normalisation.
;;;
;;; Runs peteches/packages/media-normalise.sh as a nightly mcron job and as a
;;; manual one-shot shepherd service (`herd start media-normalise`).
;;;
;;; The script re-encodes non-conforming library video to the house format
;;; (4K HEVC Main 10 HDR10 preserved, TrueHD/DTS -> AAC 5.1, image subtitles
;;; dropped, MP4 +faststart, capped bitrate) and re-imports it through the
;;; owning Arr app's Manual Import API, removing the replaced original only
;;; after the import has been verified.
;;;
;;; WHY: the LG WebOS client cannot play TrueHD/Atmos audio or PGS bitmap
;;; subtitles, and OOMs on remux bitrates, so every such item forced an
;;; on-the-fly transcode that the client then stalled on.  Normalising offline
;;; makes everything direct-playable and removes on-the-fly transcoding.
;;;
;;; ROUTING: films -> Radarr, tv -> Sonarr, standup-comedy -> STANDUP-APP.
;;; The staging root mirrors this split (/media/import/<split>/...) so each
;;; app's Manual Import only ever sees its own domain.
;;;
;;; SECRETS: the Arr API keys are read at run time from the files named by
;;; RADARR-API-KEY-FILE / SONARR-API-KEY-FILE (SOPS-decrypted into
;;; /run/secrets).  Until those sops secrets exist the job aborts at preflight
;;; without touching anything.
;;;
;;; ONE-TIME ARR SETUP (UI, per app): create a quality profile named
;;; PROFILE-NAME whose allowed qualities top out at QUALITY-NAME and whose
;;; upgrade cutoff is QUALITY-NAME, so normalised items are never "upgraded"
;;; back to a remux.  QUALITY-NAME is deliberately a truthful existing quality
;;; (Bluray-2160p = a 2160p encode, as opposed to Remux-2160p) because Arr
;;; apps do not support user-defined qualities.

(define-module (peteches services media-normalise)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mcron)
  #:use-module (gnu services base)
  #:use-module (srfi srfi-13)
  #:use-module (peteches packages media-normalise)
  #:export (media-normalise-configuration media-normalise-configuration?
            media-normalise-service-type))

(define-record-type* <media-normalise-configuration> media-normalise-configuration
  make-media-normalise-configuration
  media-normalise-configuration?
  (package media-normalise-configuration-package
           (default media-normalise))
  ;; Library roots walked for candidate media files.
  (library-roots media-normalise-configuration-library-roots
                 (default '("/media/library/films"
                            "/media/library/tv"
                            "/media/library/standup-comedy")))
  ;; Staging root; the per-item subdir mirrors the films/tv/standup split.
  (staging-root media-normalise-configuration-staging-root
                (default "/media/import"))
  (radarr-url media-normalise-configuration-radarr-url
              (default "http://192.168.51.195:7878"))
  (sonarr-url media-normalise-configuration-sonarr-url
              (default "http://192.168.51.195:8989"))
  (radarr-api-key-file media-normalise-configuration-radarr-api-key-file
                       (default "/run/secrets/radarr-api-key"))
  (sonarr-api-key-file media-normalise-configuration-sonarr-api-key-file
                       (default "/run/secrets/sonarr-api-key"))
  ;; Truthful quality tag applied at import time (Arr has no custom qualities).
  (quality-name media-normalise-configuration-quality-name
                (default "Bluray-2160p"))
  ;; Profile pinned onto normalised items so they are never upgraded back.
  (profile-name media-normalise-configuration-profile-name
                (default "Normalised-2160p"))
  ;; Which app owns standup-comedy (specials are usually movies).
  (standup-app media-normalise-configuration-standup-app
               (default "radarr"))
  ;; Encoder rate control.
  (maxrate media-normalise-configuration-maxrate (default 25000000))
  (cq media-normalise-configuration-cq (default 27))
  ;; Nightly schedule (crontab format).  Deliberately late-night: the job may
  ;; remove a replaced original, which must not happen mid-playback.
  (schedule media-normalise-configuration-schedule
            (default "17 4 * * *"))
  (log-file media-normalise-configuration-log-file
            (default "/var/log/media-normalise.log")))

(define (media-normalise-etc-file config)
  (let ((pkg (media-normalise-configuration-package config)))
    (mixed-text-file
     "media-normalise-config"
     "LIBRARY_ROOTS=\"" (string-join (media-normalise-configuration-library-roots config) " ") "\"\n"
     "STAGING_ROOT=\"" (media-normalise-configuration-staging-root config) "\"\n"
     "RADARR_URL=\"" (media-normalise-configuration-radarr-url config) "\"\n"
     "SONARR_URL=\"" (media-normalise-configuration-sonarr-url config) "\"\n"
     "RADARR_API_KEY_FILE=\"" (media-normalise-configuration-radarr-api-key-file config) "\"\n"
     "SONARR_API_KEY_FILE=\"" (media-normalise-configuration-sonarr-api-key-file config) "\"\n"
     "QUALITY_NAME=\"" (media-normalise-configuration-quality-name config) "\"\n"
     "PROFILE_NAME=\"" (media-normalise-configuration-profile-name config) "\"\n"
     "STANDUP_APP=\"" (media-normalise-configuration-standup-app config) "\"\n"
     "MAXRATE=" (number->string (media-normalise-configuration-maxrate config)) "\n"
     "CQ=" (number->string (media-normalise-configuration-cq config)) "\n"
     "LOG=\"" (media-normalise-configuration-log-file config) "\"\n")))

(define (media-normalise-shepherd-services config)
  (let ((bin (file-append (media-normalise-configuration-package config)
                          "/bin/media-normalise")))
    (list (shepherd-service
           (provision '(media-normalise))
           (documentation "Offline media library normalisation (one-shot).")
           (requirement '(file-systems networking))
           (start #~(make-forkexec-constructor (list #$bin)))
           (stop #~(const #f))
           (one-shot? #t)))))

(define (media-normalise-mcron-jobs config)
  (let ((bin (file-append (media-normalise-configuration-package config)
                          "/bin/media-normalise")))
    (list #~(job #$(media-normalise-configuration-schedule config)
                 (lambda ()
                   (system* #$bin))))))

(define media-normalise-service-type
  (service-type
   (name 'media-normalise)
   (description "Nightly offline re-encode + Arr re-import of library video.")
   (extensions
    (list (service-extension etc-service-type
                             (lambda (c)
                               `(("media-normalise/config"
                                  ,(media-normalise-etc-file c)))))
          (service-extension mcron-service-type media-normalise-mcron-jobs)
          (service-extension shepherd-root-service-type
                             media-normalise-shepherd-services)))
   (default-value (media-normalise-configuration))))
