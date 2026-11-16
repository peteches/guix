;;; peteches/services/media-normalise.scm — nightly library normalisation.
;;;
;;; Runs peteches/packages/media-normalise.sh as a nightly mcron job and as a
;;; manual one-shot shepherd service (`herd start media-normalise`).
;;;
;;; The script re-writes non-conforming library video to the house format
;;; (HEVC/H.264 STREAM-COPIED with its colour metadata intact — HDR is kept, not
;;; tone-mapped; every WebOS-decodable audio track copied and the rest dropped, or
;;; one AC-3 mix when nothing decodable exists; no embedded subtitles at all,
;;; text ones exported as sidecar .srt files beside the video; MP4 +faststart) and
;;; re-imports it by asking the owning Arr app to rescan that folder, removing the
;;; replaced original only once the rewrite is confirmed registered.
;;;
;;; WHY: measured on this fleet's LG WebOS client (2026-11-15).  The panel decodes
;;; HEVC Main 10 PQ natively — a stream-copied MP4 of the same HDR footage
;;; direct-played start to finish with no ffmpeg running at all, while the MKV it
;;; came from made Jellyfin remux to fragmented-MP4 HLS and the player tore the
;;; connection down every ~23 seconds.  The client also ignores MP4-embedded
;;; mov_text subtitles entirely but renders external .srt sidecars happily, and it
;;; cannot decode TrueHD/DTS or bitmap PGS subs — which is what forced on-the-fly
;;; transcoding before.  Normalising to direct-playable files removes server-side
;;; transcoding entirely; tone-mapping and bitrate caps turned out to be solving a
;;; problem this client never had.
;;;
;;; WHY WHOLE-LIBRARY RATHER THAN JUST THOSE FILES: consistency is the goal, not a
;;; targeted patch, so "conforming" is what direct play requires — MP4 with moov up
;;; front, exactly one video track in a codec MP4 can carry by copy, only audio
;;; tracks the client decodes, and nothing embedded where subtitles live.  Colour
;;; is deliberately NOT part of the definition: a PQ file is already correct.  The
;;; first pass therefore rewrites most of the library (MKV containers, DivX-era
;;; files, bonus video tracks, TrueHD/DTS audio, embedded subs), but as stream
;;; copies it is I/O-bound rather than encode-bound; after it, only genuinely new
;;; or changed items are touched.  Anything looser never converges.
;;;
;;; ONE-SHOT SHEPHERD SERVICE AND THE SHARE: the constructor drops no user, so the
;;; job runs as root — which is what writing to /media needs.  The CIFS mount maps
;;; everything to uid=0/gid=0 (file_mode=0664,dir_mode=0775), so only root can
;;; stage files or delete a replaced original there; the jellyfin user itself
;;; cannot write to the share at all.
;;;
;;; ROUTING: films -> Radarr, tv -> Sonarr, standup-comedy -> STANDUP-APP.
;;; The staging root mirrors this split (/media/import/<split>/...) so a rescan of
;;; one app's folder can never see the other app's media.
;;;
;;; SECRETS: the Arr API keys are read at run time from the files named by
;;; RADARR-API-KEY-FILE / SONARR-API-KEY-FILE (SOPS-decrypted into /run/secrets;
;;; see secrets/hosts/jellyfin/arr-api-keys.yaml and the matching sops-secret
;;; entries in peteches/systems/jellyfin.scm).
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
  ;; Radarr 6.x and Sonarr 4.x serve their API only under /api/v3; a bare host:port
  ;; answers 404, which makes every call fail with no readable reason because the body
  ;; that explains it is discarded.  media-normalise.sh normalises the base too, but
  ;; these defaults are what lands in /etc.
  (radarr-url media-normalise-configuration-radarr-url
              (default "http://192.168.51.195:7878/api/v3"))
  (sonarr-url media-normalise-configuration-sonarr-url
              (default "http://192.168.51.195:8989/api/v3"))
  (radarr-api-key-file media-normalise-configuration-radarr-api-key-file
                       (default "/run/secrets/radarr-api-key"))
  (sonarr-api-key-file media-normalise-configuration-sonarr-api-key-file
                       (default "/run/secrets/sonarr-api-key"))
  ;; Truthful quality tag applied at import time (Arr has no custom qualities).
  ;; NOTE: now that rewrites are stream copies rather than encodes, a copy of a
  ;; Blu-ray remux is honestly a Remux-2160p, not an encode.  Changing this means
  ;; the Arr quality profile named by PROFILE-NAME has to be re-cut around the new
  ;; ceiling first, so it stays as-is until that UI work is done deliberately.
  (quality-name media-normalise-configuration-quality-name
                (default "Bluray-2160p"))
  ;; Profile pinned onto normalised items so they are never upgraded back.
  (profile-name media-normalise-configuration-profile-name
                (default "Normalised-2160p"))
  ;; Which app owns standup-comedy (specials are usually movies).
  (standup-app media-normalise-configuration-standup-app
               (default "radarr"))
  ;; Which subtitle languages become sidecar .srt files, as three-letter tags in a
  ;; space- or comma-separated list.  "" means EVERY text subtitle track, which is
  ;; faithful to the intent but costly here: this library's Blu-ray rips carry up to
  ;; 38 text tracks and each one becomes a file beside the video.  It is left empty
  ;; because neither app has any subtitle language configured (/config/mediamanagement
  ;; answers with an empty list on both), so nothing in the fleet says which tags are
  ;; wanted — name them ("eng spa" is what was verified playable on this client) and
  ;; the folders stay readable.
  (sub-langs media-normalise-configuration-sub-langs
             (default ""))
  ;; "parsed" (the default) lets the app keep whatever quality it reads off the file
  ;; name of the rewrite — under a rescan that is the whole story, and it is honest
  ;; because a stream copy cannot change resolution.  "override" patches QUALITY-NAME
  ;; onto the registered file afterwards, which only makes sense for a profile cut
  ;; deliberately around that one ceiling: stamping Bluray-2160p on a copied 1080p file
  ;; makes the app believe it owns better video than it does.
  (quality-mode media-normalise-configuration-quality-mode
                (default "parsed"))
  ;; Rate control, used only by the rare fallback that re-encodes video because a
  ;; source codec cannot be stream-copied into MP4 at all (DivX/Xvid/MPEG-1/…).
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
     ;; One root per line: main() in media-normalise.sh reads LIBRARY_ROOTS with
     ;; `while read', so a space-joined list became one long non-existent path and
     ;; the nightly job silently processed nothing.  The separator must be a real
     ;; newline ("\n") — gexp splices strings verbatim into the file, so "\\n"
     ;; would land as a literal backslash-n and still read as a single root.
     "LIBRARY_ROOTS=\"" (string-join (media-normalise-configuration-library-roots config) "\n") "\"\n"
     "STAGING_ROOT=\"" (media-normalise-configuration-staging-root config) "\"\n"
     "RADARR_URL=\"" (media-normalise-configuration-radarr-url config) "\"\n"
     "SONARR_URL=\"" (media-normalise-configuration-sonarr-url config) "\"\n"
     "RADARR_API_KEY_FILE=\"" (media-normalise-configuration-radarr-api-key-file config) "\"\n"
     "SONARR_API_KEY_FILE=\"" (media-normalise-configuration-sonarr-api-key-file config) "\"\n"
     "QUALITY_NAME=\"" (media-normalise-configuration-quality-name config) "\"\n"
     "PROFILE_NAME=\"" (media-normalise-configuration-profile-name config) "\"\n"
     "STANDUP_APP=\"" (media-normalise-configuration-standup-app config) "\"\n"
     ;; Comma-separated, and empty means "every text subtitle track", not "none".
     ;; Both of these need the closing \" like every line above them.  Without it the
     ;; quote stays open, bash swallows every later line into one string, and LOG,
     ;; QUALITY_MODE, MAXRATE and CQ come out unset — the deployed config was unreadable.
     "SUB_LANGS=\"" (media-normalise-configuration-sub-langs config) "\"\n"
     "QUALITY_MODE=\"" (media-normalise-configuration-quality-mode config) "\"\n"
     "MAXRATE=" (number->string (media-normalise-configuration-maxrate config)) "\n"
     "CQ=" (number->string (media-normalise-configuration-cq config)) "\n"
     "LOG=\"" (media-normalise-configuration-log-file config) "\"\n")))

(define (media-normalise-shepherd-services config)
  (let ((bin (file-append (media-normalise-configuration-package config)
                          "/bin/media-normalise")))
    (list (shepherd-service
           (provision '(media-normalise))
           (documentation "Offline media library normalisation (one-shot).")
           ;; sops-secrets first: the run needs /run/secrets/*-api-key to exist, and
           ;; without this a manual `herd start media-normalise' can beat decryption.
           (requirement '(sops-secrets file-systems networking))
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
   (description "Nightly offline stream-copy normalisation + Arr re-import of library video.")
   (extensions
    (list (service-extension etc-service-type
                             (lambda (c)
                               `(("media-normalise/config"
                                  ,(media-normalise-etc-file c)))))
          (service-extension mcron-service-type media-normalise-mcron-jobs)
          (service-extension shepherd-root-service-type
                             media-normalise-shepherd-services)))
   (default-value (media-normalise-configuration))))
