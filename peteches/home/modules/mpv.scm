;;; peteches/home/modules/mpv.scm — mpv profiles shared by both desktops.
;;;
;;; Only the *profiles* are shared.  The base mpv config is per-host,
;;; because the hardware differs sharply: nug uses NVIDIA nvdec with multi-GiB
;;; demuxer caches, nyarlothotep uses AMD VAAPI/Vulkan with modest ones.
;;; See the home-mpv-service-type entries in peteches/home/configs/.
;;;
;;; Profiles are opt-in at playback time (mpv --profile=upscaler …), except
;;; where mpv auto-applies them by name.

(define-module (peteches home modules mpv)
  #:use-module (peteches home services mpv))

(define-public %mpv-profiles
  (list
   (mpv-profile
    (name "upscaler")
    (scale "ewa_lanczossharp")
    (cscale "ewa_lanczossharp")
    (dscale "mitchell")
    (deband? #t)
    (interpolation? #t)
    (video-sync 'display-resample)
    (tscale "oversample"))

   (mpv-profile
    (name "youtube")
    (ytdl-format "bestvideo[height<=1440]+bestaudio/best")
    (cache-secs 7200))

   (mpv-profile
    (name "low-power")
    (hwdec "auto-safe")
    (demuxer-max-bytes "512MiB")
    (demuxer-max-back-bytes "128MiB"))))
