(define-module (peteches home-configs mpv)
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
