(define-module (peteches home-services mpv)
  #:use-module (ice-9 match)
  #:use-module (gnu packages video)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services configuration)
  #:export (mpv-config
            mpv-profile
            mpv-profile?
            home-mpv-service-type))

;;; Commentary:
;;;
;;; Guix Home service for mpv.
;;;
;;; This service installs mpv into the Home profile and writes:
;;;
;;;   ~/.config/mpv/mpv.conf
;;;
;;; It can also generate composable profile overlay files:
;;;
;;;   ~/.config/mpv/profiles/upscaler.conf
;;;   ~/.config/mpv/profiles/youtube.conf
;;;   ~/.config/mpv/profiles/low-power.conf
;;;
;;; The base `mpv-config` record has real defaults. It represents the normal
;;; mpv configuration.
;;;
;;; The `mpv-profile` record is different: it is a partial overlay. Every field
;;; defaults to an internal "unset" value, and unset fields are omitted from the
;;; generated profile file. This means profiles can inherit the base mpv.conf
;;; cleanly without accidentally resetting options back to generic defaults.
;;;
;;; Example:
;;;
;;;   (service home-mpv-service-type
;;;            (mpv-config
;;;             (hwdec "nvdec")
;;;             (demuxer-max-bytes "4096MiB")
;;;             (demuxer-max-back-bytes "1024MiB")
;;;             (profiles
;;;              (list
;;;               (mpv-profile
;;;                (name "upscaler")
;;;                (scale "ewa_lanczossharp")
;;;                (cscale "ewa_lanczossharp")
;;;                (dscale "mitchell")
;;;                (deband? #t))
;;;
;;;               (mpv-profile
;;;                (name "youtube")
;;;                (ytdl-format "bestvideo[height<=1440]+bestaudio/best")
;;;                (script-opts "ytdl_hook-ytdl_path=yt-dlp"))
;;;
;;;               (mpv-profile
;;;                (name "low-power")
;;;                (hwdec "auto-safe")
;;;                (demuxer-max-bytes "512MiB"))))))
;;;
;;; Launch with:
;;;
;;;   mpv --include="$HOME/.config/mpv/profiles/upscaler.conf" video.mkv
;;;   mpv --include="$HOME/.config/mpv/profiles/youtube.conf" URL
;;;
;;; Layering model:
;;;
;;;   base mpv.conf
;;;     + included profile file
;;;     + command-line arguments
;;;
;;; Later layers win when an option appears more than once.


;;; Generic mpv serialization helpers.

(define %unset
  ;; Unique sentinel used by `mpv-profile`.
  ;;
  ;; Do not use this directly in user configs. It exists so profile fields can
  ;; be omitted entirely unless explicitly set.
  (list 'unset))

(define (unset? value)
  "Return true when VALUE is the internal unset sentinel."
  (eq? value %unset))

(define (mpv-option-name key)
  "Return KEY as an mpv option name.

KEY may be either a symbol or a string.

Examples:

  'demuxer-max-bytes -> \"demuxer-max-bytes\"
  \"script-opts\"    -> \"script-opts\""
  (cond
   ((symbol? key)
    (symbol->string key))
   ((string? key)
    key)
   (else
    (error "unsupported mpv option name" key))))

(define (mpv-quoted-string value)
  "Return VALUE quoted for mpv.conf.

mpv accepts quoted strings in configuration files. This service quotes all
Scheme strings because some valid mpv values begin with characters that have
special meaning to mpv.

The most important example is screenshot templates:

  %F-%wH.%wM.%wS

If this starts a config value unquoted, mpv may interpret it as fixed-length
quoting syntax. Quoting makes the intention explicit:

  \"%F-%wH.%wM.%wS\""
  (call-with-output-string
    (lambda (port)
      (display "\"" port)
      (string-for-each
       (lambda (char)
         (case char
           ((#\\ #\")
            (display "\\" port)
            (write-char char port))
           (else
            (write-char char port))))
       value)
      (display "\"" port))))

(define (mpv-plain-value->string value)
  "Serialize simple VALUE types for the right-hand side of mpv.conf.

Supported simple values:

  string   -> quoted string
  boolean  -> yes/no
  number   -> decimal number
  symbol   -> bare mpv value

Examples:

  \"4096MiB\"          -> \"4096MiB\"
  #t                  -> yes
  #f                  -> no
  7200                -> 7200
  'display-resample   -> display-resample"
  (cond
   ((string? value)
    (mpv-quoted-string value))
   ((boolean? value)
    (if value "yes" "no"))
   ((number? value)
    (number->string value))
   ((symbol? value)
    (symbol->string value))
   (else
    (error "unsupported mpv configuration value" value))))

(define (serialize-option option-name value)
  "Serialize one mpv option named OPTION-NAME with VALUE.

VALUE may be a string, boolean, number, symbol, or file-like object.

File-like values are useful for reproducible shader paths, for example:

  (glsl-shaders (local-file \"../files/mpv/shaders/foo.glsl\"))

They are emitted as quoted store paths."
  (if (file-like? value)
      #~(string-append #$option-name
                       "=\""
                       #$value
                       "\"\n")
      #~(string-append #$option-name
                       "="
                       #$(mpv-plain-value->string value)
                       "\n")))

(define (serialize-string field-name value)
  (serialize-option (symbol->string field-name) value))

(define (serialize-symbol field-name value)
  (serialize-option (symbol->string field-name) value))

(define (serialize-boolean field-name value)
  (serialize-option (symbol->string field-name) value))

(define (serialize-integer field-name value)
  (serialize-option (symbol->string field-name) value))

(define (serialize-number field-name value)
  (serialize-option (symbol->string field-name) value))

(define (serialize-option-alist options)
  "Serialize OPTIONS as mpv option=value lines.

OPTIONS is an alist:

  '((scale . \"ewa_lanczossharp\")
    (deband . #t)
    (video-sync . display-resample))

Both symbols and strings are accepted as option names."
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                (serialize-option (mpv-option-name key) value)))
             options)))

(define (serialize-extra-options field-name value)
  "Serialize the base config `extra-options` field."
  (serialize-option-alist value))

(define (serialize-profiles field-name value)
  "Do not serialize profiles into the base mpv.conf.

Profiles are written as separate files under ~/.config/mpv/profiles instead."
  #~"")


;;; Custom serializers for Scheme field names ending in ?.
;;;
;;; Guix field names such as `cache-pause?` are nice Scheme, but mpv expects
;;; option names such as `cache-pause`. These serializers remove the question
;;; mark when writing mpv.conf.

(define (serialize-save-position-on-quit? field-name value)
  (serialize-option "save-position-on-quit" value))

(define (serialize-keep-open? field-name value)
  (serialize-option "keep-open" value))

(define (serialize-osc? field-name value)
  (serialize-option "osc" value))

(define (serialize-border? field-name value)
  (serialize-option "border" value))

(define (serialize-cache-pause? field-name value)
  (serialize-option "cache-pause" value))

(define (serialize-cache-on-disk? field-name value)
  (serialize-option "cache-on-disk" value))


;;; Typed partial profile overlays.

(define-record-type* <mpv-profile>
  mpv-profile make-mpv-profile
  mpv-profile?
  (name mpv-profile-name)

  ;; Renderer / playback.
  (profile mpv-profile-profile
           (default %unset))
  (vo mpv-profile-vo
      (default %unset))
  (gpu-api mpv-profile-gpu-api
           (default %unset))
  (gpu-context mpv-profile-gpu-context
               (default %unset))
  (hwdec mpv-profile-hwdec
         (default %unset))
  (hwdec-codecs mpv-profile-hwdec-codecs
                (default %unset))

  ;; Window / UI.
  (save-position-on-quit? mpv-profile-save-position-on-quit?
                          (default %unset))
  (keep-open? mpv-profile-keep-open?
              (default %unset))
  (osc? mpv-profile-osc?
        (default %unset))
  (border? mpv-profile-border?
           (default %unset))
  (volume mpv-profile-volume
          (default %unset))

  ;; Screenshots.
  (screenshot-format mpv-profile-screenshot-format
                     (default %unset))
  (screenshot-dir mpv-profile-screenshot-dir
                  (default %unset))
  (screenshot-template mpv-profile-screenshot-template
                       (default %unset))

  ;; Buffering / cache.
  (cache mpv-profile-cache
         (default %unset))
  (cache-secs mpv-profile-cache-secs
              (default %unset))
  (cache-pause? mpv-profile-cache-pause?
                (default %unset))
  (cache-pause-wait mpv-profile-cache-pause-wait
                    (default %unset))
  (cache-on-disk? mpv-profile-cache-on-disk?
                  (default %unset))
  (demuxer-max-bytes mpv-profile-demuxer-max-bytes
                     (default %unset))
  (demuxer-max-back-bytes mpv-profile-demuxer-max-back-bytes
                          (default %unset))
  (demuxer-readahead-secs mpv-profile-demuxer-readahead-secs
                          (default %unset))
  (demuxer-hysteresis-secs mpv-profile-demuxer-hysteresis-secs
                           (default %unset))

  ;; Quality / scaling / interpolation.
  (scale mpv-profile-scale
         (default %unset))
  (cscale mpv-profile-cscale
          (default %unset))
  (dscale mpv-profile-dscale
          (default %unset))
  (deband? mpv-profile-deband?
           (default %unset))
  (interpolation? mpv-profile-interpolation?
                  (default %unset))
  (video-sync mpv-profile-video-sync
              (default %unset))
  (tscale mpv-profile-tscale
          (default %unset))
  (glsl-shaders mpv-profile-glsl-shaders
                (default %unset))

  ;; Network / yt-dlp.
  (ytdl-format mpv-profile-ytdl-format
               (default %unset))
  (script-opts mpv-profile-script-opts
               (default %unset))

  ;; Escape hatch.
  (extra-options mpv-profile-extra-options
                 (default '())))

(define (serialize-profile-option option-name value)
  "Serialize profile option OPTION-NAME only when VALUE is set."
  (if (unset? value)
      #~""
      (serialize-option option-name value)))

(define (serialize-mpv-profile profile)
  "Serialize PROFILE as a partial mpv configuration overlay.

Only fields explicitly set in the `mpv-profile` record are emitted. This is
what allows profile files to compose with the base mpv.conf without copying
every default."
  #~(string-append
     ;; Renderer / playback.
     #$(serialize-profile-option "profile"
                                 (mpv-profile-profile profile))
     #$(serialize-profile-option "vo"
                                 (mpv-profile-vo profile))
     #$(serialize-profile-option "gpu-api"
                                 (mpv-profile-gpu-api profile))
     #$(serialize-profile-option "gpu-context"
                                 (mpv-profile-gpu-context profile))
     #$(serialize-profile-option "hwdec"
                                 (mpv-profile-hwdec profile))
     #$(serialize-profile-option "hwdec-codecs"
                                 (mpv-profile-hwdec-codecs profile))

     ;; Window / UI.
     #$(serialize-profile-option "save-position-on-quit"
                                 (mpv-profile-save-position-on-quit? profile))
     #$(serialize-profile-option "keep-open"
                                 (mpv-profile-keep-open? profile))
     #$(serialize-profile-option "osc"
                                 (mpv-profile-osc? profile))
     #$(serialize-profile-option "border"
                                 (mpv-profile-border? profile))
     #$(serialize-profile-option "volume"
                                 (mpv-profile-volume profile))

     ;; Screenshots.
     #$(serialize-profile-option "screenshot-format"
                                 (mpv-profile-screenshot-format profile))
     #$(serialize-profile-option "screenshot-dir"
                                 (mpv-profile-screenshot-dir profile))
     #$(serialize-profile-option "screenshot-template"
                                 (mpv-profile-screenshot-template profile))

     ;; Buffering / cache.
     #$(serialize-profile-option "cache"
                                 (mpv-profile-cache profile))
     #$(serialize-profile-option "cache-secs"
                                 (mpv-profile-cache-secs profile))
     #$(serialize-profile-option "cache-pause"
                                 (mpv-profile-cache-pause? profile))
     #$(serialize-profile-option "cache-pause-wait"
                                 (mpv-profile-cache-pause-wait profile))
     #$(serialize-profile-option "cache-on-disk"
                                 (mpv-profile-cache-on-disk? profile))
     #$(serialize-profile-option "demuxer-max-bytes"
                                 (mpv-profile-demuxer-max-bytes profile))
     #$(serialize-profile-option "demuxer-max-back-bytes"
                                 (mpv-profile-demuxer-max-back-bytes profile))
     #$(serialize-profile-option "demuxer-readahead-secs"
                                 (mpv-profile-demuxer-readahead-secs profile))
     #$(serialize-profile-option "demuxer-hysteresis-secs"
                                 (mpv-profile-demuxer-hysteresis-secs profile))

     ;; Quality / scaling / interpolation.
     #$(serialize-profile-option "scale"
                                 (mpv-profile-scale profile))
     #$(serialize-profile-option "cscale"
                                 (mpv-profile-cscale profile))
     #$(serialize-profile-option "dscale"
                                 (mpv-profile-dscale profile))
     #$(serialize-profile-option "deband"
                                 (mpv-profile-deband? profile))
     #$(serialize-profile-option "interpolation"
                                 (mpv-profile-interpolation? profile))
     #$(serialize-profile-option "video-sync"
                                 (mpv-profile-video-sync profile))
     #$(serialize-profile-option "tscale"
                                 (mpv-profile-tscale profile))
     #$(serialize-profile-option "glsl-shaders"
                                 (mpv-profile-glsl-shaders profile))

     ;; Network / yt-dlp.
     #$(serialize-profile-option "ytdl-format"
                                 (mpv-profile-ytdl-format profile))
     #$(serialize-profile-option "script-opts"
                                 (mpv-profile-script-opts profile))

     ;; Escape hatch, always last so it can override earlier profile fields.
     #$(serialize-option-alist
        (mpv-profile-extra-options profile))))


;;; Base mpv configuration.

(define-configuration mpv-config
  ;; Core playback / rendering options.

  (profile
   (string "gpu-hq")
   "Built-in mpv profile to enable.

A profile is a named group of mpv settings. The gpu-hq profile enables higher
quality GPU rendering defaults than mpv's most conservative configuration.

Impact:

  - Better scaling and rendering quality.
  - Slightly higher GPU usage.
  - Usually sensible for modern hardware.
  - On very weak machines, use a lighter profile or override with a generated
    profile overlay.")

  (vo
   (string "gpu-next")
   "Video output driver.

This controls the renderer mpv uses to display video.

Common values:

  gpu-next
    Modern renderer based on libplacebo. This is the best default for current
    mpv setups, especially with Vulkan, HDR, and advanced scaling.

  gpu
    Older GPU renderer. Useful as a fallback if gpu-next causes trouble.

Impact:

  - Affects image quality, GPU usage, scaling quality, HDR behaviour, and
    driver compatibility.
  - gpu-next is the right first choice on most modern Guix desktops.")

  (gpu-api
   (string "vulkan")
   "GPU API used by mpv's GPU renderer.

Common values:

  vulkan
    Modern explicit graphics API. Usually best with gpu-next and current AMD,
    Intel, and NVIDIA drivers.

  opengl
    Older and widely compatible. Useful if Vulkan causes problems.

Impact:

  - Vulkan often gives better modern rendering behaviour.
  - OpenGL may be more reliable on older or unusual driver stacks.")

  (gpu-context
   (string "waylandvk")
   "Window-system GPU context.

This tells mpv how to connect its renderer to the display server.

Common values:

  waylandvk
    Wayland plus Vulkan. Good when using gpu-api=vulkan in Wayland.

  wayland
    Wayland plus OpenGL. Good when using gpu-api=opengl in Wayland.

  x11vk
    X11 plus Vulkan.

  x11egl
    X11 plus OpenGL/EGL.

Impact:

  - Should broadly match your session and gpu-api.
  - If mpv opens but fails to render, this is one of the first knobs to check.")

  (hwdec
   (string "auto-safe")
   "Hardware video decoding method.

Hardware decoding asks the GPU or media engine to decode video instead of
doing all decoding on the CPU.

Common values:

  auto-safe
    mpv chooses a safe hardware decoder automatically. Good portable default.

  vaapi
    Common choice for AMD and Intel GPUs on Linux.

  nvdec
    NVIDIA hardware decoding.

  no
    Disable hardware decoding and use CPU decoding.

Impact:

  - Can greatly reduce CPU usage, fan noise, and power consumption.
  - Can help with high-resolution video.
  - Can sometimes expose driver-specific bugs.
  - If playback glitches, try auto-safe or no.

Suggested for your machines:

  nug / azathoth:
    nvdec

  bhiyaki / nyarlothotep:
    vaapi or auto-safe")

  (hwdec-codecs
   (string "all")
   "Codecs eligible for hardware decoding.

The value all lets mpv use hardware decoding for any codec supported by the
chosen hardware decoder.

Impact:

  - all maximises hardware offload.
  - Restricting this can help if one codec behaves badly with your driver.")

  ;; Window / UI behaviour.

  (save-position-on-quit?
   (boolean #t)
   "Remember playback position when quitting.

When enabled, mpv saves your position for files and URLs so reopening them can
resume where you stopped.

Impact:

  - Very useful for long videos, films, lectures, and streams.
  - Creates small watch-later state files.
  - Disable if you want every file to always start from the beginning."
   (serializer serialize-save-position-on-quit?))

  (keep-open?
   (boolean #f)
   "Keep the mpv window open after playback ends.

When disabled, mpv exits when the current file finishes. When enabled, the
window remains open on the final frame.

Impact:

  - Useful for inspecting the final frame or taking screenshots.
  - Less convenient for casual playback and playlists.
  - If enabled, you may need to quit mpv manually more often."
   (serializer serialize-keep-open?))

  (osc?
   (boolean #t)
   "Enable mpv's on-screen controller.

The OSC is the overlay UI with a seek bar and playback controls that appears
when moving the mouse.

Impact:

  - Easier for mouse-driven use.
  - Slightly less visually minimal.
  - Keyboard-focused users may prefer disabling it."
   (serializer serialize-osc?))

  (border?
   (boolean #t)
   "Enable the window manager border around the mpv window.

Impact:

  - Enabled gives a normal decorated window, depending on compositor/window
    manager behaviour.
  - Disabled gives a cleaner borderless video window.
  - On tiling Wayland setups, your compositor may still apply its own rules."
   (serializer serialize-border?))

  (volume
   (integer 100)
   "Default playback volume.

100 means normal volume. Values above 100 may amplify audio beyond its
original level.

Impact:

  - 100 is safest.
  - Lower values are useful if mpv is usually too loud.
  - Values above 100 can cause clipping or distortion.")

  ;; Screenshot options.

  (screenshot-format
   (string "png")
   "Image format used for screenshots.

Common values:

  png
    Lossless. Larger files. Best for exact frames and text clarity.

  jpg
    Lossy. Smaller files. Good for casual screenshots.

  webp
    Smaller modern format if supported by your mpv build.

Impact:

  - png gives highest fidelity.
  - jpg saves disk space.
  - This affects screenshots only, not video playback.")

  (screenshot-dir
   (string "~/Pictures/mpv")
   "Directory where mpv saves screenshots.

Impact:

  - Keeps screenshots grouped away from the current video directory.
  - If the directory does not exist, screenshot saving may fail until it is
    created.
  - For fully explicit behaviour, use an absolute path.")

  (screenshot-template
   (string "%F-%wH.%wM.%wS.%wT-#%#00n")
   "Filename template for screenshots.

The default includes:

  %F
    Source filename or media title.

  %wH, %wM, %wS, %wT
    Playback time components.

  %#00n
    Screenshot sequence number.

Impact:

  - Makes screenshots descriptive and sortable.
  - This service quotes strings so templates beginning with % work correctly.")

  ;; Buffering / cache options.

  (cache
   (symbol 'yes)
   "Enable mpv's packet cache.

The cache stores already-read media packets so mpv can survive network stalls,
slow disks, or aggressive seeking more gracefully.

Common values:

  yes
    Enable cache.

  no
    Disable cache.

  auto
    Let mpv decide.

Impact:

  - yes improves resilience for streams, remote files, and slow storage.
  - It can increase memory usage.
  - With the large defaults here, memory usage can be substantial during long
    or high-bitrate playback.")

  (cache-secs
   (integer 3600)
   "Target amount of media time mpv may cache, in seconds.

The default is 3600 seconds, i.e. one hour.

Important:

  This does not mean mpv will always use one hour of RAM. Actual cache size is
  also constrained by byte limits such as demuxer-max-bytes.

Impact:

  - Higher values allow a longer buffer.
  - Useful for unreliable networks and remote media.
  - High values only matter when byte limits are also large enough.")

  (cache-pause?
   (boolean #t)
   "Pause playback automatically if the cache underruns.

When enabled, mpv may pause and buffer instead of stuttering through playback
when data cannot be read quickly enough.

Impact:

  - Better experience for unstable streams.
  - Can cause visible pauses while mpv refills the buffer.
  - Disable if you prefer mpv to keep trying to play no matter what."
   (serializer serialize-cache-pause?))

  (cache-pause-wait
   (number 5)
   "Seconds of cached media required before playback resumes after buffering.

If cache-pause is enabled and playback pauses because the cache ran dry, this
sets how much data mpv should collect before continuing.

Impact:

  - Higher values reduce repeated pause/refill/pause loops.
  - Lower values resume playback faster.
  - 5 seconds is a balanced default.")

  (cache-on-disk?
   (boolean #f)
   "Store cache data on disk instead of RAM.

When disabled, cache data is kept in memory. When enabled, mpv can use a
temporary file for the cache.

Impact:

  - Disabled is faster and better for machines with plenty of RAM.
  - Enabled can reduce RAM pressure for very large buffers.
  - Enabled may increase disk writes.
  - For your higher-RAM machines, disabled is sensible."
   (serializer serialize-cache-on-disk?))

  (demuxer-max-bytes
   (string "1024MiB")
   "Maximum forward demuxer cache size.

This is the main large-buffer memory limit. It controls how much data mpv may
read ahead from the current playback position.

Examples:

  \"512MiB\"
    Moderate buffer.

  \"1024MiB\"
    Large buffer.

  \"4096MiB\"
    Very large buffer, suitable for high-RAM machines such as nug.

Impact:

  - Higher values improve resilience for network playback and slow storage.
  - Higher values can use much more memory.
  - This setting is often more practically important than cache-secs.")

  (demuxer-max-back-bytes
   (string "512MiB")
   "Maximum backward demuxer cache size.

This controls how much already-played data mpv may keep available behind the
current playback position.

Impact:

  - Higher values make short backward seeks more likely to be instant.
  - Useful when scrubbing, reviewing scenes, or replaying sections.
  - Increases memory usage.
  - If you rarely seek backwards, this can be much smaller than
    demuxer-max-bytes.")

  (demuxer-readahead-secs
   (integer 600)
   "How many seconds mpv's demuxer may read ahead.

This is another time-based read-ahead control. It works alongside the byte
limits.

Impact:

  - Higher values encourage more aggressive prefetching.
  - Useful for remote files and unstable networks.
  - Byte limits still cap how much data can actually be cached.")

  (demuxer-hysteresis-secs
   (integer 10)
   "Cache refill hysteresis, in seconds.

This controls how far the cache should drop below its target before mpv starts
trying to refill more aggressively again.

Impact:

  - Prevents mpv from constantly topping up the cache in tiny increments.
  - Small values keep the cache fuller.
  - Larger values may reduce background read activity but allow the cache to
    drain further before refilling.")

  ;; Profile overlays.

  (profiles
   (list '())
   "List of typed mpv-profile records to generate as composable overlays.

Each profile becomes:

  ~/.config/mpv/profiles/<name>.conf

Profiles are intended to be included at launch time:

  mpv --include=\"$HOME/.config/mpv/profiles/upscaler.conf\" video.mkv

The base mpv.conf is loaded first, then the included profile file is layered
on top. This means profiles only need to specify the options they want to add
or override.

Example:

  (profiles
   (list
    (mpv-profile
     (name \"upscaler\")
     (scale \"ewa_lanczossharp\")
     (cscale \"ewa_lanczossharp\")
     (deband? #t))

    (mpv-profile
     (name \"low-power\")
     (hwdec \"auto-safe\")
     (demuxer-max-bytes \"512MiB\"))))

Impact:

  - Avoids copy-pasting default buffer and decoder settings into every profile.
  - Keeps profiles typed while preserving overlay semantics.
  - Prevents accidental default values from clobbering host-specific base
    settings."
   (serializer serialize-profiles))

  ;; Escape hatch.

  (extra-options
   (alist '())
   "Additional arbitrary mpv options as an alist.

Use this for mpv settings that are not represented as first-class fields in
this service.

Example:

  (extra-options
   '((audio-file-auto . fuzzy)
     (sub-auto . fuzzy)
     (force-window . immediate)))

Value handling:

  \"string\"
    Written as a quoted mpv string.

  #t / #f
    Written as yes/no.

  number
    Written directly.

  'symbol
    Written as a bare mpv value.

  file-like object
    Written as a quoted store path.

Impact:

  - Lets you use any mpv option without editing this service.
  - Good for experiments.
  - Once an option becomes permanent, consider adding it as a typed field with
    documentation.

extra-options is serialized last in mpv.conf, so it can override earlier
first-class fields if needed."
   (serializer serialize-extra-options)))


;;; Service implementation.

(define (profile-name->file-name name)
  "Return the XDG config path for profile NAME."
  (string-append "mpv/profiles/" name ".conf"))

(define (home-mpv-profile-files config)
  "Generate profile overlay files for CONFIG."
  (map (lambda (profile)
         `(,(profile-name->file-name (mpv-profile-name profile))
           ,(mixed-text-file
             (string-append "mpv-profile-"
                            (mpv-profile-name profile)
                            ".conf")
             (serialize-mpv-profile profile))))
       (mpv-config-profiles config)))

(define (home-mpv-profile-service config)
  "Install mpv into the Guix Home profile."
  (list mpv))

(define (home-mpv-xdg-files config)
  "Generate ~/.config/mpv/mpv.conf and any profile overlay files."
  (append
   (list
    `("mpv/mpv.conf"
      ,(mixed-text-file "mpv.conf"
                        (serialize-configuration config mpv-config-fields))))
   (home-mpv-profile-files config)))

(define-public home-mpv-service-type
  (service-type
   (name 'mpv-config)
   (description
    "Install mpv and generate ~/.config/mpv/mpv.conf plus optional profile overlays.")
   (default-value (mpv-config))
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-mpv-xdg-files)
     (service-extension
      home-profile-service-type
      home-mpv-profile-service)))))
