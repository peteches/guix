(define-module (peteches home-configs nyarlothotep)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (peteches channels base)
  #:use-module (peteches packages lycheeslicer)
  #:use-module (peteches home-services hyprland)
  #:use-module (peteches home-services mpv)
  #:use-module (peteches home-configs mpv)

  #:use-module (peteches home-configs base))

(home-environment
 (packages (append (list lycheeslicer-7.6.2) base-packages))
 (services
  (modify-services
   (append
    (list
     (service home-mpv-service-type
              (mpv-config
               ;; AMD iGPU on Wayland/Hyprland.
               (hwdec "vaapi")
               (hwdec-codecs "all")
               (vo "gpu-next")
               (gpu-api "vulkan")
               (gpu-context "waylandvk")

               ;; Good quality without treating a 16 GiB laptop like nug.
               (profile "gpu-hq")

               ;; Large enough to smooth network/local hiccups, not so large that
               ;; the laptop becomes a RAM goblin.
               (cache 'yes)
               (cache-secs 3600)
               (cache-pause? #t)
               (cache-pause-wait 5)
               (cache-on-disk? #f)
               (demuxer-max-bytes "1536MiB")
               (demuxer-max-back-bytes "384MiB")
               (demuxer-readahead-secs 600)
               (demuxer-hysteresis-secs 10)

               ;; Sensible desktop defaults.
               (save-position-on-quit? #t)
               (keep-open? #f)
               (osc? #t)
               (border? #t)
               (volume 100)

               ;; Screenshot defaults.
               (screenshot-format "png")
               (screenshot-dir "~/Pictures/mpv")
               (screenshot-template "%F-%wH.%wM.%wS.%wT-#%#00n")

               (profiles %mpv-profiles)))

     (service home-channels-service-type
	      %base-channels))
    base-services)
  (home-hyprland-service-type config =>
			      (home-hyprland-configuration
			       (inherit config)
			       (monitors (list
					  (monitor
					   (name "eDP-1")
					   (scale 1)))))))))
