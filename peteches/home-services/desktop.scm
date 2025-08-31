(define-module (peteches home-services desktop)
  #:use-module (peteches packages fonts)
  #:use-module (peteches packages scripts)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages )
  #:use-module (guix gexp)
  #:use-module (peteches home-services wofi)
  #:export (home-desktop-service-type))

(define (home-desktop-profile-service config)
  (list
   shell-scripts
   helvum
   ;; Generically useful packages
))

(define (home-desktop-shepherd-service-type config)
   (list
   ;; PipeWire daemon
   (shepherd-service
    (provision '(pipewire))
    (documentation "User PipeWire daemon.")
    (start #~(make-forkexec-constructor
	      (list #$(file-append pipewire "/bin/pipewire"))
              ;; optional log location:
              #:log-file (string-append (or (getenv "XDG_STATE_HOME")
                                            (string-append (getenv "HOME")
                                                           "/.local/state"))
                                        "/shepherd/pipewire.log")))
    (stop  #~(make-kill-destructor))
    (respawn? #t))

   ;; PulseAudio compatibility (so PA apps see a 'pulseaudio' server)
   (shepherd-service
    (provision '(pipewire-pulse))
    (requirement '(pipewire))
    (documentation "PipeWire PulseAudio compatibility server.")
    (start #~(make-forkexec-constructor
	      (list #$(file-append pipewire "/bin/pipewire-pulse"))))
    (stop  #~(make-kill-destructor))
    (respawn? #t))

   ;; WirePlumber session manager
   (shepherd-service
    (provision '(wireplumber))
    (requirement '(pipewire))   ; start after pipewire socket/daemon
    (documentation "WirePlumber session manager.")
    (start #~(make-forkexec-constructor
	      (list #$(file-append wireplumber "/bin/wireplumber"))))
    (stop  #~(make-kill-destructor))
    (respawn? #t))))


(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "My desktop environment service.")
                (extensions
                 (list
		  (service-extension
		   home-shepherd-service-type
		   home-desktop-shepherd-service-type)
		  (service-extension
                   home-profile-service-type
                   home-desktop-profile-service)))
                (default-value #f)))
