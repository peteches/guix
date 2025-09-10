;; /etc/config.scm
(define-module (peteches systems bhiyaki))

(use-modules (gnu)
             (nongnu packages linux)
             (gnu services)
             (gnu services base)
             (gnu services desktop)
             (gnu services sound)
             (gnu packages xdisorg)
	     (gnu packages admin)
             (gnu packages wm)
             (gnu packages glib)         ; provides 'dbus' (dbus-run-session)
             (guix gexp)
	     (peteches systems network-mounts))

(use-service-modules base linux cups desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "bhiyaki.peteches.co.uk")

 (users (cons* (user-account
                (name "peteches")
                (comment "Pete McCabe")
                (group "users")
                (home-directory "/home/peteches")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 ;; make locales available system-wide
 (packages
  (append
   (list glibc-locales)         ; <- important
   %base-packages))
 
 (services
  (append
   (list
    ;; Your original extras
    (service openssh-service-type)
    (service tor-service-type)
    (service gpm-service-type)
    ;; add near your other services; requires (guix gexp) and (use-service-modules base)
    (simple-service 'system-locale
                    etc-service-type
                    (list
                     `("profile.d/00-locale.sh"
                       ,(plain-file "00-locale.sh"
                                    "export GUIX_LOCPATH=/run/current-system/profile/lib/locale\n")))))
   (modify-services %desktop-services
		    (delete gdm-service-type))
   (list
    ;; requires: (use-modules (guix gexp))

    (service greetd-service-type
	     (greetd-configuration
	      (greeter-supplementary-groups '("video" "input"))
	      (terminals
	       (list
		(greetd-terminal-configuration
		 (terminal-vt "7")
		 (terminal-switch #t)
		 (default-session-command
 		   (greetd-agreety-session
                    (command (file-append dbus "/bin/dbus-run-session"))
		    (command-args (list "Hyprland"))))))))))))

 (bootloader (bootloader-configuration
	      (bootloader grub-efi-bootloader)
	      (targets (list "/boot/efi"))
	      (keyboard-layout keyboard-layout)))

 (mapped-devices
  (list (mapped-device
	 (source (uuid "d516a745-caf3-4094-a08b-ec60f3096c0a"))
	 (target "cryptroot")
	 (type luks-device-mapping))))

 (file-systems
  (cons*
   scoreplay-cifs-mount
   (file-system
    (mount-point "/boot/efi")
    (device (uuid "B6CE-8818" 'fat32))
    (type "vfat"))
   (file-system
    (mount-point "/")
    (device "/dev/mapper/cryptroot")
    (type "ext4")
    (dependencies mapped-devices))
   %base-file-systems)))
