;; /etc/config.scm
(define-module (peteches systems bhiyaki))

(use-modules (gnu)
             (nongnu packages linux)
             (gnu services)
             (gnu services base)
             (gnu services desktop)
             (gnu services sound)
             (gnu packages xdisorg)
	     (gnu packages display-managers)
	     (gnu packages admin)
             (gnu packages wm)
             (gnu packages glib)         ; provides 'dbus' (dbus-run-session)
             (guix gexp)
	     (peteches systems network-mounts))

(use-service-modules base linux cups desktop networking ssh xorg)

(define gtkgreet-launcher
  (program-file "gtkgreet-launch"
		#~(execl #$(file-append cage "/bin/cage")
			 "cage" "-s" "--"
			 #$(file-append gtkgreet "/bin/gtkgreet")
			 "--command" "env XDG_SESSION_TYPE=wayland XDG_CURRENT_DESKTOP=Hyprland Hyprland")))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "bhiyaki")

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
    (simple-service 'extra-hosts
		    hosts-service-type
		    (list (host "127.0.1.1" "bhiyaki.peteches.co.uk" '("bhiyaki"))))

    (service openssh-service-type)
    (service tor-service-type)
    (service gpm-service-type)
    (service greetd-service-type
             (greetd-configuration
              (greeter-supplementary-groups '("input" "video"))
              (terminals
               (list (greetd-terminal-configuration
                      (terminal-vt "7")
                      (terminal-switch #t)
                      (default-session-command gtkgreet-launcher))))))
    (simple-service 'system-locale
                    etc-service-type
                    (list
                     `("profile.d/00-locale.sh"
                       ,(plain-file "00-locale.sh"
                                    "export GUIX_LOCPATH=/run/current-system/profile/lib/locale\n")))))
   %desktop-services))

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

