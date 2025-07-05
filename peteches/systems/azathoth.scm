;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

(define-module (peteches systems azathoth))

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu) (nongnu packages linux)
	     (gnu packages admin))

(use-service-modules networking ssh desktop xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "us" "altgr-intl"))
 (host-name "azathoth")

 (packages (append (map specification->package
			'("wpa-supplicant" "hyprland"))
		   %base-packages))
 
 (services (modify-services %desktop-services
			    (wpa-supplicant-service-type config =>
							 (wpa-supplicant-configuration
							  (inherit config)
							  (extra-options '("-f" "/var/log/wpa-supplicant.log"))))
			    (gdm-service-type config =>
					      (gdm-configuration
					       (inherit config)
					       (wayland? #t)))))
 
 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "peteches")
                (comment "Pete McCabe")
                  (group "users")
                  (home-directory "/home/peteches")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 

 ;; ;; Below is the list of system services.  To search for available
 ;; ;; services, run 'guix system search KEYWORD' in a terminal.
 ;; (services
 ;;  (append (list
	   
 ;;           ;; To configure OpenSSH, pass an 'openssh-configuration'
 ;;           ;; record as a second argument to 'service' below.
 ;;           (service openssh-service-type)
 ;;           (service tor-service-type)
      	  
 ;;          ;; This is the default list of services we
 ;;          ;; are appending to.
 ;;           %base-services)))
  
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))

 (mapped-devices (list (mapped-device
                        (source (uuid
                                 "52191142-c0b5-4090-92b4-2697c17fd875"))
                        (target "root")
                        (type luks-device-mapping))))
 
 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "C4D7-28C3"
                                     'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (uuid "d7de5028-5ad1-497f-bb4e-ec3a2220a548"
				     'ext4))
                       (type "ext4")
                       (dependencies mapped-devices)) %base-file-systems)))
