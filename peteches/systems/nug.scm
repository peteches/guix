;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

(define-module (peteches systems nug))

(use-modules (gnu)
             (nongnu packages linux)
             (nongnu packages nvidia)
             (gnu services)
             (gnu services base)
             (gnu services desktop)
             (gnu services sound)
             (gnu packages xdisorg)
	     (gnu packages admin)
             (gnu packages wm)
             (gnu packages glib)         ; provides 'dbus' (dbus-run-session)
             (guix gexp))

(use-service-modules base linux cups desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list intel-microcode linux-firmware))
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "nug.peteches.co.uk")

 (users (cons* (user-account
                (name "peteches")
                (comment "Pete McCabe")
                (group "users")
                (home-directory "/home/peteches")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 (packages (cons nvidia-firmware
                 %base-packages))

 (services
  (append
   (list

    ;; Your original extras
    (service openssh-service-type)
    (service tor-service-type)
    (service gpm-service-type))
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
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "a4d076fc-c760-433d-93de-b78535bd69ec"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "9814-19E0"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems)))
