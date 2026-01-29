;; base.scm — reusable base with grouped flags, gtkgreet greeter, and sane defaults

(define-module (peteches systems base)
  ;; Core Guix/Guile
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix modules)
  ;; Services (service constructors + specific service types)
  #:use-module (gnu services) ; <-- provides 'service', 'simple-service', etc.
  #:use-module (gnu services authentication)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services dns)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services networking) ; tor-service-type

  #:use-module (gnu services ssh)        ; openssh-service-type

  #:use-module (gnu services cups)       ; cups-service-type

  ;; Packages used in helpers

  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages display-managers) ; gtkgreet

  #:use-module (gnu packages wm)               ; cage

  #:use-module (gnu packages gl)  ; libglvnd

  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)  ; linux-firmware, intel-microcode


  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)	; nvidia-firmware
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages virtualization)


  #:use-module (nongnu packages linux)

  #:use-module (gnu system nss)

  #:use-module (peteches system-services boltd)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services tailscale)
  #:use-module (peteches packages nvidia-container-runtime)
  #:use-module (peteches packages hyprland)
  #:export (make-base-os
            %peteches-user
            %common-services
            greetd-gtkgreet-service
            without-gdm
            nonguix-substitute-service))

;; ---------- Common building blocks ----------

(define %peteches-user
  (user-account
   (name "peteches")
   (comment "Pete McCabe")
   (group "users")
   (home-directory "/home/peteches")
   (supplementary-groups '("wheel" "kvm" "netdev" "audio" "libvirt" "video"))))

(define transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

;; NOTE: Do NOT add elogind here; it already comes with %desktop-services.
(define %common-services
  (list (service openssh-service-type)
        (service tor-service-type)
	(service fprintd-service-type)
	(service virtlog-service-type)     ; virtlogd (recommended)
        (service libvirt-service-type
                 (libvirt-configuration
		  (unix-sock-ro-perms "0770")   ; restrict RO socket to group
		  (unix-sock-rw-perms "0770")   ; restrict RW socket to group
		  (auth-unix-ro "none")         ; <-- disable polkit
		  (auth-unix-rw "none")
		  (unix-sock-group "libvirt")))
	(service static-networking-service-type
		 (list (static-networking
			;; The default provision is 'networking; if you're using any
			;; other service with this provision, such as
			;; `network-manager-service-type`, then you need to change the
			;; default.
			(provision '(static-networking))
			(links
			 (list (network-link
				(name "virbr0")
				(type 'bridge)
				(arguments '()))))
			(addresses
			 (list (network-address
				(device "virbr0")
				(value "192.168.10.1/24")))))))
	(service dnsmasq-service-type
		 (dnsmasq-configuration
		  ;; You can have multiple instances of `dnsmasq-service-type` as long
		  ;; as each one has a different shepherd-provision.
		  (shepherd-provision '(dnsmasq-virbr0))
		  (extra-options (list
				  ;; Only bind to the virtual bridge. This
				  ;; avoids conflicts with other running
				  ;; dnsmasq instances.
				  "--except-interface=lo"
				  "--interface=virbr0"
				  "--bind-dynamic"
				  ;; IPv4 addresses to offer to VMs. This
				  ;; should match the chosen subnet.
				  "--dhcp-range=192.168.10.2,192.168.10.254"))))

	(service firewall-service-type
         (nftables-rules
          (input (list
                  "iifname \"lo\" accept comment \"loopback\""
                  "ct state { established, related } accept comment \"established/related\""

                  "tcp dport 22 accept comment \"ssh\""
                  "tcp dport 8188 accept comment \"comfyui\""
                  "tcp dport { 5001, 5002, 5003 } accept comment \"KoboldCpp\""
                  "tcp dport 80 accept comment \"required for certbot acme approvals\""

                  "ip protocol icmp accept comment \"icmpv4\""
                  "ip6 nexthdr ipv6-icmp accept comment \"icmpv6\""

                  ;; Interface-agnostic DHCP client replies (avoids hard-coding WAN iface).
                  "udp sport 67 udp dport 68 accept comment \"dhcpv4 client\""
                  "udp sport 547 udp dport 546 accept comment \"dhcpv6 client\""

                  ;; Libvirt bridge helpers
                  "iifname \"virbr0\" udp dport 67 accept comment \"allow dhcp on virbr0\""
                  "iifname \"virbr0\" meta l4proto { tcp, udp } th dport 53 accept comment \"allow dns on virbr0\""

                  ;; Native tailscale interface (if you keep it)
                  "iifname \"tailscale0\" accept comment \"tailscale\""))

          (forward (list
                    ;; Return traffic first
                    "ct state { established, related } counter accept comment \"forward established/related\""

                    ;; Allow VM subnet to initiate outbound connections
                    "iifname { \"virbr0\", \"vnet*\" } ip saddr 192.168.10.0/24 counter accept comment \"VMs out\""

                    ;; Log what would be dropped by policy (does not terminate)
                    "counter log prefix \"nft fwd drop: \" flags all"))

          (nat-postrouting (list
                            ;; Interface-agnostic NAT for VMs: masquerade when leaving anywhere
                            ;; except back out the VM bridge itself.
                            "ip saddr 192.168.10.0/24 oifname != { \"virbr0\", \"vnet*\" } counter masquerade comment \"masquerade VMs out\""))))


	(simple-service 'my-pam-service pam-root-service-type
			(let ((my-pam-entry
			       (pam-entry
				(control "sufficient")
				(module (file-append fprintd "/lib/security/pam_fprintd.so")))))
			  (list (pam-extension
				 (transformer
				  (lambda (pam)
				    (if (string=? "sudo" (pam-service-name pam))
					(pam-service
					 (inherit pam)
					 (auth
					  (append (pam-service-auth pam)
						  (list my-pam-entry))))
					pam)))))))
	(service boltd-service-type)
        (service gpm-service-type)))

(define %common-packages
  (map specification->package (list "hyprland" "bolt" "fprintd"  "font-terminus" "virt-manager" "qemu")))

(define (without-gdm)
  (modify-services
   %desktop-services
   (delete gdm-service-type)
   (guix-service-type
    config => (guix-configuration
	       (inherit config)
	       (substitute-urls
		(append (list "http://nug.peteches.co.uk:3000")
			%default-substitute-urls))
	       (authorized-keys
		(append (list (local-file "./nug-substitute-key.pub"))
			%default-authorized-guix-keys))))))

(define (hyprland-launcher with-nvidia?)
  (if with-nvidia?
      (program-file "hyprland-session"
        #~(begin
            (use-modules (srfi srfi-13))

            ;; agreety passes "-l" -> strip it
            (let* ((args (cdr (command-line)))
                   (args (if (and (pair? args) (string=? (car args) "-l"))
                             (cdr args)
                             args)))
              (setenv "GBM_BACKEND" "nvidia-drm")
              (setenv "__GLX_VENDOR_LIBRARY_NAME" "nvidia")
              (setenv "__EGL_VENDOR_LIBRARY_DIRS"
                      "/run/current-system/profile/share/glvnd/egl_vendor.d")
              (setenv "WLR_NO_HARDWARE_CURSORS" "1")

              (let* ((old (or (getenv "LD_LIBRARY_PATH") ""))
                     (prefix (string-append
                              #$(file-append libglvnd "/lib") ":"
                              #$(file-append nvidia-driver "/lib") ":"
                              #$(file-append nvidia-driver "/lib64")))
                     (new (if (string-null? old)
                              prefix
                              (string-append prefix ":" old))))
                (setenv "LD_LIBRARY_PATH" new))

              (let ((prog #$(file-append hyprland "/bin/Hyprland")))
                (apply execl prog (cons "Hyprland" args))))))

      (program-file "hyprland-session"
        #~(begin
            ;; agreety passes "-l" -> strip it
            (let* ((args (cdr (command-line)))
                   (args (if (and (pair? args) (string=? (car args) "-l"))
                             (cdr args)
                             args)))
              (setenv "__EGL_VENDOR_LIBRARY_DIRS"
                      "/run/current-system/profile/share/glvnd/egl_vendor.d")
              (setenv "WLR_NO_HARDWARE_CURSORS" "1")
              (let ((prog #$(file-append hyprland "/bin/Hyprland")))
                (apply execl prog (cons "Hyprland" args))))))))


;; ------ Greeter (new interface) ------
;; Preferred for up-to-date Guix: configure the greeter at top level.
;; Runs gtkgreet inside cage; *no* dbus-run-session — user sessions
;; are expected to have DBus via dbus-service-type.
(define* (greetd-gtkgreet-services #:key
                                   (session-command #~#$(hyprland-launcher #f)))
  (list
   (service console-font-service-type
            `(("tty7" . ,(file-append font-terminus "/share/consolefonts/ter-132n"))))
   (service greetd-service-type
            (greetd-configuration
             (terminals
              (list
               (greetd-terminal-configuration
                (terminal-vt "7")
                (terminal-switch #t)
                (default-session-command
                  (greetd-agreety-session
                   (command
                    (greetd-user-session
                     (xdg-session-type "wayland")
                     (extra-env '(("XDG_CURRENT_DESKTOP" . "Hyprland")
                                  ("XDG_SESSION_TYPE"    . "wayland")))
                     (command session-command))))))))))))

(define (nonguix-substitute-service)
  (simple-service 'add-nonguix-substitutes
                  guix-service-type
                  (guix-extension
                   (substitute-urls
                    (append (list "https://substitutes.nonguix.org")
                            %default-substitute-urls))
                   (authorized-keys
                    (append (list (plain-file "non-guix.pub"
					      "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                            %default-authorized-guix-keys)))))

(define peteches-tailscale-services
  (list (service tailscale-service-type
            (list (tailscale-instance-configuration
              (name "peteches")
              (port 41641))))))


;; make sure this is available somewhere in your config:
;; (use-modules (gnu packages base))  ; for `glibc`


;; ---------- Main API with grouped flags ----------

(define* (make-base-os
          #:key host-name bootloader file-systems
          (mapped-devices '())
          (kernel linux)
          (firmware '())
          (users-extra '())
          (extra-services '())
          (extra-packages '())
          ;; flags
          (laptop? #f)
          (intel-cpu? #t)
	  (with-docker? #f)
          (with-printing? #f)
          (with-bluetooth? #f)
          (with-nonguix? #f)
          (with-nvidia? #f))
  (let* ((firmware*
          (append firmware
                  (if intel-cpu? (list intel-microcode) '())))
         (packages*
          (append extra-packages %common-packages
                  (if with-nvidia? (list nvidia-firmware nvidia-driver) '())
		  (if (and with-nvidia? with-docker?) (list runc nvidia-container-toolkit) '())))
         (laptop-services
          (append (if laptop? (list (service tlp-service-type)) '())
                  (if (and laptop? intel-cpu?)
                      (list (service thermald-service-type))
                      '())))
	 (nvidia-services (if with-nvidia?
			      (list
			       (simple-service 'nvidia-wayland-env
					       session-environment-service-type
					       `(("GBM_BACKEND"               . "nvidia-drm")
						 ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
						 ("__EGL_VENDOR_LIBRARY_DIRS" . "/run/current-system/profile/share/glvnd/egl_vendor.d")
						 ("WLR_NO_HARDWARE_CURSORS"   . "1")
						 ;; Critical bit: make GLVND win over Mesa at runtime.
						 ("LD_LIBRARY_PATH" .
						  ,#~(string-append
						      #$(file-append libglvnd "/lib") ":"
						      #$(file-append nvidia-driver "/lib") ":"
						      #$(file-append nvidia-driver "/lib64")
						      ":${LD_LIBRARY_PATH}"))))


			       (service nvidia-service-type)
			       (simple-service 'nvidia-runtime-state
					       activation-service-type
					       #~(begin
						   (use-modules (guix build utils))
						   (mkdir-p "/run/nvidia")))

			       (simple-service 'custom-udev-rules udev-service-type
					       (list nvidia-driver))
			       (service kernel-module-loader-service-type
					'("ipmi_devintf"
					  "nvidia"
					  "nvidia_modeset"
					  "nvidia_uvm")))
			      '()))
	 (docker-services (if with-docker? (list (service containerd-service-type)
						 (service docker-service-type))
			      '()))
         (printing-services (if with-printing? (list (service cups-service-type)) '()))
         (bluetooth-services (if with-bluetooth? (list (service bluetooth-service-type)) '()))
         (nonguix-services (if with-nonguix? (list (nonguix-substitute-service)) '()))
         (desktop* (without-gdm))
	 (hyprland-session-command
          #~#$(hyprland-launcher with-nvidia?))
	 (final-services
	  (append desktop*
		  %common-services
		  (greetd-gtkgreet-services #:session-command hyprland-session-command)
		  laptop-services
		  printing-services
		  bluetooth-services
		  nonguix-services
		  nvidia-services
		  docker-services
		  peteches-tailscale-services
		  extra-services)))
    (operating-system
     (kernel kernel)
     (kernel-arguments (append (if with-nvidia?
				   (list "modprobe.blacklist=nouveau" "nvidia_drm.modeset=1")
				   '())
			       (if intel-cpu?
				   (list "intel_iommu=on" "iommu=pt")
				   '())
			       %default-kernel-arguments))
     (kernel-loadable-modules (if with-nvidia?
				  (list nvidia-module)
				  '()))
     (firmware firmware*)
     (locale "en_GB.utf8")
     (timezone "Europe/London")
     (keyboard-layout (keyboard-layout "us"))
     (host-name host-name)
     (name-service-switch %mdns-host-lookup-nss)
     (users (append (list (if with-docker?
			      (user-account
			       (inherit %peteches-user)
			       (supplementary-groups
				(append (user-account-supplementary-groups %peteches-user)
					'("docker"))))
			      %peteches-user))
		    %base-user-accounts
		    users-extra))
     (packages (append packages* %base-packages))
     (services final-services)
     (mapped-devices mapped-devices)
     (file-systems (append file-systems %base-file-systems))
     (bootloader bootloader))))
