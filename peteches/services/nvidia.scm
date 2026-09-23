;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Pete McCabe

;;; Repo-local NVIDIA VM service type.
;;;
;;; A copy of nonguix's nvidia-service-type (nongnu/services/nvidia.scm,
;;; GPL-3) with the firmware-service-type extension replaced by an
;;; activation-service-type extension.
;;;
;;; Why: `guix system container` builds a containerized OS whose
;;; container-essential-services strips firmware-service-type, so a
;;; service-extension targeting it raises &missing-target-service-error
;;; ("no target of type 'firmware' for service 'nvidia').  An activation
;;; does the same work (activate-firmware on the nvidia firmware package)
;;; without requiring the firmware service to be in the graph.
;;;
;;; The helper functions below reimplement the unexported nonguix
;;; counterparts.  One adaptation: nonguix's module exports neither the
;;; <nvidia-configuration> record type nor its field accessors (only the
;;; constructor and predicates), and Guile's match-record only works on
;;; record types defined in the same module — so fields are read through
;;; nvidia-config-ref, which looks them up by name in the record type's
;;; field list.  The <nvidia-configuration> record has no delayed or
;;; thunked fields, so plain struct-ref yields the real values.

(define-module (peteches services nvidia)
  #:use-module (srfi srfi-11)
  #:use-module (guix gexp)
  #:use-module (gnu system privilege)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:export (nvidia-vm-service-type))

;; Read a field by name from a <nvidia-configuration> record.  See the
;; module header for why the usual accessors are not available here.
(define (nvidia-config-ref config field-name)
  (let* ((type (struct-vtable config))
         (names (record-type-fields type)))
    (struct-ref config (list-index names field-name))))

(define (nvidia-config-driver config)
  (nvidia-config-ref config 'driver))

(define (nvidia-config-module config)
  (nvidia-config-ref config 'module))

(define (nvidia-firmware-activation config)
  (let ((firmware (nvidia-config-ref config 'firmware)))
    (if (not firmware)
        #~#t
        (let ((directory (directory-union "nvidia-firmware" (list firmware))))
          #~(activate-firmware
             (string-append #$directory "/lib/firmware"))))))

(define (nvidia-profile config)
  (let ((driver (nvidia-config-ref config 'driver))
        (settings (nvidia-config-ref config 'settings)))
    (filter identity
            (list driver
                  settings
                  nvidia-prime))))

(define (nvidia-privileged-program config)
  (let ((modprobe (nvidia-config-ref config 'modprobe)))
    (list (file-like->setuid-program
           (file-append modprobe "/bin/nvidia-modprobe")))))

;; Create paths hard-coded in NVIDIA libraries.
(define (nvidia-special-files config)
  `(("/usr/bin/nvidia-modprobe" "/run/privileged/bin/nvidia-modprobe")
    ("/usr/share/nvidia" "/run/booted-system/profile/share/nvidia")))

;; https://github.com/Frogging-Family/nvidia-all/blob/master/system/60-nvidia.rules
(define (nvidia-udev-rule config)
  (list (udev-rule "90-nvidia.rules" "\
# Device nodes are created by nvidia-modprobe, which is called by the nvidia DDX.
# In case the DDX is not started, the device nodes are never created, so call
# nvidia-modprobe in the udev rules to cover the Wayland/EGLStream and compute
# case without a started display. In the case where vfio-pci is used
# nvidia-modprobe should not be invoked.
ACTION==\"add|bind\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", \\
    DRIVER==\"nvidia\", TEST!=\"/dev/nvidia-uvm\", \\
    RUN+=\"/usr/bin/nvidia-modprobe\", \\
    RUN+=\"/usr/bin/nvidia-modprobe -c0 -u\"

# Enable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA Audio devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"auto\"

# Disable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA Audio devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"on\"
")))

(define (nvidia-shepherd-service config)
  (let ((driver (nvidia-config-ref config 'driver))
        (powerd (nvidia-config-ref config 'powerd)))
    (if powerd
        (list (shepherd-service
                (documentation "NVIDIA Dynamic Boost support.")
                (provision '(nvidia-powerd))
                (requirement '(user-processes))
                (respawn? #f)
                (start
                 #~(make-forkexec-constructor
                    (list #$(file-append driver "/bin/nvidia-powerd"))))
                (stop #~(make-kill-destructor))))
        '())))

(define nvidia-vm-service-type
  (service-type
   (name 'nvidia-vm)
   (extensions
    (list (service-extension profile-service-type
                             nvidia-profile)
          (service-extension privileged-program-service-type
                             nvidia-privileged-program)
          (service-extension special-files-service-type
                             nvidia-special-files)
          (service-extension shepherd-root-service-type
                             nvidia-shepherd-service)
          (service-extension dbus-root-service-type
                             (compose list nvidia-config-driver))
          (service-extension udev-service-type
                             nvidia-udev-rule)
          (service-extension activation-service-type
                             nvidia-firmware-activation)
          (service-extension linux-loadable-module-service-type
                             (compose list nvidia-config-module))))
   (default-value (nvidia-configuration))
   (description "Prepare system environment for NVIDIA driver.")))