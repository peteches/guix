;;; intel-gpu-firmware.scm — Intel GPU firmware subset of linux-firmware.
;;;
;;; Why this package exists rather than using an existing one:
;;;
;;;  * The monolithic `linux-firmware' is 766 MiB, which is a lot to add to a
;;;    VM's store closure for three blobs.
;;;
;;;  * nonguix's `i915-firmware' is the obvious candidate but is NOT
;;;    sufficient for a discrete Arc card.  It selects `^i915/' only.  The `xe'
;;;    driver that drives Battlemage-and-later GPUs reads its GuC/HuC blobs
;;;    from the *separate* `xe/' directory, so `i915-firmware' yields
;;;    i915/bmg_dmc.bin but omits xe/bmg_guc_70.bin — and GuC is the one whose
;;;    absence is fatal.  The probe fails with:
;;;
;;;      xe 0000:02:00.0: [drm] Tile0: GT0: GuC firmware ... not found
;;;      xe 0000:02:00.0: [drm] *ERROR* Tile0: GT0: Failed to initialize uC (-ENOENT)
;;;      xe 0000:02:00.0: probe with driver xe failed with error -2
;;;
;;;    Verified against linux-firmware-20260810's WHENCE: the bmg (Battlemage)
;;;    blobs the `xe' driver requests are exactly xe/bmg_guc_70.bin,
;;;    xe/bmg_huc.bin and i915/bmg_dmc.bin.  Hence both directories.
;;;
;;;  * nonguix has no `xe-firmware'.  So the subset is defined here, mirroring
;;;    the pattern nonguix itself uses for amdgpu-firmware/i915-firmware/
;;;    iwlwifi-firmware.
;;;
;;; Selecting `^(i915|xe)/' gives ~13 MiB (9.6 MiB i915 + 3.3 MiB xe) instead
;;; of 766 MiB — a 98% reduction, and still covering every Intel GPU the fleet
;;; is likely to meet rather than hardcoding only the three bmg files.
;;;
;;; IMPORTANT — this package is useless on a deblobbed kernel.  linux-libre's
;;; deblob strips the firmware *filenames* out of in-tree drivers, so its
;;; xe.ko declares zero MODULE_FIRMWARE entries and calls request_firmware()
;;; with an empty name (which is why the failure surfaces as -ENOENT with
;;; `/*(DEBLOBBED)*/' in place of the filename in dmesg).  Measured on the same
;;; 7.1.8 source both ways:
;;;
;;;      linux-libre-7.1.8 xe.ko ->  0 firmware: entries
;;;      linux-7.1.8       xe.ko -> 41 firmware: entries (incl. xe/bmg_guc_70.bin)
;;;
;;; NVIDIA is unaffected by this because nvidia.ko is an out-of-tree
;;; proprietary module and keeps its own firmware strings.  That asymmetry is
;;; exactly why a 4090 works on linux-libre while an Arc card cannot.
;;;
;;; This is why make-vm-os's #:with-intel-gpu? refuses a *-libre kernel rather
;;; than silently installing firmware nothing can request.

(define-module (peteches packages intel-gpu-firmware)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix licenses)
  #:use-module (nonguix licenses)
  #:use-module (nongnu packages linux)
  #:export (intel-gpu-firmware))

;; Local reimplementation of nonguix's `select-firmware' — that helper is
;; private to (nongnu packages linux) and not exported, so it cannot be
;; reused.  Kept byte-for-byte equivalent in behaviour: it rewrites WHENCE so
;; that copy-firmware.sh skips every entry whose path does not match KEEP.
(define (select-firmware keep)
  "Modify linux-firmware copy list to retain only files matching KEEP regex."
  #~(lambda _
      (use-modules (ice-9 regex))
      (substitute* "WHENCE"
        (("^(File|RawFile): *([^ ]*)(.*)" _ type file rest)
         (string-append (if (string-match #$keep file) type "Skip") ": " file rest))
        (("^Link: *(.*) *-> *(.*)" _ file target)
         (string-append (if (string-match #$keep target) "Link" "Skip")
                        ": " file " -> " target)))))

;; Doc directory for the install-intel-licences phase below.  Bound outside
;; the `package' form because a gexp inside `arguments' has no lexical access
;; to the package's own name/version fields -- `#$name' there would be an
;; unbound variable, not the field.
(define %intel-gpu-firmware-doc-dir
  (string-append "share/doc/intel-gpu-firmware-"
                 (package-version linux-firmware)))

(define-public intel-gpu-firmware
  (package
    (inherit linux-firmware)
    (name "intel-gpu-firmware")
    (arguments
     (substitute-keyword-arguments (package-arguments linux-firmware)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; The standard phase would install every LICEN* file in the
            ;; tarball's LICENSES/ tree (a megabyte of amdgpu/nvidia/etc.
            ;; docs).  We only want the two Intel GPU licence texts, which
            ;; the install-intel-licences phase below installs by hand.
            (delete 'install-license-files)
            (add-after 'unpack 'select-firmware
              #$(select-firmware "^(i915|xe)/"))
            ;; Install the two wanted licence texts by hand (the standard
            ;; phase is deleted above because it would install every LICEN*
            ;; file in the tarball).  Both are wanted because the selection
            ;; spans both directories -- WHENCE puts i915/ under LICENSE.i915
            ;; and xe/ under LICENSE.xe.  They live in the tarball's LICENSES/
            ;; subdirectory, so they are copied from there.
            (add-after 'install 'install-intel-licences
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((doc (string-append
                            (assoc-ref outputs "out")
                            "/" #$%intel-gpu-firmware-doc-dir)))
                  (mkdir-p doc)
                  (for-each
                    (lambda (f)
                      (install-file (string-append "LICENSES/" f) doc))
                    '("LICENSE.i915" "LICENSE.xe")))))))))
    (home-page "https://docs.kernel.org/gpu/intel/xe.html")
    (synopsis "Nonfree firmware for Intel i915 and xe GPUs")
    (description
     "Nonfree firmware for Intel GPUs, covering both the long-standing
@code{i915} driver and the newer @code{xe} driver used by discrete Arc cards
from Xe-HP onward.  This is @code{linux-firmware} restricted to the
@file{i915/} and @file{xe/} directories.

Both directories are required: GuC and HuC live under @file{xe/} while DMC
remains under @file{i915/}.  nonguix's @code{i915-firmware} selects only the
latter and therefore omits the GuC blob whose absence makes @code{xe} fail to
probe.

Note that these blobs can only ever be loaded by a kernel whose in-tree Intel
drivers still contain their firmware filenames — i.e. not linux-libre.")
    (license
     (nonfree
      (string-append
       "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
       "/linux-firmware.git/plain/LICENSE.xe")))))
