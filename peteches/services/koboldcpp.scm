;; peteches/services/koboldcpp.scm — KoboldCpp text-generation server (system).
;;
;; System-service port of (peteches home services koboldcpp): same model/
;; whisper/tts/draft/mmproj/sd/wavtokenizer surface and forkexec'd-binary
;; approach (koboldcpp-bin is a self-contained PyInstaller binary, no FHS
;; container needed — same reasoning as colibri.scm, unlike comfyui.scm's
;; Python wheel tree), but running under a dedicated system account rather
;; than the interactive user's Home Shepherd, so it can be declared on a
;; GPU-serving VM (e.g. comfyui.scm) without needing a logged-in session.
;;
;; auto-start? defaults to #f: the model file is a large, manually-placed,
;; out-of-band artifact (see model-path/model-name below), and a boot-time
;; crash loop over a missing model helps no one. Also deliberately off by
;; default so it doesn't contend for VRAM with whatever else is already
;; running on the box (e.g. ComfyUI) — start it explicitly with
;; `herd start <service-name>` for an ad-hoc session.
;;
;; --usecuda needs libcuda.so.1 (the driver's userspace lib, not part of
;; cuda-toolkit) on LD_LIBRARY_PATH -- confirmed live testing Qwen3.8-27B
;; on comfyui.peteches.co.uk: the prebuilt binary's own rpath only covers
;; the CUDA toolkit it was patchelf'd against, not the actual driver, and
;; fails with "libcuda.so.1: cannot open shared object file" without this.
;; Same fix colibri.scm's own module comment already documents for this
;; machine's nonguix nvidia-driver layout.
(define-module (peteches services koboldcpp)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu packages base)
  #:use-module (peteches packages koboldcpp)
  #:use-module (peteches services firewall)
  #:export (koboldcpp-configuration koboldcpp-configuration?
                                    koboldcpp-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

(define (non-empty? s)
  (and s (not (string=? s ""))))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <koboldcpp-configuration> koboldcpp-configuration
                     make-koboldcpp-configuration
  koboldcpp-configuration?
  (package koboldcpp-configuration-package
           (default koboldcpp-bin))
  (service-name koboldcpp-configuration-service-name
                (default "koboldcpp"))
  (documentation koboldcpp-configuration-documentation
                 (default "KoboldCpp text-generation server."))
  (auto-start? koboldcpp-configuration-auto-start?
               (default #f))

  (create-account? koboldcpp-configuration-create-account?
                   (default #t))
  (user koboldcpp-configuration-user
        (default "koboldcpp"))
  (group koboldcpp-configuration-group
         (default "koboldcpp"))

  ;; Same split as the home service: directory + filename, so multiple
  ;; auxiliary models (whisper/tts/draft/mmproj/sd/wavtokeniser below) can
  ;; share model-path without repeating it.
  (model-path koboldcpp-configuration-model-path)
  (model-name koboldcpp-configuration-model-name)
  (whisper-model koboldcpp-configuration-whisper-model
                 (default ""))
  (tts-model koboldcpp-configuration-tts-model
             (default ""))
  (draft-model koboldcpp-configuration-draft-model
               (default ""))
  (mmproj-model koboldcpp-configuration-mmproj-model
                (default ""))
  (wavtokeniser-model koboldcpp-configuration-wavtokeniser-model
                      (default ""))
  (sd-model koboldcpp-configuration-sd-model
            (default ""))
  (ssl-cert koboldcpp-configuration-ssl-cert
            (default ""))
  (ssl-key koboldcpp-configuration-ssl-key
           (default ""))

  (host koboldcpp-configuration-host
        (default "127.0.0.1"))
  (port koboldcpp-configuration-port
        (default 5001))

  ;; --contextsize [256 to 524288]. #f omits the flag (koboldcpp's own
  ;; default, 4096, applies) -- confirmed live up to 98304 on Qwen3.8-27B-
  ;; Q4_K_M/RTX 4090 (24GB), the practical ceiling for that checkpoint on
  ;; that card (~518MB VRAM headroom left at that point).
  (context-size koboldcpp-configuration-context-size
                (default #f))
  ;; --gpulayers. #f omits the flag (koboldcpp defaults to 0, i.e.
  ;; CPU-only). 999 (more than any real model's layer count) offloads
  ;; everything to GPU -- the value confirmed live for the above test.
  (gpu-layers koboldcpp-configuration-gpu-layers
              (default #f))
  ;; --usecuda <device>, e.g. "0". #f switches to --usecpu instead (no
  ;; GPU use at all) -- set explicitly per deployment rather than
  ;; defaulting to CUDA, since this service type may run on non-GPU
  ;; hosts too.
  (cuda-device koboldcpp-configuration-cuda-device
               (default #f))

  (extra-args koboldcpp-configuration-extra-args
              (default '()))
  (extra-environment-variables
   koboldcpp-configuration-extra-environment-variables
   (default '()))

  (state-dir koboldcpp-configuration-state-dir
             (default #f))
  (log-file koboldcpp-configuration-log-file
            (default #f))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? koboldcpp-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (koboldcpp-resolved-state-dir config)
  (or (koboldcpp-configuration-state-dir config)
      (string-append "/var/lib/koboldcpp/"
                     (koboldcpp-configuration-service-name config))))

(define (koboldcpp-resolved-log-file config)
  (or (koboldcpp-configuration-log-file config)
      (string-append "/var/log/koboldcpp/"
                     (koboldcpp-configuration-service-name config) ".log")))

;;; ── koboldcpp invocation ─────────────────────────────────────────────────

(define (koboldcpp-serve-args config)
  (let* ((model-path (koboldcpp-configuration-model-path config))
         (aux-model (lambda (name)
                      (string-append model-path "/" name))))
    (append (list "--host" (koboldcpp-configuration-host config)
                  "--port" (number->string (koboldcpp-configuration-port
                                            config))
                  "--model" (aux-model (koboldcpp-configuration-model-name
                                        config)))
            (if (koboldcpp-configuration-cuda-device config)
                (list "--usecuda" (koboldcpp-configuration-cuda-device
                                   config))
                (list "--usecpu"))
            (maybe-option "--contextsize"
                          (koboldcpp-configuration-context-size config))
            (maybe-option "--gpulayers"
                          (koboldcpp-configuration-gpu-layers config))
            (if (and (non-empty? (koboldcpp-configuration-ssl-cert config))
                     (non-empty? (koboldcpp-configuration-ssl-key config)))
                (list "--ssl"
                      (koboldcpp-configuration-ssl-cert config)
                      (koboldcpp-configuration-ssl-key config))
                '())
            (if (non-empty? (koboldcpp-configuration-sd-model config))
                (list "--sdmodel"
                      (aux-model (koboldcpp-configuration-sd-model config)))
                '())
            (if (non-empty? (koboldcpp-configuration-tts-model config))
                (list "--ttsmodel"
                      (aux-model (koboldcpp-configuration-tts-model config)))
                '())
            (if (non-empty? (koboldcpp-configuration-draft-model config))
                (list "--draftmodel"
                      (aux-model (koboldcpp-configuration-draft-model
                                  config)))
                '())
            (if (non-empty? (koboldcpp-configuration-mmproj-model config))
                (list "--mmproj"
                      (aux-model (koboldcpp-configuration-mmproj-model
                                  config)))
                '())
            (if (non-empty? (koboldcpp-configuration-wavtokeniser-model
                             config))
                (list "--ttswavtokenizer"
                      (aux-model (koboldcpp-configuration-wavtokeniser-model
                                  config)))
                '())
            (if (non-empty? (koboldcpp-configuration-whisper-model config))
                (list "--whispermodel"
                      (aux-model (koboldcpp-configuration-whisper-model
                                  config)))
                '())
            (koboldcpp-configuration-extra-args config))))

(define (koboldcpp-environment config)
  (let ((state-dir (koboldcpp-resolved-state-dir config))
        (cuda-device (koboldcpp-configuration-cuda-device config)))
    (append (list (string-append "HOME=" state-dir)
                  "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin")
            (if cuda-device
                (list "CUDA_DEVICE_ORDER=PCI_BUS_ID"
                      "LD_LIBRARY_PATH=/run/current-system/profile/lib:/run/current-system/profile/lib64")
                '())
            (koboldcpp-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (koboldcpp-accounts config)
  (if (koboldcpp-configuration-create-account? config)
      (list (user-group (name (koboldcpp-configuration-group config))
                        (system? #t))
            (user-account (name (koboldcpp-configuration-user config))
                          (group (koboldcpp-configuration-group config))
                          (system? #t)
                          (comment "KoboldCpp text-generation server daemon")
                          (home-directory (koboldcpp-resolved-state-dir
                                           config))
                          (shell (file-append shadow "/sbin/nologin"))))
      '()))

(define (koboldcpp-activation config)
  (let* ((user (koboldcpp-configuration-user config))
         (state-dir (koboldcpp-resolved-state-dir config))
         (log-file (koboldcpp-resolved-log-file config))
         (log-dir (dirname log-file)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$state-dir #$log-dir))))))

(define (koboldcpp-shepherd-service config)
  (let* ((service-name (koboldcpp-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (pkg (koboldcpp-configuration-package config))
         (bin (file-append pkg "/bin/koboldcpp"))
         (user (koboldcpp-configuration-user config))
         (group (koboldcpp-configuration-group config))
         (log-file (koboldcpp-resolved-log-file config))
         (env (koboldcpp-environment config))
         (args (koboldcpp-serve-args config)))
    (list (shepherd-service
           (provision (list provision-name))
           (documentation (koboldcpp-configuration-documentation config))
           (requirement '(networking file-systems))
           (auto-start? (koboldcpp-configuration-auto-start? config))
           (start #~(make-forkexec-constructor
                     (append (list #$bin) (list #$@args))
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file
                     #:environment-variables (list #$@env)))
           (stop #~(make-kill-destructor))))))

(define (koboldcpp-firewall-rules config)
  (if (koboldcpp-configuration-open-firewall? config)
      (nftables-rules
       (input (list (string-append "tcp dport "
                                   (number->string
                                    (koboldcpp-configuration-port config))
                                   " accept comment \""
                                   (koboldcpp-configuration-service-name
                                    config)
                                   "\""))))
      (nftables-rules (input '()))))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public koboldcpp-service-type
  (service-type (name 'koboldcpp)
                (description
                 "Run the KoboldCpp text-generation server as a system service.")
                (extensions (list (service-extension account-service-type
                                                     koboldcpp-accounts)
                                  (service-extension activation-service-type
                                                     koboldcpp-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   koboldcpp-shepherd-service)
                                  (service-extension firewall-service-type
                                                     koboldcpp-firewall-rules)))))
