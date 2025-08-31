(define-module (peteches home-services ai)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix build utils))

;; Install the files into ~/.local/bin
(define (home-files config)
  (list
   `(".local/bin/ai-mode" ,(local-file "./scripts/ai-mode" #:recursive? #t))
   `(".local/bin/gguf-blocks.py" ,(local-file "./scripts/gguf-blocks.py" #:recursive? #t))
   `(".local/bin/kobold-launch.sh" ,(local-file "./scripts/kobold-launch.sh"#:recursive? #t))
   `(".local/bin/comfyui-launch.sh" ,(local-file "./scripts/comfyui-launch.sh" #:recursive? #t))))

;; ---------- Shepherd services wired to the executables ----------

(define (services config)
  (list
    (shepherd-service
      (provision '(comfyui))
      (documentation "ComfyUI on detected 4090 (mutex against kobold).")
      (start #~(make-forkexec-constructor
                (list (string-append (getenv "HOME") "/.local/bin/comfyui-launch.sh"))))
      (stop  #~(make-kill-destructor))
      (respawn? #t))

    ;; Unified Kobold: regex GPU selector + GGUF metadata + backoff; same port 5001.
    ;; Usage:
    ;;   herd start kobold cpu   ~/.local/share/models/foo.gguf [layers]
    ;;   herd start kobold 1060  ~/.local/share/models/bar.gguf [layers]
    ;;   herd start kobold 4090  ~/.local/share/models/baz.gguf [layers]
    (shepherd-service
      (provision '(kobold))
      (documentation "KoboldCPP with GPU regex, GGUF probe, dynamic gpu_layers, mutex vs ComfyUI on 4090.")
      (start
       #~(lambda (gpu-regex model . maybe-layers)
           (unless (and gpu-regex model)
             (begin
               (display "Usage: herd start kobold {cpu|<GPU regex>} ~/.local/share/models/model.gguf [gpu_layers]\n")
               (exit 2)))
           (let ((launch (string-append (getenv "HOME") "/.local/bin/kobold-launch.sh"))
                 (layers (if (null? maybe-layers) "" (car maybe-layers))))
             ((make-forkexec-constructor
               (if (string-null? layers)
                   (list launch gpu-regex model "5001")
                   (list launch gpu-regex model "5001" layers)))))))
      (stop  #~(make-kill-destructor))
      (respawn? #t))))

(define-public ai-service-type
  (service-type
   (name 'ai)
   (description "petes ai stuff")
   (extensions
    (list
     (service-extension
      home-files-service-type
      home-files)
     (service-extension
      home-shepherd-service-type
      services)))))

