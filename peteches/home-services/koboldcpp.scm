(define-module (peteches home-services koboldcpp)
  #:use-module (peteches packages koboldcpp)
    ;; Services
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  ;; guix
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (koboldcpp
            koboldcpp-cuda
            koboldcpp-configuration
            koboldcpp-service-type))
;; -----------------------------------------------------------------------------
;; Service (add a 'package' field so you can switch CPU/CUDA)
;; -----------------------------------------------------------------------------

(define-record-type* <koboldcpp-configuration>
  koboldcpp-configuration make-koboldcpp-configuration
  koboldcpp-configuration?
  (package      koboldcpp-configuration-package (default koboldcpp)) ; choose koboldcpp-cuda here
  (model-path   koboldcpp-configuration-model-path (default "/var/lib/koboldcpp/model.gguf"))
  (host         koboldcpp-configuration-host       (default "127.0.0.1"))
  (port         koboldcpp-configuration-port       (default 5001))
  (extra-args   koboldcpp-configuration-extra-args (default '()))
  (user         koboldcpp-configuration-user       (default "koboldcpp"))
  (work-dir     koboldcpp-configuration-work-dir   (default "/var/lib/koboldcpp")))

(define (koboldcpp-shepherd-service cfg)
  (let* ((pkg        (koboldcpp-configuration-package cfg))
         (bin        (file-append pkg "/bin/koboldcpp"))
         (model      (koboldcpp-configuration-model-path cfg))
         (host       (koboldcpp-configuration-host cfg))
         (port       (koboldcpp-configuration-port cfg))
         (work       (koboldcpp-configuration-work-dir cfg))
         (extra-args (koboldcpp-configuration-extra-args cfg))
         (user       (koboldcpp-configuration-user cfg)))
    (list
     (shepherd-service
      (documentation "KoboldCpp text-generation server.")
      (provision '(koboldcpp))
      (requirement '())
      (start #~(make-forkexec-constructor
                (list #$bin
                      "--host" #$host
                      "--port" (number->string #$port)
                      "--model" #$model
                      #$@extra-args)
                #:user #$user
                #:group #$user
                #:directory #$work
                #:environment-variables
                (list "LANG=C.UTF-8")))
      (stop #~(make-kill-destructor))
      (auto-start? #t)))))

(define koboldcpp-service-type
  (service-type
   (name 'koboldcpp)
   (extensions
    (list (service-extension shepherd-root-service-type
                             koboldcpp-shepherd-service)
          (service-extension account-service-type
                             (lambda (cfg)
                               (let ((u (koboldcpp-configuration-user cfg)))
                                 (list (user-account
                                        (name u) (group u) (system? #t)
                                        (home-directory (koboldcpp-configuration-work-dir cfg)))
                                       (user-group (name u))))))))
   (default-value (koboldcpp-configuration))
   (description "Run KoboldCpp as a simple system Shepherd service.")))
