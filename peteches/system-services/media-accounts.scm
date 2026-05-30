;; peteches/system-services/media-accounts.scm
;;
;; Shared `media' group (GID 490) used by all *Arr and download services.
;; Each service adds its own user to group "media" but the group itself is
;; declared exactly once via media-accounts-service-type to avoid duplicate
;; account records when multiple services share a VM.

(define-module (peteches system-services media-accounts)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:export (%media-gid
            media-accounts-service-type))

(define-public %media-gid 490)

(define-public media-accounts-service-type
  (service-type
   (name 'media-accounts)
   (description "Declare the shared `media' group (GID 490) for *Arr services.")
   (extensions
    (list
     (service-extension account-service-type
                        (lambda (_)
                          (list (user-group
                                 (name "media")
                                 (id %media-gid)
                                 (system? #t)))))))
   (default-value #f)))
