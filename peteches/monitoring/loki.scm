(define-module (peteches monitoring loki)
  #:use-module (guix gexp)
  #:use-module (gnu packages guile-xyz))


(define* (loki-event-gexp message
                          #:key
                          url
                          (job "guix")
                          (labels '())
                          tenant
                          authorization)
  #~(begin
      (use-modules (srfi srfi-19)
                   (ice-9 format)
                   (web client)
                   (web response)
                   (json))

      (catch #t
        (lambda ()
          (let* ((host (gethostname))
                 (t (current-time time-utc))
                 (timestamp-ns
                  (number->string
                   (+ (* (time-second t) 1000000000)
                      (time-nanosecond t))))
                 (stream-labels
                  `(("job" . #$job)
                    ("host" . ,host)
                    ,@'#$labels))
                 (payload
                  (scm->json-string
                   `(("streams"
                      . #((("stream" . ,stream-labels)
                           ("values" . #(#(,timestamp-ns #$message)))))))))
                 (headers
                  (append
                   '((content-type . (application/json)))
                   (if #$tenant
                       (list (cons 'x-scope-orgid #$tenant))
                       '())
                   (if #$authorization
                       (list (cons 'authorization #$authorization))
                       '())))
                 (response body
                  (http-post
                   #$url
                   #:headers headers
                   #:body payload)))

            (unless (and (response? response)
                         (let ((code (response-code response)))
                           (and (>= code 200) (< code 300))))
              (format
               (current-error-port)
               "warning: Loki returned non-2xx response: ~a~%"
               (if (response? response)
                   (response-code response)
                   response)))))
        (lambda args
          (format
           (current-error-port)
           "warning: failed to send Loki event: ~a~%"
           args)))))
