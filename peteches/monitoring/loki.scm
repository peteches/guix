;;; peteches/monitoring/loki.scm — gexp helper for pushing events to Loki.
;;;
;;; STATUS: dead code.  `loki-event-gexp' is neither exported (the module has
;;; no #:export and the definition is `define*', not `define-public') nor
;;; called anywhere, so it is unreachable from outside this file.  Add it to
;;; an #:export list before trying to use it.  CLAUDE.md describes it as an
;;; available helper — that is aspirational.
;;;
;;; Routine log shipping does NOT go through this: every VM runs Grafana
;;; Alloy (see the alloy-service-type entries in each peteches/systems/*.scm)
;;; which tails files and pushes to the loki VM.  This helper was for emitting
;;; one-off events from inside a G-expression — e.g. an activation snippet
;;; announcing a deploy — which Alloy cannot do.
;;;
;;; Returns a gexp for splicing into a service; the code inside runs on the
;;; *target* host at activation/run time, hence the `use-modules' inside the
;;; #~(begin …) rather than at the top of this file, and the guile-json
;;; dependency has to be present in that host's profile.
;;;
;;; Failures are caught and downgraded to a warning on stderr, by design:
;;; an unreachable Loki must never abort the activation that is reporting to
;;; it.  Note the corollary — dropped events are silent.

(define-module (peteches monitoring loki)
  #:use-module (guix gexp)
  #:use-module (gnu packages guile-xyz))


;; MESSAGE, and every keyword argument, are spliced with #$ — so they must be
;; literals or gexp-serialisable values, not runtime expressions.
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
