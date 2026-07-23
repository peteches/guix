#!/bin/sh
exec guix repl -- "$0" "$@"
!#
;;; scripts/deploy.scm --- Wraps `guix deploy' with optional host filtering.
;;;
;;; Usage: scripts/deploy.scm [--hosts PATTERNS | -h PATTERNS] [GUIX-DEPLOY-ARGS...]
;;;
;;; PATTERNS  Comma-separated list.  Each element is:
;;;   REGEX          matches the host-name field (default when no KEY= prefix)
;;;   KEY=REGEX      matches the named machine-ssh-configuration string field
;;;
;;; Supported KEYs: name  host-name  host-key  user
;;; A bare REGEX (no KEY=) matches against both 'name' and 'host-name'.
;;; REGEX is a POSIX extended regular expression (Guile ice-9 regex).
;;; A machine is included if it matches ANY pattern (OR logic).
;;;
;;; When --hosts is absent, all machines in %all-machines are deployed.
;;; All other arguments are forwarded verbatim to `guix deploy'.
;;;
;;; Examples:
;;;   scripts/deploy.scm -h git                          ; by machine name
;;;   scripts/deploy.scm --hosts 192.168.51.187          ; by IP
;;;   scripts/deploy.scm --hosts "192.168.51.18[78]"     ; regex on IP
;;;   scripts/deploy.scm -h "prometheus,loki" --dry-run  ; OR + passthrough
;;;   scripts/deploy.scm -h "host-name=192.168.51.19[01]"

(use-modules (ice-9 regex))

;;; --- Locate repo root ------------------------------------------------------
;;; Script lives in scripts/; repo root is its parent directory.
;;; Guard: skip execution when this file is loaded as a Guile module by
;;; `guix home reconfigure -L .'.  The module scanner picks up every .scm
;;; file in the load path; the top-level (exit ...) call would spawn a
;;; real `guix deploy' subprocess and crash the reconfigure.
;;; Heuristic: when run as a script, argv[0] ends with "deploy.scm".
(define-module (scripts deploy))
(when (string-suffix? "deploy.scm"
                        (or (false-if-exception
                             (canonicalize-path (car (command-line))))
                            ""))
(define repo-root
  (dirname (dirname (canonicalize-path (car (command-line))))))

(add-to-load-path repo-root)

(use-modules (gnu machine)
             (gnu machine ssh)
             (peteches machines))

;;; --- Argument parsing -------------------------------------------------------
(define (parse-args args)
  "Return two values: hosts-string-or-#f and passthrough-list."
  (let loop ((remaining args) (hosts #f) (pass '()))
    (cond
      ((null? remaining)
       (values hosts (reverse pass)))
      ((or (string=? (car remaining) "--hosts")
           (string=? (car remaining) "-h"))
       (when (null? (cdr remaining))
         (error "Missing value after" (car remaining)))
       (loop (cddr remaining) (cadr remaining) pass))
      ((string-prefix? "--hosts=" (car remaining))
       (loop (cdr remaining)
             (substring (car remaining) (string-length "--hosts="))
             pass))
      (else
       (loop (cdr remaining) hosts (cons (car remaining) pass))))))

(define-values (hosts-value passthrough)
  (parse-args (cdr (command-line))))

;;; --- Pattern parsing --------------------------------------------------------
(define (parse-pattern str)
  "Return a list of (key . regex-string) pairs.
For bare patterns (no KEY= prefix), tries both 'name' and 'host-name'."
  (let ((eq-pos (string-index str #\=)))
    (if eq-pos
        (list (cons (substring str 0 eq-pos)
                    (substring str (+ eq-pos 1))))
        (list (cons "name"      str)
              (cons "host-name" str)))))

;;; --- Machine filtering ------------------------------------------------------
;;; Guix define-record-type* field accessors are syntax-transformers, not
;;; first-class procedures. They also cannot be macro-expanded inside a (when)
;;; body (the whole body is compiled before use-modules runs, so the macros are
;;; invisible to the compiler). Use @@ to grab the internal %-procedure
;;; accessors directly — these are real procedures and work at runtime.
(define %get-machine-config
  (@@ (gnu machine) %machine-configuration-procedure))
(define %get-ssh-host-name
  (@@ (gnu machine ssh) %machine-ssh-configuration-host-name-procedure))
(define %get-ssh-host-key
  (@@ (gnu machine ssh) %machine-ssh-configuration-host-key-procedure))
(define %get-ssh-user
  (@@ (gnu machine ssh) %machine-ssh-configuration-user-procedure))

(define %field-accessors
  `(("host-name" . ,(lambda (m) (%get-ssh-host-name (%get-machine-config m))))
    ("host-key"  . ,(lambda (m) (%get-ssh-host-key  (%get-machine-config m))))
    ("user"      . ,(lambda (m) (%get-ssh-user       (%get-machine-config m))))))

(define (machine-matches-pattern? m key regex-str)
  (let ((value (if (string=? key "name")
                   (machine-name m)
                   (let* ((entry    (assoc key %field-accessors))
                          (accessor (and entry (cdr entry))))
                     (and accessor (accessor m))))))
    (and (string? value)
         (regexp-exec (make-regexp regex-str) value)
         #t)))

(define (machine-matches-any? m patterns)
  (let loop ((pats patterns))
    (cond
      ((null? pats) #f)
      ((machine-matches-pattern? m (caar pats) (cdar pats)) #t)
      (else (loop (cdr pats))))))

;;; --- Machine name lookup ----------------------------------------------------
;;; Maps each machine record back to its exported variable name string.
;;; Names (not records) are used in the -e expression because machine records
;;; contain procedures and cannot be serialised into a string.
(define %machine-names
  `((,prometheus-machine . "prometheus-machine")
    (,grafana-machine    . "grafana-machine")
    (,loki-machine       . "loki-machine")
    (,pihole-machine     . "pihole-machine")
    (,git-machine        . "git-machine")
    (,jellyfin-machine   . "jellyfin-machine")
    (,caddy-machine      . "caddy-machine")
    (,prowlarr-machine   . "prowlarr-machine")
    (,arr-machine        . "arr-machine")
    (,downloads-machine  . "downloads-machine")
    (,rustdesk-machine   . "rustdesk-machine")
    (,concourse-db-machine       . "concourse-db-machine")
    (,concourse-web01-machine    . "concourse-web01-machine")
    (,concourse-worker01-machine . "concourse-worker01-machine")
    (,vault-machine              . "vault-machine")
    (,critical-grind-outline-machine . "critical-grind-outline-machine")
    (,plane-machine                  . "plane-machine")
    (,critical-grind-campaign-machine . "critical-grind-campaign-machine")
    (,claude-workstation-machine      . "claude-workstation-machine")))

(define (machine-name m)
  (let ((entry (assq m %machine-names)))
    (or (and entry (cdr entry))
        (error "Unknown machine" m))))

;;; --- Build -e expression and exec guix deploy -------------------------------
(define deploy-expr
  (if hosts-value
      (let* ((patterns (apply append (map parse-pattern (string-split hosts-value #\,))))
             (selected (filter (lambda (m) (machine-matches-any? m patterns))
                               %all-machines))
             (names    (map machine-name selected)))
        (when (null? selected)
          (format (current-error-port)
                  "deploy.scm: no machines matched ~s~%"
                  hosts-value)
          (exit 1))
        (string-append "(begin (use-modules (peteches machines)) (list "
                       (string-join names " ")
                       "))"))
      "(begin (use-modules (peteches machines)) %all-machines)"))

(exit
 (status:exit-val
  (apply system* "guix" "deploy" "-L" repo-root "-e" deploy-expr passthrough)))
) ;; end when
