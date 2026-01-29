;; peteches/system-services/firewall.scm
;;
;; Modular, *authoritative* nftables firewall service for Guix System.
;;
;; - Base config provides common rule fragments.
;; - Hosts add host-specific fragments.
;; - Other services (e.g. tailscale) can extend this service-type with their own
;;   fragments.
;; - This service renders one complete nftables ruleset and applies it via a
;;   Shepherd one-shot service (does NOT rely on nftables-service-type being
;;   “mergeable”).
;;
;; NOTE: You should ensure you do NOT also enable nftables-service-type elsewhere,
;; or it may overwrite rules after we apply them.

(define-module (peteches system-services firewall)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)   ; nftables package
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (nftables-rules
            nftables-rules?
            rules-merge
            firewall-service-type))

;; A structured rules fragment.
;; Each field is a list of nft statements (strings), without surrounding table/chain
;; headers. Semicolons are optional but recommended.
(define-record-type* <nftables-rules>
  nftables-rules make-nftables-rules
  nftables-rules?
  (input           nftables-rules-input           (default '()))
  (forward         nftables-rules-forward         (default '()))
  (nat-postrouting nftables-rules-nat-postrouting (default '()))
  ;; Raw text appended verbatim at the end (e.g. extra tables, sets, maps).
  (raw             nftables-rules-raw             (default '())))

;; Merge two fragments.
(define-public (rules-merge a b)
  (nftables-rules
   (input (append (nftables-rules-input a) (nftables-rules-input b)))
   (forward (append (nftables-rules-forward a) (nftables-rules-forward b)))
   (nat-postrouting (append (nftables-rules-nat-postrouting a)
                            (nftables-rules-nat-postrouting b)))
   (raw (append (nftables-rules-raw a) (nftables-rules-raw b)))))

;; Compose a list of fragments (Guix calls (compose ...) with one argument: a list).
(define (rules-compose lst)
  (fold rules-merge (nftables-rules) lst))

(define (lines xs indent)
  (apply string-append
         (map (lambda (s) (string-append indent s "\n")) xs)))

(define (render-nftables-conf rules)
  ;; We render a complete ruleset. If you want to manage additional tables/chains,
  ;; append them via (raw ...).
  (string-append
   "flush ruleset\n\n"
   "table inet filter {\n\n"
   "  chain input {\n"
   "    type filter hook input priority filter; policy drop;\n\n"
   (lines (nftables-rules-input rules) "    ")
   "  }\n\n"
   "  chain forward {\n"
   "    type filter hook forward priority filter; policy drop;\n\n"
   (lines (nftables-rules-forward rules) "    ")
   "  }\n"
   "}\n\n"
   "table ip nat {\n"
   "  chain postrouting {\n"
   "    type nat hook postrouting priority srcnat; policy accept;\n\n"
   (lines (nftables-rules-nat-postrouting rules) "    ")
   "  }\n"
   "}\n\n"
   (lines (nftables-rules-raw rules) "")
   ))

(define (firewall-ruleset-file rules)
  (plain-file "nftables.conf" (render-nftables-conf rules)))

(define (firewall-etc-files rules)
  ;; Install /etc/nftables.conf so it’s inspectable and can be reused by humans.
  (list `("nftables.conf" ,(firewall-ruleset-file rules))))

(define (firewall-shepherd-service rules)
  (let ((ruleset (firewall-ruleset-file rules)))
    (list
     (shepherd-service
      (provision '(peteches-firewall))
      (documentation "Apply peteches modular nftables firewall ruleset.")
      ;; Run late enough that interfaces exist and routing is sane.
      (requirement '(user-processes networking))
      (auto-start? #t)
      (one-shot? #t)
      (start #~(lambda _
                 (let ((nft #$(file-append nftables "/sbin/nft")))
                   (format #t "peteches-firewall: applying ~a\n" #$ruleset)
                   (let ((rc (system* nft "-f" #$ruleset)))
                     (unless (zero? rc)
                       (error "peteches-firewall: nft failed" rc)))
                   #t)))
      (actions
       (list (shepherd-action
              (name 'reload)
              (documentation "Reload nftables ruleset.")
              (procedure
               #~(lambda _
                   (let ((nft #$(file-append nftables "/sbin/nft")))
                     (format #t "peteches-firewall: reloading ~a\n" #$ruleset)
                     (let ((rc (system* nft "-f" #$ruleset)))
                       (unless (zero? rc)
                         (error "peteches-firewall: nft reload failed" rc)))
                     #t))))))
      (stop #~(lambda _ #t))))))

(define-public firewall-service-type
  (service-type
   (name 'peteches-firewall)
   (compose rules-compose)
   (extend rules-merge)
   (default-value (nftables-rules))
   (extensions
    (list (service-extension etc-service-type
                             firewall-etc-files)
	  (service-extension profile-service-type
                   (lambda (_rules) (list nftables)))
          (service-extension shepherd-root-service-type
                             firewall-shepherd-service)))
   (description "Modular, authoritative nftables firewall: merges fragments, writes /etc/nftables.conf, and applies it via Shepherd.")))
