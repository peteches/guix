;; peteches/systems/monitored-hosts.scm — central registry of Prometheus-monitored hosts.
;; Add one entry here when a new machine is provisioned; the Prometheus scrape
;; config is regenerated automatically on the next `guix deploy`.

(define-module (peteches systems monitored-hosts)
  #:export (%monitored-hosts))

(define-public %monitored-hosts
  ;; alist of (instance-label . "address:9100")
  ;; Replace TODO entries with actual IPs or resolvable hostnames once known.
  '(
    ;; ("azathoth"     . "TODO:9100")
    ;; ("bhiyaki"      . "TODO:9100")
    ("nug"          . "nug.peteches.co.uk:9100")
    ;; ("nyarlothotep" . "TODO:9100")
    ("pihole"       . "192.168.51.189:9100")
    ("prometheus"   . "192.168.51.187:9100")
    ("grafana"      . "192.168.51.188:9100")
    ("loki"         . "192.168.51.190:9100")
    ("git"          . "192.168.51.191:9100")))
