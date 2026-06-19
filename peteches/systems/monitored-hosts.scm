;; peteches/systems/monitored-hosts.scm — central registry of Prometheus-monitored hosts.
;; Add one entry here when a new machine is provisioned; the Prometheus scrape
;; config is regenerated automatically on the next `guix deploy'.

(define-module (peteches systems monitored-hosts)
  #:export (%monitored-hosts))

(define-public %monitored-hosts
  ;; alist of (instance-label . "address:9100")
  ;; Replace TODO entries with actual IPs or resolvable hostnames once known.
  '(
    ("nug"          . "nug.peteches.co.uk:9100")
    ;; ("nyarlothotep" . "TODO:9100")
    ("pihole"       . "192.168.51.189:9100")
    ("prometheus"   . "192.168.51.187:9100")
    ("grafana"      . "192.168.51.188:9100")
    ("loki"         . "192.168.51.190:9100")
    ("git"          . "192.168.51.191:9100")
    ("jellyfin"     . "192.168.51.192:9100")
    ("caddy"        . "192.168.51.193:9100")
    ("prowlarr"     . "192.168.51.194:9100")
    ("arr"          . "192.168.51.195:9100")
    ("downloads"    . "192.168.51.196:9100")
    ("rustdesk"         . "192.168.51.197:9100")
    ("concourse-db"     . "192.168.51.198:9100")
    ("concourse-web01"  . "192.168.51.199:9100")
    ("concourse-worker01" . "192.168.51.200:9100")
    ("vault"        . "192.168.51.201:9100")))
