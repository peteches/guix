;; peteches/system-services/pihole.scm — Guix service type for Pi-hole.
;;
;; Pi-hole FTL (Faster-Than-Light) is a modified dnsmasq that provides DNS
;; ad-blocking, a built-in web server, and a SQLite query database.
;;
;; Key runtime notes:
;;  - FTL must start as root to bind port 53; it manages its own privilege
;;    handling internally.
;;  - Config files installed via etc-service-type are read-only (store-derived).
;;    Activation copies pihole.toml to /etc/pihole/ for FTL to read/write.
;;  - Gravity (blocklist database) lives in data-dir.  Adlist URLs are seeded
;;    (INSERT OR IGNORE) on every reconfigure; pihole-FTL gravity downloads
;;    the actual blocklists on every reconfigure and nightly via mcron.

(define-module (peteches system-services pihole)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu services mcron)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dns)
  #:use-module (srfi srfi-1)
  #:use-module (peteches packages pihole)
  #:use-module (peteches packages pihole-exporter)
  #:use-module (peteches system-services firewall)
  #:export (pihole-unbound-configuration
            pihole-unbound-configuration?
            pihole-custom-host
            pihole-custom-host?
            pihole-configuration
            pihole-configuration?
            %default-pihole-adlists
            pihole-service-type))

;;; ── Default values ───────────────────────────────────────────────────────

(define %default-pihole-adlists
  '("https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"))

;; Verbatim content of advanced/Templates/gravity.db.sql from the pi-hole/pi-hole
;; repository (schema v20, Pi-hole v6).  Used by pihole-activation to seed an
;; empty gravity.db on first boot via pihole-FTL's built-in sqlite3 subcommand.
(define %gravity-db-sql
  "PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;

CREATE TABLE \"group\"
(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    enabled BOOLEAN NOT NULL DEFAULT 1,
    name TEXT UNIQUE NOT NULL,
    date_added INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    date_modified INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    description TEXT
);
INSERT INTO \"group\" (id,enabled,name,description) VALUES (0,1,'Default','The default group');

CREATE TABLE domainlist
(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    type INTEGER NOT NULL DEFAULT 0,
    domain TEXT NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT 1,
    date_added INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    date_modified INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    comment TEXT,
    UNIQUE(domain, type)
);

CREATE TABLE adlist
(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    address TEXT NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT 1,
    date_added INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    date_modified INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    comment TEXT,
    date_updated INTEGER,
    number INTEGER NOT NULL DEFAULT 0,
    invalid_domains INTEGER NOT NULL DEFAULT 0,
    status INTEGER NOT NULL DEFAULT 0,
    abp_entries INTEGER NOT NULL DEFAULT 0,
    type INTEGER NOT NULL DEFAULT 0,
    UNIQUE(address, type)
);

CREATE TABLE adlist_by_group
(
    adlist_id INTEGER NOT NULL REFERENCES adlist (id) ON DELETE CASCADE,
    group_id INTEGER NOT NULL REFERENCES \"group\" (id) ON DELETE CASCADE,
    PRIMARY KEY (adlist_id, group_id)
);

CREATE TABLE gravity
(
    domain TEXT NOT NULL,
    adlist_id INTEGER NOT NULL REFERENCES adlist (id)
);

CREATE TABLE antigravity
(
    domain TEXT NOT NULL,
    adlist_id INTEGER NOT NULL REFERENCES adlist (id)
);

CREATE TABLE info
(
    property TEXT PRIMARY KEY,
    value TEXT NOT NULL
);

INSERT INTO \"info\" VALUES('version','20');
INSERT INTO \"info\" VALUES('gravity_restored','false');
INSERT INTO \"info\" VALUES('updated','0');

CREATE TABLE domainlist_by_group
(
    domainlist_id INTEGER NOT NULL REFERENCES domainlist (id) ON DELETE CASCADE,
    group_id INTEGER NOT NULL REFERENCES \"group\" (id) ON DELETE CASCADE,
    PRIMARY KEY (domainlist_id, group_id)
);

CREATE TABLE client
(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ip TEXT NOT NULL UNIQUE,
    date_added INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    date_modified INTEGER NOT NULL DEFAULT (cast(strftime('%s', 'now') as int)),
    comment TEXT
);

CREATE TABLE client_by_group
(
    client_id INTEGER NOT NULL REFERENCES client (id) ON DELETE CASCADE,
    group_id INTEGER NOT NULL REFERENCES \"group\" (id) ON DELETE CASCADE,
    PRIMARY KEY (client_id, group_id)
);

CREATE TRIGGER tr_adlist_update AFTER UPDATE OF address,enabled,comment ON adlist
    BEGIN
      UPDATE adlist SET date_modified = (cast(strftime('%s', 'now') as int)) WHERE id = NEW.id;
    END;

CREATE TRIGGER tr_client_update AFTER UPDATE ON client
    BEGIN
      UPDATE client SET date_modified = (cast(strftime('%s', 'now') as int)) WHERE ip = NEW.ip;
    END;

CREATE TRIGGER tr_domainlist_update AFTER UPDATE ON domainlist
    BEGIN
      UPDATE domainlist SET date_modified = (cast(strftime('%s', 'now') as int)) WHERE domain = NEW.domain;
    END;

CREATE VIEW vw_allowlist AS SELECT domain, domainlist.id AS id, domainlist_by_group.group_id AS group_id
    FROM domainlist
    LEFT JOIN domainlist_by_group ON domainlist_by_group.domainlist_id = domainlist.id
    LEFT JOIN \"group\" ON \"group\".id = domainlist_by_group.group_id
    WHERE domainlist.enabled = 1 AND (domainlist_by_group.group_id IS NULL OR \"group\".enabled = 1)
    AND domainlist.type = 0
    ORDER BY domainlist.id;

CREATE VIEW vw_denylist AS SELECT domain, domainlist.id AS id, domainlist_by_group.group_id AS group_id
    FROM domainlist
    LEFT JOIN domainlist_by_group ON domainlist_by_group.domainlist_id = domainlist.id
    LEFT JOIN \"group\" ON \"group\".id = domainlist_by_group.group_id
    WHERE domainlist.enabled = 1 AND (domainlist_by_group.group_id IS NULL OR \"group\".enabled = 1)
    AND domainlist.type = 1
    ORDER BY domainlist.id;

CREATE VIEW vw_regex_allowlist AS SELECT domain, domainlist.id AS id, domainlist_by_group.group_id AS group_id
    FROM domainlist
    LEFT JOIN domainlist_by_group ON domainlist_by_group.domainlist_id = domainlist.id
    LEFT JOIN \"group\" ON \"group\".id = domainlist_by_group.group_id
    WHERE domainlist.enabled = 1 AND (domainlist_by_group.group_id IS NULL OR \"group\".enabled = 1)
    AND domainlist.type = 2
    ORDER BY domainlist.id;

CREATE VIEW vw_regex_denylist AS SELECT domain, domainlist.id AS id, domainlist_by_group.group_id AS group_id
    FROM domainlist
    LEFT JOIN domainlist_by_group ON domainlist_by_group.domainlist_id = domainlist.id
    LEFT JOIN \"group\" ON \"group\".id = domainlist_by_group.group_id
    WHERE domainlist.enabled = 1 AND (domainlist_by_group.group_id IS NULL OR \"group\".enabled = 1)
    AND domainlist.type = 3
    ORDER BY domainlist.id;

CREATE VIEW vw_gravity AS SELECT domain, adlist.id AS adlist_id, adlist_by_group.group_id AS group_id
    FROM gravity
    LEFT JOIN adlist_by_group ON adlist_by_group.adlist_id = gravity.adlist_id
    LEFT JOIN adlist ON adlist.id = gravity.adlist_id
    LEFT JOIN \"group\" ON \"group\".id = adlist_by_group.group_id
    WHERE adlist.enabled = 1 AND (adlist_by_group.group_id IS NULL OR \"group\".enabled = 1);

CREATE VIEW vw_antigravity AS SELECT domain, adlist.id AS adlist_id, adlist_by_group.group_id AS group_id
    FROM antigravity
    LEFT JOIN adlist_by_group ON adlist_by_group.adlist_id = antigravity.adlist_id
    LEFT JOIN adlist ON adlist.id = antigravity.adlist_id
    LEFT JOIN \"group\" ON \"group\".id = adlist_by_group.group_id
    WHERE adlist.enabled = 1 AND (adlist_by_group.group_id IS NULL OR \"group\".enabled = 1) AND adlist.type = 1;

CREATE VIEW vw_adlist AS SELECT DISTINCT address, id, type
    FROM adlist
    WHERE enabled = 1
    ORDER BY id;

CREATE TRIGGER tr_domainlist_add AFTER INSERT ON domainlist
    BEGIN
      INSERT INTO domainlist_by_group (domainlist_id, group_id) VALUES (NEW.id, 0);
    END;

CREATE TRIGGER tr_client_add AFTER INSERT ON client
    BEGIN
      INSERT INTO client_by_group (client_id, group_id) VALUES (NEW.id, 0);
    END;

CREATE TRIGGER tr_adlist_add AFTER INSERT ON adlist
    BEGIN
      INSERT INTO adlist_by_group (adlist_id, group_id) VALUES (NEW.id, 0);
    END;

CREATE TRIGGER tr_group_update AFTER UPDATE ON \"group\"
    BEGIN
      UPDATE \"group\" SET date_modified = (cast(strftime('%s', 'now') as int)) WHERE id = NEW.id;
    END;

CREATE TRIGGER tr_group_zero AFTER DELETE ON \"group\"
    BEGIN
      INSERT OR IGNORE INTO \"group\" (id,enabled,name) VALUES (0,1,'Default');
    END;

CREATE TRIGGER tr_domainlist_delete AFTER DELETE ON domainlist
    BEGIN
      DELETE FROM domainlist_by_group WHERE domainlist_id = OLD.id;
    END;

CREATE TRIGGER tr_adlist_delete AFTER DELETE ON adlist
    BEGIN
      DELETE FROM adlist_by_group WHERE adlist_id = OLD.id;
    END;

CREATE TRIGGER tr_client_delete AFTER DELETE ON client
    BEGIN
      DELETE FROM client_by_group WHERE client_id = OLD.id;
    END;

COMMIT;
")

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <pihole-custom-host>
  pihole-custom-host make-pihole-custom-host
  pihole-custom-host?
  (address  pihole-custom-host-address)
  (hostname pihole-custom-host-hostname))

(define-record-type* <pihole-unbound-configuration>
  pihole-unbound-configuration make-pihole-unbound-configuration
  pihole-unbound-configuration?
  (listen-port    pihole-unbound-configuration-listen-port    (default 5335))
  (listen-address pihole-unbound-configuration-listen-address (default "127.0.0.1"))
  ;; Raw text appended to the server: block of unbound.conf.
  (extra-server   pihole-unbound-configuration-extra-server   (default "")))

(define-record-type* <pihole-configuration>
  pihole-configuration make-pihole-configuration
  pihole-configuration?
  (package                 pihole-configuration-package                 (default pihole-ftl))
  ;; Network interface FTL binds to.
  (interface               pihole-configuration-interface               (default "eth0"))
  ;; Upstream DNS servers.  When with-unbound? is #t this is overridden to
  ;; point at the local Unbound instance.
  (dns-upstreams           pihole-configuration-dns-upstreams           (default '("8.8.8.8" "8.8.4.4")))
  ;; Blocking reply mode: "NULL" | "NXDOMAIN" | "NODATA" | "IP" | "IP_NODATA_AAAA"
  (blocking-mode           pihole-configuration-blocking-mode           (default "NULL"))
  ;; Enable DNS query logging.
  (query-logging?          pihole-configuration-query-logging?          (default #t))
  ;; Writable directory for FTL databases (gravity.db, pihole-FTL.db, etc.)
  (data-dir                pihole-configuration-data-dir                (default "/var/lib/pihole"))
  ;; Directory for FTL, dnsmasq and webserver log files.
  (log-dir                 pihole-configuration-log-dir                 (default "/var/log/pihole"))
  ;; HTTP port for the built-in web interface.  Set to #f to disable.
  (web-port                pihole-configuration-web-port                (default 80))
  ;; Bcrypt hash for the web interface admin password.  Empty string = no
  ;; password (open access).  Generate with: pihole setpassword
  (webserver-password-hash pihole-configuration-webserver-password-hash (default ""))
  ;; When #t, also start an Unbound recursive resolver and point FTL at it.
  (with-unbound?           pihole-configuration-with-unbound?           (default #f))
  ;; Unbound configuration (used only when with-unbound? is #t).
  (unbound                 pihole-configuration-unbound                 (default (pihole-unbound-configuration)))
  ;; Blocklist URLs seeded into gravity.db (INSERT OR IGNORE on every reconfigure).
  (adlists                 pihole-configuration-adlists                 (default %default-pihole-adlists))
  ;; Custom local DNS records installed as /etc/pihole/hosts/custom.list.
  (custom-hosts            pihole-configuration-custom-hosts            (default '()))
  ;; mcron schedule for nightly gravity updates (crontab format).
  (gravity-update-schedule pihole-configuration-gravity-update-schedule (default "15 3 * * *"))
  ;; Raw TOML appended verbatim after the generated sections.
  (extra-toml              pihole-configuration-extra-toml              (default ""))
  ;; Extra CLI arguments passed to pihole-FTL.
  (extra-args              pihole-configuration-extra-args              (default '()))
  ;; Prometheus metrics exporter (eko/pihole-exporter).
  (with-exporter?          pihole-configuration-with-exporter?          (default #f))
  (exporter-package        pihole-configuration-exporter-package        (default pihole-exporter))
  ;; TCP port the exporter listens on.
  (exporter-port           pihole-configuration-exporter-port           (default 9617))
  ;; Plaintext Pi-hole web password for API access.  Empty = no auth required.
  (exporter-password       pihole-configuration-exporter-password       (default "")))

;;; ── TOML / config-file rendering ─────────────────────────────────────────
;;
;; All rendering is pure Scheme (no gexps); results are passed to plain-file.

(define (toml-bool b)
  (if b "true" "false"))

(define (toml-string s)
  (string-append "\"" s "\""))

(define (toml-string-array lst)
  (string-append
   "["
   (string-join (map toml-string lst) ", ")
   "]"))

(define (render-pihole-toml config)
  "Produce a complete pihole.toml string for pihole-FTL v6."
  (let* ((with-ub?  (pihole-configuration-with-unbound? config))
         (ub        (pihole-configuration-unbound config))
         (upstreams (if with-ub?
                        (list (string-append
                               (pihole-unbound-configuration-listen-address ub)
                               "#"
                               (number->string
                                (pihole-unbound-configuration-listen-port ub))))
                        (pihole-configuration-dns-upstreams config)))
         (hosts     (map (lambda (h)
                           (string-append (pihole-custom-host-address h)
                                          " "
                                          (pihole-custom-host-hostname h)))
                         (pihole-configuration-custom-hosts config)))
         (data-dir  (pihole-configuration-data-dir config))
         (log-dir   (pihole-configuration-log-dir config))
         (web-port  (pihole-configuration-web-port config))
         (pw-hash   (pihole-configuration-webserver-password-hash config))
         (extra     (pihole-configuration-extra-toml config)))
    (string-append
     "[dns]\n"
     "  upstreams = " (toml-string-array upstreams) "\n"
     "  interface = " (toml-string (pihole-configuration-interface config)) "\n"
     "  listeningMode = \"SINGLE\"\n"
     "  queryLogging = " (toml-bool (pihole-configuration-query-logging? config)) "\n"
     "  blocking.active = true\n"
     "  blocking.mode = " (toml-string (pihole-configuration-blocking-mode config)) "\n"
     "  hosts = " (toml-string-array hosts) "\n"
     "\n"
     "[files]\n"
     "  database = " (toml-string (string-append data-dir "/pihole-FTL.db")) "\n"
     "  gravity = " (toml-string (string-append data-dir "/gravity.db")) "\n"
     "  gravity_tmp = \"/tmp\"\n"
     "  macvendor = " (toml-string (string-append data-dir "/macvendor.db")) "\n"
     "  log.ftl = " (toml-string (string-append log-dir "/FTL.log")) "\n"
     "  log.dnsmasq = " (toml-string (string-append log-dir "/pihole.log")) "\n"
     "  log.webserver = " (toml-string (string-append log-dir "/webserver.log")) "\n"
     "\n"
     "[webserver]\n"
     "  port = " (toml-string (if web-port
                                  (number->string web-port)
                                  "")) "\n"
     (if (string=? pw-hash "")
         ""
         (string-append "  api.password = " (toml-string pw-hash) "\n"))
     "\n"
     "[ntp]\n"
     "  sync.active = false\n"
     "  ipv4.active = false\n"
     "  ipv6.active = false\n"
     "\n"
     extra)))

(define (render-unbound-conf config)
  "Produce an unbound.conf for use as a Pi-hole recursive upstream resolver."
  (let* ((ub   (pihole-configuration-unbound config))
         (port (pihole-unbound-configuration-listen-port ub))
         (addr (pihole-unbound-configuration-listen-address ub))
         (xtra (pihole-unbound-configuration-extra-server ub)))
    (string-append
     "server:\n"
     "    verbosity: 0\n"
     "    username: \"unbound\"\n"
     "    interface: " addr "\n"
     "    port: " (number->string port) "\n"
     "    do-ip4: yes\n"
     "    do-udp: yes\n"
     "    do-tcp: yes\n"
     "    do-ip6: no\n"
     "    harden-glue: yes\n"
     "    harden-dnssec-stripped: yes\n"
     "    use-caps-for-id: no\n"
     "    edns-buffer-size: 1232\n"
     "    prefetch: yes\n"
     "    num-threads: 1\n"
     "    so-rcvbuf: 1m\n"
     "    private-address: 192.168.0.0/16\n"
     "    private-address: 169.254.0.0/16\n"
     "    private-address: 172.16.0.0/12\n"
     "    private-address: 10.0.0.0/8\n"
     "    private-address: fd00::/8\n"
     "    private-address: fe80::/10\n"
     xtra "\n")))

(define (render-custom-hosts hosts)
  "Render a list of <pihole-custom-host> records to custom.list format (ip hostname)."
  (if (null? hosts)
      ""
      (string-append
       (string-join
        (map (lambda (h)
               (string-append (pihole-custom-host-address h)
                              " "
                              (pihole-custom-host-hostname h)))
             hosts)
        "\n")
       "\n")))

(define (render-adlist-sql adlists)
  "Generate SQL to INSERT OR IGNORE adlist URLs into gravity.db."
  (if (null? adlists)
      ""
      (string-append
       "BEGIN TRANSACTION;\n"
       (string-join
        (map (lambda (url)
               (string-append
                "INSERT OR IGNORE INTO adlist (address,enabled,comment)"
                " VALUES ('" url "',1,'Added by Guix configuration');\n"
                "INSERT OR IGNORE INTO adlist_by_group (adlist_id, group_id)"
                " SELECT id, 0 FROM adlist WHERE address='" url "';"))
             adlists)
        "\n")
       "\nCOMMIT;\n")))

;;; ── Service extension helpers ─────────────────────────────────────────────

(define (pihole-accounts config)
  (append
   (list
    (user-group
     (name "pihole")
     (system? #t))
    (user-account
     (name "pihole")
     (group "pihole")
     (system? #t)
     (comment "Pi-hole FTL daemon")
     (home-directory "/var/lib/pihole")
     (shell (file-append shadow "/sbin/nologin"))))
   (if (pihole-configuration-with-unbound? config)
       (list
        (user-group
         (name "unbound")
         (system? #t))
        (user-account
         (name "unbound")
         (group "unbound")
         (system? #t)
         (comment "Unbound DNS resolver")
         (home-directory "/var/lib/unbound")
         (shell (file-append shadow "/sbin/nologin"))))
       '())))

(define (pihole-activation config)
  (let* ((pkg             (pihole-configuration-package config))
         (data-dir        (pihole-configuration-data-dir config))
         (log-dir         (pihole-configuration-log-dir config))
         (adlists         (pihole-configuration-adlists config))
         (toml-file       (plain-file "pihole.toml" (render-pihole-toml config)))
         (custom-list     (plain-file "custom.list"
                                      (render-custom-hosts
                                       (pihole-configuration-custom-hosts config))))
         (sql-file        (plain-file "gravity.db.sql" %gravity-db-sql))
         (adlist-sql      (plain-file "adlist-seed.sql" (render-adlist-sql adlists))))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$data-dir)
        (mkdir-p #$log-dir)
        (mkdir-p "/run/pihole")
        ;; /etc/pihole must be a real writable directory — not a store symlink —
        ;; so FTL can chown its config file and write runtime state.
        ;; etc-service-type must NOT install anything under pihole/ for the same reason.
        (mkdir-p "/etc/pihole/hosts")
        ;; Ensure pihole user owns writable directories.
        (let* ((pw  (getpwnam "pihole"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (chown #$data-dir uid gid)
          (chown #$log-dir  uid gid)
          (chown "/run/pihole" uid gid)
          (chown "/etc/pihole" uid gid)
          (chown "/etc/pihole/hosts" uid gid))
        ;; Create /var/www/html as a symlink into the pihole-web store package.
        ;; FTL's built-in web server uses webroot + webhome = /var/www/html/admin/
        ;; by default, so this makes the admin UI available without any TOML changes.
        (mkdir-p "/var/www")
        (false-if-exception (delete-file "/var/www/html"))
        (symlink #$(file-append pihole-web "/share/pihole-web") "/var/www/html")
        ;; Always replace /etc/pihole/pihole.toml with the store-derived version.
        ;; FTL expands unset keys to their defaults on startup; keys we explicitly
        ;; set here survive that expansion.  Always replacing ensures reconfigure
        ;; propagates config changes (store files have mtime=1, so mtime comparison
        ;; would never trigger a replace after FTL first modifies the file).
        (let ((src #$toml-file)
              (dst "/etc/pihole/pihole.toml"))
          (when (file-exists? dst)
            (delete-file dst))
          (copy-file src dst)
          (let* ((pw  (getpwnam "pihole"))
                 (uid (passwd:uid pw))
                 (gid (passwd:gid pw)))
            (chown dst uid gid)))
        ;; Install custom.list (always replace from store version on reconfigure).
        (let ((src #$custom-list)
              (dst "/etc/pihole/hosts/custom.list"))
          (when (file-exists? dst) (delete-file dst))
          (copy-file src dst)
          (chmod dst #o660)
          (let* ((pw  (getpwnam "pihole"))
                 (uid (passwd:uid pw))
                 (gid (passwd:gid pw)))
            (chown dst uid gid)))
        ;; Seed gravity.db with the Pi-hole v20 schema if it does not yet exist.
        ;; FTL auto-creates pihole-FTL.db but not gravity.db; the official installer
        ;; does this via gravity.sh using pihole-FTL's built-in sqlite3 subcommand.
        (let ((gravity-db (string-append #$data-dir "/gravity.db")))
          (unless (file-exists? gravity-db)
            (invoke #$(file-append pkg "/bin/pihole-FTL")
                    "sqlite3" gravity-db
                    (string-append ".read " #$sql-file)
                    ".quit")
            (let* ((pw  (getpwnam "pihole"))
                   (uid (passwd:uid pw))
                   (gid (passwd:gid pw)))
              (chown gravity-db uid gid))))
        ;; Seed configured adlists on every reconfigure (INSERT OR IGNORE is
        ;; idempotent; a no-op if the URL is already present).
        (invoke #$(file-append pkg "/bin/pihole-FTL")
                "sqlite3" (string-append #$data-dir "/gravity.db")
                (string-append ".read " #$adlist-sql)
                ".quit")
        ;; Download blocklists in the background.  PATH is set explicitly so
        ;; gravity.sh can find curl and dig from the system profile.  FTL does
        ;; not need to be running for the download/db-build phase.
        (system (string-append
                 "su -s /bin/sh pihole -c '"
                 "PATH=/run/current-system/profile/bin"
                 ":/run/current-system/profile/sbin "
                 #$(file-append pihole-scripts "/bin/pihole")
                 " -g >>" #$log-dir "/gravity.log 2>&1' &"))
        (system* #$(file-append shepherd "/bin/herd") "restart" "pihole")
        #$(if (pihole-configuration-with-unbound? config)
              #~(system* #$(file-append shepherd "/bin/herd") "restart" "unbound")
              #~(begin)))))

(define (pihole-etc-files config)
  ;; Neither pihole.toml nor custom.list are installed here — /etc/pihole must
  ;; be a real writable directory (not a store symlink) for FTL to function.
  ;; Both are copied into /etc/pihole/ by the activation gexp instead.
  (if (pihole-configuration-with-unbound? config)
      (list `("unbound/unbound.conf"
              ,(plain-file "unbound.conf" (render-unbound-conf config))))
      '()))

(define (make-unbound-shepherd-service config)
  (shepherd-service
   (provision '(unbound))
   (documentation "Unbound recursive DNS resolver (Pi-hole upstream).")
   (requirement '(networking file-systems))
   (start #~(make-forkexec-constructor
              (list #$(file-append unbound "/sbin/unbound")
                    "-d"                      ; don't daemonise (shepherd controls it)
                    "-c" "/etc/unbound/unbound.conf")
              ;; Start as root so unbound can raise fd limits and bind; it
              ;; drops to the "unbound" user internally via username: in conf.
              #:log-file #$(string-append
                            (pihole-configuration-log-dir config)
                            "/unbound.log")))
   (stop #~(make-kill-destructor))))

(define (make-pihole-exporter-shepherd-service config)
  (let* ((pkg      (pihole-configuration-exporter-package config))
         (port     (pihole-configuration-exporter-port config))
         (password (pihole-configuration-exporter-password config))
         (web-port (or (pihole-configuration-web-port config) 80)))
    (shepherd-service
     (provision '(pihole-exporter))
     (documentation "Prometheus exporter for Pi-hole statistics.")
     (requirement '(pihole networking))
     (start #~(make-forkexec-constructor
               (append
                (list #$(file-append pkg "/bin/pihole-exporter")
                      "-port" #$(number->string port)
                      "-pihole_protocol" "http"
                      "-pihole_hostname" "localhost"
                      "-pihole_port" #$(number->string web-port))
                (if (string=? #$password "")
                    '()
                    (list "-pihole_password" #$password)))
               #:log-file "/var/log/pihole/exporter.log"))
     (stop #~(make-kill-destructor)))))


(define (pihole-shepherd-service config)
  (let* ((pkg      (pihole-configuration-package config))
         (data-dir (pihole-configuration-data-dir config))
         (log-dir  (pihole-configuration-log-dir config))
         (with-ub? (pihole-configuration-with-unbound? config))
         (ftl-svc
          (shepherd-service
           (provision '(pihole))
           (documentation "Pi-hole FTL DNS ad-blocking daemon.")
           (requirement (append '(networking file-systems)
                                (if with-ub? '(unbound) '())))
           (start
            ;; Wrap the forkexec constructor with prestart setup that mirrors
            ;; the official pihole-FTL-prestart.sh script.  This ensures correct
            ;; ownership and permissions on /etc/pihole/ and /var/log/pihole/
            ;; before FTL tries to read its config and open log files.
            #~(let ((make-it
                     (make-forkexec-constructor
                      (append
                       (list #$(file-append pkg "/bin/pihole-FTL")
                             "no-daemon")
                       '#$(pihole-configuration-extra-args config))
                      #:log-file #$(string-append log-dir "/FTL.log"))))
                (lambda args
                  ;; Ensure writable directories exist.
                  (for-each (lambda (d)
                              (unless (file-exists? d)
                                (mkdir d)))
                            (list #$log-dir "/run/pihole"))
                  ;; Touch required files before recursive chown so they are
                  ;; created with correct ownership in one pass.
                  (for-each
                   (lambda (path)
                     (unless (file-exists? path)
                       (call-with-output-file path (lambda (_) #f))))
                   '("/etc/pihole/versions"
                     "/etc/pihole/dhcp.leases"))
                  ;; Recursive chown of config and log dirs to pihole:pihole.
                  (system* "chown" "-R" "pihole:pihole"
                           "/etc/pihole" #$log-dir)
                  ;; Directories: 755 so FTL can traverse them.
                  (system* "find" "/etc/pihole" #$log-dir
                           "-type" "d" "-exec" "chmod" "755" "{}" "+")
                  ;; Regular files: 640 (pihole rw, group r); TLS files 600.
                  (system* "find" "/etc/pihole" #$log-dir
                           "-type" "f"
                           "!" "(" "-name" "*.pem" "-o" "-name" "*.crt" ")"
                           "-exec" "chmod" "640" "{}" "+")
                  (system* "find" "/etc/pihole"
                           "-type" "f"
                           "(" "-name" "*.pem" "-o" "-name" "*.crt" ")"
                           "-exec" "chmod" "600" "{}" "+")
                  ;; versions must be world-readable for 'pihole -v'.
                  (when (file-exists? "/etc/pihole/versions")
                    (chmod "/etc/pihole/versions" #o644))
                  ;; PID placeholder is outside /etc/pihole so needs separate handling.
                  (unless (file-exists? "/run/pihole-FTL.pid")
                    (call-with-output-file "/run/pihole-FTL.pid"
                      (lambda (_) #f)))
                  (chmod "/run/pihole-FTL.pid" #o644)
                  (system* "chown" "pihole:pihole" "/run/pihole-FTL.pid")
                  ;; Fork FTL.
                  (apply make-it args))))
           (stop
            ;; Kill FTL then clean up runtime files (mirrors pihole-FTL-poststop.sh).
            #~(lambda (pid . args)
                ((make-kill-destructor) pid)
                (for-each (lambda (f)
                            (when (file-exists? f) (delete-file f)))
                          '("/run/pihole-FTL.pid"
                            "/run/pihole/FTL.sock"))
                (system* "sh" "-c" "rm -f /dev/shm/FTL-*")
                #t)))))
    (let ((services (if with-ub?
                        (list (make-unbound-shepherd-service config) ftl-svc)
                        (list ftl-svc))))
      (if (pihole-configuration-with-exporter? config)
          (append services (list (make-pihole-exporter-shepherd-service config)))
          services))))

(define (pihole-firewall-rules config)
  (let ((web-port (pihole-configuration-web-port config)))
    (nftables-rules
     (input
      (append
       (list "udp dport 53 accept comment \"pihole-dns\""
             "tcp dport 53 accept comment \"pihole-dns\"")
       (if web-port
           (list (string-append "tcp dport "
                                (number->string web-port)
                                " accept comment \"pihole-web\""))
           '())
       (if (pihole-configuration-with-exporter? config)
           (list (string-append "tcp dport "
                                (number->string (pihole-configuration-exporter-port config))
                                " accept comment \"pihole-exporter\""))
           '()))))))

(define (pihole-profile config)
  (append
   (list (pihole-configuration-package config)
         pihole-scripts
         curl
         `(,(@ (gnu packages dns) isc-bind) "utils"))
   (if (pihole-configuration-with-unbound? config)
       (list unbound)
       '())
   (if (pihole-configuration-with-exporter? config)
       (list (pihole-configuration-exporter-package config))
       '())))

(define (pihole-log-rotation config)
  (let ((log-dir (pihole-configuration-log-dir config)))
    (list (string-append log-dir "/FTL.log")
          (string-append log-dir "/pihole.log")
          (string-append log-dir "/webserver.log"))))

(define (pihole-mcron-jobs config)
  (let* ((sched (pihole-configuration-gravity-update-schedule config)))
    (list
     #~(job #$sched
            (lambda ()
              (system* "su" "-s" "/bin/sh" "-c"
                       (string-append
                        "PATH=/run/current-system/profile/bin"
                        ":/run/current-system/profile/sbin "
                        #$(file-append pihole-scripts "/bin/pihole") " -g")
                       "pihole"))))))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public pihole-service-type
  (service-type
   (name 'pihole)
   (description
    "Pi-hole FTL DNS ad-blocking daemon.  Optionally starts Unbound as a
recursive upstream resolver.  Adlist URLs are seeded into gravity.db on every
reconfigure and blocklists are downloaded via pihole-FTL gravity.  A nightly
mcron job keeps lists current between reconfigures.  Set webserver-password-hash
to a bcrypt hash or run 'pihole setpassword' after first boot to protect the
web interface.")
   (extensions
    (list
     (service-extension account-service-type        pihole-accounts)
     (service-extension activation-service-type     pihole-activation)
     (service-extension etc-service-type            pihole-etc-files)
     (service-extension shepherd-root-service-type  pihole-shepherd-service)
     (service-extension firewall-service-type       pihole-firewall-rules)
     (service-extension profile-service-type        pihole-profile)
     (service-extension log-rotation-service-type   pihole-log-rotation)
     (service-extension mcron-service-type          pihole-mcron-jobs)))
   (default-value (pihole-configuration))))
