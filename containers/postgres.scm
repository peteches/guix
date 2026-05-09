(use-modules (gnu))
(use-package-modules databases)
(use-service-modules databases)

(operating-system
  (host-name "postgres-test")
  (timezone "Europe/London")
  (file-systems (cons (file-system
                        (device (file-system-label "does-not-matter"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sdX"))))
  (services
   (cons* (service postgresql-service-type
                   (postgresql-configuration
                    (postgresql postgresql-14)
                    (config-file
                     (postgresql-config-file
                      (log-destination "stderr")
                      (hba-file
                       (plain-file "pg_hba.conf"
                                   "\
local	all	all			trust
host	all	all	10.0.0.1/32 	trust"))
                      (extra-config
                       '(("listen_addresses" "*")
                         ("log_directory"    "/var/log/postgresql")))))))
          (service postgresql-role-service-type
                   (postgresql-role-configuration
                    (roles
                     (list (postgresql-role
                            (name "test")
                            (create-database? #t))))))
          %base-services)))
