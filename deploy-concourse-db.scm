(use-modules (gnu machine)
             (gnu machine ssh))

(list
 (machine
  (operating-system
   (load "/home/peteches/area_51/guix/peteches/systems/concourse-db.scm"))
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.50.4")
    (system "x86_64-linux")
    (user "peteches")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdDypH5/tQh+ZXp5vNk8bnADKgSJ03GglQRc4mWfkMt")))))
