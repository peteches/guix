(use-modules (gnu machine)
             (gnu machine ssh))

(list
 (machine
  (operating-system
   (load "/home/peteches/area_51/guix/peteches/systems/concourse-web01.scm"))
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.50.126")
    (system "x86_64-linux")
    (user "peteches")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBi4OydXz+VS2GhJ3tG8SEbI8MtY9C62iGYD3DBjYGsq")))))
