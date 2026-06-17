(use-modules (gnu machine)
             (gnu machine ssh))

(list
 (machine
  (operating-system
   (load "/home/peteches/area_51/guix/peteches/systems/concourse-worker01.scm"))
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.50.57")
    (system "x86_64-linux")
    (user "peteches")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFaB9+BPAly8+5hdeufEvQFzr+XhJSND9LxMHRqVbE7B")))))
