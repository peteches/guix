(use-modules (gnu machine)
             (gnu machine ssh)
             (peteches systems prometheus))

(list
 (machine
  (operating-system prometheus-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.50.187")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFh/NM/YrG0YSdc9kdhkO9loE9eHSji8y0AVOI4krpKp")
    (system "x86_64-linux")
    (user "peteches")       ; must have passwordless sudo in sudoers
    (identity "/home/peteches/.ssh/id_ed25519")))))
