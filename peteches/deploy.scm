(use-modules (gnu machine)
             (gnu machine ssh)
             (peteches systems prometheus))

(list
 (machine
  (operating-system prometheus-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.187")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsfnyYYTnWc9jkaaWEL7LwpBPtEtb/aaPJ+KcDc2KWu")
    (system "x86_64-linux")
    (user "peteches")       ; must have passwordless sudo in sudoers
    (identity "/home/peteches/.ssh/id_ed25519")))))
