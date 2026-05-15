(use-modules (gnu machine)
             (gnu machine ssh)
             (peteches systems prometheus)
             (peteches systems grafana)
             (peteches systems pihole))

(list
 (machine
  (operating-system prometheus-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.187")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFotMdW7oX/lF10PtIFjFvaIqAoDEOPLQR77a7XZpD5Q")
    (system "x86_64-linux")
    (user "peteches")       ; must have passwordless sudo in sudoers
    (identity "/home/peteches/.ssh/id_ed25519"))))
 (machine
  (operating-system grafana-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.188")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID5R1FfTSIFbe4S9LqVnmJnwwFxdKrgwrctrnnRUCjhD")               ; fill in after first boot: ssh-keyscan 192.168.51.188
    (system "x86_64-linux")
    (user "peteches")
    (identity "/home/peteches/.ssh/id_ed25519"))))
 (machine
  (operating-system pihole-os)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "192.168.51.189")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGOq7Sq7GtYp3AD8/XiHjgfZlTxlIguLGrChunMAPKCB")    ; fill in after first boot: ssh-keyscan 192.168.51.189
    (system "x86_64-linux")
    (user "peteches")
    (identity "/home/peteches/.ssh/id_ed25519")))))
