(define-module (peteches machines)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (peteches systems prometheus)
  #:use-module (peteches systems grafana)
  #:use-module (peteches systems loki)
  #:use-module (peteches systems pihole)
  #:use-module (peteches systems git))

(define-public prometheus-machine
  (machine
   (operating-system prometheus-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.187")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINxQwnyL7Fm08s8UwzXXuSwbahwySM//Jv2jxpfmryHj")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public grafana-machine
  (machine
   (operating-system grafana-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.188")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA3sVMg8QH+g6Xtj2NmIzV90gbkSPMiCnlaaAJx+a7tG")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public loki-machine
  (machine
   (operating-system loki-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.190")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII2AxOh6ksCO2dnP+S92mNnOR76J/ewMW1QrhkSvN/Xx")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public pihole-machine
  (machine
   (operating-system pihole-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.189")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5+ZCQgM0b8HJjRmzN2bpDkbtwqdbgop+g4ZiB4ZqjH")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public git-machine
  (machine
   (operating-system git-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name "192.168.51.191")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII/nIMuSdo5NHolPHogjR+xrudcnpLFROLYc6fpL+fkp")
     (system "x86_64-linux")
     (user "peteches")
     (identity "/home/peteches/.ssh/id_ed25519")))))

(define-public %all-machines
  (list prometheus-machine
        grafana-machine
        loki-machine
        pihole-machine
        git-machine))
