(define-module (peteches packages tailscale)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (gnu packages linux))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.102.2")
    (supported-systems '("x86_64-linux"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                           version "_amd64.tgz"))
       (sha256
        (base32 "1n04mcwfnh8pzcrhmmmr88yn0719abk0210y7awzg5fyz09dwb5d"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("tailscale" "bin/")
          ("tailscaled" "bin/"))))
    (inputs (list iptables))
    (synopsis "Private WireGuard networks made easy")
    (description
     "Tailscale is a VPN service that makes the devices and applications you
own accessible anywhere in the world, securely and effortlessly.")
    (home-page "https://tailscale.com/")
    (license bsd-3)))
