(define-module (peteches packages tailscale)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses)
  #:use-module (gnu packages linux))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.88.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_amd64.tgz"))
              (sha256
               (base32
                "1f6c7yqx2c3lcxbfn5v82jrfp0jv2n38g4bzp5z3bikk8pbha21n"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("tailscale" "bin/")
         ("tailscaled" "bin/"))))
    (inputs
     (list iptables))
    (synopsis "Private WireGuard networks made easy")
    (description
     "Tailscale is a VPN service that makes the devices and applications you
own accessible anywhere in the world, securely and effortlessly.")
    (home-page "https://tailscale.com/")
    (license bsd-3)))
tailscale
