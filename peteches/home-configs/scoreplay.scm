(define-module (peteches home-configs scoreplay)
  #:use-module (gnu home services ssh))


(define-public %scoreplay-ssh-hosts
  (list
   (openssh-host
    (name "scoreplay-fft-01")
    (host-name "10.109.102.21")
    (user "scoreplay"))
   (openssh-host
    (name "scoreplay-fft-02")
    (host-name "10.109.102.22")
    (user "scoreplay"))
   (openssh-host
    (name "scoreplay-fft-03")
    (host-name "10.109.102.23")
    (user "scoreplay"))))
