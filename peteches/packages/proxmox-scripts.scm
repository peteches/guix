(define-module (peteches packages proxmox-scripts)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages web)    ;; jq
  #:use-module (gnu packages ssh))  ;; openssh

(define-public proxmox-scripts
  (package
   (name "proxmox-scripts")
   (version "0.1.0")
   (synopsis "Proxmox VM management scripts")
   (description "Shell scripts for managing Proxmox VMs from a workstation,
including cloning the bootstrap template and retrieving age keys.")
   (license gpl2)
   (home-page "https://git.peteches.co.uk")
   (source (local-file "../../scripts" "proxmox-scripts" #:recursive? #t))
   (inputs (list openssh jq))
   (build-system copy-build-system)
   (arguments (list #:install-plan #~'(("proxmox-clone-bootstrap" "bin/"))))))

proxmox-scripts
