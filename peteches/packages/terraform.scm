(define-module (peteches packages terraform)
    #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public terragrunt
  (let* ((pkg-version "0.93.9")
	 (tarball (string-append "https://github.com/gruntwork-io/terragrunt/releases/download/v"
				 pkg-version "/terragrunt_linux_amd64.tar.gz")))
    (package
     (name "terragrunt")
     (version pkg-version)
     (source
      (origin
       (method url-fetch)
       (uri tarball)
       (sha256 (base32 "01mdbam6pbg49631vz53zabvaarjrv1zbgi2ih9115s47lmap124"))))
     (build-system copy-build-system)
     (arguments
      (list
       #:install-plan
       #~(list
	  (list
	   "terragrunt_linux_amd64"
	   "bin/terragrunt"))
       #:phases
       #~(modify-phases
	  %standard-phases
	  (add-after 'install 'make-terragrunt-executable
		     (lambda* (#:key outputs #:allow-other-keys)
		       (let* ((out (assoc-ref outputs "out"))
			      (bin (string-append out "/bin/terragrunt")))
			 ;; Ensure the file is executable.
			 (chmod bin #o555)))))))

     (home-page "https://terragrunt.gruntwork.io/")
     (synopsis "Terragrunt is a flexible orchestration tool that allows Infrastructure as Code written in OpenTofu/Terraform to scale.")
     (description "Terragrunt is a flexible orchestration tool that allows Infrastructure as Code written in OpenTofu/Terraform to scale.")
     (license expat))))
