(define-module (peteches packages vault)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages compression))
;unzip

(define %version
  "2.0.3")

(define-public vault
  (package
    (name "vault")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/vault/" version
                           "/vault_" version "_linux_amd64.zip"))
       (sha256
        ;; guix hash -H sha256 --format=nix-base32 vault_2.0.3_linux_amd64.zip
        (base32 "0amw57r631i28ad7jabw178hasvm5mgf19id4k3ij4j9h9xgn3qy"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     (list
      #:install-plan
      #~'(("vault" "bin/vault"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-executable
            (lambda _
              (chmod "vault" #o755)))
          (delete 'strip)
          (delete 'validate-runpath))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.vaultproject.io/")
    (synopsis "HashiCorp Vault secrets management tool")
    (description
     "Vault is a tool for securely accessing secrets.  It provides a unified
interface to any secret, while providing tight access control and recording a
detailed audit log.  The binary is pre-built by HashiCorp as a static binary.")
    (license (license:non-copyleft
              "https://github.com/hashicorp/vault/blob/main/LICENSE"
              "Business Source License 1.1"))))
