(define-module (peteches packages rclone)
  ;; core Guix modules you actually use:
  #:use-module (guix packages)
  #:use-module (guix git-download)          ; if you used git-fetch
  #:use-module (guix download)              ; if you used url-fetch
  #:use-module ((guix licenses) #:prefix license:)
  ;; pick exactly one build system you use:
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages prometheus)
  #:use-module (guix build-system go))

(define unknown-license!
 (license:non-copyleft "https://example.invalid/unknown"
                  "Unknown license; treat as nonfree until verified"))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal
  (package
   (name "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal")
   (version "3.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/Azure/azure-sdk-for-go")
           (commit (go-version->git-ref version
                                        #:subdir
                                        "sdk/resourcemanager/internal"))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0000000000000000000000000000000000000000000000000000"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path
     "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/internal/v3"
     #:unpack-path "github.com/Azure/azure-sdk-for-go"))
   (propagated-inputs (list go-github-com-stretchr-testify
                            go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                            go-github-com-azure-azure-sdk-for-go-sdk-internal
                            go-github-com-azure-azure-sdk-for-go-sdk-azcore))
   (home-page "https://github.com/Azure/azure-sdk-for-go")
   (synopsis #f)
   (description #f)
   (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azcore
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azcore")
    (version "1.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azcore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azcore"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-internal))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Core Client Module for Go")
    (description
     "Package azcore implements an HTTP request/response middleware pipeline used by
Azure SDK clients.")
    (license license:expat)))

(define-public go-github-com-keybase-go-keychain
  (package
    (name "go-github-com-keybase-go-keychain")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keybase/go-keychain")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/keybase/go-keychain"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-keybase-dbus))
    (home-page "https://github.com/keybase/go-keychain")
    (synopsis "Go Keychain")
    (description
     "This package provides a library for accessing the Keychain for @code{macOS},
@code{iOS}, and Linux in Go (golang).")
    (license license:expat)))

(define-public go-github-com-azuread-microsoft-authentication-extensions-for-go-cache
  (package
    (name
     "go-github-com-azuread-microsoft-authentication-extensions-for-go-cache")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AzureAD/microsoft-authentication-extensions-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "cache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/AzureAD/microsoft-authentication-extensions-for-go/cache"
      #:unpack-path
      "github.com/AzureAD/microsoft-authentication-extensions-for-go"))
    (propagated-inputs (list go-gopkg-in-check-v1 go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-keybase-go-keychain
                        go-github-com-azuread-microsoft-authentication-library-for-go))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-extensions-for-go")
    (synopsis "Microsoft Authentication Library (MSAL) Extensions for Go")
    (description
     "This module contains a persistent cache for
@@url{https://github.com/@code{AzureAD/microsoft-authentication-library-for-go,Microsoft}
Authentication Library (MSAL) for Go} public client applications such as CLI
tools.  It isn't recommended for web applications or RPC APIs, in which it can
cause scaling and performance problems.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azidentity/cache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azidentity/cache"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-google-uuid
                        go-github-com-azuread-microsoft-authentication-library-for-go
                        go-github-com-azuread-microsoft-authentication-extensions-for-go-cache
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Identity Cache Module for Go")
    (description
     "This module implements a cross-platform persistent token cache for
@@url{https://pkg.go.dev/github.com/Azure/azure-sdk-for-go/sdk/azidentity,azidentity}
credentials.  See that module's
@@url{https://pkg.go.dev/github.com/Azure/azure-sdk-for-go/sdk/azidentity#pkg-examples,examples}
for sample code showing how to configure persistent caching for a credential,
and its @@url{https://aka.ms/azsdk/go/identity/caching,token caching document}
for more information about the implementation.")
    (license license:expat)))

(define-public go-github-com-azuread-microsoft-authentication-library-for-go
  (package
    (name "go-github-com-azuread-microsoft-authentication-library-for-go")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AzureAD/microsoft-authentication-library-for-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/AzureAD/microsoft-authentication-library-for-go"))
    (propagated-inputs (list go-github-com-pkg-browser
                             go-github-com-montanaflynn-stats
                             go-github-com-kylelemons-godebug
                             go-github-com-google-uuid
                             go-github-com-golang-jwt-jwt-v5))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-library-for-go")
    (synopsis "Microsoft Authentication Library (MSAL) for Go")
    (description
     "The Microsoft Authentication Library (MSAL) for Go is part of the
@@url{https://aka.ms/aaddevv2,Microsoft identity platform for developers}
(formerly named Azure AD) v2.0.  It allows you to sign in users or apps with
Microsoft identities
(@@url{https://azure.microsoft.com/services/active-directory/,Azure AD} and
@@url{https://account.microsoft.com,Microsoft Accounts}) and obtain tokens to
call Microsoft APIs such as @@url{https://graph.microsoft.io/,Microsoft Graph}
or your own APIs registered with the Microsoft identity platform.  It is built
using industry standard OAuth2 and @code{OpenID} Connect protocols.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azidentity
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azidentity")
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azidentity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azidentity"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-crypto
                        go-github-com-stretchr-testify
                        go-github-com-google-uuid
                        go-github-com-golang-jwt-jwt-v5
                        go-github-com-azuread-microsoft-authentication-library-for-go
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Identity Client Module for Go")
    (description
     "The Azure Identity module provides Microsoft Entra ID
(@@url{https://learn.microsoft.com/entra/fundamentals/new-name,formerly Azure
Active Directory}) token authentication support across the Azure SDK. It
includes a set of @@code{@code{TokenCredential}} implementations, which can be
used with Azure SDK clients supporting token authentication.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-internal
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-internal")
    (version "1.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/internal"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-text go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure.Core Internal Module for Go")
    (description "internal contains content for Azure SDK developers.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "sdk/resourcemanager/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/internal/v2"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-managementgroups-armmanagementgroups
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-managementgroups-armmanagementgroups")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                      #:subdir
                      "sdk/resourcemanager/managementgroups/armmanagementgroups"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/managementgroups/armmanagementgroups"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v2
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Management Groups Module for Go")
    (description
     "The @@code{armmanagementgroups} module provides operations for working with
Azure Management Groups.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                      #:subdir "sdk/resourcemanager/resources/armresources"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/resources/armresources"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-managementgroups-armmanagementgroups
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v2
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Resources Module for Go")
    (description
     "The @@code{armresources} module provides operations for working with Azure
Resources.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-storage-armstorage
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-storage-armstorage")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                      #:subdir "sdk/resourcemanager/storage/armstorage"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/storage/armstorage"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v3
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Storage Module for Go")
    (description
     "The @@code{armstorage} module provides operations for working with Azure
Storage.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/storage/azblob"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/storage/azblob"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-storage-armstorage
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Blob Storage module for Go")
    (description
     "Azure Blob Storage is Microsoft's object storage solution for the cloud.  Blob
Storage is optimized for storing massive amounts of unstructured data - data
that does not adhere to a particular data model or definition, such as text or
binary data.  For more information, see
@@url{https://learn.microsoft.com/azure/storage/blobs/storage-blobs-introduction,Introduction
to Azure Blob Storage}.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-storage-azfile
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-storage-azfile")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/storage/azfile"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/storage/azfile"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure File Storage SDK for Go")
    (description
     "Azure File Shares offers fully managed file shares in the cloud that are
accessible via the industry standard
@@url{https://learn.microsoft.com/windows/desktop/@code{FileIO/microsoft-smb-protocol-and-cifs-protocol-overview,Server}
Message Block (SMB) protocol}.  Azure file shares can be mounted concurrently by
cloud or on-premises deployments of Windows, Linux, and @code{macOS}.
Additionally, Azure file shares can be cached on Windows Servers with Azure File
Sync for fast access near where the data is being used.")
    (license license:expat)))

(define-public go-github-com-appscode-go-querystring
  (package
    (name "go-github-com-appscode-go-querystring")
    (version "0.0.0-20170504095604-0126cfb3f1dc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/appscode/go-querystring")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/appscode/go-querystring"))
    (home-page "https://github.com/appscode/go-querystring")
    (synopsis "go-querystring")
    (description
     "go-querystring is Go library for encoding structs into URL query parameters.")
    (license license:bsd-3)))

(define-public go-github-com-chilts-sid
  (package
    (name "go-github-com-chilts-sid")
    (version "0.0.0-20190607042430-660e94789ec9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chilts/sid")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/chilts/sid"))
    (home-page "https://github.com/chilts/sid")
    (synopsis "sid : generate sortable identifiers")
    (description
     "Package sid provides the ability to generate Sortable Identifiers.  These are
also universally unique.")
    (license license:expat)))

(define-public go-github-com-modocache-gover
  (package
    (name "go-github-com-modocache-gover")
    (version "0.0.0-20171022184752-b58185e213c5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sozorogami/gover")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/modocache/gover"))
    (home-page "https://github.com/modocache/gover")
    (synopsis "gover")
    (description "Usage: gover [root] [out].")
    (license license:asl2.0)))

(define-public go-github-com-dnaeon-go-vcr
  (package
    (name "go-github-com-dnaeon-go-vcr")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dnaeon/go-vcr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dnaeon/go-vcr"))
    (propagated-inputs (list go-gopkg-in-yaml-v2 go-github-com-modocache-gover))
    (home-page "https://github.com/dnaeon/go-vcr")
    (synopsis "go-vcr")
    (description
     "@@code{go-vcr} simplifies testing by recording your HTTP interactions and
replaying them in future runs in order to provide fast, deterministic and
accurate testing of your code.")
    (license license:bsd-2)))

(define-public go-github-com-lpar-date
  (package
    (name "go-github-com-lpar-date")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lpar/date")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lpar/date"))
    (home-page "https://github.com/lpar/date")
    (synopsis "date")
    (description
     "Minimal utility functions for working with SQL dates and other date-only dates
in Go.")
    (license license:bsd-3)))

(define-public go-github-com-panjf2000-ants
  (package
    (name "go-github-com-panjf2000-ants")
    (version "2.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/panjf2000/ants")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/panjf2000/ants/v2"
      #:unpack-path "github.com/panjf2000/ants"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/panjf2000/ants")
    (synopsis "📖 Introduction")
    (description
     "Package ants implements an efficient and reliable goroutine pool for Go.")
    (license license:expat)))

(define-public go-github-com-samber-lo
  (package
    (name "go-github-com-samber-lo")
    (version "1.51.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/samber/lo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/samber/lo"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/samber/lo")
    (synopsis "lo - Iterate over slices, maps, channels...")
    (description "✨.")
    (license license:expat)))

(define-public go-github-com-avvmoto-buf-readerat
  (package
    (name "go-github-com-avvmoto-buf-readerat")
    (version "0.0.0-20171115124131-a17c8cb89270")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/avvmoto/buf-readerat")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/avvmoto/buf-readerat"))
    (home-page "https://github.com/avvmoto/buf-readerat")
    (synopsis "buf-readerat")
    (description
     "Package buf-readerat implements buffered io.@code{ReaderAt}.  It wraps an
io.@code{ReaderAt} object, creating another io.@code{ReaderAt} object that also
implements the interface but provides buffering.")
    (license license:expat)))

(define-public go-github-com-snabb-httpreaderat
  (package
    (name "go-github-com-snabb-httpreaderat")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snabb/httpreaderat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/snabb/httpreaderat"))
    (propagated-inputs (list go-github-com-pkg-errors
                             go-github-com-avvmoto-buf-readerat))
    (home-page "https://github.com/snabb/httpreaderat")
    (synopsis "httpreaderat")
    (description
     "Package httpreaderat implements io.@code{ReaderAt} that makes HTTP Range
Requests.")
    (license license:expat)))

(define-public go-github-com-tailscale-depaware
  (package
    (name "go-github-com-tailscale-depaware")
    (version "0.0.0-20251001183927-9c2ad255ef3f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tailscale/depaware")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tailscale/depaware"))
    (propagated-inputs (list go-golang-org-x-tools go-github-com-pkg-diff))
    (home-page "https://github.com/tailscale/depaware")
    (synopsis "depaware")
    (description
     "The depaware command makes you aware of your dependencies by putting them in
your face in git and during code review.")
    (license license:bsd-3)))

(define-public go-moul-io-http2curl
  (package
    (name "go-moul-io-http2curl")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moul/http2curl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "moul.io/http2curl/v2"
      #:unpack-path "moul.io/http2curl"))
    (propagated-inputs (list go-github-com-tailscale-depaware))
    (home-page "https://moul.io/http2curl")
    (synopsis "http2curl")
    (description "📐 Convert Golang's http.Request to CURL command line.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-files-com-files-sdk-go
  (package
    (name "go-github-com-files-com-files-sdk-go")
    (version "3.2.244")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Files-com/files-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Files-com/files-sdk-go/v3"
      #:unpack-path "github.com/Files-com/files-sdk-go"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-moul-io-http2curl-v2
                             go-golang-org-x-text
                             go-github-com-winfsp-cgofuse
                             go-github-com-stretchr-testify
                             go-github-com-snabb-httpreaderat
                             go-github-com-samber-lo
                             go-github-com-sabhiram-go-gitignore
                             go-github-com-panjf2000-ants-v2
                             go-github-com-lpar-date
                             go-github-com-hashicorp-go-retryablehttp
                             go-github-com-gin-gonic-gin
                             go-github-com-dnaeon-go-vcr
                             go-github-com-chilts-sid
                             go-github-com-bradfitz-iter
                             go-github-com-appscode-go-querystring))
    (home-page "https://github.com/Files-com/files-sdk-go")
    (synopsis "Files.com Go Client")
    (description
     "The content included here should be enough to get started, but please visit our
@@url{https://developers.files.com/go/,Developer Documentation Website} for the
complete documentation.")
    (license license:expat)))

(define-public go-github-com-max-sum-base32768
  (package
    (name "go-github-com-max-sum-base32768")
    (version "0.0.0-20230304063302-18e6ce5945fd")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Max-Sum/base32768")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Max-Sum/base32768"))
    (home-page "https://github.com/Max-Sum/base32768")
    (synopsis "base32768")
    (description "go implementation of base32768, optimized for UTF-16.")
    (license license:expat)))

(define-public go-github-com-a8m-tree
  (package
    (name "go-github-com-a8m-tree")
    (version "0.0.0-20240104212747-2c8764a5f17e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a8m/tree")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/a8m/tree"))
    (home-page "https://github.com/a8m/tree")
    (synopsis "tree")
    (description
     "You can take a look on
@@url{https://github.com/a8m/tree/raw/master/cmd/tree/tree.go,(code cmd/tree)},
and @@url{http://github.com/a8m/s3tree,s3tree} or see the example below.")
    (license license:expat)))

(define-public go-github-com-aalpar-deheap
  (package
    (name "go-github-com-aalpar-deheap")
    (version "0.0.0-20210914013432-0cc84d79dec3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aalpar/deheap")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aalpar/deheap"))
    (home-page "https://github.com/aalpar/deheap")
    (synopsis "deheap")
    (description
     "Package deheap provides the implementation of a doubly ended heap.  Doubly ended
heaps are heaps with two sides, a min side and a max side.  Like normal
single-sided heaps, elements can be pushed onto and pulled off of a deheap.
deheaps have an additional Pop function, @code{PopMax}, that returns elements
from the opposite side of the ordering.")
    (license license:expat)))

(define-public go-github-com-abbot-go-http-auth
  (package
    (name "go-github-com-abbot-go-http-auth")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abbot/go-http-auth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/abbot/go-http-auth"))
    (home-page "https://github.com/abbot/go-http-auth")
    (synopsis "HTTP Authentication implementation in Go")
    (description
     "Package auth is an implementation of HTTP Basic and HTTP Digest authentication.")
    (license license:asl2.0)))

(define-public go-crawshaw-io-sqlite
  (package
    (name "go-crawshaw-io-sqlite")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/crawshaw/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "crawshaw.io/sqlite"))
    (propagated-inputs (list go-crawshaw-io-iox))
    (home-page "https://crawshaw.io/sqlite")
    (synopsis "Go interface to SQLite.")
    (description "Package sqlite provides a Go interface to SQLite 3.")
    (license license:isc)))

(define-public go-github-com-mschoch-smat
  (package
    (name "go-github-com-mschoch-smat")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mschoch/smat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mschoch/smat"))
    (home-page "https://github.com/mschoch/smat")
    (synopsis "smat – State Machine Assisted Testing")
    (description
     "The concept is simple, describe valid uses of your library as states and
actions.  States describe which actions are possible, and with what probability
they should occur.  Actions mutate the context and transition to another state.")
    (license license:asl2.0)))

(define-public go-github-com-roaringbitmap-roaring
  (package
    (name "go-github-com-roaringbitmap-roaring")
    (version "1.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RoaringBitmap/roaring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/RoaringBitmap/roaring"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-mschoch-smat
                             go-github-com-bits-and-blooms-bitset))
    (home-page "https://github.com/RoaringBitmap/roaring")
    (synopsis "roaring")
    (description
     "Package roaring is an implementation of Roaring Bitmaps in Go.  They provide
fast compressed bitmap data structures (also called bitset).  They are ideally
suited to represent sets of integers over relatively small ranges.  See
@@url{http://roaringbitmap.org,http://roaringbitmap.org} for details.")
    (license (list unknown-license! license:asl2.0))))

(define-public go-github-com-alecthomas-atomic
  (package
    (name "go-github-com-alecthomas-atomic")
    (version "0.1.0-alpha2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/atomic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/atomic"))
    (propagated-inputs (list go-github-com-alecthomas-assert-v2))
    (home-page "https://github.com/alecthomas/atomic")
    (synopsis "Type-safe atomic values for Go")
    (description "Package atomic contains type-safe atomic types.")
    (license license:expat)))

(define-public go-github-com-anacrolix-envpprof
  (package
    (name "go-github-com-anacrolix-envpprof")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/envpprof")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/envpprof"))
    (propagated-inputs (list go-github-com-anacrolix-log))
    (home-page "https://github.com/anacrolix/envpprof")
    (synopsis "envpprof")
    (description
     "Allows run-time configuration of Go's pprof features and default HTTP mux using
the environment variable @@code{GOPPROF}.  Import the package with @@code{import
_ \"github.com/anacrolix/envpprof\"}. @@code{envpprof} has an @@code{init}
function that will run at process initialization that checks the value of the
@@code{GOPPROF} environment variable.  The variable can contain a
comma-separated list of values, for example @@code{GOPPROF=http,block}.  The
supported keys are:.")
    (license license:expat)))

(define-public go-github-com-benbjohnson-immutable
  (package
    (name "go-github-com-benbjohnson-immutable")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/benbjohnson/immutable")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/benbjohnson/immutable"))
    (home-page "https://github.com/benbjohnson/immutable")
    (synopsis "Immutable")
    (description "Package immutable provides immutable collection types.")
    (license license:expat)))

(define-public go-github-com-anacrolix-stm
  (package
    (name "go-github-com-anacrolix-stm")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/stm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/stm"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-benbjohnson-immutable
                             go-github-com-anacrolix-missinggo-v2
                             go-github-com-anacrolix-missinggo
                             go-github-com-anacrolix-envpprof
                             go-github-com-alecthomas-atomic))
    (home-page "https://github.com/anacrolix/stm")
    (synopsis "stm")
    (description
     "Package stm provides Software Transactional Memory operations for Go.  This is
an alternative to the standard way of writing concurrent code (channels and
mutexes).  STM makes it easy to perform arbitrarily complex operations in an
atomic fashion.  One of its primary advantages over traditional locking is that
STM transactions are composable, whereas locking functions are not -- the
composition will either deadlock or release the lock between functions (making
it non-atomic).")
    (license license:expat)))

(define-public go-crawshaw-io-iox
  (package
    (name "go-crawshaw-io-iox")
    (version "0.0.0-20181124134642-c51c3df30797")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/crawshaw/iox")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "crawshaw.io/iox"))
    (home-page "https://crawshaw.io/iox")
    (synopsis "iox: I/O tools for Go programs")
    (description "Package iox contains I/O utilities.")
    (license license:isc)))

(define-public go-zombiezen-com-go-sqlite
  (package
    (name "go-zombiezen-com-go-sqlite")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zombiezen/go-sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "zombiezen.com/go/sqlite"))
    (propagated-inputs (list go-modernc-org-sqlite
                             go-modernc-org-libc
                             go-golang-org-x-text
                             go-github-com-google-go-cmp
                             go-github-com-chzyer-readline
                             go-crawshaw-io-iox))
    (home-page "https://zombiezen.com/go/sqlite")
    (synopsis #f)
    (description "Package sqlite provides a Go interface to SQLite 3.")
    (license license:isc)))

(define-public go-github-com-anacrolix-missinggo
  (package
    (name "go-github-com-anacrolix-missinggo")
    (version "2.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/missinggo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/missinggo/v2"
      #:unpack-path "github.com/anacrolix/missinggo"))
    (propagated-inputs (list go-zombiezen-com-go-sqlite
                             go-golang-org-x-exp
                             go-go-opencensus-io
                             go-github-com-stretchr-testify
                             go-github-com-ryszard-goskiplist
                             go-github-com-prometheus-client-model
                             go-github-com-prometheus-client-golang
                             go-github-com-huandu-xstrings
                             go-github-com-google-btree
                             go-github-com-frankban-quicktest
                             go-github-com-dustin-go-humanize
                             go-github-com-docopt-docopt-go
                             go-github-com-bradfitz-iter
                             go-github-com-anacrolix-tagflag
                             go-github-com-anacrolix-stm
                             go-github-com-anacrolix-missinggo
                             go-github-com-anacrolix-log
                             go-github-com-anacrolix-envpprof
                             go-github-com-roaringbitmap-roaring
                             go-crawshaw-io-sqlite))
    (home-page "https://github.com/anacrolix/missinggo")
    (synopsis "missinggo")
    (description
     "Package missinggo contains miscellaneous helpers used in many of anacrolix
projects.")
    (license license:expat)))

(define-public go-github-com-anacrolix-tagflag
  (package
    (name "go-github-com-anacrolix-tagflag")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/tagflag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/tagflag"))
    (propagated-inputs (list go-golang-org-x-xerrors
                             go-github-com-stretchr-testify
                             go-github-com-pkg-errors
                             go-github-com-huandu-xstrings
                             go-github-com-dustin-go-humanize
                             go-github-com-bradfitz-iter
                             go-github-com-anacrolix-missinggo-v2))
    (home-page "https://github.com/anacrolix/tagflag")
    (synopsis "tagflag")
    (description
     "Package tagflag uses reflection to derive flags and positional arguments to a
program, and parses and sets them from a slice of arguments.")
    (license license:expat)))

(define-public go-github-com-bradfitz-iter
  (package
    (name "go-github-com-bradfitz-iter")
    (version "0.0.0-20191230175014-e8f45d346db8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bradfitz/iter")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bradfitz/iter"))
    (home-page "https://github.com/bradfitz/iter")
    (synopsis #f)
    (description
     "Package iter provides a syntactically different way to iterate over integers.
That's it.")
    (license license:bsd-3)))

(define-public go-github-com-anacrolix-missinggo
  (package
    (name "go-github-com-anacrolix-missinggo")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/missinggo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/missinggo"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-ryszard-goskiplist
                             go-github-com-huandu-xstrings
                             go-github-com-google-btree
                             go-github-com-dustin-go-humanize
                             go-github-com-docopt-docopt-go
                             go-github-com-bradfitz-iter
                             go-github-com-anacrolix-tagflag
                             go-github-com-anacrolix-missinggo-v2
                             go-github-com-anacrolix-envpprof
                             go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/anacrolix/missinggo")
    (synopsis "missinggo")
    (description
     "Package missinggo contains miscellaneous helpers used in many of anacrolix
projects.")
    (license license:expat)))

(define-public go-github-com-anacrolix-ffprobe
  (package
    (name "go-github-com-anacrolix-ffprobe")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/ffprobe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/ffprobe"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-anacrolix-missinggo
                             go-github-com-anacrolix-envpprof))
    (home-page "https://github.com/anacrolix/ffprobe")
    (synopsis #f)
    (description
     "Package ffprobe wraps and interprets ffmpeg's ffprobe for Go.")
    (license license:mpl2.0)))

(define-public go-github-com-nfnt-resize
  (package
    (name "go-github-com-nfnt-resize")
    (version "0.0.0-20180221191011-83c6a9932646")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nfnt/resize")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nfnt/resize"))
    (home-page "https://github.com/nfnt/resize")
    (synopsis
     "This package is no longer being updated! Please look for alternatives if that bothers you.")
    (description "Package resize implements various image resizing methods.")
    (license license:isc)))

(define-public go-github-com-anacrolix-dms
  (package
    (name "go-github-com-anacrolix-dms")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/dms")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/dms"))
    (propagated-inputs (list go-golang-org-x-sys go-golang-org-x-net
                             go-github-com-nfnt-resize
                             go-github-com-anacrolix-log
                             go-github-com-anacrolix-ffprobe))
    (home-page "https://github.com/anacrolix/dms")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-anacrolix-generics
  (package
    (name "go-github-com-anacrolix-generics")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/generics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/generics"))
    (propagated-inputs (list go-golang-org-x-exp))
    (home-page "https://github.com/anacrolix/generics")
    (synopsis #f)
    (description #f)
    (license license:mpl2.0)))

(define-public go-github-com-anacrolix-log
  (package
    (name "go-github-com-anacrolix-log")
    (version "0.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anacrolix/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anacrolix/log"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-frankban-quicktest
                             go-github-com-anacrolix-generics))
    (home-page "https://github.com/anacrolix/log")
    (synopsis "log")
    (description
     "Package log implements a std log compatible logging system that draws some
inspiration from the @@url{https://docs.python.org/3/library/logging.html,Python
logging module} from Python's standard library.  It supports multiple handlers,
log levels, zero-allocation, scopes, custom formatting, and environment and
runtime configuration.")
    (license license:mpl2.0)))

(define-public go-github-com-buengese-sgzip
  (package
    (name "go-github-com-buengese-sgzip")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buengese/sgzip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/buengese/sgzip"))
    (home-page "https://github.com/buengese/sgzip")
    (synopsis #f)
    (description
     "Package sgzip implements a seekable version of gzip format compressed files,
compliant with @@url{https://rfc-editor.org/rfc/rfc1952.html,RFC 1952}.")
    (license license:expat)))

(define-public go-github-com-heimdalr-dag
  (package
    (name "go-github-com-heimdalr-dag")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/heimdalr/dag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/heimdalr/dag"))
    (propagated-inputs (list go-github-com-google-uuid
                             go-github-com-go-test-deep
                             go-github-com-emirpasic-gods))
    (home-page "https://github.com/heimdalr/dag")
    (synopsis "dag")
    (description "Package dag implements directed acyclic graphs (DAGs).")
    (license license:bsd-3)))

(define-public go-github-com-cloudinary-cloudinary-go
  (package
    (name "go-github-com-cloudinary-cloudinary-go")
    (version "2.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudinary/cloudinary-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudinary/cloudinary-go/v2"
      #:unpack-path "github.com/cloudinary/cloudinary-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-heimdalr-dag
                             go-github-com-gorilla-schema
                             go-github-com-google-uuid
                             go-github-com-creasty-defaults))
    (home-page "https://github.com/cloudinary/cloudinary-go")
    (synopsis "Cloudinary Go SDK")
    (description
     "The Cloudinary Go SDK allows you to quickly and easily integrate your
application with Cloudinary.  Effortlessly optimize, transform, upload and
manage your cloud's assets.")
    (license license:expat)))

(define-public go-github-com-cloudsoda-sddl
  (package
    (name "go-github-com-cloudsoda-sddl")
    (version "0.0.0-20250224235906-926454e91efc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CloudSoda/sddl")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/cloudsoda/sddl"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/cloudsoda/sddl")
    (synopsis "sddl - Windows Security Descriptor Library and CLI Tool")
    (description
     "This package provides a cross-platform Go library and command-line tool for
working with Windows Security Descriptors, providing conversion between binary
and SDDL (Security Descriptor Definition Language) string formats.")
    (license license:lgpl3)))

(define-public go-github-com-geoffgarside-ber
  (package
    (name "go-github-com-geoffgarside-ber")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/geoffgarside/ber")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/geoffgarside/ber"))
    (home-page "https://github.com/geoffgarside/ber")
    (synopsis "BER Package")
    (description
     "Package asn1 implements parsing of DER-encoded ASN.1 data structures, as defined
in ITU-T Rec X.690.")
    (license license:bsd-3)))

(define-public go-github-com-cloudsoda-go-smb2
  (package
    (name "go-github-com-cloudsoda-go-smb2")
    (version "0.0.0-20250228001242-d4c70e6251cc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CloudSoda/go-smb2")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/cloudsoda/go-smb2"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-jcmturner-gokrb5-v8
                             go-github-com-geoffgarside-ber
                             go-github-com-cloudsoda-sddl))
    (home-page "https://github.com/cloudsoda/go-smb2")
    (synopsis "smb2")
    (description "Package smb2 implements the SMB2/3 client in [MS-SMB2].")
    (license license:bsd-2)))

(define-public go-github-com-colinmarc-hdfs
  (package
    (name "go-github-com-colinmarc-hdfs")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/colinmarc/hdfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/colinmarc/hdfs/v2"
      #:unpack-path "github.com/colinmarc/hdfs"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-stretchr-testify
                             go-github-com-pborman-getopt
                             go-github-com-jcmturner-gokrb5-v8))
    (home-page "https://github.com/colinmarc/hdfs")
    (synopsis "HDFS for Go")
    (description
     "Package hdfs provides a native, idiomatic interface to HDFS. Where possible, it
mimics the functionality and signatures of the standard `os` package.")
    (license license:expat)))

(define-public go-github-com-dop251-scsu
  (package
    (name "go-github-com-dop251-scsu")
    (version "0.0.0-20220106150536-84ac88021d00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dop251/scsu")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dop251/scsu"))
    (home-page "https://github.com/dop251/scsu")
    (synopsis "SCSU")
    (description
     "This package provides a Standard Compression Scheme for Unicode implementation
in Go.")
    (license license:expat)))

(define-public go-github-com-dropbox-dropbox-sdk-go-unofficial
  (package
    (name "go-github-com-dropbox-dropbox-sdk-go-unofficial")
    (version "6.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dropbox/dropbox-sdk-go-unofficial")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dropbox/dropbox-sdk-go-unofficial/v6"
      #:unpack-path "github.com/dropbox/dropbox-sdk-go-unofficial"))
    (propagated-inputs (list go-golang-org-x-oauth2))
    (home-page "https://github.com/dropbox/dropbox-sdk-go-unofficial")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-go-darwin-apfs
  (package
    (name "go-github-com-go-darwin-apfs")
    (version "0.0.0-20211011131704-f84b94dbf348")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-darwin/apfs")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-darwin/apfs"))
    (home-page "https://github.com/go-darwin/apfs")
    (synopsis "apfs")
    (description
     "Package apfs implements an Apple File System(APFS) bindings for Go.")
    (license license:bsd-3)))

(define-public go-github-com-relvacode-iso8601
  (package
    (name "go-github-com-relvacode-iso8601")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/relvacode/iso8601")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/relvacode/iso8601"))
    (home-page "https://github.com/relvacode/iso8601")
    (synopsis "Usage")
    (description
     "Package iso8601 is a utility for parsing ISO8601 datetime strings into native Go
times.  The standard library's RFC3339 reference layout can be too strict for
working with 3rd party APIs, especially ones written in other languages.")
    (license license:expat)))

(define-public go-github-com-henrybear327-proton-api-bridge
  (package
    (name "go-github-com-henrybear327-proton-api-bridge")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/henrybear327/Proton-API-Bridge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/henrybear327/Proton-API-Bridge"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-relvacode-iso8601
                             go-github-com-henrybear327-go-proton-api
                             go-github-com-protonmail-gopenpgp-v2
                             go-github-com-protonmail-gluon))
    (home-page "https://github.com/henrybear327/Proton-API-Bridge")
    (synopsis "Proton API Bridge")
    (description
     "Thanks to Proton open sourcing
@@url{https://github.com/@code{ProtonMail/go-proton-api,proton-go-api}} and the
web, @code{iOS}, and Android client codebases, we don't need to completely
reverse engineer the APIs by observing the web client traffic!")
    (license license:expat)))

(define-public go-github-com-protonmail-go-mbox
  (package
    (name "go-github-com-protonmail-go-mbox")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/go-mbox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/go-mbox"))
    (home-page "https://github.com/ProtonMail/go-mbox")
    (synopsis "go-mbox")
    (description "Package mbox parses and formats the mbox file format.")
    (license license:expat)))

(define-public go-github-com-emersion-go-imap-uidplus
  (package
    (name "go-github-com-emersion-go-imap-uidplus")
    (version "0.0.0-20200503180755-e75854c361e9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-imap-uidplus")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-imap-uidplus"))
    (home-page "https://github.com/emersion/go-imap-uidplus")
    (synopsis "go-imap-uidplus")
    (description
     "This package implements the IMAP UIDPLUS extension, as defined in
@@url{https://rfc-editor.org/rfc/rfc4315.html,RFC 4315}.")
    (license license:expat)))

(define-public go-github-com-protonmail-gluon
  (package
    (name "go-github-com-protonmail-gluon")
    (version "0.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/gluon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/gluon"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-golang-org-x-text
                             go-golang-org-x-sys
                             go-golang-org-x-exp
                             go-go-uber-org-goleak
                             go-github-com-stretchr-testify
                             go-github-com-sirupsen-logrus
                             go-github-com-pkg-profile
                             go-github-com-pierrec-lz4-v4
                             go-github-com-mattn-go-sqlite3
                             go-github-com-google-uuid
                             go-github-com-golang-mock
                             go-github-com-emersion-go-imap-uidplus
                             go-github-com-emersion-go-imap
                             go-github-com-bradenaw-juniper
                             go-github-com-protonmail-go-mbox))
    (home-page "https://github.com/ProtonMail/gluon")
    (synopsis "Demo")
    (description
     "Package gluon implements an IMAP4rev1 (+ extensions) mailserver.")
    (license license:expat)))

(define-public go-github-com-protonmail-bcrypt
  (package
    (name "go-github-com-protonmail-bcrypt")
    (version "0.0.0-20211005172633-e235017c1baf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/bcrypt")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/bcrypt"))
    (home-page "https://github.com/ProtonMail/bcrypt")
    (synopsis "github.com/ProtonMail/bcrypt")
    (description
     "This package provides a golang implementation of the bcrypt hash algorithm.  It
is a fork of
@@url{https://github.com/jameskeane/bcrypt,github.com/jameskeane/bcrypt}.")
    (license license:bsd-3)))

(define-public go-github-com-cronokirby-saferith
  (package
    (name "go-github-com-cronokirby-saferith")
    (version "0.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cronokirby/saferith")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cronokirby/saferith"))
    (home-page "https://github.com/cronokirby/saferith")
    (synopsis "saferith")
    (description
     "The purpose of this package is to provide a version of arbitrary sized
arithmetic, in a safer (i.e.  constant-time) way, for cryptography.")
    (license license:expat)))

(define-public go-github-com-protonmail-go-srp
  (package
    (name "go-github-com-protonmail-go-srp")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/go-srp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/go-srp"))
    (propagated-inputs (list go-golang-org-x-crypto go-github-com-pkg-errors
                             go-github-com-cronokirby-saferith
                             go-github-com-protonmail-go-crypto
                             go-github-com-protonmail-bcrypt))
    (home-page "https://github.com/ProtonMail/go-srp")
    (synopsis "go-srp")
    (description
     "Golang implementation of the
@@url{https://datatracker.ietf.org/doc/html/rfc5054,SRP protocol}, used for
authentication of @code{ProtonMail} users.")
    (license license:expat)))

(define-public go-github-com-protonmail-go-mime
  (package
    (name "go-github-com-protonmail-go-mime")
    (version "0.0.0-20230322103455-7d82a3887f2f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/go-mime")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/go-mime"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/ProtonMail/go-mime")
    (synopsis "Go Mime Wrapper Library")
    (description "This package provides a parser for MIME messages.")
    (license license:expat)))

(define-public go-github-com-protonmail-gopenpgp
  (package
    (name "go-github-com-protonmail-gopenpgp")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/gopenpgp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/ProtonMail/gopenpgp/v2"
      #:unpack-path "github.com/ProtonMail/gopenpgp"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-pkg-errors
                             go-github-com-protonmail-go-mime
                             go-github-com-protonmail-go-crypto))
    (home-page "https://github.com/ProtonMail/gopenpgp")
    (synopsis "GopenPGP V2")
    (description
     "@code{GopenPGP} is a high-level @code{OpenPGP} library built on top of .")
    (license license:expat)))

(define-public go-github-com-bradenaw-juniper
  (package
    (name "go-github-com-bradenaw-juniper")
    (version "0.15.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bradenaw/juniper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bradenaw/juniper"))
    (propagated-inputs (list go-golang-org-x-sync go-golang-org-x-exp))
    (home-page "https://github.com/bradenaw/juniper")
    (synopsis "Juniper")
    (description
     "Juniper is a library of extensions to the Go standard library using generics,
including containers, iterators, and streams.")
    (license license:expat)))

(define-public go-github-com-go-resty-resty
  (package
    (name "go-github-com-go-resty-resty")
    (version "2.16.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-resty/resty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-resty/resty/v2"
      #:unpack-path "github.com/go-resty/resty"))
    (propagated-inputs (list go-golang-org-x-time go-golang-org-x-net))
    (home-page "https://github.com/go-resty/resty")
    (synopsis "News")
    (description
     "Package resty provides Simple HTTP and REST client library for Go.")
    (license license:expat)))

(define-public go-github-com-henrybear327-go-proton-api
  (package
    (name "go-github-com-henrybear327-go-proton-api")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/henrybear327/go-proton-api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/henrybear327/go-proton-api"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-golang-org-x-text
                             go-golang-org-x-net
                             go-golang-org-x-exp
                             go-go-uber-org-goleak
                             go-github-com-urfave-cli-v2
                             go-github-com-stretchr-testify
                             go-github-com-sirupsen-logrus
                             go-github-com-google-uuid
                             go-github-com-go-resty-resty-v2
                             go-github-com-gin-gonic-gin
                             go-github-com-emersion-go-vcard
                             go-github-com-emersion-go-message
                             go-github-com-bradenaw-juniper
                             go-github-com-puerkitobio-goquery
                             go-github-com-protonmail-gopenpgp-v2
                             go-github-com-protonmail-go-srp
                             go-github-com-protonmail-go-crypto
                             go-github-com-protonmail-gluon
                             go-github-com-masterminds-semver-v3))
    (home-page "https://github.com/henrybear327/go-proton-api")
    (synopsis "Go Proton API")
    (description
     "Package proton implements types for accessing the Proton API.")
    (license license:expat)))

(define-public go-github-com-akavel-rsrc
  (package
    (name "go-github-com-akavel-rsrc")
    (version "0.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/akavel/rsrc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/akavel/rsrc"))
    (home-page "https://github.com/akavel/rsrc")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-josephspurrier-goversioninfo
  (package
    (name "go-github-com-josephspurrier-goversioninfo")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josephspurrier/goversioninfo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/josephspurrier/goversioninfo"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-akavel-rsrc))
    (home-page "https://github.com/josephspurrier/goversioninfo")
    (synopsis "GoVersionInfo")
    (description
     "Package goversioninfo creates a syso file which contains Microsoft Version
Information and an optional icon.")
    (license license:expat)))

(define-public go-github-com-koofr-go-httpclient
  (package
    (name "go-github-com-koofr-go-httpclient")
    (version "0.0.0-20240520111329-e20f8f203988")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/koofr/go-httpclient")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/koofr/go-httpclient"))
    (propagated-inputs (list go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo-v2))
    (home-page "https://github.com/koofr/go-httpclient")
    (synopsis "go-httpclient")
    (description "Go HTTP client.")
    (license license:expat)))

(define-public go-github-com-koofr-go-koofrclient
  (package
    (name "go-github-com-koofr-go-koofrclient")
    (version "0.0.0-20221207135200-cbd7fc9ad6a6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/koofr/go-koofrclient")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/koofr/go-koofrclient"))
    (home-page "https://github.com/koofr/go-koofrclient")
    (synopsis "go-koofrclient")
    (description "Go Koofr client.")
    (license license:expat)))

(define-public go-github-com-lanrat-extsort
  (package
    (name "go-github-com-lanrat-extsort")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lanrat/extsort")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/lanrat/extsort"))
    (propagated-inputs (list go-golang-org-x-sync))
    (home-page "https://github.com/lanrat/extsort")
    (synopsis "extsort")
    (description
     "Package extsort implements an unstable external sort for all the records in a
chan or iterator.")
    (license license:asl2.0)))

(define-public go-github-com-ncw-swift
  (package
    (name "go-github-com-ncw-swift")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncw/swift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncw/swift/v2"
      #:unpack-path "github.com/ncw/swift"))
    (home-page "https://github.com/ncw/swift")
    (synopsis "Swift")
    (description
     "Package swift provides an easy to use interface to Swift / Openstack Object
Storage / Rackspace Cloud Files.")
    (license license:expat)))

(define-public go-github-com-sony-gobreaker
  (package
    (name "go-github-com-sony-gobreaker")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sony/gobreaker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sony/gobreaker"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/sony/gobreaker")
    (synopsis "gobreaker")
    (description
     "Package gobreaker implements the Circuit Breaker pattern.  See
@@url{https://msdn.microsoft.com/en-us/library/dn589784.aspx,https://msdn.microsoft.com/en-us/library/dn589784.aspx}.")
    (license license:expat)))

(define-public go-github-com-oracle-oci-go-sdk
  (package
    (name "go-github-com-oracle-oci-go-sdk")
    (version "65.101.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oracle/oci-go-sdk")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/oracle/oci-go-sdk/v65"
      #:unpack-path "github.com/oracle/oci-go-sdk"))
    (propagated-inputs (list go-github-com-youmark-pkcs8
                             go-github-com-stretchr-testify
                             go-github-com-sony-gobreaker
                             go-github-com-gofrs-flock))
    (home-page "https://github.com/oracle/oci-go-sdk")
    (synopsis "Oracle Cloud Infrastructure Golang SDK")
    (description
     "This is the official Go SDK for Oracle Cloud Infrastructure.")
    (license unknown-license!)))

(define-public go-github-com-peterh-liner
  (package
    (name "go-github-com-peterh-liner")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peterh/liner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/peterh/liner"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/peterh/liner")
    (synopsis "Liner")
    (description
     "Package liner implements a simple command line editor, inspired by linenoise
(@@url{https://github.com/antirez/linenoise/,https://github.com/antirez/linenoise/}).
 This package supports WIN32 in addition to the xterm codes supported by
everything else.")
    (license license:expat)))

(define-public go-github-com-putdotio-go-putio-putio
  (package
    (name "go-github-com-putdotio-go-putio-putio")
    (version "0.0.0-20200123120452-16d982cac2b8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/putdotio/go-putio")
             (commit (go-version->git-ref version
                                          #:subdir "putio"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/putdotio/go-putio/putio"
      #:unpack-path "github.com/putdotio/go-putio"))
    (home-page "https://github.com/putdotio/go-putio")
    (synopsis #f)
    (description "Package putio is the Put.io API v2 client for Go.")
    (license license:expat)))

(define-public go-github-com-quasilyte-go-ruleguard-dsl
  (package
    (name "go-github-com-quasilyte-go-ruleguard-dsl")
    (version "0.3.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quasilyte/go-ruleguard")
             (commit (go-version->git-ref version
                                          #:subdir "dsl"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/quasilyte/go-ruleguard/dsl"
      #:unpack-path "github.com/quasilyte/go-ruleguard"))
    (home-page "https://github.com/quasilyte/go-ruleguard")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-minio-xxml
  (package
    (name "go-github-com-minio-xxml")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/xxml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/xxml"))
    (home-page "https://github.com/minio/xxml")
    (synopsis "xxml")
    (description
     "Package xml implements a simple XML 1.0 parser that understands XML name spaces.")
    (license license:bsd-3)))

(define-public go-github-com-ryszard-goskiplist
  (package
    (name "go-github-com-ryszard-goskiplist")
    (version "0.0.0-20150312221310-2dfbae5fcf46")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryszard/goskiplist")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ryszard/goskiplist"))
    (home-page "https://github.com/ryszard/goskiplist")
    (synopsis "About")
    (description
     "This is a library implementing skip lists for the Go programming language
(@@url{http://golang.org/,http://golang.org/}).")
    (license license:asl2.0)))

(define-public go-github-com-shabbyrobe-gocovmerge
  (package
    (name "go-github-com-shabbyrobe-gocovmerge")
    (version "0.0.0-20230507112040-c3350d9342df")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shabbyrobe/gocovmerge")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shabbyrobe/gocovmerge"))
    (propagated-inputs (list go-golang-org-x-tools))
    (home-page "https://github.com/shabbyrobe/gocovmerge")
    (synopsis "gocovmerge")
    (description
     "Package gocovmerge takes the results from multiple `go test -coverprofile` runs
and merges them into one profile.")
    (license license:bsd-2)))

(define-public go-github-com-rclone-gofakes3
  (package
    (name "go-github-com-rclone-gofakes3")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rclone/gofakes3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rclone/gofakes3"))
    (propagated-inputs (list go-golang-org-x-tools
                        go-github-com-stretchr-testify
                        go-github-com-shabbyrobe-gocovmerge
                        go-github-com-ryszard-goskiplist
                        go-github-com-minio-xxml
                        go-github-com-aws-smithy-go
                        go-github-com-aws-aws-sdk-go-v2-service-s3
                        go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
                        go-github-com-aws-aws-sdk-go-v2-credentials
                        go-github-com-aws-aws-sdk-go-v2))
    (home-page "https://github.com/rclone/gofakes3")
    (synopsis #f)
    (description "Package s3 implements a fake s3 server for rclone.")
    (license license:expat)))

(define-public go-github-com-ebitengine-purego
  (package
    (name "go-github-com-ebitengine-purego")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ebitengine/purego")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ebitengine/purego"))
    (home-page "https://github.com/ebitengine/purego")
    (synopsis "purego")
    (description
     "This package provides a library for calling C functions from Go without Cgo.")
    (license license:asl2.0)))

(define-public go-github-com-lufia-plan9stats
  (package
    (name "go-github-com-lufia-plan9stats")
    (version "0.0.0-20250827001030-24949be3fa54")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lufia/plan9stats")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lufia/plan9stats"))
    (propagated-inputs (list go-github-com-google-go-cmp))
    (home-page "https://github.com/lufia/plan9stats")
    (synopsis "plan9stats")
    (description "Package stats provides statistic utilities for Plan 9.")
    (license license:bsd-3)))

(define-public go-github-com-power-devops-perfstat
  (package
    (name "go-github-com-power-devops-perfstat")
    (version "0.0.0-20240221224432-82ca36839d55")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/power-devops/perfstat")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/power-devops/perfstat"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/power-devops/perfstat")
    (synopsis #f)
    (description
     "Copyright 2020 Power-Devops.com.  All rights reserved.  Use of this source code
is governed by the license that can be found in the LICENSE file.")
    (license license:expat)))

(define-public go-github-com-go-ole-go-ole
  (package
    (name "go-github-com-go-ole-go-ole")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ole/go-ole")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ole/go-ole"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/go-ole/go-ole")
    (synopsis "Go OLE")
    (description
     "Go bindings for Windows COM using shared libraries instead of cgo.")
    (license license:expat)))

(define-public go-github-com-yusufpapurcu-wmi
  (package
    (name "go-github-com-yusufpapurcu-wmi")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yusufpapurcu/wmi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yusufpapurcu/wmi"))
    (propagated-inputs (list go-github-com-go-ole-go-ole))
    (home-page "https://github.com/yusufpapurcu/wmi")
    (synopsis "wmi")
    (description "Package wmi provides a WQL interface for WMI on Windows.")
    (license license:expat)))

(define-public go-github-com-shirou-gopsutil
  (package
    (name "go-github-com-shirou-gopsutil")
    (version "4.25.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/shirou/gopsutil/v4"
      #:unpack-path "github.com/shirou/gopsutil"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-yusufpapurcu-wmi
                             go-github-com-tklauser-go-sysconf
                             go-github-com-stretchr-testify
                             go-github-com-power-devops-perfstat
                             go-github-com-lufia-plan9stats
                             go-github-com-google-go-cmp
                             go-github-com-ebitengine-purego))
    (home-page "https://github.com/shirou/gopsutil")
    (synopsis "gopsutil: psutil for golang")
    (description "SPDX-License-Identifier: BSD-3-Clause.")
    (license license:bsd-3)))

(define-public go-github-com-t3rm1n4l-go-mega
  (package
    (name "go-github-com-t3rm1n4l-go-mega")
    (version "0.0.0-20250926104142-ccb8d3498e6c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/t3rm1n4l/go-mega")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/t3rm1n4l/go-mega"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/t3rm1n4l/go-mega")
    (synopsis "go-mega")
    (description
     "This package provides a client library in go for mega.co.nz storage service.")
    (license license:expat)))

(define-public go-github-com-unknwon-goconfig
  (package
    (name "go-github-com-unknwon-goconfig")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unknwon/goconfig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unknwon/goconfig"))
    (home-page "https://github.com/unknwon/goconfig")
    (synopsis "goconfig")
    (description
     "Package goconfig is a fully functional and comments-support configuration
file(.ini) parser.")
    (license license:asl2.0)))

(define-public go-github-com-rasky-go-xdr
  (package
    (name "go-github-com-rasky-go-xdr")
    (version "0.0.0-20170124162913-1a41d1a06c93")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rasky/go-xdr")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rasky/go-xdr"))
    (home-page "https://github.com/rasky/go-xdr")
    (synopsis "go-xdr")
    (description
     "[]
(@@url{https://travis-ci.org/davecgh/go-xdr,https://travis-ci.org/davecgh/go-xdr})
[![Coverage Status]
(@@url{https://coveralls.io/repos/davecgh/go-xdr/badge.png?branch=master)%5D,https://coveralls.io/repos/davecgh/go-xdr/badge.png?branch=master)]}
(@@url{https://coveralls.io/r/davecgh/go-xdr?branch=master,https://coveralls.io/r/davecgh/go-xdr?branch=master}).")
    (license license:isc)))

(define-public go-github-com-willscott-go-nfs-client
  (package
    (name "go-github-com-willscott-go-nfs-client")
    (version "0.0.0-20240104095149-b44639837b00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/willscott/go-nfs-client")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/willscott/go-nfs-client"))
    (propagated-inputs (list go-github-com-rasky-go-xdr))
    (home-page "https://github.com/willscott/go-nfs-client")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-polydawn-go-timeless-api
  (package
    (name "go-github-com-polydawn-go-timeless-api")
    (version "0.0.0-20220821201550-b93919e12c56")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/polydawn/go-timeless-api")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/polydawn/go-timeless-api"))
    (propagated-inputs (list go-github-com-warpfork-go-wish
                             go-github-com-warpfork-go-errcat
                             go-github-com-polydawn-refmt))
    (home-page "https://github.com/polydawn/go-timeless-api")
    (synopsis "Timeless Stack APIs for Go(lang)")
    (description "Go(lang) APIs for the Timeless Stack
(@@url{https://github.com/polydawn/timeless/,📖 docs here}.")
    (license license:asl2.0)))

(define-public go-github-com-syndtr-gocapability
  (package
    (name "go-github-com-syndtr-gocapability")
    (version "0.0.0-20200815063812-42c35b437635")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syndtr/gocapability")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/syndtr/gocapability"))
    (home-page "https://github.com/syndtr/gocapability")
    (synopsis #f)
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-warpfork-go-errcat
  (package
    (name "go-github-com-warpfork-go-errcat")
    (version "0.0.0-20180917083543-335044ffc86e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/warpfork/go-errcat")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/warpfork/go-errcat"))
    (home-page "https://github.com/warpfork/go-errcat")
    (synopsis "errcat")
    (description
     "errcat is a simple universal error type that helps you produce errors that are
both easy to categorize and handle, and also easy to maintain the original
messages of.")
    (license license:asl2.0)))

(define-public go-github-com-polydawn-rio
  (package
    (name "go-github-com-polydawn-rio")
    (version "0.0.0-20220823181337-7c31ad9831a4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/polydawn/rio")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/polydawn/rio"))
    (propagated-inputs (list go-gopkg-in-src-d-go-git-v4
                             go-gopkg-in-src-d-go-billy-v4
                             go-gopkg-in-alecthomas-kingpin-v2
                             go-golang-org-x-sys
                             go-github-com-xi2-xz
                             go-github-com-warpfork-go-errcat
                             go-github-com-syndtr-gocapability
                             go-github-com-smartystreets-goconvey
                             go-github-com-polydawn-refmt
                             go-github-com-polydawn-go-timeless-api))
    (home-page "https://github.com/polydawn/rio")
    (synopsis "rio")
    (description "@@strong{R}epeatable @@strong{I}/@@strong{O}.")
    (license license:asl2.0)))

(define-public go-github-com-willscott-memphis
  (package
    (name "go-github-com-willscott-memphis")
    (version "0.0.0-20241203204924-a148a489d367")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/willscott/memphis")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/willscott/memphis"))
    (propagated-inputs (list go-github-com-polydawn-rio
                             go-github-com-go-git-go-billy-v5))
    (home-page "https://github.com/willscott/memphis")
    (synopsis "Memphis")
    (description "Status: Minimum Viable.")
    (license license:asl2.0)))

(define-public go-github-com-willscott-go-nfs
  (package
    (name "go-github-com-willscott-go-nfs")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/willscott/go-nfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/willscott/go-nfs"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-willscott-memphis
                             go-github-com-willscott-go-nfs-client
                             go-github-com-rasky-go-xdr
                             go-github-com-hashicorp-golang-lru-v2
                             go-github-com-google-uuid
                             go-github-com-go-git-go-billy-v5))
    (home-page "https://github.com/willscott/go-nfs")
    (synopsis "Golang Network File Server")
    (description "NFSv3 protocol implementation in pure Golang.")
    (license license:asl2.0)))

(define-public go-github-com-winfsp-cgofuse
  (package
    (name "go-github-com-winfsp-cgofuse")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/winfsp/cgofuse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/winfsp/cgofuse"))
    (home-page "https://github.com/winfsp/cgofuse")
    (synopsis "Cross-platform FUSE library for Go")
    (description
     "User mode file systems are expected to implement
@@code{fuse.@code{FileSystemInterface}}.  To make implementation simpler a file
system can embed (\"inherit\") a @@code{fuse.@code{FileSystemBase}} which provides
default implementations for all operations.  To mount a file system one must
instantiate a @@code{fuse.@code{FileSystemHost}} using
@@code{fuse.@code{NewFileSystemHost}}.")
    (license license:expat)))

(define-public go-github-com-pengsrc-go-shared
  (package
    (name "go-github-com-pengsrc-go-shared")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pengsrc/go-shared")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pengsrc/go-shared"))
    (home-page "https://github.com/pengsrc/go-shared")
    (synopsis "go-shared")
    (description "Useful packages for the Go programming language.")
    (license license:asl2.0)))

(define-public go-github-com-yunify-qingstor-sdk-go
  (package
    (name "go-github-com-yunify-qingstor-sdk-go")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qingstor/qingstor-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yunify/qingstor-sdk-go/v3"
      #:unpack-path "github.com/yunify/qingstor-sdk-go"))
    (propagated-inputs (list go-gopkg-in-yaml-v2
                             go-github-com-stretchr-testify
                             go-github-com-pengsrc-go-shared))
    (home-page "https://github.com/yunify/qingstor-sdk-go")
    (synopsis "QingStor SDK for Go")
    (description
     "Package sdk is the official @code{QingStor} SDK for the Go programming language.")
    (license license:asl2.0)))

(define-public go-github-com-minio-crc64nvme
  (package
    (name "go-github-com-minio-crc64nvme")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/crc64nvme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/minio/crc64nvme"))
    (propagated-inputs (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/minio/crc64nvme")
    (synopsis "crc64nvme")
    (description
     "Package crc64nvme implements the 64-bit cyclic redundancy check with NVME
polynomial.")
    (license license:asl2.0)))

(define-public go-github-com-minio-md5-simd
  (package
    (name "go-github-com-minio-md5-simd")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/md5-simd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/md5-simd"))
    (propagated-inputs (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/minio/md5-simd")
    (synopsis "md5-simd")
    (description
     "This is a SIMD accelerated MD5 package, allowing up to either 8 (AVX2) or 16
(AVX512) independent MD5 sums to be calculated on a single CPU core.")
    (license license:asl2.0)))

(define-public go-github-com-minio-minio-go
  (package
    (name "go-github-com-minio-minio-go")
    (version "7.0.95")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/minio-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/minio/minio-go/v7"
      #:unpack-path "github.com/minio/minio-go"))
    (propagated-inputs (list go-golang-org-x-net
                             go-golang-org-x-crypto
                             go-github-com-tinylib-msgp
                             go-github-com-rs-xid
                             go-github-com-minio-md5-simd
                             go-github-com-minio-crc64nvme
                             go-github-com-klauspost-compress
                             go-github-com-google-uuid
                             go-github-com-goccy-go-json
                             go-github-com-go-ini-ini
                             go-github-com-dustin-go-humanize))
    (home-page "https://github.com/minio/minio-go")
    (synopsis "MinIO Go Client SDK for Amazon S3 Compatible Cloud Storage")
    (description
     "The @code{MinIO} Go Client SDK provides straightforward APIs to access any
Amazon S3 compatible object storage.")
    (license license:asl2.0)))

(define-public go-goftp-io-server
  (package
    (name "go-goftp-io-server")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/goftp/server")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12gyq0jpp03bxzas5439d4a7shqak7vg7s9q7j4fa1vq5n2pd2qn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "goftp.io/server/v2"
      #:unpack-path "goftp.io/server"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-minio-minio-go-v7
                             go-github-com-jlaffaye-ftp))
    (home-page "https://goftp.io/server")
    (synopsis "server")
    (description
     "This package provides a FTP server framework forked from
@@url{http://github.com/yob/graval,github.com/yob/graval} and changed a lot.")
    (license license:expat)))

(define-public go-cloud-google-com-go-profiler
  (package
    (name "go-cloud-google-com-go-profiler")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "profiler"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/profiler"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-pprof
                             go-github-com-golang-mock
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-compute-metadata
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Profiler")
    (description
     "Package profiler is a client for the Cloud Profiler service.")
    (license license:asl2.0)))

(define-public go-github-com-bmkessler-fastdiv
  (package
    (name "go-github-com-bmkessler-fastdiv")
    (version "0.0.0-20190227075523-41d5178f2044")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmkessler/fastdiv")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bmkessler/fastdiv"))
    (home-page "https://github.com/bmkessler/fastdiv")
    (synopsis "fastdiv")
    (description
     "Package fastdiv implements fast division, modulus and divisibility checks for
divisors known only at runtime via the method of:.")
    (license license:expat)))

(define-public go-github-com-calebcase-tmpfile
  (package
    (name "go-github-com-calebcase-tmpfile")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/calebcase/tmpfile")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/calebcase/tmpfile"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/calebcase/tmpfile")
    (synopsis "Cross Platform Temporary Files")
    (description
     "Package tmpfile provides a cross platform facility for creating temporary files
that are automatically cleaned up (even in the event of an unexpected process
exit).")
    (license license:expat)))

(define-public go-github-com-jtolds-tracetagger
  (package
    (name "go-github-com-jtolds-tracetagger")
    (version "2.0.0-rc5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtolio/tracetagger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jtolds/tracetagger/v2"
      #:unpack-path "github.com/jtolds/tracetagger"))
    (propagated-inputs (list go-github-com-spacemonkeygo-monkit-v3))
    (home-page "https://github.com/jtolds/tracetagger")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-jtolio-crawlspace
  (package
    (name "go-github-com-jtolio-crawlspace")
    (version "0.0.0-20240521193440-69abbbe5a93f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtolio/crawlspace")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jtolio/crawlspace"))
    (home-page "https://github.com/jtolio/crawlspace")
    (synopsis "crawlspace")
    (description
     "package crawlspace provides a means to dynamically interact with registered Go
objects in a live process, using small scripting language based around the
reflect package.")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-goof
  (package
    (name "go-github-com-zeebo-goof")
    (version "0.0.0-20240528180144-f1d11ca928f2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/goof")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/goof"))
    (propagated-inputs (list go-github-com-zeebo-errs))
    (home-page "https://github.com/zeebo/goof")
    (synopsis "Goof")
    (description "package goof is something you should never import.")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-sudo
  (package
    (name "go-github-com-zeebo-sudo")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/sudo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/sudo"))
    (home-page "https://github.com/zeebo/sudo")
    (synopsis "Sudo")
    (description
     "sudo is a package to make reflect more powerful (and dangerous).")
    (license license:expat)))

(define-public go-github-com-jtolio-crawlspace-tools
  (package
    (name "go-github-com-jtolio-crawlspace-tools")
    (version "0.0.0-20240521193440-69abbbe5a93f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtolio/crawlspace")
             (commit (go-version->git-ref version
                                          #:subdir "tools"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jtolio/crawlspace/tools"
      #:unpack-path "github.com/jtolio/crawlspace"))
    (propagated-inputs (list go-github-com-zeebo-sudo go-github-com-zeebo-goof
                             go-github-com-kr-pretty
                             go-github-com-jtolio-crawlspace))
    (home-page "https://github.com/jtolio/crawlspace")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-dsnet-try
  (package
    (name "go-github-com-dsnet-try")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dsnet/try")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dsnet/try"))
    (home-page "https://github.com/dsnet/try")
    (synopsis "Try: Simplified Error Handling in Go")
    (description
     "Package try emulates aspects of the ill-fated \"try\" proposal using generics.
See @@url{https://golang.org/issue/32437,https://golang.org/issue/32437} for
inspiration.")
    (license license:bsd-3)))

(define-public go-github-com-jtolio-noiseconn
  (package
    (name "go-github-com-jtolio-noiseconn")
    (version "0.0.0-20231127013910-f6d9ecbf1de7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtolio/noiseconn")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jtolio/noiseconn"))
    (propagated-inputs (list go-golang-org-x-sync go-github-com-zeebo-errs
                             go-github-com-flynn-noise go-github-com-dsnet-try))
    (home-page "https://github.com/jtolio/noiseconn")
    (synopsis "noiseconn")
    (description
     "This package provides a net.Conn wrapper around github.com/flynn/noise.")
    (license license:expat)))

(define-public go-github-com-zeebo-float16
  (package
    (name "go-github-com-zeebo-float16")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/float16")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/float16"))
    (home-page "https://github.com/zeebo/float16")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-zeebo-incenc
  (package
    (name "go-github-com-zeebo-incenc")
    (version "0.0.0-20180505221441-0d92902eec54")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/incenc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/incenc"))
    (home-page "https://github.com/zeebo/incenc")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-zeebo-admission
  (package
    (name "go-github-com-zeebo-admission")
    (version "3.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/admission")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/admission/v3"
      #:unpack-path "github.com/zeebo/admission"))
    (propagated-inputs (list go-github-com-zeebo-incenc
                             go-github-com-zeebo-float16
                             go-github-com-zeebo-errs
                             go-github-com-zeebo-assert
                             go-github-com-spacemonkeygo-monkit-v3))
    (home-page "https://github.com/zeebo/admission")
    (synopsis #f)
    (description "package admission is a fast way to ingest/send metrics.")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-structs
  (package
    (name "go-github-com-zeebo-structs")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/structs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/structs"))
    (propagated-inputs (list go-github-com-zeebo-errs
                             go-github-com-zeebo-assert
                             go-github-com-spf13-cast))
    (home-page "https://github.com/zeebo/structs")
    (synopsis "Structs")
    (description "Option controls the operation of a Decode.")
    (license license:expat)))

(define-public go-github-com-zeebo-mwc
  (package
    (name "go-github-com-zeebo-mwc")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/mwc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/mwc"))
    (propagated-inputs (list go-github-com-zeebo-assert))
    (home-page "https://github.com/zeebo/mwc")
    (synopsis #f)
    (description #f)
    (license license:cc0)))

(define-public go-storj-io-monkit-jaeger
  (package
    (name "go-storj-io-monkit-jaeger")
    (version "0.0.0-20250523220404-454c1b072fad")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/monkit-jaeger")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "storj.io/monkit-jaeger"))
    (propagated-inputs (list go-storj-io-common
                             go-golang-org-x-sync
                             go-go-uber-org-zap
                             go-github-com-zeebo-mwc
                             go-github-com-zeebo-errs
                             go-github-com-stretchr-testify
                             go-github-com-spacemonkeygo-monkit-v3
                             go-github-com-apache-thrift))
    (home-page "https://storj.io/monkit-jaeger")
    (synopsis "monkit-jaeger")
    (description
     "Package jaeger provides a monkit plugin for sending traces to Jaeger Agent.")
    (license license:asl2.0)))

(define-public go-storj-io-common
  (package
    (name "go-storj-io-common")
    (version "0.0.0-20250918032746-784a656bec7e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/common")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "storj.io/common"))
    (propagated-inputs (list go-storj-io-picobuf
                             go-storj-io-monkit-jaeger
                             go-storj-io-eventkit
                             go-storj-io-drpc
                             go-gopkg-in-yaml-v2
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-golang-org-x-crypto
                             go-go-uber-org-zap
                             go-github-com-zeebo-structs
                             go-github-com-zeebo-errs
                             go-github-com-zeebo-blake3
                             go-github-com-zeebo-admission-v3
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-spf13-pflag
                             go-github-com-spf13-cobra
                             go-github-com-spf13-cast
                             go-github-com-spacemonkeygo-monkit-v3
                             go-github-com-shopspring-decimal
                             go-github-com-quic-go-quic-go
                             go-github-com-jtolio-noiseconn
                             go-github-com-jtolio-crawlspace-tools
                             go-github-com-jtolio-crawlspace
                             go-github-com-jtolds-tracetagger-v2
                             go-github-com-google-pprof
                             go-github-com-google-gopacket
                             go-github-com-gogo-protobuf
                             go-github-com-flynn-noise
                             go-github-com-calebcase-tmpfile
                             go-github-com-bmkessler-fastdiv
                             go-github-com-blang-semver-v4
                             go-cloud-google-com-go-profiler))
    (home-page "https://storj.io/common")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-zeebo-errs
  (package
    (name "go-github-com-zeebo-errs")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/errs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/errs"))
    (home-page "https://github.com/zeebo/errs")
    (synopsis "errs")
    (description
     "Package errs provides a simple error package with stack traces.")
    (license license:expat)))

(define-public go-storj-io-drpc
  (package
    (name "go-storj-io-drpc")
    (version "0.0.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/drpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "storj.io/drpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-zeebo-errs
                             go-github-com-zeebo-assert))
    (home-page "https://storj.io/drpc")
    (synopsis #f)
    (description "Package drpc is a light replacement for gprc.")
    (license license:expat)))

(define-public go-github-com-elek-bubbles
  (package
    (name "go-github-com-elek-bubbles")
    (version "0.0.0-20230923192006-860c0efc50ae")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elek/bubbles")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elek/bubbles"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-charmbracelet-lipgloss
                             go-github-com-charmbracelet-bubbletea
                             go-github-com-charmbracelet-bubbles))
    (home-page "https://github.com/elek/bubbles")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-spacemonkeygo-monkit
  (package
    (name "go-github-com-spacemonkeygo-monkit")
    (version "3.0.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spacemonkeygo/monkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spacemonkeygo/monkit/v3"
      #:unpack-path "github.com/spacemonkeygo/monkit"))
    (home-page "https://github.com/spacemonkeygo/monkit")
    (synopsis #f)
    (description
     "Package monkit is a flexible code instrumenting and data collection library.")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-errs
  (package
    (name "go-github-com-zeebo-errs")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/errs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/errs/v2"
      #:unpack-path "github.com/zeebo/errs"))
    (propagated-inputs (list go-github-com-zeebo-assert))
    (home-page "https://github.com/zeebo/errs")
    (synopsis "errs/v2")
    (description
     "Package errs provides a simple error package with stack traces.")
    (license license:expat)))

(define-public go-cloud-google-com-go-auth-oauth2adapt
  (package
    (name "go-cloud-google-com-go-auth-oauth2adapt")
    (version "0.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "auth/oauth2adapt"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/auth/oauth2adapt"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-golang-org-x-oauth2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-auth))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description "Package oauth2adapt helps converts types used in
@@url{/cloud.google.com/go/auth,cloud.google.com/go/auth} and
@@url{/golang.org/x/oauth2,golang.org/x/oauth2}.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-accessapproval
  (package
    (name "go-cloud-google-com-go-accessapproval")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "accessapproval"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/accessapproval"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Access Approval API")
    (description "Go Client Library for Access Approval API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-aiplatform
  (package
    (name "go-cloud-google-com-go-aiplatform")
    (version "1.103.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "aiplatform"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/aiplatform"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Vertex AI API")
    (description "Go Client Library for Vertex AI API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-analytics
  (package
    (name "go-cloud-google-com-go-analytics")
    (version "0.30.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "analytics"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/analytics"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Analytics API")
    (description "Go Client Library for Analytics API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigateway
  (package
    (name "go-cloud-google-com-go-apigateway")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigateway"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/apigateway"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "API Gateway API")
    (description "Go Client Library for API Gateway API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigeeconnect
  (package
    (name "go-cloud-google-com-go-apigeeconnect")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigeeconnect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/apigeeconnect"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Apigee Connect API")
    (description "Go Client Library for Apigee Connect API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigeeregistry
  (package
    (name "go-cloud-google-com-go-apigeeregistry")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigeeregistry"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/apigeeregistry"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Apigee Registry API")
    (description "Go Client Library for Apigee Registry API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-appengine
  (package
    (name "go-cloud-google-com-go-appengine")
    (version "1.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "appengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/appengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "App Engine Admin API")
    (description "Go Client Library for App Engine Admin API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-area120
  (package
    (name "go-cloud-google-com-go-area120")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "area120"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/area120"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Area120 API")
    (description "Go Client Library for Area120 API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-artifactregistry
  (package
    (name "go-cloud-google-com-go-artifactregistry")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "artifactregistry"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/artifactregistry"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Artifact Registry API")
    (description "Go Client Library for Artifact Registry API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-accesscontextmanager
  (package
    (name "go-cloud-google-com-go-accesscontextmanager")
    (version "1.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "accesscontextmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/accesscontextmanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Access Context Manager API")
    (description "Go Client Library for Access Context Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-asset
  (package
    (name "go-cloud-google-com-go-asset")
    (version "1.21.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "asset"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/asset"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-osconfig
                             go-cloud-google-com-go-orgpolicy
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-accesscontextmanager))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Asset API")
    (description "Go Client Library for Cloud Asset API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-assuredworkloads
  (package
    (name "go-cloud-google-com-go-assuredworkloads")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "assuredworkloads"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/assuredworkloads"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Assured Workloads API")
    (description "Go Client Library for Assured Workloads API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-automl
  (package
    (name "go-cloud-google-com-go-automl")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "automl"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/automl"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud AutoML API")
    (description "Go Client Library for Cloud @code{AutoML} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-baremetalsolution
  (package
    (name "go-cloud-google-com-go-baremetalsolution")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "baremetalsolution"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/baremetalsolution"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Bare Metal Solution API")
    (description "Go Client Library for Bare Metal Solution API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-batch
  (package
    (name "go-cloud-google-com-go-batch")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "batch"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/batch"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Batch API")
    (description "Go Client Library for Batch API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-beyondcorp
  (package
    (name "go-cloud-google-com-go-beyondcorp")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "beyondcorp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/beyondcorp"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "BeyondCorp API")
    (description "Go Client Library for @code{BeyondCorp} API.")
    (license license:asl2.0)))

(define-public go-github-com-apache-thrift
  (package
    (name "go-github-com-apache-thrift")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/apache/thrift"))
    (home-page "https://github.com/apache/thrift")
    (synopsis "Apache Thrift")
    (description
     "Thrift is a lightweight, language-independent software stack for point-to-point
RPC implementation.  Thrift provides clean abstractions and implementations for
data transport, data serialization, and application level processing.  The code
generation system takes a simple definition language as input and generates code
across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.")
    (license unknown-license!)))

(define-public go-github-com-google-flatbuffers
  (package
    (name "go-github-com-google-flatbuffers")
    (version "25.9.23+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/flatbuffers")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/flatbuffers"))
    (home-page "https://github.com/google/flatbuffers")
    (synopsis "FlatBuffers")
    (description
     "@@strong{@code{FlatBuffers}} is a cross platform serialization library
architected for maximum memory efficiency.  It allows you to directly access
serialized data without parsing/unpacking it first, while still having great
forwards/backwards compatibility.")
    (license license:asl2.0)))

(define-public go-github-com-minio-asm2plan9s
  (package
    (name "go-github-com-minio-asm2plan9s")
    (version "0.0.0-20200509001527-cdd76441f9d8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/asm2plan9s")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/asm2plan9s"))
    (home-page "https://github.com/minio/asm2plan9s")
    (synopsis "asm2plan9s")
    (description
     "Tool to generate BYTE sequences for Go assembly as generated by YASM/GAS (for
Intel) or GAS (for ARM).")
    (license license:asl2.0)))

(define-public go-github-com-minio-c2goasm
  (package
    (name "go-github-com-minio-c2goasm")
    (version "0.0.0-20190812172519-36a3d3bbc4f3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/c2goasm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/c2goasm"))
    (home-page "https://github.com/minio/c2goasm")
    (synopsis "c2goasm: C to Go Assembly")
    (description
     "This is a tool to convert assembly as generated by a C/C++ compiler into Golang
assembly.  It is meant to be used in combination with
@@url{https://github.com/minio/asm2plan9s,asm2plan9s} in order to automatically
generate pure Go wrappers for C/C++ code (that may for instance take advantage
of compiler SIMD intrinsics or @@code{template<>} code).")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-xxh3
  (package
    (name "go-github-com-zeebo-xxh3")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/xxh3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/xxh3"))
    (propagated-inputs (list go-github-com-zeebo-assert
                             go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/zeebo/xxh3")
    (synopsis "XXH3")
    (description
     "This package is a port of the
@@url{https://github.com/Cyan4973/@code{xxHash,xxh3}} library to Go.")
    (license license:bsd-2)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "4.26.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r978xlqp3g52bfcg7bs19cvgdgr2vlp9y6qh53aiqwr53acsj7i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v4"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-sortutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-ccorpus2
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v4")
    (description "Package cc is a C99 compiler front end.")
    (license license:bsd-3)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "3.41.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khw9qsaz4ab0vb4kazgfm481cjpcyxj6ld2ma4d9hva3ca9h8ji"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v3"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-mathutil
                             go-lukechampine-com-uint128
                             go-github-com-google-go-cmp
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v3")
    (description "Package cc is a C99 compiler front end (Work in progress).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccorpus
  (package
    (name "go-modernc-org-ccorpus")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccorpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18d5npw8aw5qzy6qcrlrili2zxvmc2v4kkwjps6c3ayvi7aj7j09"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccorpus"))
    (propagated-inputs (list go-modernc-org-httpfs))
    (home-page "https://modernc.org/ccorpus")
    (synopsis "ccorpus")
    (description "Package ccorpus provides a test corpus of C code.")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo
  (package
    (name "go-modernc-org-ccgo")
    (version "3.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkxzhmkm5pgalcnlhw0whiii6vhdpnnnpwkx9b5ah21ajk6qqlc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccgo/v3"
      #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs (list go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-ccorpus
                             go-modernc-org-ccgo-v4
                             go-modernc-org-cc-v3
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-github-com-pmezard-go-difflib
                             go-github-com-kballard-go-shellquote
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo")
    (synopsis "ccgo/v3")
    (description "Package ccgo translates C to Go source code.")
    (license license:bsd-3)))

(define-public go-modernc-org-lex
  (package
    (name "go-modernc-org-lex")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/lex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fad093cdkgdwk3sf0vklk05qzkis1ivri3hig1wigv4z908nmdj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/lex"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-fileutil))
    (home-page "https://modernc.org/lex")
    (synopsis #f)
    (description
     "Package lex provides support for a *nix (f)lex like tool on .l sources.  The
syntax is similar to a subset of (f)lex, see also:
@@url{http://flex.sourceforge.net/manual/Format.html#Format,http://flex.sourceforge.net/manual/Format.html#Format}.")
    (license license:bsd-3)))

(define-public go-modernc-org-scannertest
  (package
    (name "go-modernc-org-scannertest")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/scannertest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06hk8pqaihhmfxfprg1fmdl2y8ffvrblm10z7qq3l921jjxc1ch7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/scannertest"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-lex))
    (home-page "https://modernc.org/scannertest")
    (synopsis #f)
    (description #f)
    (license unknown-license!)))

(define-public go-modernc-org-gc
  (package
    (name "go-modernc-org-gc")
    (version "2.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/gc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06bxad6md34ngxfgbwqxwdfw2cgkr2i4s50zwy9afqymds1nlmvh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/gc/v2"
      #:unpack-path "modernc.org/gc"))
    (propagated-inputs (list go-modernc-org-token go-modernc-org-scannertest
                             go-github-com-pmezard-go-difflib
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/gc")
    (synopsis "gc")
    (description
     "Package GC is a Go compiler front end. (Work in progress, API unstable).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo
  (package
    (name "go-modernc-org-ccgo")
    (version "4.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cklsi5j23syqnmwqj0qy29isb3vl9lh3h1pij3nhyc7swxby1nh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "modernc.org/ccgo/v4"
      #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs (list go-modernc-org-strutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-gc-v2
                             go-modernc-org-fileutil
                             go-modernc-org-ccorpus2
                             go-modernc-org-ccgo-v3
                             go-modernc-org-cc-v4
                             go-golang-org-x-tools
                             go-golang-org-x-mod
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo")
    (synopsis "ccgo/v4")
    (description "Command ccgo is a C compiler producing Go code.")
    (license license:bsd-3)))

(define-public go-modernc-org-goabi0
  (package
    (name "go-modernc-org-goabi0")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/goabi0")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nf3mcql5vjfymhy7l867zan2ynjg3wc3jplh4k1gby077h7jb3h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "modernc.org/goabi0"))
    (home-page "https://modernc.org/goabi0")
    (synopsis "goabi0")
    (description "Package goabi0 provides helpers for generating Go assembler
@@url{https://go.dev/doc/asm,ABI0} code.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.66.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/libc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kd7zc72dmynhn8l9pvjjgs6ip0iq4xdbyi44fsv1pxm4b4f9sy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "modernc.org/libc"))
    (propagated-inputs (list go-modernc-org-memory
                             go-modernc-org-mathutil
                             go-modernc-org-goabi0
                             go-modernc-org-fileutil
                             go-modernc-org-ccgo-v4
                             go-modernc-org-cc-v4
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-exp
                             go-github-com-ncruces-go-strftime
                             go-github-com-mattn-go-isatty
                             go-github-com-google-uuid
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/libc")
    (synopsis "libc")
    (description
     "Package libc is a partial reimplementation of C libc in pure Go.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "060p3b1xjjmsax0pkyviv9fv52p2mxnijnxwdxikmx1sw5fpayy5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "modernc.org/sqlite"))
    (propagated-inputs (list go-modernc-org-mathutil go-modernc-org-libc
                             go-modernc-org-fileutil go-golang-org-x-sys
                             go-github-com-google-pprof))
    (home-page "https://modernc.org/sqlite")
    (synopsis #f)
    (description
     "Package sqlite is a sql/database driver using a CGo-free port of the C SQLite3
library.")
    (license license:bsd-3)))

(define-public go-github-com-hamba-avro
  (package
    (name "go-github-com-hamba-avro")
    (version "2.30.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hamba/avro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/hamba/avro/v2"
      #:unpack-path "github.com/hamba/avro"))
    (propagated-inputs (list go-golang-org-x-tools
                             go-github-com-stretchr-testify
                             go-github-com-modern-go-reflect2
                             go-github-com-klauspost-compress
                             go-github-com-json-iterator-go
                             go-github-com-golang-snappy
                             go-github-com-go-viper-mapstructure-v2
                             go-github-com-ettle-strcase))
    (home-page "https://github.com/hamba/avro")
    (synopsis "Overview")
    (description
     "Package avro implements encoding and decoding of Avro as defined by the Avro
specification.")
    (license license:expat)))

(define-public go-github-com-substrait-io-substrait
  (package
    (name "go-github-com-substrait-io-substrait")
    (version "0.76.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/substrait-io/substrait")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/substrait-io/substrait"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/substrait-io/substrait")
    (synopsis "Substrait")
    (description
     "Package substrait provides access to Substrait artifacts via embed.FS. Use
@code{substrait.GetSubstraitFS()} to retrieve the embed.FS object.")
    (license license:asl2.0)))

(define-public go-github-com-substrait-io-substrait-go
  (package
    (name "go-github-com-substrait-io-substrait-go")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/substrait-io/substrait-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/substrait-io/substrait-go"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-google-golang-org-protobuf
                             go-golang-org-x-exp
                             go-github-com-substrait-io-substrait
                             go-github-com-stretchr-testify
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-goccy-go-yaml
                             go-github-com-creasty-defaults
                             go-github-com-cockroachdb-apd-v3
                             go-github-com-alecthomas-participle-v2))
    (home-page "https://github.com/substrait-io/substrait-go")
    (synopsis "substrait-go")
    (description
     "Package substraitgo contains the experimental go bindings for substrait
(@@url{https://substrait.io,https://substrait.io}).")
    (license license:asl2.0)))

(define-public go-github-com-apache-arrow-go
  (package
    (name "go-github-com-apache-arrow-go")
    (version "15.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (go-version->git-ref version
                                          #:subdir "go"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/apache/arrow/go/v15"
      #:unpack-path "github.com/apache/arrow"))
    (propagated-inputs (list go-github-com-tidwall-sjson
                             go-github-com-substrait-io-substrait-go
                             go-github-com-hamba-avro-v2
                             go-github-com-google-uuid
                             go-modernc-org-sqlite
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-gonum-org-v1-gonum
                             go-golang-org-x-xerrors
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-golang-org-x-exp
                             go-github-com-zeebo-xxh3
                             go-github-com-stretchr-testify
                             go-github-com-pierrec-lz4-v4
                             go-github-com-minio-c2goasm
                             go-github-com-minio-asm2plan9s
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-klauspost-compress
                             go-github-com-klauspost-asmfmt
                             go-github-com-google-flatbuffers
                             go-github-com-golang-snappy
                             go-github-com-goccy-go-json
                             go-github-com-docopt-docopt-go
                             go-github-com-apache-thrift
                             go-github-com-andybalholm-brotli
                             go-github-com-johncgriffin-overflow))
    (home-page "https://github.com/apache/arrow")
    (synopsis "Apache Arrow for Go")
    (description
     "@@url{https://arrow.apache.org,Apache Arrow} is a cross-language development
platform for in-memory data.  It specifies a standardized language-independent
columnar memory format for flat and hierarchical data, organized for efficient
analytic operations on modern hardware.  It also provides computational
libraries and zero-copy streaming messaging and inter-process communication.")
    (license unknown-license!)))

(define-public go-cloud-google-com-go-bigquery
  (package
    (name "go-cloud-google-com-go-bigquery")
    (version "1.71.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "bigquery"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/bigquery"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-xerrors
                             go-golang-org-x-sync
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-apache-arrow-go-v15
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-datacatalog
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "BigQuery")
    (description
     "Package bigquery provides a client for the @code{BigQuery} service.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-cloud-bigtable-clients-test
  (package
    (name "go-github-com-googleapis-cloud-bigtable-clients-test")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/cloud-bigtable-clients-test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/googleapis/cloud-bigtable-clients-test"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-bigtable))
    (home-page "https://github.com/googleapis/cloud-bigtable-clients-test")
    (synopsis "Test Framework for Cloud Bigtable Client Libraries")
    (description
     "This repository contains the test framework to validate the correctness of Cloud
Bigtable @@url{https://cloud.google.com/bigtable/docs/reference/libraries,client
libraries}.  Specifically, all of the client libraries should exhibit correct
and consistent behaviors when interacting with the server (e.g. retry on
transient error) However, writing test cases in every language would present
maintainability and scalability challenges.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-bigtable
  (package
    (name "go-cloud-google-com-go-bigtable")
    (version "1.40.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "bigtable"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/bigtable"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-rsc-io-binaryregexp
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-cloud-bigtable-clients-test
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-google-btree
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
                        go-cloud-google-com-go-monitoring
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description "Package bigtable is an API to Google Cloud Bigtable.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-billing
  (package
    (name "go-cloud-google-com-go-billing")
    (version "1.20.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "billing"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/billing"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Billing API")
    (description "Go Client Library for Cloud Billing API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-binaryauthorization
  (package
    (name "go-cloud-google-com-go-binaryauthorization")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "binaryauthorization"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/binaryauthorization"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Binary Authorization API")
    (description "Go Client Library for Binary Authorization API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-certificatemanager
  (package
    (name "go-cloud-google-com-go-certificatemanager")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "certificatemanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/certificatemanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Certificate Manager API")
    (description "Go Client Library for Certificate Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-channel
  (package
    (name "go-cloud-google-com-go-channel")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "channel"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/channel"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Channel API")
    (description "Go Client Library for Cloud Channel API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-cloudbuild
  (package
    (name "go-cloud-google-com-go-cloudbuild")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "cloudbuild"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/cloudbuild"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Build API")
    (description "Go Client Library for Cloud Build API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-clouddms
  (package
    (name "go-cloud-google-com-go-clouddms")
    (version "1.8.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "clouddms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/clouddms"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Database Migration API")
    (description "Go Client Library for Database Migration API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-cloudtasks
  (package
    (name "go-cloud-google-com-go-cloudtasks")
    (version "1.13.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "cloudtasks"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/cloudtasks"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Tasks API")
    (description "Go Client Library for Cloud Tasks API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-compute
  (package
    (name "go-cloud-google-com-go-compute")
    (version "1.48.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "compute"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/compute"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Compute API")
    (description "Go Client Library for Compute API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-contactcenterinsights
  (package
    (name "go-cloud-google-com-go-contactcenterinsights")
    (version "1.17.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "contactcenterinsights"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/contactcenterinsights"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Contact Center AI Insights API")
    (description "Go Client Library for Contact Center AI Insights API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-container
  (package
    (name "go-cloud-google-com-go-container")
    (version "1.44.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "container"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/container"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Kubernetes Engine API")
    (description
     "Package container contains a deprecated Google Container Engine client.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-grafeas
  (package
    (name "go-cloud-google-com-go-grafeas")
    (version "0.3.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "grafeas"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/grafeas"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Grafeas API")
    (description "Go Client Library for Grafeas API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-containeranalysis
  (package
    (name "go-cloud-google-com-go-containeranalysis")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "containeranalysis"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/containeranalysis"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-grafeas
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Container Analysis API")
    (description "Go Client Library for Container Analysis API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datacatalog
  (package
    (name "go-cloud-google-com-go-datacatalog")
    (version "1.26.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datacatalog"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/datacatalog"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Data Catalog API")
    (description "Go Client Library for Google Cloud Data Catalog API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataflow
  (package
    (name "go-cloud-google-com-go-dataflow")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataflow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dataflow"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataflow API")
    (description "Go Client Library for Dataflow API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataform
  (package
    (name "go-cloud-google-com-go-dataform")
    (version "0.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataform"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dataform"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataform API")
    (description "Go Client Library for Dataform API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datafusion
  (package
    (name "go-cloud-google-com-go-datafusion")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datafusion"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/datafusion"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Data Fusion API")
    (description "Go Client Library for Cloud Data Fusion API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datalabeling
  (package
    (name "go-cloud-google-com-go-datalabeling")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datalabeling"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/datalabeling"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Data Labeling API")
    (description "Go Client Library for Data Labeling API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataplex
  (package
    (name "go-cloud-google-com-go-dataplex")
    (version "1.27.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataplex"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dataplex"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Dataplex API")
    (description "Go Client Library for Cloud Dataplex API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataproc
  (package
    (name "go-cloud-google-com-go-dataproc")
    (version "2.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataproc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dataproc/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Dataproc API")
    (description "Go Client Library for Cloud Dataproc API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataqna
  (package
    (name "go-cloud-google-com-go-dataqna")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataqna"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dataqna"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Data QnA API")
    (description "Go Client Library for Data @code{QnA} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datastore
  (package
    (name "go-cloud-google-com-go-datastore")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datastore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/datastore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Datastore")
    (description
     "Package datastore provides a client for Google Cloud Datastore.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datastream
  (package
    (name "go-cloud-google-com-go-datastream")
    (version "1.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datastream"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/datastream"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Datastream API")
    (description "Go Client Library for Datastream API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-deploy
  (package
    (name "go-cloud-google-com-go-deploy")
    (version "1.27.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "deploy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/deploy"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Deploy API")
    (description "Go Client Library for Google Cloud Deploy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dialogflow
  (package
    (name "go-cloud-google-com-go-dialogflow")
    (version "1.69.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dialogflow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dialogflow"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dialogflow API")
    (description "Go Client Library for Dialogflow API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dlp
  (package
    (name "go-cloud-google-com-go-dlp")
    (version "1.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dlp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/dlp"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Data Loss Prevention (DLP) API")
    (description "Go Client Library for Cloud Data Loss Prevention (DLP) API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-documentai
  (package
    (name "go-cloud-google-com-go-documentai")
    (version "1.38.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "documentai"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/documentai"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Document AI API")
    (description "Go Client Library for Cloud Document AI API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-domains
  (package
    (name "go-cloud-google-com-go-domains")
    (version "0.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "domains"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/domains"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Domains API")
    (description "Go Client Library for Cloud Domains API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-edgecontainer
  (package
    (name "go-cloud-google-com-go-edgecontainer")
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "edgecontainer"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/edgecontainer"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Distributed Cloud Edge Container API")
    (description "Go Client Library for Distributed Cloud Edge Container API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-errorreporting
  (package
    (name "go-cloud-google-com-go-errorreporting")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "errorreporting"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/errorreporting"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Error Reporting API")
    (description
     "Package errorreporting is a Google Cloud Error Reporting library.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-essentialcontacts
  (package
    (name "go-cloud-google-com-go-essentialcontacts")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "essentialcontacts"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/essentialcontacts"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Essential Contacts API")
    (description "Go Client Library for Essential Contacts API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-eventarc
  (package
    (name "go-cloud-google-com-go-eventarc")
    (version "1.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "eventarc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/eventarc"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Eventarc API")
    (description "Go Client Library for Eventarc API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-filestore
  (package
    (name "go-cloud-google-com-go-filestore")
    (version "1.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "filestore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/filestore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Filestore API")
    (description "Go Client Library for Cloud Filestore API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-firestore
  (package
    (name "go-cloud-google-com-go-firestore")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "firestore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/firestore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-time
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description
     "Package firestore provides a client for reading and writing to a Cloud Firestore
database.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-functions
  (package
    (name "go-cloud-google-com-go-functions")
    (version "1.19.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "functions"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/functions"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Functions API")
    (description "Go Client Library for Cloud Functions API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkebackup
  (package
    (name "go-cloud-google-com-go-gkebackup")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkebackup"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/gkebackup"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Backup for GKE API")
    (description "Go Client Library for Backup for GKE API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkeconnect
  (package
    (name "go-cloud-google-com-go-gkeconnect")
    (version "0.12.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkeconnect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/gkeconnect"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "GKE Connect APIs")
    (description "Go Client Library for GKE Connect APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkehub
  (package
    (name "go-cloud-google-com-go-gkehub")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkehub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/gkehub"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "GKE Hub")
    (description "Go Client Library for GKE Hub.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkemulticloud
  (package
    (name "go-cloud-google-com-go-gkemulticloud")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkemulticloud"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/gkemulticloud"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Anthos Multi-Cloud API")
    (description "Go Client Library for Anthos Multi-Cloud API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gsuiteaddons
  (package
    (name "go-cloud-google-com-go-gsuiteaddons")
    (version "1.7.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gsuiteaddons"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/gsuiteaddons"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Workspace Add-ons API")
    (description "Go Client Library for Google Workspace Add-ons API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iap
  (package
    (name "go-cloud-google-com-go-iap")
    (version "1.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iap"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/iap"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Identity-Aware Proxy API")
    (description "Go Client Library for Cloud Identity-Aware Proxy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-ids
  (package
    (name "go-cloud-google-com-go-ids")
    (version "1.5.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "ids"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/ids"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud IDS API")
    (description "Go Client Library for Cloud IDS API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iot
  (package
    (name "go-cloud-google-com-go-iot")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iot"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/iot"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud IoT API")
    (description "Go Client Library for Cloud @code{IoT} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-language
  (package
    (name "go-cloud-google-com-go-language")
    (version "1.14.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "language"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/language"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Natural Language API")
    (description "Go Client Library for Cloud Natural Language API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-lifesciences
  (package
    (name "go-cloud-google-com-go-lifesciences")
    (version "0.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "lifesciences"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/lifesciences"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Life Sciences API")
    (description "Go Client Library for Cloud Life Sciences API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-logging
  (package
    (name "go-cloud-google-com-go-logging")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "logging"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/logging"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opencensus-io
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-compute-metadata
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Logging")
    (description
     "Package logging contains a Cloud Logging client suitable for writing logs.  For
reading logs, and working with sinks, metrics and monitored resources, see
package cloud.google.com/go/logging/logadmin.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-managedidentities
  (package
    (name "go-cloud-google-com-go-managedidentities")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "managedidentities"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/managedidentities"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Managed Service for Microsoft Active Directory API")
    (description
     "Go Client Library for Managed Service for Microsoft Active Directory API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-maps
  (package
    (name "go-cloud-google-com-go-maps")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "maps"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/maps"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Maps Platform APIs")
    (description "Go Client Library for Google Maps Platform APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-mediatranslation
  (package
    (name "go-cloud-google-com-go-mediatranslation")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "mediatranslation"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/mediatranslation"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Media Translation API")
    (description "Go Client Library for Media Translation API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-memcache
  (package
    (name "go-cloud-google-com-go-memcache")
    (version "1.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "memcache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/memcache"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Memorystore for Memcached API")
    (description "Go Client Library for Cloud Memorystore for Memcached API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-metastore
  (package
    (name "go-cloud-google-com-go-metastore")
    (version "1.14.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "metastore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/metastore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataproc Metastore API")
    (description "Go Client Library for Dataproc Metastore API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networkconnectivity
  (package
    (name "go-cloud-google-com-go-networkconnectivity")
    (version "1.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networkconnectivity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/networkconnectivity"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Connectivity API")
    (description "Go Client Library for Network Connectivity API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networkmanagement
  (package
    (name "go-cloud-google-com-go-networkmanagement")
    (version "1.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networkmanagement"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/networkmanagement"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Management API")
    (description "Go Client Library for Network Management API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networksecurity
  (package
    (name "go-cloud-google-com-go-networksecurity")
    (version "0.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networksecurity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/networksecurity"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Security API")
    (description "Go Client Library for Network Security API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-notebooks
  (package
    (name "go-cloud-google-com-go-notebooks")
    (version "1.12.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "notebooks"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/notebooks"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Notebooks API")
    (description "Go Client Library for Notebooks API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-optimization
  (package
    (name "go-cloud-google-com-go-optimization")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "optimization"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/optimization"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Optimization API")
    (description "Go Client Library for Cloud Optimization API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-orchestration
  (package
    (name "go-cloud-google-com-go-orchestration")
    (version "1.11.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "orchestration"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/orchestration"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Composer API")
    (description "Go Client Library for Cloud Composer API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-orgpolicy
  (package
    (name "go-cloud-google-com-go-orgpolicy")
    (version "1.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "orgpolicy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/orgpolicy"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Organization Policy API")
    (description "Go Client Library for Organization Policy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-osconfig
  (package
    (name "go-cloud-google-com-go-osconfig")
    (version "1.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "osconfig"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/osconfig"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "OS Config API")
    (description "Go Client Library for OS Config API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-oslogin
  (package
    (name "go-cloud-google-com-go-oslogin")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "oslogin"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/oslogin"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud OS Login API")
    (description "Go Client Library for Cloud OS Login API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-phishingprotection
  (package
    (name "go-cloud-google-com-go-phishingprotection")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "phishingprotection"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/phishingprotection"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Phishing Protection API")
    (description "Go Client Library for Phishing Protection API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-policytroubleshooter
  (package
    (name "go-cloud-google-com-go-policytroubleshooter")
    (version "1.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "policytroubleshooter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/policytroubleshooter"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Policy Troubleshooter API")
    (description "Go Client Library for Policy Troubleshooter API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-privatecatalog
  (package
    (name "go-cloud-google-com-go-privatecatalog")
    (version "0.10.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "privatecatalog"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/privatecatalog"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Private Catalog API")
    (description "Go Client Library for Cloud Private Catalog API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-kms
  (package
    (name "go-cloud-google-com-go-kms")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "kms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/kms"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Key Management Service (KMS) API")
    (description
     "Go Client Library for Cloud Key Management Service (KMS) API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-pubsub
  (package
    (name "go-cloud-google-com-go-pubsub")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/pubsub/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-go-einride-tech-aip
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description
     "Package pubsub provides an easy way to publish and receive Google Cloud Pub/Sub
messages, hiding the details of the underlying server RPCs.  Pub/Sub is a
many-to-many, asynchronous messaging system that decouples senders and
receivers.")
    (license license:asl2.0)))

(define-public go-go-einride-tech-aip
  (package
    (name "go-go-einride-tech-aip")
    (version "0.73.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/einride/aip-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.einride.tech/aip"))
    (propagated-inputs (list go-gotest-tools-v3
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-github-com-stoewer-go-strcase
                             go-github-com-google-uuid))
    (home-page "https://go.einride.tech/aip")
    (synopsis "AIP Go")
    (description
     "Package aip provides primitives for implementing API Improvement Proposals
(AIP).")
    (license license:expat)))

(define-public go-cloud-google-com-go-pubsub
  (package
    (name "go-cloud-google-com-go-pubsub")
    (version "1.50.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/pubsub"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-time
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-go-einride-tech-aip
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-pubsub-v2
                             go-cloud-google-com-go-kms
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Pub/Sub")
    (description
     "Package pubsub provides an easy way to publish and receive Google Cloud Pub/Sub
messages, hiding the details of the underlying server RPCs.  Pub/Sub is a
many-to-many, asynchronous messaging system that decouples senders and
receivers.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-pubsublite
  (package
    (name "go-cloud-google-com-go-pubsublite")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsublite"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/pubsublite"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Pub/Sub Lite")
    (description
     "Package pubsublite provides an easy way to publish and receive messages using
the Pub/Sub Lite service.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recaptchaenterprise
  (package
    (name "go-cloud-google-com-go-recaptchaenterprise")
    (version "2.20.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recaptchaenterprise"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/recaptchaenterprise/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "reCAPTCHA Enterprise API")
    (description "Go Client Library for @code{reCAPTCHA} Enterprise API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recommendationengine
  (package
    (name "go-cloud-google-com-go-recommendationengine")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recommendationengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/recommendationengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Recommendations AI")
    (description "Go Client Library for Recommendations AI.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recommender
  (package
    (name "go-cloud-google-com-go-recommender")
    (version "1.13.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recommender"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/recommender"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Recommender API")
    (description "Go Client Library for Recommender API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-redis
  (package
    (name "go-cloud-google-com-go-redis")
    (version "1.18.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "redis"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/redis"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Memorystore for Redis API")
    (description
     "Go Client Library for Google Cloud Memorystore for Redis API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-resourcemanager
  (package
    (name "go-cloud-google-com-go-resourcemanager")
    (version "1.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "resourcemanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/resourcemanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Resource Manager API")
    (description "Go Client Library for Cloud Resource Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-resourcesettings
  (package
    (name "go-cloud-google-com-go-resourcesettings")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "resourcesettings"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/resourcesettings"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Resource Settings API")
    (description "Go Client Library for Resource Settings API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-retail
  (package
    (name "go-cloud-google-com-go-retail")
    (version "1.25.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "retail"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/retail"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Retail API")
    (description "Go Client Library for Retail API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-run
  (package
    (name "go-cloud-google-com-go-run")
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "run"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/run"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Run Admin API")
    (description "Go Client Library for Cloud Run Admin API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-scheduler
  (package
    (name "go-cloud-google-com-go-scheduler")
    (version "1.11.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "scheduler"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/scheduler"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Scheduler API")
    (description "Go Client Library for Cloud Scheduler API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-secretmanager
  (package
    (name "go-cloud-google-com-go-secretmanager")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "secretmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/secretmanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Secret Manager API")
    (description "Go Client Library for Secret Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-security
  (package
    (name "go-cloud-google-com-go-security")
    (version "1.19.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "security"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/security"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Security APIs")
    (description "Go Client Library for Security APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-securitycenter
  (package
    (name "go-cloud-google-com-go-securitycenter")
    (version "1.38.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "securitycenter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/securitycenter"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Security Command Center API")
    (description "Go Client Library for Security Command Center API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-servicedirectory
  (package
    (name "go-cloud-google-com-go-servicedirectory")
    (version "1.12.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "servicedirectory"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/servicedirectory"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Service Directory API")
    (description "Go Client Library for Service Directory API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-shell
  (package
    (name "go-cloud-google-com-go-shell")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "shell"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/shell"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Shell API")
    (description "Go Client Library for Cloud Shell API.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp
  (package
    (name "go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GoogleCloudPlatform/grpc-gcp-go")
             (commit (go-version->git-ref version
                                          #:subdir "grpcgcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/GoogleCloudPlatform/grpc-gcp-go/grpcgcp"
      #:unpack-path "github.com/GoogleCloudPlatform/grpc-gcp-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-google-go-cmp
                             go-github-com-golang-mock))
    (home-page "https://github.com/GoogleCloudPlatform/grpc-gcp-go")
    (synopsis "How to test Spanner integration")
    (description
     "Package grpcgcp provides grpc supports for Google Cloud APIs.  For now it
provides connection management with affinity support.")
    (license license:asl2.0)))

(define-public go-go-opencensus-io
  (package
    (name "go-go-opencensus-io")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/census-instrumentation/opencensus-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opencensus.io"))
    (propagated-inputs (list go-google-golang-org-grpc
                             go-golang-org-x-net
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-github-com-golang-protobuf
                             go-github-com-golang-groupcache))
    (home-page "https://go.opencensus.io")
    (synopsis "OpenCensus Libraries for Go")
    (description
     "Package opencensus contains Go support for @code{OpenCensus}.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-spanner
  (package
    (name "go-cloud-google-com-go-spanner")
    (version "1.86.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "spanner"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/spanner"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-go-opencensus-io
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp
                        go-cloud-google-com-go-monitoring
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Spanner")
    (description
     "Package spanner provides a client for reading and writing to Cloud Spanner
databases.  See the packages under admin for clients that operate on databases
and instances.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-speech
  (package
    (name "go-cloud-google-com-go-speech")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "speech"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/speech"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Speech-to-Text API")
    (description "Go Client Library for Cloud Speech-to-Text API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-storagetransfer
  (package
    (name "go-cloud-google-com-go-storagetransfer")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "storagetransfer"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/storagetransfer"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Storage Transfer API")
    (description "Go Client Library for Storage Transfer API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-talent
  (package
    (name "go-cloud-google-com-go-talent")
    (version "1.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "talent"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/talent"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Talent Solution API")
    (description "Go Client Library for Cloud Talent Solution API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-texttospeech
  (package
    (name "go-cloud-google-com-go-texttospeech")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "texttospeech"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/texttospeech"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Text-to-Speech API")
    (description "Go Client Library for Cloud Text-to-Speech API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-tpu
  (package
    (name "go-cloud-google-com-go-tpu")
    (version "1.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "tpu"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/tpu"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud TPU API")
    (description "Go Client Library for Cloud TPU API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-trace
  (package
    (name "go-cloud-google-com-go-trace")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "trace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/trace"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Stackdriver Trace API")
    (description "Go Client Library for Stackdriver Trace API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-video
  (package
    (name "go-cloud-google-com-go-video")
    (version "1.27.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "video"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/video"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Video APIs")
    (description "Go Client Library for Video APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-videointelligence
  (package
    (name "go-cloud-google-com-go-videointelligence")
    (version "1.12.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "videointelligence"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/videointelligence"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Video Intelligence API")
    (description "Go Client Library for Google Cloud Video Intelligence API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vision
  (package
    (name "go-cloud-google-com-go-vision")
    (version "2.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vision"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/vision/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Vision API")
    (description "Go Client Library for Cloud Vision API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vmmigration
  (package
    (name "go-cloud-google-com-go-vmmigration")
    (version "1.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vmmigration"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/vmmigration"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "VM Migration API")
    (description "Go Client Library for VM Migration API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vmwareengine
  (package
    (name "go-cloud-google-com-go-vmwareengine")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vmwareengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/vmwareengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "VMware Engine API")
    (description "Go Client Library for VMware Engine API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vpcaccess
  (package
    (name "go-cloud-google-com-go-vpcaccess")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vpcaccess"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/vpcaccess"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Serverless VPC Access API")
    (description "Go Client Library for Serverless VPC Access API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-webrisk
  (package
    (name "go-cloud-google-com-go-webrisk")
    (version "1.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "webrisk"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/webrisk"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Web Risk API")
    (description "Go Client Library for Web Risk API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-websecurityscanner
  (package
    (name "go-cloud-google-com-go-websecurityscanner")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "websecurityscanner"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/websecurityscanner"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Web Security Scanner API")
    (description "Go Client Library for Web Security Scanner API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-translate
  (package
    (name "go-cloud-google-com-go-translate")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "translate"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/translate"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-text
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Translation API")
    (description
     "Package translate is the v2 client for the Google Translation API.")
    (license license:asl2.0)))

(define-public go-github-com-google-s2a-go
  (package
    (name "go-github-com-google-s2a-go")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/s2a-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/s2a-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-appengine
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-golang-org-x-crypto
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-translate))
    (home-page "https://github.com/google/s2a-go")
    (synopsis "Secure Session Agent Client Libraries")
    (description
     "Package s2a provides the S2A transport credentials used by a @code{gRPC}
application.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-pkcs11
  (package
    (name "go-github-com-google-go-pkcs11")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-pkcs11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-pkcs11"))
    (home-page "https://github.com/google/go-pkcs11")
    (synopsis "Go PKCS #11")
    (description
     "This package provides a Go package for loading PKCS #11 modules.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-enterprise-certificate-proxy
  (package
    (name "go-github-com-googleapis-enterprise-certificate-proxy")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/enterprise-certificate-proxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/googleapis/enterprise-certificate-proxy"))
    (propagated-inputs (list go-golang-org-x-sys go-golang-org-x-crypto
                             go-github-com-google-go-pkcs11))
    (home-page "https://github.com/googleapis/enterprise-certificate-proxy")
    (synopsis "Google Proxies for Enterprise Certificates (GA)")
    (description
     "If you use
@@url{https://cloud.google.com/beyondcorp-enterprise/docs/securing-resources-with-certificate-based-access,certificate-based
access} to protect your Google Cloud resources, the end user
@@url{https://en.wikipedia.org/wiki/Client_certificate,device certificate} is
one of the credentials that is verified before access to a resource is granted.
You can configure Google Cloud to use the device certificates in your operating
system key store when verifying access to a resource from the gcloud CLI or
Terraform by using the enterprise certificates feature.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-auth
  (package
    (name "go-cloud-google-com-go-auth")
    (version "0.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "auth"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/auth"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-golang-org-x-time
                        go-golang-org-x-net
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-enterprise-certificate-proxy
                        go-github-com-google-s2a-go
                        go-github-com-google-go-cmp
                        go-cloud-google-com-go-compute-metadata))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Auth Library for Go")
    (description
     "Package auth provides utilities for managing Google Cloud credentials, including
functionality for creating, caching, and refreshing OAuth2 tokens.  It offers
customizable options for different OAuth2 flows, such as 2-legged (2LO) and
3-legged (3LO) OAuth, along with support for PKCE and automatic token
management.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iam
  (package
    (name "go-cloud-google-com-go-iam")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iam"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/iam"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "IAM API")
    (description
     "Package iam supports the resource-specific operations of Google Cloud IAM
(Identity and Access Management) for the Google Cloud Libraries.  See
@@url{https://cloud.google.com/iam,https://cloud.google.com/iam} for more about
IAM.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-monitoring
  (package
    (name "go-cloud-google-com-go-monitoring")
    (version "1.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "monitoring"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/monitoring"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Monitoring API")
    (description "Go Client Library for Cloud Monitoring API.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
  (package
    (name
     "go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric")
    (version "0.54.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporter/metric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go/exporter/metric"
      #:unpack-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go"))
    (propagated-inputs (list go-google-golang-org-genproto-googleapis-api
                             go-go-opentelemetry-io-otel-trace
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-sdk-metric
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-monitoring))
    (home-page
     "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
    (synopsis "OpenTelemetry Google Cloud Monitoring Exporter")
    (description
     "@code{OpenTelemetry} Google Cloud Monitoring Exporter allows the user to send
collected metrics to Google Cloud.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "exporters/stdout/stdoutmetric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdoutmetric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Metric Exporter")
    (description
     "Package stdoutmetric provides an exporter for @code{OpenTelemetry} metric
telemetry.")
    (license unknown-license!)))

(define-public go-cloud-google-com-go-storage
  (package
    (name "go-cloud-google-com-go-storage")
    (version "1.57.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "storage"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/storage"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go-compute-metadata
                        go-cloud-google-com-go-auth
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Storage")
    (description
     "Package storage provides an easy way to work with Google Cloud Storage.  Google
Cloud Storage stores data in named objects, which are grouped into buckets.")
    (license license:asl2.0)))

(define-public go-github-com-google-martian
  (package
    (name "go-github-com-google-martian")
    (version "3.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/martian")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/martian/v3"
      #:unpack-path "github.com/google/martian"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc go-golang-org-x-net
                             go-github-com-golang-snappy))
    (home-page "https://github.com/google/martian")
    (synopsis "Martian Proxy")
    (description
     "Package martian provides an HTTP/1.1 proxy with an API for configurable request
and response modifiers.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go
  (package
    (name "go-cloud-google-com-go")
    (version "0.123.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-martian-v3
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-storage))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Client Libraries for Go")
    (description
     "Package cloud is the root of the packages used to access Google Cloud Services.
See
@@url{https://pkg.go.dev/cloud.google.com/go#section-directories,https://pkg.go.dev/cloud.google.com/go#section-directories}
for a full list of sub-modules.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-longrunning
  (package
    (name "go-cloud-google-com-go-longrunning")
    (version "0.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "longrunning"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/longrunning"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "longrunning")
    (description
     "Package longrunning supports Long Running Operations for the Google Cloud
Libraries.  See google.golang.org/genproto/googleapis/longrunning for its
service definition.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-workflows
  (package
    (name "go-cloud-google-com-go-workflows")
    (version "1.14.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "workflows"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cloud.google.com/go/workflows"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Workflows API")
    (description "Go Client Library for Workflows API.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20251002232023-7c0ddcbb5797")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-github-com-golang-protobuf
                             go-cloud-google-com-go-workflows
                             go-cloud-google-com-go-websecurityscanner
                             go-cloud-google-com-go-webrisk
                             go-cloud-google-com-go-vpcaccess
                             go-cloud-google-com-go-vmwareengine
                             go-cloud-google-com-go-vmmigration
                             go-cloud-google-com-go-vision-v2
                             go-cloud-google-com-go-videointelligence
                             go-cloud-google-com-go-video
                             go-cloud-google-com-go-translate
                             go-cloud-google-com-go-trace
                             go-cloud-google-com-go-tpu
                             go-cloud-google-com-go-texttospeech
                             go-cloud-google-com-go-talent
                             go-cloud-google-com-go-storagetransfer
                             go-cloud-google-com-go-speech
                             go-cloud-google-com-go-spanner
                             go-cloud-google-com-go-shell
                             go-cloud-google-com-go-servicedirectory
                             go-cloud-google-com-go-securitycenter
                             go-cloud-google-com-go-security
                             go-cloud-google-com-go-secretmanager
                             go-cloud-google-com-go-scheduler
                             go-cloud-google-com-go-run
                             go-cloud-google-com-go-retail
                             go-cloud-google-com-go-resourcesettings
                             go-cloud-google-com-go-resourcemanager
                             go-cloud-google-com-go-redis
                             go-cloud-google-com-go-recommender
                             go-cloud-google-com-go-recommendationengine
                             go-cloud-google-com-go-recaptchaenterprise-v2
                             go-cloud-google-com-go-pubsublite
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-privatecatalog
                             go-cloud-google-com-go-policytroubleshooter
                             go-cloud-google-com-go-phishingprotection
                             go-cloud-google-com-go-oslogin
                             go-cloud-google-com-go-osconfig
                             go-cloud-google-com-go-orgpolicy
                             go-cloud-google-com-go-orchestration
                             go-cloud-google-com-go-optimization
                             go-cloud-google-com-go-notebooks
                             go-cloud-google-com-go-networksecurity
                             go-cloud-google-com-go-networkmanagement
                             go-cloud-google-com-go-networkconnectivity
                             go-cloud-google-com-go-monitoring
                             go-cloud-google-com-go-metastore
                             go-cloud-google-com-go-memcache
                             go-cloud-google-com-go-mediatranslation
                             go-cloud-google-com-go-maps
                             go-cloud-google-com-go-managedidentities
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-logging
                             go-cloud-google-com-go-lifesciences
                             go-cloud-google-com-go-language
                             go-cloud-google-com-go-kms
                             go-cloud-google-com-go-iot
                             go-cloud-google-com-go-ids
                             go-cloud-google-com-go-iap
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-gsuiteaddons
                             go-cloud-google-com-go-gkemulticloud
                             go-cloud-google-com-go-gkehub
                             go-cloud-google-com-go-gkeconnect
                             go-cloud-google-com-go-gkebackup
                             go-cloud-google-com-go-functions
                             go-cloud-google-com-go-firestore
                             go-cloud-google-com-go-filestore
                             go-cloud-google-com-go-eventarc
                             go-cloud-google-com-go-essentialcontacts
                             go-cloud-google-com-go-errorreporting
                             go-cloud-google-com-go-edgecontainer
                             go-cloud-google-com-go-domains
                             go-cloud-google-com-go-documentai
                             go-cloud-google-com-go-dlp
                             go-cloud-google-com-go-dialogflow
                             go-cloud-google-com-go-deploy
                             go-cloud-google-com-go-datastream
                             go-cloud-google-com-go-datastore
                             go-cloud-google-com-go-dataqna
                             go-cloud-google-com-go-dataproc-v2
                             go-cloud-google-com-go-dataplex
                             go-cloud-google-com-go-datalabeling
                             go-cloud-google-com-go-datafusion
                             go-cloud-google-com-go-dataform
                             go-cloud-google-com-go-dataflow
                             go-cloud-google-com-go-datacatalog
                             go-cloud-google-com-go-containeranalysis
                             go-cloud-google-com-go-container
                             go-cloud-google-com-go-contactcenterinsights
                             go-cloud-google-com-go-compute
                             go-cloud-google-com-go-cloudtasks
                             go-cloud-google-com-go-clouddms
                             go-cloud-google-com-go-cloudbuild
                             go-cloud-google-com-go-channel
                             go-cloud-google-com-go-certificatemanager
                             go-cloud-google-com-go-binaryauthorization
                             go-cloud-google-com-go-billing
                             go-cloud-google-com-go-bigtable
                             go-cloud-google-com-go-bigquery
                             go-cloud-google-com-go-beyondcorp
                             go-cloud-google-com-go-batch
                             go-cloud-google-com-go-baremetalsolution
                             go-cloud-google-com-go-automl
                             go-cloud-google-com-go-assuredworkloads
                             go-cloud-google-com-go-asset
                             go-cloud-google-com-go-artifactregistry
                             go-cloud-google-com-go-area120
                             go-cloud-google-com-go-appengine
                             go-cloud-google-com-go-apigeeregistry
                             go-cloud-google-com-go-apigeeconnect
                             go-cloud-google-com-go-apigateway
                             go-cloud-google-com-go-analytics
                             go-cloud-google-com-go-aiplatform
                             go-cloud-google-com-go-accesscontextmanager
                             go-cloud-google-com-go-accessapproval))
    (home-page "https://google.golang.org/genproto")
    (synopsis "Go generated proto packages")
    (description
     "This repository contains the generated Go packages for common protocol buffer
types, and the generated @@url{http://grpc.io,@code{gRPC}} code necessary for
interacting with Google's @code{gRPC} APIs.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-gax-go
  (package
    (name "go-github-com-googleapis-gax-go")
    (version "2.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/gax-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/googleapis/gax-go/v2"
      #:unpack-path "github.com/googleapis/gax-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/googleapis/gax-go")
    (synopsis #f)
    (description
     "Package gax contains a set of modules which aid the development of APIs for
clients and servers based on @code{gRPC} and Google API conventions.")
    (license license:bsd-3)))

(define-public go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
  (package
    (name
     "go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc")
    (version "0.63.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                      #:subdir
                      "instrumentation/google.golang.org/grpc/otelgrpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path
      "go.opentelemetry.io/contrib/instrumentation/google.golang.org/grpc/otelgrpc"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk-metric
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis #f)
    (description "Package otelgrpc is the instrumentation library for
@@url{/google.golang.org/grpc,google.golang.org/grpc}.")
    (license unknown-license!)))

(define-public go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
  (package
    (name "go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp")
    (version "0.63.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "instrumentation/net/http/otelhttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path
      "go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk-metric
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-felixge-httpsnoop))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis #f)
    (description
     "Package otelhttp provides an http.Handler and functions that are intended to be
used to add tracing by wrapping existing handlers (with Handler) and routes
@code{WithRouteTag}.")
    (license unknown-license!)))

(define-public go-google-golang-org-genproto-googleapis-bytestream
  (package
    (name "go-google-golang-org-genproto-googleapis-bytestream")
    (version "0.0.0-20251002232023-7c0ddcbb5797")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/bytestream"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/genproto/googleapis/bytestream"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-envoyproxy-go-control-plane
  (package
    (name "go-github-com-envoyproxy-go-control-plane")
    (version "0.13.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/envoyproxy/go-control-plane")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/envoyproxy/go-control-plane"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-go-uber-org-goleak
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/envoyproxy/go-control-plane")
    (synopsis "control-plane")
    (description
     "This repository contains a Go-based implementation of an API server that
implements the discovery service APIs defined in
@@url{https://github.com/envoyproxy/data-plane-api,data-plane-api}.")
    (license license:asl2.0)))

(define-public go-github-com-planetscale-vtprotobuf
  (package
    (name "go-github-com-planetscale-vtprotobuf")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/planetscale/vtprotobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/planetscale/vtprotobuf"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/planetscale/vtprotobuf")
    (synopsis ", the Vitess Protocol Buffers compiler")
    (description
     "This repository provides the @@code{protoc-gen-go-vtproto} plug-in for
@@code{protoc}, which is used by Vitess to generate optimized marshall &
unmarshal code.")
    (license license:bsd-3)))

(define-public go-github-com-grpc-ecosystem-grpc-gateway
  (package
    (name "go-github-com-grpc-ecosystem-grpc-gateway")
    (version "2.27.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc-ecosystem/grpc-gateway")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/grpc-ecosystem/grpc-gateway/v2"
      #:unpack-path "github.com/grpc-ecosystem/grpc-gateway"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-golang-org-x-text
                             go-golang-org-x-oauth2
                             go-go-yaml-in-yaml-v3
                             go-github-com-rogpeppe-fastuuid
                             go-github-com-google-go-cmp
                             go-github-com-antihax-optional))
    (home-page "https://github.com/grpc-ecosystem/grpc-gateway")
    (synopsis "About")
    (description
     "The @code{gRPC-Gateway} is a plugin of the Google protocol buffers compiler
@@url{https://github.com/protocolbuffers/protobuf,protoc}.  It reads protobuf
service definitions and generates a reverse-proxy server which translates a
RESTful HTTP API into @code{gRPC}.  This server is generated according to the
@@url{https://github.com/googleapis/googleapis/raw/master/google/api/http.proto#L46,(code
google.api.http)} annotations in your service definitions.")
    (license license:bsd-3)))

(define-public go-go-opentelemetry-io-proto-otlp
  (package
    (name "go-go-opentelemetry-io-proto-otlp")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-proto-go")
             (commit (go-version->git-ref version
                                          #:subdir "otlp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/proto/otlp"
      #:unpack-path "go.opentelemetry.io/proto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-grpc-ecosystem-grpc-gateway-v2))
    (home-page "https://go.opentelemetry.io/proto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-envoyproxy-go-control-plane-envoy
  (package
    (name "go-github-com-envoyproxy-go-control-plane-envoy")
    (version "1.35.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/envoyproxy/go-control-plane")
             (commit (go-version->git-ref version
                                          #:subdir "envoy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/envoyproxy/go-control-plane/envoy"
      #:unpack-path "github.com/envoyproxy/go-control-plane"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-go-opentelemetry-io-proto-otlp
                             go-github-com-prometheus-client-model
                             go-github-com-planetscale-vtprotobuf
                             go-github-com-envoyproxy-protoc-gen-validate
                             go-github-com-envoyproxy-go-control-plane
                             go-github-com-cncf-xds-go))
    (home-page "https://github.com/envoyproxy/go-control-plane")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-microsoft-go-winio
  (package
    (name "go-github-com-microsoft-go-winio")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/go-winio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Microsoft/go-winio"))
    (propagated-inputs (list go-golang-org-x-tools go-golang-org-x-sys
                             go-github-com-sirupsen-logrus))
    (home-page "https://github.com/Microsoft/go-winio")
    (synopsis "go-winio")
    (description
     "This package provides utilities for efficiently performing Win32 IO operations
in Go.  Currently, this package is provides support for genreal IO and
management of.")
    (license license:expat)))

(define-public go-cel-dev-expr
  (package
    (name "go-cel-dev-expr")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/cel-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "cel.dev/expr"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-genproto-googleapis-rpc))
    (home-page "https://cel.dev/expr")
    (synopsis "Common Expression Language")
    (description
     "The Common Expression Language (CEL) implements common semantics for expression
evaluation, enabling different applications to more easily interoperate.")
    (license license:asl2.0)))

(define-public go-github-com-envoyproxy-protoc-gen-validate
  (package
    (name "go-github-com-envoyproxy-protoc-gen-validate")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bufbuild/protoc-gen-validate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/envoyproxy/protoc-gen-validate"))
    (propagated-inputs (list go-google-golang-org-protobuf go-golang-org-x-net
                             go-github-com-lyft-protoc-gen-star-v2
                             go-github-com-iancoleman-strcase))
    (home-page "https://github.com/envoyproxy/protoc-gen-validate")
    (synopsis "protoc-gen-validate (PGV)")
    (description
     "PGV is a protoc plugin to generate polyglot message validators.  While protocol
buffers effectively guarantee the types of structured data, they cannot enforce
semantic rules for values.  This plugin adds support to protoc-generated code to
validate such constraints.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto-googleapis-api
  (package
    (name "go-google-golang-org-genproto-googleapis-api")
    (version "0.0.0-20251002232023-7c0ddcbb5797")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/api"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/genproto/googleapis/api"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-cncf-xds-go
  (package
    (name "go-github-com-cncf-xds-go")
    (version "0.0.0-20250501225837-2ac532fd4443")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cncf/xds")
             (commit (go-version->git-ref version
                                          #:subdir "go"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cncf/xds/go"
      #:unpack-path "github.com/cncf/xds"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-github-com-envoyproxy-protoc-gen-validate
                             go-cel-dev-expr))
    (home-page "https://github.com/cncf/xds")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-prometheus-otlptranslator
  (package
    (name "go-github-com-prometheus-otlptranslator")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/otlptranslator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/prometheus/otlptranslator"))
    (home-page "https://github.com/prometheus/otlptranslator")
    (synopsis "OTLP Prometheus Translator")
    (description
     "Copyright 2025 The Prometheus Authors Licensed under the Apache License, Version
2.0 (the \"License\"); you may not use this file except in compliance with the
License.  You may obtain a copy of the License at.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-prometheus
  (package
    (name "go-go-opentelemetry-io-otel-exporters-prometheus")
    (version "0.60.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporters/prometheus"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/otel/exporters/prometheus"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-stretchr-testify
                             go-github-com-prometheus-otlptranslator
                             go-github-com-prometheus-common
                             go-github-com-prometheus-client-model
                             go-github-com-prometheus-client-golang))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "Prometheus Exporter")
    (description
     "Package prometheus provides a Prometheus Exporter that converts OTLP metrics
into the Prometheus exposition format and implements prometheus.Collector to
provide a handler for these metrics.")
    (license unknown-license!)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "exporters/stdout/stdouttrace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdouttrace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Trace Exporter")
    (description
     "Package stdouttrace contains an @code{OpenTelemetry} exporter for tracing
telemetry to be written to an output destination as JSON.")
    (license unknown-license!)))

(define-public go-google-golang-org-grpc-security-advancedtls
  (package
    (name "go-google-golang-org-grpc-security-advancedtls")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (go-version->git-ref version
                                          #:subdir "security/advancedtls"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/grpc/security/advancedtls"
      #:unpack-path "google.golang.org/grpc"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-google-go-cmp))
    (home-page "https://google.golang.org/grpc")
    (synopsis #f)
    (description
     "Package advancedtls provides @code{gRPC} transport credentials that allow easy
configuration of advanced TLS features.  The APIs here give the user more
customizable control to fit their security landscape, thus the \"advanced\"
moniker.  This package provides both interfaces and generally useful
implementations of those interfaces, for example periodic credential reloading,
support for certificate revocation lists, and customizable certificate
verification behaviors.  If the provided implementations do not fit a given use
case, a custom implementation of the interface can be injected.")
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc-examples
  (package
    (name "go-google-golang-org-grpc-examples")
    (version "0.0.0-20251003040857-632550491c3f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (go-version->git-ref version
                                          #:subdir "examples"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/grpc/examples"
      #:unpack-path "google.golang.org/grpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc-security-advancedtls
                        go-google-golang-org-genproto-googleapis-rpc
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
                        go-go-opentelemetry-io-otel-exporters-prometheus
                        go-go-opentelemetry-io-otel
                        go-github-com-prometheus-client-golang
                        go-github-com-cncf-xds-go))
    (home-page "https://google.golang.org/grpc")
    (synopsis "Examples")
    (description
     "The following examples are provided to help users get started with
@code{gRPC-Go}.  They are arranged as follows:.")
    (license license:asl2.0)))

(define-public go-github-com-spiffe-go-spiffe
  (package
    (name "go-github-com-spiffe-go-spiffe")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spiffe/go-spiffe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/spiffe/go-spiffe/v2"
      #:unpack-path "github.com/spiffe/go-spiffe"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc-examples
                             go-google-golang-org-grpc
                             go-github-com-stretchr-testify
                             go-github-com-go-jose-go-jose-v4
                             go-github-com-microsoft-go-winio))
    (home-page "https://github.com/spiffe/go-spiffe")
    (synopsis "go-spiffe (v2)")
    (description "This library is a convenient Go library for working with
@@url{https://spiffe.io/,SPIFFE}.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp
  (package
    (name
     "go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp")
    (version "1.30.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
             (commit (go-version->git-ref version
                                          #:subdir "detectors/gcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go/detectors/gcp"
      #:unpack-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-cloud-google-com-go-compute-metadata))
    (home-page
     "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
    (synopsis "GCP Resource detection library")
    (description
     "This is a library intended to be used by Upstream @code{OpenTelemetry} resource
detectors.  It exists within this repository to allow for integration testing of
the detection functions in real GCP environments.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-contrib-detectors-gcp
  (package
    (name "go-go-opentelemetry-io-contrib-detectors-gcp")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                                          #:subdir "detectors/gcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/contrib/detectors/gcp"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel
                        go-github-com-stretchr-testify
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp
                        go-cloud-google-com-go-compute-metadata))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis "GCP Resource detector")
    (description
     "Package gcp provides a resource detector for GCP Cloud Function.")
    (license unknown-license!)))

(define-public go-go-opentelemetry-io-otel-metric
  (package
    (name "go-go-opentelemetry-io-otel-metric")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "metric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/otel/metric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "Metric API")
    (description
     "Package metric provides the @code{OpenTelemetry} API used to measure metrics
about source code operation.")
    (license unknown-license!)))

(define-public go-go-opentelemetry-io-otel-trace
  (package
    (name "go-go-opentelemetry-io-otel-trace")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "trace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "go.opentelemetry.io/otel/trace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-google-go-cmp))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "Trace API")
    (description
     "Package trace provides an implementation of the tracing part of the
@code{OpenTelemetry} API.")
    (license unknown-license!)))

(define-public go-google-golang-org-genproto-googleapis-rpc
  (package
    (name "go-google-golang-org-genproto-googleapis-rpc")
    (version "0.0.0-20251002232023-7c0ddcbb5797")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/rpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/genproto/googleapis/rpc"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc
  (package
    (name "go-google-golang-org-grpc")
    (version "1.75.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/grpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-genproto-googleapis-rpc
                             go-gonum-org-v1-gonum
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-golang-org-x-net
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk-metric
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-go-opentelemetry-io-contrib-detectors-gcp
                             go-github-com-spiffe-go-spiffe-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-golang-protobuf
                             go-github-com-golang-glog
                             go-github-com-envoyproxy-go-control-plane-envoy
                             go-github-com-envoyproxy-go-control-plane
                             go-github-com-cncf-xds-go
                             go-github-com-cespare-xxhash-v2))
    (home-page "https://google.golang.org/grpc")
    (synopsis "gRPC-Go")
    (description "Package grpc implements an RPC system called @code{gRPC}.")
    (license license:asl2.0)))

(define-public go-google-golang-org-api
  (package
    (name "go-google-golang-org-api")
    (version "0.251.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-api-go-client")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "google.golang.org/api"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-bytestream
                        go-golang-org-x-time
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-golang-org-x-net
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-enterprise-certificate-proxy
                        go-github-com-google-uuid
                        go-github-com-google-s2a-go
                        go-github-com-google-go-cmp
                        go-cloud-google-com-go-compute-metadata
                        go-cloud-google-com-go-auth-oauth2adapt
                        go-cloud-google-com-go-auth))
    (home-page "https://google.golang.org/api")
    (synopsis "Google APIs Client Library for Go")
    (description
     "Package api is the root of the packages used to access Google Cloud Services.
See
@@url{https://godoc.org/google.golang.org/api,https://godoc.org/google.golang.org/api}
for a full list of sub-packages.")
    (license license:bsd-3)))

(define-public go-storj-io-eventkit
  (package
    (name "go-storj-io-eventkit")
    (version "0.0.0-20250410172343-61f26d3de156")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/eventkit")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "storj.io/eventkit"))
    (propagated-inputs (list go-storj-io-picobuf
                             go-google-golang-org-protobuf
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-go-uber-org-zap
                             go-github-com-zeebo-errs-v2
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-spf13-cobra
                             go-github-com-spacemonkeygo-monkit-v3
                             go-github-com-pkg-errors
                             go-github-com-google-gopacket
                             go-github-com-elek-bubbles
                             go-github-com-charmbracelet-lipgloss
                             go-github-com-charmbracelet-bubbletea
                             go-cloud-google-com-go-bigquery))
    (home-page "https://storj.io/eventkit")
    (synopsis "eventkit")
    (description
     "a go library for reporting multidimensional events over UDP.")
    (license license:expat)))

(define-public go-storj-io-infectious
  (package
    (name "go-storj-io-infectious")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/infectious")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "storj.io/infectious"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://storj.io/infectious")
    (synopsis "infectious")
    (description
     "Package infectious implements Reed-Solomon forward error correction [1].  It
uses the Berlekamp-Welch [2] error correction algorithm to achieve the ability
to actually correct errors.")
    (license unknown-license!)))

(define-public go-storj-io-picobuf
  (package
    (name "go-storj-io-picobuf")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/picobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "storj.io/picobuf"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-zeebo-assert))
    (home-page "https://storj.io/picobuf")
    (synopsis "Picobuf")
    (description
     "Package picobuf is a light replacement for a subset of protobuf.")
    (license license:expat)))

(define-public go-storj-io-uplink
  (package
    (name "go-storj-io-uplink")
    (version "1.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storj/uplink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "storj.io/uplink"))
    (propagated-inputs (list go-storj-io-picobuf
                             go-storj-io-infectious
                             go-storj-io-eventkit
                             go-storj-io-drpc
                             go-storj-io-common
                             go-golang-org-x-sync
                             go-golang-org-x-exp
                             go-github-com-zeebo-sudo
                             go-github-com-zeebo-errs
                             go-github-com-stretchr-testify
                             go-github-com-spacemonkeygo-monkit-v3
                             go-github-com-klauspost-compress))
    (home-page "https://storj.io/uplink")
    (synopsis "Libuplink")
    (description
     "Package uplink is the main entrypoint to interacting with Storj Labs
decentralized storage network.")
    (license license:expat)))

(define-public go-github-com-ibm-go-sdk-core
  (package
    (name "go-github-com-ibm-go-sdk-core")
    (version "5.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IBM/go-sdk-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/IBM/go-sdk-core/v5"
      #:unpack-path "github.com/IBM/go-sdk-core"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                             go-github-com-stretchr-testify
                             go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo
                             go-github-com-hashicorp-go-retryablehttp
                             go-github-com-hashicorp-go-cleanhttp
                             go-github-com-go-playground-validator-v10
                             go-github-com-go-openapi-strfmt))
    (home-page "https://github.com/IBM/go-sdk-core")
    (synopsis "IBM Go SDK Core Version 5.21.0")
    (description
     "This project contains core functionality required by Go code generated by the
IBM Cloud @code{OpenAPI} SDK Generator (openapi-sdkgen).")
    (license license:asl2.0)))

(define-public go-dmitri-shuralyov-com-gpu-mtl
  (package
    (name "go-dmitri-shuralyov-com-gpu-mtl")
    (version "0.0.0-20221208032759-85de2813cf6b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://dmitri.shuralyov.com/gpu/mtl")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1smmksax508kyh62cjmdmpk1ia0a90xm340r94k56js99zmxprgp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "dmitri.shuralyov.com/gpu/mtl"))
    (home-page "https://dmitri.shuralyov.com/gpu/mtl")
    (synopsis #f)
    (description
     "Package mtl provides access to Apple's Metal API
(@@url{https://developer.apple.com/documentation/metal,https://developer.apple.com/documentation/metal}).")
    (license license:bsd-3)))

(define-public go-github-com-go-gl-glfw-v3-3-glfw
  (package
    (name "go-github-com-go-gl-glfw-v3-3-glfw")
    (version "0.0.0-20250301202403-da16c1255728")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-gl/glfw")
             (commit (go-version->git-ref version
                                          #:subdir "v3.3/glfw"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-gl/glfw/v3.3/glfw"
      #:unpack-path "github.com/go-gl/glfw"))
    (home-page "https://github.com/go-gl/glfw")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-jezek-xgb
  (package
    (name "go-github-com-jezek-xgb")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jezek/xgb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jezek/xgb"))
    (home-page "https://github.com/jezek/xgb")
    (synopsis #f)
    (description
     "Package XGB provides the X Go Binding, which is a low-level API to communicate
with the core X protocol and many of the X extensions.")
    (license unknown-license!)))

(define-public go-golang-org-x-exp-shiny
  (package
    (name "go-golang-org-x-exp-shiny")
    (version "0.0.0-20251002181428-27f1f14c8bb9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version
                                          #:subdir "shiny"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01p6zi6b2q2sjciaa542b331qxwzklfbw30cidr2nk5i3y2gvcl9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "golang.org/x/exp/shiny"
      #:unpack-path "golang.org/x/exp"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-golang-org-x-mobile
                             go-golang-org-x-image
                             go-github-com-jezek-xgb
                             go-github-com-go-gl-glfw-v3-3-glfw
                             go-dmitri-shuralyov-com-gpu-mtl))
    (home-page "https://golang.org/x/exp")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-golang-org-x-tools-go-expect
  (package
    (name "go-golang-org-x-tools-go-expect")
    (version "0.1.1-deprecated")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (go-version->git-ref version
                                          #:subdir "go/expect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjvngpahkb5x573i855fjlb1fdmr6n269nmb5xxnbabjb27mnvg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "golang.org/x/tools/go/expect"
      #:unpack-path "golang.org/x/tools"))
    (propagated-inputs (list go-golang-org-x-mod))
    (home-page "https://golang.org/x/tools")
    (synopsis #f)
    (description
     "Package expect provides support for interpreting structured comments in Go
source code (including go.mod and go.work files) as test expectations.")
    (license license:bsd-3)))

(define-public go-golang-org-x-tools-go-packages-packagestest
  (package
    (name "go-golang-org-x-tools-go-packages-packagestest")
    (version "0.1.1-deprecated")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (go-version->git-ref version
                                          #:subdir "go/packages/packagestest"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjvngpahkb5x573i855fjlb1fdmr6n269nmb5xxnbabjb27mnvg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "golang.org/x/tools/go/packages/packagestest"
      #:unpack-path "golang.org/x/tools"))
    (propagated-inputs (list go-golang-org-x-tools-go-expect
                             go-golang-org-x-tools))
    (home-page "https://golang.org/x/tools")
    (synopsis #f)
    (description
     "Package packagestest creates temporary projects on disk for testing go tools on.")
    (license license:bsd-3)))

(define-public go-golang-org-x-mobile
  (package
    (name "go-golang-org-x-mobile")
    (version "0.0.0-20250911085028-6912353760cf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/mobile")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yp0hiy5nqvs7gf91w1bn29s42i01lwm5mg1qmqkfy8bvzzkz3gm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "golang.org/x/mobile"))
    (propagated-inputs (list go-golang-org-x-tools-go-packages-packagestest
                             go-golang-org-x-tools
                             go-golang-org-x-sync
                             go-golang-org-x-mod
                             go-golang-org-x-image
                             go-golang-org-x-exp-shiny))
    (home-page "https://golang.org/x/mobile")
    (synopsis "Go support for Mobile devices")
    (description
     "The Go mobile repository holds packages and build tools for using Go on mobile
platforms.")
    (license license:bsd-3)))

(define-public go-github-com-rclone-rclone
  (package
    (name "go-github-com-rclone-rclone")
    (version "1.71.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rclone/rclone")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go #{go-#f}#
      #:import-path "github.com/rclone/rclone"))
    (propagated-inputs (list go-golang-org-x-term
                        go-golang-org-x-mobile
                        go-github-com-pkg-xattr
                        go-github-com-golang-jwt-jwt-v4
                        go-github-com-protonmail-go-crypto
                        go-github-com-ibm-go-sdk-core
                        go-storj-io-uplink
                        go-gopkg-in-yaml-v3
                        go-gopkg-in-natefinch-lumberjack-v2
                        go-google-golang-org-api
                        go-golang-org-x-time
                        go-golang-org-x-text
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-golang-org-x-net
                        go-golang-org-x-crypto
                        go-goftp-io-server
                        go-go-etcd-io-bbolt
                        go-github-com-zeebo-xxh3
                        go-github-com-zeebo-blake3
                        go-github-com-yunify-qingstor-sdk-go
                        go-github-com-youmark-pkcs8
                        go-github-com-xanzy-ssh-agent
                        go-github-com-winfsp-cgofuse
                        go-github-com-willscott-go-nfs
                        go-github-com-unknwon-goconfig
                        go-github-com-t3rm1n4l-go-mega
                        go-github-com-stretchr-testify
                        go-github-com-spf13-pflag
                        go-github-com-spf13-cobra
                        go-github-com-skratchdot-open-golang
                        go-github-com-shirou-gopsutil
                        go-github-com-rogpeppe-go-internal
                        go-github-com-rivo-uniseg
                        go-github-com-rfjakob-eme
                        go-github-com-rclone-gofakes3
                        go-github-com-quasilyte-go-ruleguard-dsl
                        go-github-com-putdotio-go-putio-putio
                        go-github-com-prometheus-client-golang
                        go-github-com-pmezard-go-difflib
                        go-github-com-pkg-sftp
                        go-github-com-peterh-liner
                        go-github-com-patrickmn-go-cache
                        go-github-com-oracle-oci-go-sdk
                        go-github-com-ncw-swift
                        go-github-com-moby-sys-mountinfo
                        go-github-com-mitchellh-go-homedir
                        go-github-com-minio-minio-go-v7
                        go-github-com-mattn-go-runewidth
                        go-github-com-mattn-go-colorable
                        go-github-com-lanrat-extsort
                        go-github-com-koofr-go-koofrclient
                        go-github-com-koofr-go-httpclient
                        go-github-com-klauspost-compress
                        go-github-com-jzelinskie-whirlpool
                        go-github-com-josephspurrier-goversioninfo
                        go-github-com-jlaffaye-ftp
                        go-github-com-jcmturner-gokrb5-v8
                        go-github-com-henrybear327-go-proton-api
                        go-github-com-henrybear327-proton-api-bridge
                        go-github-com-hanwen-go-fuse-v2
                        go-github-com-google-uuid
                        go-github-com-go-git-go-billy-v5
                        go-github-com-go-darwin-apfs
                        go-github-com-go-chi-chi-v5
                        go-github-com-gdamore-tcell-v2
                        go-github-com-gabriel-vasile-mimetype
                        go-github-com-dropbox-dropbox-sdk-go-unofficial
                        go-github-com-dop251-scsu
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-coreos-go-semver
                        go-github-com-colinmarc-hdfs
                        go-github-com-cloudsoda-go-smb2
                        go-github-com-cloudinary-cloudinary-go
                        go-github-com-buengese-sgzip
                        go-github-com-aws-smithy-go
                        go-github-com-aws-aws-sdk-go-v2-service-s3
                        go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
                        go-github-com-aws-aws-sdk-go-v2-credentials
                        go-github-com-aws-aws-sdk-go-v2-config
                        go-github-com-aws-aws-sdk-go-v2
                        go-github-com-atotto-clipboard
                        go-github-com-anacrolix-log
                        go-github-com-anacrolix-dms
                        go-github-com-abbot-go-http-auth
                        go-github-com-aalpar-deheap
                        go-github-com-a8m-tree
                        go-github-com-max-sum-base32768
                        go-github-com-files-com-files-sdk-go
                        go-github-com-azure-go-ntlmssp
                        go-github-com-azure-azure-sdk-for-go-sdk-storage-azfile
                        go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore
                        go-bazil-org-fuse))
    (home-page "https://github.com/rclone/rclone")
    (synopsis "Rclone")
    (description
     "Sync files and directories to and from local and remote object stores.")
    (license license:expat)))

