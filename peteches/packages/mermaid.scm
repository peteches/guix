(define-module (peteches packages mermaid)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix gexp) ;for #~ gexps
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (gnu packages chromium)
  #:use-module (peteches packages mermaid-deps)
  #:export (node-mermaid-js-mermaid-cli))

(define-public node-mermaid-js-mermaid-cli
  (package
    (name "node-mermaid-js-mermaid-cli")
    (version "11.16.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@mermaid-js/mermaid-cli/-/mermaid-cli-11.16.0.tgz")
       (sha256
        (base32 "0fal0zljzrlx9pywlx3640g4jsja6lqfm8a01alnrjpr3ccrbmv5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)

          (add-before 'configure 'force-ignore-scripts
            (lambda _
              ;; Ensure npm lifecycle scripts never run during the build.
              (setenv "npm_config_ignore_scripts" "true")))

          (add-after 'patch-dependencies 'disable-npm-scripts
            (lambda _
              (define (alist-delete* keys alist)
                (let loop
                  ((ks keys)
                   (a alist))
                  (if (null? ks) a
                      (loop (cdr ks)
                            (alist-delete (car ks) a)))))
              (modify-json (lambda (j)
                             (let* ((scripts (assoc-ref j "scripts"))
                                    (scripts* (if (and scripts
                                                       (list? scripts))
                                                  (alist-delete* '("prepare"
                                                                   "prepublish"
                                                                   "prepublishOnly"
                                                                   "build"
                                                                   "build:main"
                                                                   "prebuild"
                                                                   "postbuild"
                                                                   "prepack"
                                                                   "postinstall"
                                                                   "install")
                                                                 scripts)
                                                  scripts))
                                    (j* (cond
                                          ((not (and scripts
                                                     (list? scripts)))
                                           j)
                                          ((or (not scripts*)
                                               (null? scripts*))
                                           (alist-delete "scripts" j))
                                          (else (alist-replace "scripts"
                                                               scripts* j)))))
                               (alist-delete "husky" j*))))))

          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@fortawesome/fontawesome-free"
                                                  "@mermaid-js/layout-elk"
                                                  "@tsconfig/node18"
                                                  "@types/node"
                                                  "jest"
                                                  "puppeteer"
                                                  "standard"
                                                  "typescript"
                                                  "vite"
                                                  "yarn-upgrade-all")))))
          (add-after 'install 'link-runtime-node-deps
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (pkg (string-append out
                           "/lib/node_modules/@mermaid-js/mermaid-cli"))
                     (nm (string-append pkg "/node_modules"))
                     (pup (search-input-directory inputs
                           "lib/node_modules/puppeteer"))
                     (pupc (search-input-directory inputs
                            "lib/node_modules/puppeteer-core")))
                (mkdir-p nm)
                ;; Make "import 'puppeteer'" resolvable from mermaid-cli’s src/index.js.
                (when pup
                  (let ((dst (string-append nm "/puppeteer")))
                    (when (file-exists? dst)
                      (delete-file-recursively dst))
                    (symlink pup dst)))
                ;; Optional: also make puppeteer-core resolvable locally.
                (when pupc
                  (let ((dst (string-append nm "/puppeteer-core")))
                    (when (file-exists? dst)
                      (delete-file-recursively dst))
                    (symlink pupc dst))))))
          (add-after 'install 'wrap-mmdc-with-chromium
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (mmdc (string-append out "/bin/mmdc"))
                     (chrome (search-input-file inputs "/bin/chromium")))
                (when (file-exists? mmdc)
                  (wrap-program mmdc
                    `("PUPPETEER_EXECUTABLE_PATH" =
                      (,chrome))
                    `("PUPPETEER_SKIP_DOWNLOAD" =
                      ("1")))) #t))))))
    (inputs (list ungoogled-chromium
                  node-mermaid-11.12.2
                  node-import-meta-resolve-4.2.0
                  node-commander-14.0.2
                  node-chalk-5.6.2
                  node-mermaid-js-mermaid-zenuml-0.2.2
                  node-puppeteer-core-23.11.1
                  node-puppeteer-23.11.1))
    (home-page "https://github.com/mermaid-js/mermaid-cli#readme")
    (synopsis "Command-line interface for mermaid")
    (description "Command-line interface for mermaid")
    (license license:expat)))
