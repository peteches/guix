(define-module (peteches home-configs firefox)
  #:use-module (guix gexp)
  #:use-module (peteches home-services firefox))

(define-public base-firefox-profiles
  (list
   (firefox-profile "Default" "default"
		    #:prefs '(("browser.startup.homepage" . "about:blank")))
   (firefox-profile "Other"   "other"
		    #:prefs '(("browser.startup.homepage" . "about:blank")))))

(define-public base-firefox-global-prefs
  '(
    ;; Privacy / fingerprinting / HTTPS
    ("privacy.resistFingerprinting"                         . #t)
    ("privacy.trackingprotection.enabled"                   . #t)
    ("privacy.trackingprotection.socialtracking.enabled"    . #t)
    ("dom.security.https_only_mode"                         . #t)

    ;; Cookies: 5 = Total Cookie Protection / partitioned cookies
    ("network.cookie.cookieBehavior"                        . 5)

    ;; Reduce passive leaks / speculation
    ("network.http.referer.XOriginPolicy"                   . 2)
    ("network.predictor.enabled"                            . #f)
    ("network.prefetch-next"                                . #f)
    ("network.dns.disablePrefetch"                          . #t)

    ;; Telemetry / studies / Pocket / suggestions
    ("toolkit.telemetry.enabled"                            . #f)
    ("toolkit.telemetry.unified"                            . #f)
    ("datareporting.healthreport.uploadEnabled"             . #f)
    ("app.shield.optoutstudies.enabled"                     . #f)
    ("experiments.enabled"                                  . #f)
    ("browser.ping-centre.telemetry"                        . #f)
    ("extensions.pocket.enabled"                            . #f)
    ("browser.newtabpage.activity-stream.feeds.section.topstories" . #f)
    ("browser.urlbar.quicksuggest.enabled"                  . #f)
    ("browser.urlbar.suggest.quicksuggest.sponsored"        . #f)
    ("browser.urlbar.suggest.quicksuggest.nonsponsored"     . #f)
    ("browser.urlbar.suggest.trending"                      . #f)

    ;; Safe browsing remote checks (set to #f if you prefer fewer pings)
    ;; ("browser.safebrowsing.downloads.remote.enabled"     . #f)
    ))

(define-public base-firefox-global-extensions
  `(
    ;; uBlock Origin
    ("uBlock0@raymondhill.net"
     . ,(local-file "./firefox-extensions/uBlock0_1.65.0.firefox.signed.xpi"))

    ;; PassFF (Password Store)
    ("passff@invicem.pro"
     . ,(local-file "./firefox-extensions/passff-1.22.1.xpi"))))
