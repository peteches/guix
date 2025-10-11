(define-module (peteches home-configs firefox)
  #:use-module (guix gexp)
  #:use-module (peteches home-services firefox))

(define-public base-firefox-profiles
  (list
   (firefox-profile "Default" "default"
		    #:prefs '(("browser.startup.homepage" . "about:blank")))
   
   (firefox-profile "ScorePlay" "scoreplay"
		    #:extensions `(("5c474add-03f0-4c67-9479-f32939d7599a"
				    . ,(local-file "./firefox-extensions/aws_sso_container-v1.14.xpi")))
		    #:prefs '(
			      ;; Startup: open specific pages
			      ("browser.startup.page"                                 . 1) ; 1 = homepage(s)
			      ("browser.startup.homepage"                             . "https://mail.google.com/|https://calendar.google.com/|https://meet.google.com/")))
   
   (firefox-profile "Other"   "other"
		    #:prefs '(
			      ("browser.startup.homepage" . "about:blank")
			      ;; Tor SOCKS proxy settings
			      ("network.proxy.type"                                   . 1) ; manual proxy
			      ("network.proxy.socks"                                  . "127.0.0.1")
			      ("network.proxy.socks_port"                             . 9050) ; or 9150 if using TBB
			      ("network.proxy.socks_version"                          . 5)
			      ("network.proxy.socks_remote_dns"                       . #t) ; DNS through Tor
			      ("network.proxy.no_proxies_on"                          . "")

			      ;; Ensure DoH doesn’t bypass Tor
			      ("network.trr.mode"                                     . 5)

			      ;; WebRTC handling (disable or restrict to proxy only)
			      ("media.peerconnection.enabled"                         . #f)
			      ("media.peerconnection.ice.proxy_only"                  . #t)))))

(define-public base-firefox-global-prefs
  '(
    ;; ("privacy.resistFingerprinting"                         . #f)
    ;; ("privacy.trackingprotection.enabled"                   . #t)
    ;; ("privacy.trackingprotection.socialtracking.enabled"    . #t)
    ;; ("dom.security.https_only_mode"                         . #t)

    ;; ;; Cookies: 5 = Total Cookie Protection / partitioned cookies

    ;; ("network.cookie.cookieBehavior"                        . 5)

    ;; ;; Reduce passive leaks / speculation
    ;; ("network.http.referer.XOriginPolicy"                   . 2)
    ;; ("network.predictor.enabled"                            . #f)
    ;; ("network.prefetch-next"                                . #f)
    ;; ("network.dns.disablePrefetch"                          . #t)

    ;; ;; Disable all password saving & prompts
    ;; ("signon.rememberSignons"                               . #f)
    ;; ("signon.autofillForms"                                 . #f)
    ;; ("signon.formlessCapture.enabled"                       . #f)
    ;; ("signon.generation.enabled"                            . #f)
    ;; ("signon.management.page.breach-alerts.enabled"         . #f)
    ;; ("signon.autologin.proxy"                               . #f)

    ;; ;; Telemetry / studies / Pocket / suggestions
    ;; ("toolkit.telemetry.enabled"                            . #f)
    ;; ("toolkit.telemetry.unified"                            . #f)
    ;; ("datareporting.healthreport.uploadEnabled"             . #f)
    ;; ("app.shield.optoutstudies.enabled"                     . #f)
    ;; ("experiments.enabled"                                  . #f)
    ;; ("browser.ping-centre.telemetry"                        . #f)
    ;; ("extensions.pocket.enabled"                            . #f)
    ;; ("browser.newtabpage.activity-stream.feeds.section.topstories" . #f)
    ;; ("browser.urlbar.quicksuggest.enabled"                  . #f)
    ;; ("browser.urlbar.suggest.quicksuggest.sponsored"        . #f)
    ;; ("browser.urlbar.suggest.quicksuggest.nonsponsored"     . #f)
    ;; ("browser.urlbar.suggest.trending"                      . #f)

    ;; ;; Default search engine (must match installed engine name)
    ;; ("browser.search.selectedEngine"                        . "DuckDuckGo")
    ;; ("browser.urlbar.placeholderName"                       . "DuckDuckGo")

    ;; Safe browsing remote checks (set to #f if you prefer fewer pings)
    ;; ("browser.safebrowsing.downloads.remote.enabled"     . #f)
    ))

(define-public base-firefox-global-extensions
  `(
    ;; Dark Reader
    ("addon@darkreader.org"
     . ,(local-file "./firefox-extensions/darkreader-4.9.110.xpi"))
    
    ;; uBlock Origin
    ("uBlock0@raymondhill.net"
     . ,(local-file "./firefox-extensions/uBlock0_1.65.0.firefox.signed.xpi"))

    ;; PassFF (Password Store)
    ("passff@invicem.pro"
     . ,(local-file "./firefox-extensions/passff-1.22.1.xpi"))))
