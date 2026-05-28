;;; Network-level blocking: append a custom hostlist of known cookie-consent
;;; CDN domains to the default blocklist (keeps StevenBlack malware/ad blocking).
(define-configuration nyxt/mode/blocker:blocker-mode
  ((nyxt/mode/blocker:hostlists
    (append (list (make-instance 'nyxt/mode/blocker:hostlist
                                 :hosts '("consent.cookiebot.com"
                                          "cdn.cookielaw.org"
                                          "cookie-cdn.cookiepro.com"
                                          "geolocation.onetrust.com"
                                          "optanon.blob.core.windows.net"
                                          "privacymanager.io"
                                          "cdn.privacy-mgmt.com"
                                          "api.privacy-mgmt.com"
                                          "quantcast.mgr.consensu.org"
                                          "cmp.quantcast.com"
                                          "cdn.sourcepoint.com"
                                          "api.sourcepoint.com"
                                          "sourcepoint.com")))
            %slot-value%))))

(define-configuration web-buffer
  ((default-modes (append '(nyxt/mode/blocker:blocker-mode) %slot-value%))))

;;; CSS injection: hides any banners that slip through network blocking
;;; (first-party/inline implementations). Uses user-styles, not user-scripts.
(define-configuration nyxt/mode/user-script:user-script-mode
  ((nyxt/mode/user-script:user-styles
    (list (make-instance 'nyxt/mode/user-script:user-style
                         :code "
/* OneTrust / OptAnon */
#onetrust-banner-sdk, #onetrust-accept-btn-handler,
.optanon-alert-box-wrapper,
/* SourcePoint (dynamic IDs) */
[id^='sp_message_container'], [id^='sp_message_id'],
.message-overlay, .message-container,
[class*='message-overlay'], [class*='sp-message'],
/* Generic cookie banners */
#cookiebanner, #cookie-notice, #cookie-bar,
.cookieconsent, .cc-window, .cc-banner,
.cookie-banner, .cookie-notice, .cookie-consent,
[id*='cookie-banner'], [id*='cookie-notice'], [class*='cookie-banner'],
[class*='cookie-notice'], [class*='cookie-consent'],
[id*='gdpr'], [class*='gdpr'],
[id*='consent-popup'], [class*='consent-popup'],
[id*='consent-banner'], [class*='consent-banner'],
.cmpbox, #cmpbox, .cmpmain,
.sp-message-container,
div[aria-label*='cookie' i], div[aria-label*='consent' i]
{ display: none; visibility: hidden; }

/* Restore scrolling when banners lock the page body */
body.has-overlay, body.overflow-hidden, body.noscroll
{ overflow: auto; }"
                         :level :user)))))

(define-configuration web-buffer
  ((default-modes (append '(nyxt/mode/user-script:user-script-mode) %slot-value%))))
