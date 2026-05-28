;;; Named proxy configurations for the interactive selector
(defvar *peteches-proxies*
  '(("None"      . nil)
    ("Tailscale" . "socks5://localhost:1080")
    ("Tor"       . "socks5://127.0.0.1:9050")))

(defun peteches/make-proxy (url-string)
  (make-instance 'nyxt:proxy :url (quri:uri url-string)))

;;; URL pattern → proxy URL rules for automatic routing
(defvar *peteches-url-proxy-rules*
  '(("\\.spaniel-cordylus\\.ts\\.net" . "socks5://localhost:1080")))

;;; Mode holds per-buffer manual proxy preference and installs hooks.
;;; Follows the force-https-mode pattern from nyxt/source/mode/force-https.lisp.
(define-mode peteches/auto-proxy-mode ()
  "Route requests through a proxy based on URL patterns, with manual fallback.
Auto-matched proxy always wins for configured URL patterns.
For unmatched URLs the buffer falls back to the manual-proxy preference."
  ((manual-proxy nil :type (or nyxt:proxy null)
                 :documentation "User-selected fallback proxy for this buffer.")
   (load-handler nil :type t
                 :documentation "Stored buffer-load-hook handler for removal on disable.")))

(defun peteches/proxy-url-string (proxy)
  "Return the proxy URL as a string, or nil when no proxy is set."
  (when proxy (quri:render-uri (nyxt:url proxy))))

(defun peteches/auto-proxy-apply (buf mode url)
  "Set the effective proxy on BUF before a navigation or sub-request.
Returns URL unchanged (required by buffer-load-hook contract)."
  (let* ((host    (or (quri:uri-host url) ""))
         (matched (loop for (pattern . proxy-url) in *peteches-url-proxy-rules*
                        when (cl-ppcre:scan pattern host)
                        return proxy-url))
         (new-proxy (if matched
                        (peteches/make-proxy matched)
                        (manual-proxy mode))))
    ;; Only call (setf proxy) when the value changes.  Calling it
    ;; unconditionally invokes webkit_web_context_set_network_proxy_settings
    ;; on every navigation, which disrupts in-flight network state in
    ;; WebKitGTK 2.50+ and causes the first request to fail immediately.
    (unless (equal (peteches/proxy-url-string (nyxt:proxy buf))
                   (peteches/proxy-url-string new-proxy))
      (setf (nyxt:proxy buf) new-proxy)))
  url)

(defun peteches/auto-proxy-handler (request-data)
  "Keep proxy in sync for sub-requests after initial navigation."
  (let* ((buf  (buffer request-data))
         (mode (find-submode 'peteches/auto-proxy-mode buf)))
    (when mode
      (peteches/auto-proxy-apply buf mode (url request-data))))
  request-data)

(defmethod enable ((mode peteches/auto-proxy-mode) &key)
  (let* ((buf (buffer mode))
         (handler (make-instance 'nhooks:handler
                                 :fn (lambda (nav-url)
                                       (peteches/auto-proxy-apply buf mode nav-url))
                                 :name 'peteches/auto-proxy-load-handler)))
    (setf (load-handler mode) handler)
    (hooks:add-hook (slot-value buf 'nyxt::buffer-load-hook) handler)
    (hooks:add-hook (request-resource-hook buf)
                    'peteches/auto-proxy-handler)))

(defmethod disable ((mode peteches/auto-proxy-mode) &key)
  (let ((buf (buffer mode)))
    (when (load-handler mode)
      (hooks:remove-hook (slot-value buf 'nyxt::buffer-load-hook)
                         (load-handler mode)))
    (hooks:remove-hook (request-resource-hook buf)
                       'peteches/auto-proxy-handler)))

;;; Enable auto-proxy on all web buffers
(define-configuration web-buffer
  ((default-modes (append '(peteches/auto-proxy-mode) %slot-value%))))

;;; Interactive command: sets preference on mode AND applies it immediately.
(define-command set-buffer-proxy (&optional (buffer (nyxt:current-buffer)))
  "Set the proxy for the current buffer.
For URLs matching an auto-proxy rule, the rule still takes priority."
  (let* ((name          (prompt1 :prompt "Select proxy"
                                 :sources (make-instance 'prompter:source
                                                        :name "Proxies"
                                                        :constructor (mapcar #'car *peteches-proxies*))))
         (url           (cdr (assoc name *peteches-proxies* :test #'string=)))
         (mode          (find-submode 'peteches/auto-proxy-mode buffer))
         (proxy-instance (when url (peteches/make-proxy url))))
    (if mode
        (progn
          (setf (manual-proxy mode) proxy-instance)
          (setf (nyxt:proxy buffer) proxy-instance)
          (nyxt:echo "Proxy: ~a" name))
        (nyxt:echo-warning "peteches/auto-proxy-mode not active on this buffer"))))

;;; Status bar indicator — shows active proxy name, or nothing when no proxy.

(defun peteches/proxy-display-name (proxy)
  "Return the human-readable name for PROXY from *peteches-proxies*, or the raw URL."
  (when proxy
    (or (car (rassoc (quri:render-uri (url proxy))
                     *peteches-proxies*
                     :test #'string=))
        (quri:render-uri (url proxy)))))

(defmethod mode-status ((status status-buffer) (mode peteches/auto-proxy-mode))
  ;; when-let triggers Nyxt 4's static method-body analyser; use let+if instead.
  ;; Return "" (not nil) when no proxy is active: Nyxt 4 requires mode-status to return a string.
  (let ((name (peteches/proxy-display-name (nyxt:proxy (buffer mode)))))
    (if name
        (format nil "[proxy:~a]" name)
        "")))
