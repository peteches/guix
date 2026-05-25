(defun peteches/mpv-args (url-string)
  "Return mpv argument list for URL-STRING, using the youtube profile where appropriate."
  (if (cl-ppcre:scan "youtube\\.com|youtu\\.be" url-string)
      (list "mpv" "--include=~/.config/mpv/profiles/youtube.conf" url-string)
      (list "mpv" url-string)))

(define-command open-hint-in-mpv ()
  "Activate hint overlay and open the chosen link in mpv."
  (nyxt/mode/hint:query-hints "Open in MPV"
   (lambda (elements)
     (nyxt:echo "mpv: ~a element(s) selected" (length elements))
     (let* ((elem (first elements))
            (href (plump:attribute elem "href")))
       (nyxt:echo "mpv: href=~s type=~s" href (type-of elem))
       (if href
           (handler-case
               (let* ((base (nyxt:url (nyxt:current-buffer)))
                      (resolved (quri:render-uri (quri:merge-uris (quri:uri href) base))))
                 (nyxt:echo "mpv: launching ~s" resolved)
                 (uiop:launch-program (peteches/mpv-args resolved)
                                      :output "/tmp/mpv-nyxt.log"
                                      :error-output "/tmp/mpv-nyxt.log"))
             (error (e)
               (nyxt:echo-warning "mpv error: ~a" e)))
           (nyxt:echo-warning "mpv: selected element has no href"))))))
