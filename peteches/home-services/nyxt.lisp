(define-configuration buffer
    ((default-modes
         (pushnew 'nyxt/mode/emacs:emacs-mode  %slot-value%))))

(defvar peteches/keymap (make-keymap "peteches-keymap"))
(define-key peteches/keymap
    "M-x" 'execute-command
    "C-Spc" 'nothing
    "C-o" 'follow-hint
    "M-o" 'follow-hint-new-buffer-focus)

(define-mode peteches-mode
    nil
  "Dummy mode for the custom key bindings in peteches/keymap"
  ((keyscheme-map
    (nkeymaps/core:make-keyscheme-map nyxt/keyscheme:emacs peteches/keymap))))
(define-configuration buffer
    "enable peteches-mode by default"
  ((default-modes (append '(peteches-mode)
                          %slot-value%))))

(define-configuration web-buffer
    "Add ssl only"
  ((default-modes (append '(nyxt/mode/blocker:blocker-mode
                            nyxt/mode/force-https:force-https-mode)
                          %slot-value%))))


(let ((ddg-completion
       (make-search-completion-function :base-url
					"https://duckduckgo.com/ac/?q=~a"
					:processing-function
					#'(lambda (results)
                                            (when results
                                              (map 'list
                                                   (lambda (hash-table)
                                                     (first
                                                      (alexandria:hash-table-values
                                                       hash-table)))
                                                   (njson:decode results)))))))
  (defvar peteches/search-engines
    (list
     (make-instance 'search-engine
                    :name "Wikipedia"
                    :shortcut "wiki"
                    :search-url "https://en.wikipedia.org/w/index.php?search=~a"
                    :fallback-url (quri.uri:uri "https://en.wikipedia.org/")
                    :completion-function (make-search-completion-function
					  :base-url "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
					  :processing-function #'(lambda (results)
								   (alexandria:when-let* ((results results)
											  (results (njson:decode results)))
                                                                     (map 'list #'list
                                                                          (njson:jget 1 results)
                                                                          (njson:jget 3 results))))))
     (make-instance 'search-engine
                    :name "GoLang Packages"
                    :shortcut "gop"
                    :search-url "https://pkg.go.dev/search?q=~a"
                    :fallback-url (quri.uri:uri "https://pkg.go.dev/"))
     (make-instance 'search-engine
                    :name "DuckDuckGo"
                    :shortcut "ddg"
                    :search-url "https://duckduckgo.com/?q=~a"
                    :fallback-url (quri.uri:uri "https://duckduckgo.com/")
                    :completion-function ddg-completion)
     (make-instance 'search-engine
                    :name "Atlas SearXNG instance"
                    :shortcut "a"
                    :search-url "https://search.atlas.engineer/searxng/search?q=~a"
                    :fallback-url (quri.uri:uri "https://search.atlas.engineer")
                    :completion-function ddg-completion))))


(define-configuration context-buffer
    "Setup my prefered search engines"
  ((search-engines peteches/search-engines)))

;; (define-configuration buffer
;;     (style (str:concat
;;             %slot-default%
;;             (cl-css:css '((body :background-color #600))))))
