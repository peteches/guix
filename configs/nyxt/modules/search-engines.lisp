;;; Atlas SearXNG — searches Atlas but uses DDG's completion API
(define-class peteches/atlas-search-engine (ddg-search-engine)
  ((name "Atlas SearXNG")
   (shortcut "a")
   (control-url "https://search.atlas.engineer/searxng/search?q=~a")
   (control-completion-url "https://duckduckgo.com/ac/?q=~a"))
  (:export-class-name-p t))

(define-configuration browser
  ((search-engines
    (list
     (make-instance 'ddg-search-engine)
     (make-instance 'wikipedia-search-engine)

     (make-instance 'search-engine
		    :name "YouTube"
		    :shortcut "yt"
		    :control-url "https://youtube.com/search?q=~a")
     (make-instance 'search-engine
                    :name "GitHub"
                    :shortcut "gh"
                    :control-url "https://github.com/search?q=~a&type=repositories")
     (make-instance 'search-engine
                    :name "GoLang Packages"
                    :shortcut "gop"
                    :control-url "https://pkg.go.dev/search?q=~a")
     (make-instance 'peteches/atlas-search-engine)))))
