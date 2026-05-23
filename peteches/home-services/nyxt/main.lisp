(let ((config-dir (uiop:xdg-config-home "nyxt/")))
  (dolist (module '("modules/styling.lisp"
                    "modules/keybinds.lisp"
                    "modules/search-engines.lisp"
                    "modules/password-store.lisp"))
    (load (uiop:merge-pathnames* module config-dir))))
