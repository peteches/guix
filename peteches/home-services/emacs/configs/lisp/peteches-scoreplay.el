
(add-to-list 'org-capture-templates
             '("s" "ScorePlay Task" entry
               (file ,(expand-file-name "agenda/ScorePlay.org" org-directory))
               "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n\n"))


(provide 'peteches-scoreplay)
