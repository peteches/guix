;;; peteches-treesit.el --- Shared tree-sitter setup -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Keep tree-sitter grammar discovery, fallback source installation, and
;;; tree-sitter major-mode selection in one place.
;;;
;;; On Guix systems the preferred path is declarative grammar installation via
;;; Guix.  `treesit-language-source-alist' is still configured here as a manual
;;; fallback for grammars you want to build with `treesit-install-language-grammar'.
;;;
;;; Straight.el remains the preferred path for fast-moving Emacs Lisp packages;
;;; this file only deals with native grammar libraries and built-in `treesit'.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar treesit-extra-load-path nil)
(defvar treesit-language-source-alist nil)
(defvar major-mode-remap-alist nil)

(defgroup peteches-treesit nil
  "Shared tree-sitter configuration."
  :group 'tools
  :prefix "peteches-treesit-")

(defcustom peteches-treesit-extra-load-path-candidates
  (list (expand-file-name "lib/tree-sitter" "~/.guix-home/profile")
        (expand-file-name "lib/tree-sitter" "~/.guix-profile"))
  "Candidate directories that may contain Guix-provided tree-sitter grammars."
  :type '(repeat directory)
  :group 'peteches-treesit)

(defcustom peteches-treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
    (gowork "https://github.com/omertuc/tree-sitter-go-work")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (scheme "https://github.com/6cdh/tree-sitter-scheme")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  "Manual fallback grammar sources for `treesit-install-language-grammar'.

These are not auto-installed.  Guix-provided grammar packages should remain the
normal path on Guix systems."
  :type '(repeat sexp)
  :group 'peteches-treesit)

(defcustom peteches-treesit-guix-grammar-languages
  '(awk bash c dockerfile go gomod hcl html json lua luadoc markdown org python scheme yaml)
  "Grammar languages expected from the Guix profile.

This is used for diagnostics only; installing the corresponding Guix packages
still happens in the Guix Home config."
  :type '(repeat symbol)
  :group 'peteches-treesit)

(defcustom peteches-treesit-mode-remaps
  '((python-mode python-ts-mode python)
    (go-mode go-ts-mode go)
    (go-mod-mode go-mod-ts-mode gomod)
    (json-mode json-ts-mode json)
    (yaml-mode yaml-ts-mode yaml)
    (lua-mode lua-ts-mode lua)
    (scheme-mode scheme-ts-mode scheme)
    (c-mode c-ts-mode c))
  "Mode remaps to install when the target mode and grammar are available.

Each entry has the form (FROM-MODE TO-MODE GRAMMAR).  Bash is intentionally not
included because `sh-mode' covers more than Bash; it is handled in
`peteches-bash.el' with a shell-specific predicate."
  :type '(repeat (list symbol symbol symbol))
  :group 'peteches-treesit)

(defun peteches/treesit-available-p ()
  "Return non-nil when this Emacs can use built-in tree-sitter."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun peteches/treesit-language-ready-p (language)
  "Return non-nil when LANGUAGE is available to built-in tree-sitter."
  (and (peteches/treesit-available-p)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p language)))

(defun peteches/treesit-mode-ready-p (mode language)
  "Return non-nil when MODE exists and LANGUAGE is available."
  (and (fboundp mode)
       (peteches/treesit-language-ready-p language)))

(defun peteches/treesit--merge-language-sources ()
  "Merge `peteches-treesit-language-source-alist' into Emacs' source alist."
  (dolist (source peteches-treesit-language-source-alist)
    (setf (alist-get (car source) treesit-language-source-alist)
          (cdr source))))

(defun peteches/treesit--configure-extra-load-paths ()
  "Add existing Guix tree-sitter grammar directories to `treesit-extra-load-path'."
  (setq treesit-extra-load-path
        (delete-dups
         (append
          (seq-filter #'file-directory-p peteches-treesit-extra-load-path-candidates)
          treesit-extra-load-path))))

(defun peteches/treesit-add-remap (from-mode to-mode language)
  "Remap FROM-MODE to TO-MODE when LANGUAGE is ready."
  (when (and (boundp 'major-mode-remap-alist)
             (peteches/treesit-mode-ready-p to-mode language))
    (add-to-list 'major-mode-remap-alist (cons from-mode to-mode))))

(defun peteches/treesit-configure-remaps ()
  "Install configured tree-sitter mode remaps when they are safe to use."
  (interactive)
  (dolist (entry peteches-treesit-mode-remaps)
    (pcase-let ((`(,from-mode ,to-mode ,language) entry))
      (peteches/treesit-add-remap from-mode to-mode language))))

(defun peteches/treesit-known-languages ()
  "Return languages known from Guix diagnostics, source fallbacks, and remaps."
  (sort
   (delete-dups
    (append peteches-treesit-guix-grammar-languages
            (mapcar #'car peteches-treesit-language-source-alist)
            (mapcar (lambda (entry) (nth 2 entry)) peteches-treesit-mode-remaps)))
   (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun peteches/treesit-install-language (language)
  "Manually install tree-sitter grammar LANGUAGE from the configured source alist."
  (interactive
   (list
    (intern
     (completing-read
      "Install tree-sitter grammar: "
      (mapcar (lambda (source) (symbol-name (car source)))
              peteches-treesit-language-source-alist)
      nil t))))
  (unless (peteches/treesit-available-p)
    (user-error "This Emacs cannot use built-in tree-sitter"))
  (peteches/treesit--merge-language-sources)
  (unless (assoc language treesit-language-source-alist)
    (user-error "No source configured for %s" language))
  (treesit-install-language-grammar language)
  (peteches/treesit-configure-remaps)
  (message "Installed tree-sitter grammar: %s" language))

(defun peteches/treesit-install-languages (languages)
  "Manually install each grammar in LANGUAGES."
  (dolist (language languages)
    (peteches/treesit-install-language language)))

(defun peteches/treesit-install-bash-grammar ()
  "Manually install the Bash tree-sitter grammar."
  (interactive)
  (peteches/treesit-install-language 'bash))

(defun peteches/treesit-install-python-grammar ()
  "Manually install the Python tree-sitter grammar."
  (interactive)
  (peteches/treesit-install-language 'python))

(defun peteches/treesit-install-scheme-grammar ()
  "Manually install the Scheme tree-sitter grammar."
  (interactive)
  (peteches/treesit-install-language 'scheme))

(defun peteches/treesit-install-go-grammars ()
  "Manually install Go-related tree-sitter grammars."
  (interactive)
  (peteches/treesit-install-languages '(go gomod gowork)))

(defalias 'peteches-bash-install-treesit-grammers
  #'peteches/treesit-install-bash-grammar)
(defalias 'peteches-python-install-treesit-grammers
  #'peteches/treesit-install-python-grammar)
(defalias 'peteches-guile-install-treesit-grammers
  #'peteches/treesit-install-scheme-grammar)
(defalias 'peteches-go-install-treesit-grammars
  #'peteches/treesit-install-go-grammars)

(defun peteches/treesit-status ()
  "Show the current tree-sitter grammar and remap status."
  (interactive)
  (peteches/treesit--configure-extra-load-paths)
  (with-help-window "*Peteches Tree-sitter*"
    (princ (format "Built-in tree-sitter: %s\n"
                   (if (peteches/treesit-available-p) "available" "unavailable")))
    (princ (format "\nGrammar load path:\n%s\n"
                   (if treesit-extra-load-path
                       (mapconcat (lambda (dir) (concat "  " dir))
                                  treesit-extra-load-path "\n")
                     "  (empty)")))
    (princ "\nLanguages:\n")
    (dolist (language (peteches/treesit-known-languages))
      (princ (format "  %-12s %s\n"
                     language
                     (if (peteches/treesit-language-ready-p language)
                         "available"
                       "missing"))))
    (princ "\nMode remaps:\n")
    (dolist (entry peteches-treesit-mode-remaps)
      (pcase-let ((`(,from-mode ,to-mode ,language) entry))
        (princ (format "  %-18s -> %-18s %s\n"
                       from-mode
                       to-mode
                       (if (peteches/treesit-mode-ready-p to-mode language)
                           "enabled"
                         "not ready")))))))

(peteches/treesit--merge-language-sources)
(peteches/treesit--configure-extra-load-paths)
(peteches/treesit-configure-remaps)
(add-hook 'after-init-hook #'peteches/treesit-configure-remaps)

(provide 'peteches-treesit)
;;; peteches-treesit.el ends here
