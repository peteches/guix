;;;; Minimal, unobtrusive Company configuration (no use-package)

;; Core Company setup
(require 'company)

;; Don’t pop up the menu automatically; only show it when you invoke it.
(setq company-idle-delay nil)            ; manual trigger only (no auto popup)
(setq company-minimum-prefix-length 1)   ; start completing after 1 char when invoked
(setq company-require-match 'never)      ; don’t force selecting from the menu
(setq company-tooltip-align-annotations t)

;; Keep the UI slim (no icon margin). Safe on recent Company; ignored if unknown.
(setq company-format-margin-function nil)

;; Enable globally after init.
(add-hook 'after-init-hook #'global-company-mode)

;; Keybindings for manual completion:
;; C-M-i is a good, WM-safe binding (equivalent to M-TAB in Emacs).
(global-set-key (kbd "C-M-i") #'company-complete)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "C-n")   #'company-select-next)
  (define-key company-active-map (kbd "C-p")   #'company-select-previous))

;; ---------------------------------------------------------------------------
;; LSP backend (company-lsp)
;; ---------------------------------------------------------------------------
;;
;; company-lsp: Queries your active lsp-mode server via the LSP
;; textDocument/completion request. Asynchronous; returns rich, language-aware
;; candidates straight from the server.
;;
;; Notes:
;; - Only produces candidates when lsp-mode is active for the buffer.
;; - We combine it :with a few light, local backends so you also get file paths,
;;   mode keywords, and nearby identifiers while LSP is thinking.
(when (require 'company-lsp nil t)
  ;; No global push; we curate backends per-mode below.
  )

;; ---------------------------------------------------------------------------
;; Emoji backend (company-emoji)
;; ---------------------------------------------------------------------------
;;
;; company-emoji: Completes emoji by shortcode (e.g., :smile:) and/or Unicode.
;; Inserts the actual emoji character. Great for chat logs, docs, commit msgs.
(when (require 'company-emoji nil t)
  ;; Optionally, ensure your fontset can render color emoji:
 (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)))

;; ---------------------------------------------------------------------------
;; Curated backends per context, with comments describing each one.
;; Company consults the first “group” that yields candidates; within a group
;; we use `:with` to merge results from multiple backends.
;; ---------------------------------------------------------------------------

(defun peteches/company-backends-prog ()
  "Backends for programming modes (code + comments/strings)."
  (set (make-local-variable 'company-backends)
       '(
         ;; Primary group for code:
         (company-lsp                 ; LSP server completions (async, semantic)
          :with
          company-capf               ; Bridge to `completion-at-point-functions`
                                     ; (good fallback: eglot, major-mode CAPFs)
          company-dabbrev-code       ; Identifiers from current/other code buffers
          company-files              ; File/dir paths in strings/imports/etc.
          company-keywords)          ; Language keywords for current major mode

         ;; Secondary helpers (queried if the first group yields nothing):
         company-emoji               ; Emoji by :shortcode: or Unicode
         company-dabbrev             ; Plain words from all open buffers
         )))

(add-hook 'prog-mode-hook #'peteches/company-backends-prog)

(defun peteches/company-backends-text ()
  "Backends for text-ish modes (org, markdown, commit messages, mail…)."
  (set (make-local-variable 'company-backends)
       '(
         ;; Keep it lightweight and writing-centric:
         company-emoji               ; Emoji completion for prose/notes/commits
         company-dabbrev             ; Words from open buffers (great for prose)
         company-files               ; Insert file paths as needed
         )))

(add-hook 'text-mode-hook #'peteches/company-backends-text)

;; Optional tweaks for dabbrev to feel smarter:
(setq company-dabbrev-other-buffers t)   ; also scan other buffers
(setq company-dabbrev-code-everywhere t) ; complete code symbols in strings/comments
(setq company-dabbrev-ignore-case 'keep) ; preserve case

(provide 'peteches-company)
