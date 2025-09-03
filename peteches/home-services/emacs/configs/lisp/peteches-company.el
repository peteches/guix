;;;; Minimal, unobtrusive Company UI polish (no use-package)

(require 'company)

;; Core behavior: manual trigger, early prefix, slim UI
(setq company-idle-delay nil
      company-minimum-prefix-length 1
      company-require-match nil
      company-tooltip-align-annotations t
      company-format-margin-function nil)

;; Popup feel
(setq company-selection-wrap-around t      ; cycle at ends
      company-show-quick-access t          ; M-1..M-0 jump to candidate
      company-tooltip-limit 12             ; keep list compact
      company-echo-delay 0)                ; show metadata instantly
;; ^ quick-access hints are documented in Company’s frontends.  ; FYI

;; Enable globally
(add-hook 'after-init-hook #'global-company-mode)

;; Manual trigger: keep WM-safe key, avoid clobbering C-c C-c
(global-set-key (kbd "C-<tab>") #'company-complete)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "C-n")   #'company-select-next)
  (define-key company-active-map (kbd "C-p")   #'company-select-previous))

;; --- LSP completion via CAPF (recommended path) ----------------------------
(with-eval-after-load 'lsp-mode
  ;; Use CAPF (company-capf will read from this).
  (setq lsp-completion-provider :capf)) ; lsp-mode recommends CAPF

;; Backends per context
(defun peteches/company-backends-prog ()
  "Backends for programming modes."
  (setq-local company-backends
              '((company-capf           ; LSP/Eglot/major-mode CAPFs
                 :with company-files company-keywords company-dabbrev-code)
                company-emoji           ; if loaded (see below)
                company-dabbrev)))

(add-hook 'prog-mode-hook #'peteches/company-backends-prog)

(defun peteches/company-backends-text ()
  "Backends for text-ish modes."
  (setq-local company-backends
              '(company-emoji company-dabbrev company-files)))

(add-hook 'text-mode-hook #'peteches/company-backends-text)

;; Emoji backend (optional)
(when (require 'company-emoji nil t)
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)))

;; Dabbrev tweaks
(setq company-dabbrev-other-buffers t
      company-dabbrev-code-everywhere t
      company-dabbrev-ignore-case 'keep)

(use-package company-box
  :ensure t)
;;;; --- company-box: icons + richer metadata --------------------------------
;; Degrades gracefully if packages aren’t installed.
(when (require 'company-box nil t)
  (add-hook 'company-mode-hook #'company-box-mode)

  ;; Compact, modern list; right-aligned annotations come from CAPF/LSP.
  (setq company-tooltip-align-annotations t
        company-box-show-single-candidate t
        company-box-max-candidates 50
        company-box-scrollbar t)

  ;; Use all-the-icons if available for nice per-kind glyphs.
  (when (require 'all-the-icons nil t)
    (setq company-box-icons-alist 'company-box-icons-all-the-icons)))

;; Make LSP/CAPF expose extra info (e.g., kind) so it shows up in the UI.
(with-eval-after-load 'lsp-mode
  (setq lsp-completion-show-kind t))  ; metadata in the candidate list

;;;; --- Quickhelp: manual docs popup (no auto-show) -------------------------
(when (require 'company-quickhelp nil t)
  ;; Keep it quiet by default; only show when you ask for it.
  (company-quickhelp-mode -1)

  ;; During an active completion popup, press this to show the tooltip.
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

  ;; Optional: use a child frame if you have posframe installed.
  (setq company-quickhelp-use-posframe (fboundp 'posframe-workable-p)))


(provide 'peteches-company)
