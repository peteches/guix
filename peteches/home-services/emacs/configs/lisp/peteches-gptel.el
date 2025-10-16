;;; gptel-min.el --- Minimal GPtel: Copilot Enterprise first -*- lexical-binding: t; -*-

(straight-use-package 'gptel)

;; Requirements (Guix packages): emacs, emacs-gptel
(require 'gptel)
(require 'gptel-gh)     ;; Copilot factory



(push '(gptel) warning-suppress-types)

;; Use curl + streaming (works well with Copilot’s SSE responses)
(setq gptel-use-curl t
      gptel-stream   t
      gptel-default-mode #'org-mode)

;; --- Copilot Enterprise backend --------------------------------------------
(defvar peteches/gptel-copilot-enterprise
  (gptel-make-gh-copilot "Copilot (Enterprise)"
    :host "api.githubcopilot.com"
    :endpoint "/chat/completions"))

(defvar peteches/gptel-koboldcpp
  (gptel-make-openai "KoboldCPP"
    :protocol "http"	      ; change to "https" if you terminate TLS
    :host "nug.peteches.co.uk:5001"
    :endpoint "/v1/chat/completions"
    :stream t))

;; Make it the default backend.
(setq gptel-backend peteches/gptel-koboldcpp)

;; Define a keymap for gptel-mode
(define-prefix-command 'gptel-mode-prefix-map)

;; Bind gptel commands to specific keys in gptel-mode-prefix-map
(define-key gptel-mode-prefix-map (kbd "m") 'gptel-menu)
(define-key gptel-mode-prefix-map (kbd "s") 'gptel-send)
(define-key gptel-mode-prefix-map (kbd "g") 'gptel)
(define-key gptel-mode-prefix-map (kbd "r") 'gptel-rewrite)
(define-key gptel-mode-prefix-map (kbd "a") 'gptel-abort)

;; Bind C-c a g to the prefix map
(global-set-key (kbd "C-c a") 'gptel-mode-prefix-map)

(use-package gptel-commit
  :ensure t
  :after (gptel magit))

(setq gptel-commit-backend peteches/gptel-koboldcpp
      gptel-commit-stream t
      gptel-commit-prompt "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

Use conventional commit guidelines:
The commit message should be structured as follows:

<type>[optional scope]: <description>

[optional body]

[optional footer(s)]

The commit contains the following structural elements, to communicate intent to the consumers of your library:

    fix: a commit of the type fix patches a bug in your codebase (this correlates with PATCH in Semantic Versioning).
    feat: a commit of the type feat introduces a new feature to the codebase (this correlates with MINOR in Semantic Versioning).
    BREAKING CHANGE: a commit that has a footer BREAKING CHANGE:, or appends a ! after the type/scope, introduces a breaking API change (correlating with MAJOR in Semantic Versioning). A BREAKING CHANGE can be part of commits of any type.
    types other than fix: and feat: are allowed, for example @commitlint/config-conventional (based on the Angular convention) recommends build:, chore:, ci:, docs:, style:, refactor:, perf:, test:, and others.
    footers other than BREAKING CHANGE: <description> may be provided and follow a convention similar to git trailer format.


Follow good Git style:

- Separate the subject from the body with a blank line
- Try to limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- Use the imperative mood in the subject line
- Wrap the body at 72 characters
- Keep the body short and concise (omit it entirely if not useful)")
(provide 'peteches-gptel)
;;; gptel-min.el ends here
