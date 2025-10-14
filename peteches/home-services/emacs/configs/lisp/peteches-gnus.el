;; setup for gnus to read from pete@peteches.co.uk from fastmail.
;; IMAP Server	imap.fastmail.com
;; IMAP Port	993
;; IMAP Security	SSL/TLS (required)
;; Username	Your Fastmail email address
;; Password	Your app password (NOT your regular Fastmail password!)
;;
;; SMTP Server	smtp.fastmail.com
;; SMTP Port	465
;; SMTP Security	SSL/TLS
;; Username	Your Fastmail email address
;; Password	Your app password (NOT your regular Fastmail password!)

;;; peteches-gnus.el --- Minimal Gnus + Fastmail setup  -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal, Guix-friendly Gnus config for Fastmail that:
;; - Uses IMAP/SMTP over SSL/TLS (imap.fastmail.com:993, smtp.fastmail.com:465).
;; - Pulls `user` and `password` fields from password-store via auth-source-pass-get.
;; - Runs exactly once from `first-frame-ready-hook` (provided by peteches-first-frame.el).
;; - Avoids storing passwords in files or persistent variables.
;;
;; If you keep a *single* entry for both, just point both defcustoms to that entry.

;;; Code:

(require 'gnus)
(require 'smtpmail)

(defgroup peteches-gnus nil
  "Simple Fastmail configuration for Gnus using password-store."
  :group 'mail
  :prefix "peteches-gnus-")

;; ---- Gnus IMAP (Fastmail) ----
;; We do NOT set or save any passwords; Gnus will fetch them via auth-source.
(setq gnus-select-method '(nnnil ""))
(add-to-list 'gnus-secondary-select-methods
      '(nnimap "peteches.co.uk"
	       (nnimap-address "imap.fastmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnimap-user "pete@peteches.co.uk")
	       (nnimap-authenticator login)
	       (nnir-search-engine imap)))

;; Optional quality-of-life (kept minimal)
(setq gnus-use-cache t
      gnus-asynchronous t)

;; ---- SMTP (send mail) ----
;; smtpmail will fetch the password at send-time from auth-source.
(setq message-send-mail-function #'smtpmail-send-it
      smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "pete@peteches.co.uk"
      smtpmail-auth-supported '(login plain))

(add-to-list 'gnus-posting-styles
      '((".*"
	 (address "Pete McCabe <pete@peteches.co.uk"))))

(add-hook 'gnus-startup-hook
	  '(lambda ()
	     (gnus-demon-init)
	     (setq gnus-demon-timestep 60) ;; each timestep is 60 seconds
	     ;; Check for new mail every 1 timestep (1 minute)
	     (gnus-demon-add-handler 'gnus-demon-add-scanmail 5 t)))

(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-rescan)

(provide 'peteches-gnus)
;;; peteches-gnus.el ends here

