;;; peteches-scoreplay --- My work related configs
;;; Commentary:
;;;
;;; 1. Add Org-Capture templates
;;; 2. Gnus email config
;;; 3. Abreviations

;;; Code:
(require 'gnus)
(require 'gnus-msg)
(require 'message)

(add-to-list 'org-capture-templates
             `("s" "ScorePlay Task" entry
               (file ,(expand-file-name "agenda/ScorePlay.org" org-directory))
               "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n\n"))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "peter.mccabe@scoreplay.io"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)
		      (nnimap-user "peter.mccabe@scoreplay.io")
		      (nnimap-authenticator login)
		      (nnir-search-engine imap)))

(add-to-list 'gnus-posting-styles
	     '("peter.mccabe@scoreplay.io"
	       (address "Pete McCabe <peter.mccabe@scoreplay.io>")
	       (organization "ScorePlay Inc")
	       ("X-Message-SMTP-Method"  "smtp smtp.gmail.com 587 peter.mccabe@scoreplay.io")))

(dolist (abbrevs '(( "sp" . "ScorePlay")
		   ( "ha" . "hybrid-agent")

		   ( "agentPing" . "PingHealth" )
		   ( "agentPong" . "PongHealth" )

		   ( "TConfig" . "TestConfig" )
		   ( "RConfig" . "RequestConfig" )

		   ( "AddedFile" . "AddedFileNotification" )
		   ( "DoneWriting" . "DoneWritingFileNotification" )
		   ( "ChangedFile" . "ChangedFileNotification" )
		   ( "RemovedFile" . "RemovedFileNotification" )

		   ( "CreateVideo" . "CreateVideoProxyTask" )
		   ( "CreateVideoP" . "CreateVideoProxyTaskProgressNotification" )
		   ( "CreateVideoC" . "CreateVideoProxyTaskCompleteNotification" )

		   ( "CreateLive" . "CreateLiveStreamProxyTask" )
		   ( "CreateLiveP" . "CreateLiveStreamProxyTaskProgressNotification" )
		   ( "CreateLiveC" . "CreateLiveStreamProxyTaskCompleteNotification" )

		   ( "UploadTask" . "UploadToCloudTask" )
		   ( "UploadTaskP" . "UploadToCloudTaskProgressNotification" )
		   ( "UploadTaskC" . "UploadToCloudTaskCompleteNotification" )

		   ( "DownloadTask" . "DownloadFromCloudTask" )
		   ( "DownloadTaskP" . "DownloadFromCloudTaskProgressNotification" )
		   ( "DownloadTaskC" . "DownloadFromCloudTaskCompleteNotification" )

		   ( "ClipLiveTask" . "ClipLiveStreamTask" )
		   ( "ClipLiveTaskP" . "ClipLiveStreamTaskProgressNotification" )
		   ( "ClipLiveTaskC" . "ClipLiveStreamTaskCompleteNotification" )

		   ( "ClipVideoTask" . "ClipVideoTask" )
		   ( "ClipVideoTaskP" . "ClipVideoTaskProgressNotification" )
		   ( "ClipVideoTaskC" . "ClipVideoTaskCompleteNotification" )

		   ( "DeliverTask" . "DeliverToUserTask" )
		   ( "DeliverTaskP" . "DeliverToUserTaskProgressNotification" )
		   ( "DeliverTaskC" . "DeliverToUserTaskCompleteNotification" )))
  (define-abbrev global-abbrev-table (car abbrevs) (cdr abbrevs)))

(provide 'peteches-scoreplay)
;;; peteches-scoreplay.el ends here
