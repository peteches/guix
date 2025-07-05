(provide 'peteches-abbrev)
(require 'abbrev)

(dolist (hook '(prog-mode-hook
		text-mode-hook))
  (add-hook hook #'abbrev-mode))

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
