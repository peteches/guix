;; peteches-sql --- My SQL configs
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(require 'subr-x) ;; for `string-trim`

;; Alist mapping SQL connection names to SSM tunnel configs.

;; Each element is of the form:

;;   (CONNECTION-SYMBOL
;;    :service                             ; remote DB service
;;    :region \"us-east-1\"                ; aws region
;;    :env \"dev\"                         ; aws env
;;    :local-port 5432                     ; local port
;;    :remote-port 5432                    ; remote port
;;    :connection                          ; an entry suitable for adding to sql-connection-alist.
;;                                           if sql-password-cmd is present it will be evaluated and overrite the sql-password entry
;; If a connection is not in this alist, no SSM tunnel is started.
(setq peteches/sql-connection-ssm-alist
  '((peteches/media-backend-dev
     :local-port "5432"
     :remote-port "5432"
     :service "media-backend"
     :region "us-east-1"
     :env "dev"
     :connection
     ((sql-product 'postgres)
      (sql-user "scoreplay")
      (sql-database "iamback")
      (sql-server "127.0.0.1")
      (sql-port 5432)))

    (peteches/media-backend-mgmnt
     :local-port "5433"
     :remote-port "5432"
     :service "media-backend"
     :region "us-east-1"
     :env "management"
     :connection
     ((sql-product 'postgres)
      (sql-user "scoreplay")
      (sql-database "iamback")
      (sql-server "127.0.0.1")
      (sql-port 5433)))

    (peteches/hybrid-agent-dev
     :local-port "5434"
     :remote-port "5432"
     :service "hybrid-agent"
     :region "us-east-1"
     :env "dev"
     :connection
     ((sql-product 'postgres)
      (sql-user "postgres")
      (sql-database "postgres")
      (sql-server "127.0.0.1")
      (sql-port 5434)))

    (peteches/hybrid-agent-mgmnt
     :local-port "5435"
     :remote-port "5432"
     :service "hybrid-agent"
     :region "us-east-1"
     :env "management"
     :connection
     ((sql-product 'postgres)
      (sql-user "postgres")
      (sql-database "postgres")
      (sql-server "127.0.0.1")
      (sql-port 5435)))))

(defun peteches/wait-for-tcp-port (host port &optional timeout)
  "Wait until HOST:PORT accepts TCP connections.
Return non-nil on success, nil on TIMEOUT seconds.

PORT may be a string or integer.  TIMEOUT defaults to 5 seconds."
  (let* ((timeout (or timeout 5))
         (start   (float-time))
         (done    nil)
         (port    (if (stringp port) (string-to-number port) port)))
    (while (and (not done)
                (< (- (float-time) start) timeout))
      (condition-case nil
          (let ((proc (open-network-stream
                       "peteches-port-test" nil host port)))
            (setq done t)
            (when proc (delete-process proc)))
        (error
         ;; Not ready yet, wait a bit and retry
         (sleep-for 0.1))))
    done))

(defun peteches/aws-secret-command (env secret &optional region)
  "Return an aws secretsmanager command to retrieve SECRET in ENV."
  (concat
   "aws-vault exec " env
   " -- aws secretsmanager "
   "get-secret-value --secret-id " secret
   " --region "
   (if (not region)
       "us-east-1"
     region)
   " --query SecretString"
   ))

(defun peteches/aws-secret-from-command (env secret &optional region)
  "Run `peteches/aws-secret-command` passing ENV, SECRET and REGION.
Return the password as a string.
Trims trailing newlines/whitespace."
  (let* ((output (shell-command-to-string (peteches/aws-secret-command env secret region)))
         (password (string-trim output)))
    (unless (and password (not (string-empty-p password)))
      (user-error "peteches/aws-rds-password-from-command: Empty password output"))
    (string-trim password "\"" "\"")))


(defun peteches/aws-bastion-host-get (env region)
  "Return aws db hostname for SERVICE in ENV and REGION."
  (string-trim (shell-command-to-string (concat
			     "aws-vault exec " env " -- aws ec2 describe-instances "
			     "--region " region " "
			     "--filters 'Name=tag:Name,Values=bastion' 'Name=instance-state-name,Values=running' "
			     "--query 'Reservations[*].Instances[*].InstanceId' "
			     "--output text"))))


(defun peteches/aws-sql-host-get (env service region)
  "Return aws db hostname for SERVICE in ENV and REGION."
  (string-trim (shell-command-to-string
		(concat
		 "aws-vault exec " env " -- "
		 "aws rds describe-db-instances "
		 "--region " region " "
		 "--query \"DBInstances[?TagList[?(Key=='service' || Key=='Service') && Value=='"
		 service
		 "']].Endpoint.Address | [0]\" "
		 "--output text"))))

(defun peteches/aws-ssm-start-port-forward (localport remoteport host env region)
  "Start ssm session forwarding LOCALPORT to REMOTEPORT on HOST in ENV and REGION."
  (let ((cmd (concat
               "aws-vault exec " env " -- "
               "aws ssm start-session "
               "--target " (peteches/aws-bastion-host-get env region) " "
               "--document-name AWS-StartPortForwardingSessionToRemoteHost "
               "--parameters '{\"host\":[\"" host "\"],\"portNumber\":[\"" remoteport "\"],\"localPortNumber\":[\"" localport "\"]}' "
               "--region " region)))
    (let ((display-buffer-alist nil))
      (compilation-start cmd 'compilation-mode (lambda (_) (concat "*peteches-ssm-session-" env "-" region "-" localport "-" host "-" remoteport "*"))))))

(defun peteches/sql--ssm-config-for (connection)
  "Return SSM config plist for CONNECTION (symbol or string), or nil."
  (let ((key (if (symbolp connection)
                 connection
               (intern connection))))
    (alist-get key peteches/sql-connection-ssm-alist)))


(defun peteches/sql--materialize-password (conn-spec)
  "Return CONN-SPEC with `sql-password-cmd` evaluated into `sql-password`.

CONN-SPEC is the list of (VAR VALUE) pairs such as:
  ((sql-product 'postgres)
   (sql-user \"scoreplay\")
   (sql-password-cmd (peteches/aws-secret-from-command ...)))

If a (sql-password-cmd FORM) entry is present, FORM is `eval`uated
to get the password string, the `sql-password-cmd` entry is removed,
and a (sql-password PASSWORD) entry is added. Any existing
`sql-password` entry is removed first."
  (let* ((spec (copy-tree conn-spec))
         (pw-cmd-cell (assoc 'sql-password-cmd spec)))
    (when pw-cmd-cell
      (let* ((form (cadr pw-cmd-cell)) ; (peteches/aws-secret-from-command ...)
             (password (eval form)))
        (unless (and password (stringp password)
                     (not (string-empty-p password)))
          (user-error "peteches/sql--materialize-password: Empty or non-string password"))
        ;; Remove old entries
        (setq spec (assq-delete-all 'sql-password-cmd spec))
        (setq spec (assq-delete-all 'sql-password spec))
        ;; Insert new password entry
        (push `(sql-password ,password) spec)))
    spec))


(defun peteches/sql-register-ssm-connections ()
  "Extend `sql-connection-alist` from `peteches/sql-connection-ssm-alist`.

For each entry (NAME :connection CONN ...) in
`peteches/sql-connection-ssm-alist`, add

  (NAME . CONN)

to `sql-connection-alist` if it is not already present."
  (dolist (entry peteches/sql-connection-ssm-alist)
    (let* ((name (symbol-name (car entry)))
           (plist (cdr entry))
           (conn  (plist-get plist :connection))
           (alist-entry (cons name conn)))
      ;; Avoid duplicates; use `eq` on the connection name symbol.
      (add-to-list 'sql-connection-alist alist-entry t
                   (lambda (a b) (eq (car a) (car b)))))))

(defun peteches/sql-connect-with-ssm (orig-fun connection &rest args)
  "Around advice for `sql-connect` ORIG-FUN to start an SSM tunnel for CONNECTION.
When needed passing ARGS if supplied."
  (if (not connection)
      (apply orig-fun connection args)
    (let* ((cfg (peteches/sql--ssm-config-for connection)))
      (if (not cfg)
          (apply orig-fun connection args)
        (let* ((localport (plist-get cfg :local-port))
               (remoteport (plist-get cfg :remote-port))
               (env       (plist-get cfg :env))
               (service   (plist-get cfg :service))
               (region    (plist-get cfg :region))
               (conn-spec (plist-get cfg :connection))
               (conn-spec* (peteches/sql--materialize-password conn-spec))
               ;; Always use a string name for sql-connection-alist
               (conn-name (if (symbolp connection)
                              (symbol-name connection)
                            connection))
               (conn-entry (cons conn-name conn-spec*)))
          ;; Start the SSM port forward first.
          (peteches/aws-ssm-start-port-forward
           localport remoteport
           (peteches/aws-sql-host-get env service region)
           env region)
	  (unless (peteches/wait-for-tcp-port "127.0.0.1" localport 10)
            (user-error "SSM tunnel to %s:%s did not become ready in time"
                        "127.0.0.1" localport))
          ;; Temporarily ensure this connection exists for `sql-connect`.
          (apply orig-fun conn-name args))))))

(with-eval-after-load 'sql
  (peteches/sql-register-ssm-connections)
  (advice-add 'sql-connect :around #'peteches/sql-connect-with-ssm))


(provide 'peteches-sql)
;;; peteches-sql.el ends here
