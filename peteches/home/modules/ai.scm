;;; peteches/home/modules/ai.scm — ~/.config/eca/config.json for ECA.
;;;
;;; Scope: ECA only.  Claude Code's own MCP servers are registered by
;;; `home-claude-service-type' in (peteches home modules claude); the two
;;; are independent and both must be updated when an MCP server changes.
;;;
;;; Points ECA's code and chat agents at the local KoboldCpp instance on
;;; nug (see the koboldcpp services in peteches/home/configs/nug.scm; port
;;; 5001 is the qwen coder model).
;;;
;;; KNOWN STALE: `script-path' below points at
;;; ~/.config/emacs/straight/repos/anvil.el/anvil-stdio.sh, a straight.el
;;; checkout that no longer exists — anvil is now the `emacs-anvil' Guix
;;; package (see the comment in (peteches home modules base) where
;;; home-claude-service-type resolves it via file-append).  ECA's anvil MCP
;;; servers are therefore broken; fix by substituting
;;; #$(file-append emacs-anvil "/bin/anvil-stdio.sh") the way base.scm does.
;;;
;;; The JSON is assembled by string-append rather than a JSON writer, so
;;; quoting mistakes here surface as an unparseable config at runtime, not
;;; at reconfigure time.

(define-module (peteches home modules ai)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (base-ai-service))

(define-public base-ai-service
  ;; Configure ECA with MCP servers pointing to anvil.
  ;; Claude Code MCP servers are managed by home-claude-service-type.
  (simple-service 'ai-mcp-config
		  home-activation-service-type
		  #~(begin
		      (use-modules (ice-9 popen) (ice-9 textual-ports))
		      (let* ((home        (getenv "HOME"))
			     (bash-path   #$(file-append bash "/bin/bash"))
			     (script-path (string-append home
							 "/.config/emacs/straight/repos/anvil.el/anvil-stdio.sh"))
			     (mcp-json
			      (string-append
			       "{"
			       "\"anvil\":{"
			       "\"type\":\"stdio\","
			       "\"command\":\"" bash-path "\","
			       "\"args\":[\"" script-path "\","
			       "\"--server-id=anvil\","
			       "\"--init-function=anvil-enable\","
			       "\"--stop-function=anvil-disable\"],"
			       "\"env\":{}},"
			       "\"anvil-emacs-eval\":{"
			       "\"type\":\"stdio\","
			       "\"command\":\"" bash-path "\","
			       "\"args\":[\"" script-path "\","
			       "\"--server-id=emacs-eval\"],"
			       "\"env\":{}}"
			       "}"))
			     (eca-dir     (string-append home "/.config/eca"))
			     (eca-file    (string-append eca-dir "/config.json")))
			(unless (file-exists? eca-dir)
			  (mkdir eca-dir))
			(call-with-output-file eca-file
			  (lambda (port)
			    (display
			     (string-append
			      "{"
			      "\"providers\":{"
			      "\"koboldcpp\":{"
			      "\"api\":\"openai-chat\","
			      "\"url\":\"https://nug.peteches.co.uk:5001\","
			      "\"key\":\"local\","
			      "\"completionUrlRelativePath\":\"/v1/chat/completions\","
			      "\"models\":{\"default\":{}}"
			      "}},"
			      "\"agents\":{"
			      "\"code\":{\"provider\":\"koboldcpp\",\"model\":\"default\"},"
			      "\"chat\":{\"provider\":\"koboldcpp\",\"model\":\"default\"}"
			      "},"
			      "\"mcpServers\":" mcp-json
			      "}")
			     port)))))))
