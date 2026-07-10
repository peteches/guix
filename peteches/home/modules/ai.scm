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
