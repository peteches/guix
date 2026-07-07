(define-module (peteches home modules ai)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages web)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (base-ai-service))

(define-public base-ai-service
  ;; Configure MCP servers for both Claude Code CLI and ECA.
  ;; The MCP server JSON block is built once and reused for both
  ;; ~/.claude/settings.json and ~/.config/eca/config.json.
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
			     (claude-file (string-append home "/.claude/settings.json"))
			     (claude-tmp  (string-append claude-file ".guix-tmp"))
			     (eca-dir     (string-append home "/.config/eca"))
			     (eca-file    (string-append eca-dir "/config.json")))
			(when (file-exists? claude-file)
			  (let* ((jq-filter (string-append ".mcpServers = " mcp-json))
				 (pipe (open-pipe* OPEN_READ
						   #$(file-append jq "/bin/jq")
						   jq-filter
						   claude-file))
				 (output (get-string-all pipe)))
			    (close-pipe pipe)
			    (call-with-output-file claude-tmp
			      (lambda (port) (display output port)))
			    (rename-file claude-tmp claude-file)))
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
