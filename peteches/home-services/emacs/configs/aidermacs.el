(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :ensure t
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  ;(setenv "ANTHROPIC_API_KEY" "sk-...")
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  ;(setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  (setenv "OLLAMA_API_BASE" "http://macbook-pro.local.peteches.co.uk:11434")
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))
