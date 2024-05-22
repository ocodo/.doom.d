(let ((llama3-uncensored "hub/zillomab/llama3-uncensored:latest")
      (zephyr "zephyr:latest")
      (wizard "wizardlm2:7b")
      (stable-code "stable-code:3b")
      (code-companion "hub/hakamgh/code-companion:latest")
      (dolphincoder "dolphincoder:15b-starcoder2-q5_K_M")
      (deepseek-coder-uncensored "gdisney/deepseek-coder-uncensored:latest"))  
 (use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  (setopt ellama-provider
		    (make-llm-ollama
		     :chat-model ""
		     :embedding-model "hub/zillomab/llama3-uncensored:latest"))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
		    '(("zephyr" . (make-llm-ollama :chat-model zephyr :embedding-model zephyr))
		      ("wizard" . (make-llm-ollama :chat-model wizard :embedding-model wizard))
		      ("code-companion" . (make-llm-ollama :chat-model code-companion :embedding-model code-companion))
		      ("dolphincoder" . (make-llm-ollama :chat-model dolphincoder :embedding-model dolphincoder))
		      ("deepseek-coder-uncensored" . (make-llm-ollama :chat-model deepseek-coder-uncensored :embedding-model deepseek-coder-uncensored))
		      ("stable-code" . (make-llm-ollama :chat-model stable-code :embedding-model stable-code))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	    (make-llm-ollama
	     :chat-model "hub/zillomab/llama3-uncensored:latest"
	     :embedding-model "hub/zillomab/llama3-uncensored:latest"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)))

