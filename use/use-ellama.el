(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
           :host "nebula"
	   :chat-model "superdrew100/llama3-abliterated:latest"
	   :embedding-model "superdrew100/llama3-abliterated:latest"))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
           :host "nebula"
	   :chat-model "superdrew100/llama3-abliterated:latest"
	   :embedding-model "superdrew100/llama3-abliterated:latest"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))
