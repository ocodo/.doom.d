(require 's)

(defun ocodo/local-ollama-server-p ()
  "Check for local Ollama LLM server."
  (let ((ollama-result
         (shell-command-to-string "curl localhost:11434")))
    (s-contains-p "Ollama is running" ollama-result)))

(let
    ((host
      (if (ocodo/local-ollama-server-p)
          "localhost"
        "nebula")))

  (use-package ellama
    :init
    (setopt ellama-keymap-prefix "C-c =")
    (require 'llm-ollama)
    (setopt ellama-provider
      (make-llm-ollama
       :host host
       :chat-model "superdrew100/llama3-abliterated:latest"
       :embedding-model "superdrew100/llama3-abliterated:latest"))
    ;; Naming new sessions with llm
    (setopt ellama-naming-provider
            (make-llm-ollama
             :host host
             :chat-model "superdrew100/llama3-abliterated:latest"
             :embedding-model "superdrew100/llama3-abliterated:latest"))
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)))
