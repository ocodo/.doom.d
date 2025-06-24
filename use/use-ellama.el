(require 's)

(defvar ocodo/ellama-hosts '("localhost" "polaris" "pulsar" "neptune" "jupiter"))

(defun ocodo/ellama-choose-server-and-model ()
  "Select ellama host and model"
  (interactive)
  (let* ((host (completing-read "Select host" (ocodo/active-ollama-servers)))
         (model-list (seq-filter
                      (lambda (line)
                        (not (or
                              (s-blank-p line)
                              (s-starts-with-p "all-minilm" line)
                              (s-starts-with-p "NAME" line))))
                      (s-lines (shell-command-to-string "ollama ls"))))
         (model (first
                  (string-split
                    (completing-read "Select chat model: " model-list)
                   " "))))
    (message "Selected chat model: %s on %s" model host)
    (setopt ellama-provider
     (make-llm-ollama
      :host host
      :chat-model model
      :embedding-model "all-minilm:latest"))))

(defun ocodo/active-ollama-servers ()
  "Check for active Ollama LLM servers"
  (interactive)
  (seq-filter 'ocodo/ollama-server-check
              ocodo/ellama-hosts))

(defun ocodo/ollama-server-check (host)
  (s-contains-p "Ollama is running"
    (shell-command-to-string (format "curl -m 0.05  %s:11434" host))))

(defun ocodo/local-ollama-server-p ()
  "Check for local Ollama LLM server."
  (let ((ollama-result))
     (ocodo/ollama-server-check "localhost")))

(let ((host (first (ocodo/active-ollama-servers))))
  (use-package ellama
    :init
    (setopt ellama-keymap-prefix "C-c =")
    (require 'llm-ollama)
    (setopt ellama-provider
      (make-llm-ollama
       :host host
       :chat-model "gemma3:latest"
       :embedding-model "all-minilm:latest"))
    ;; Naming new sessions with llm
    (setopt ellama-naming-provider
            (make-llm-ollama
             :host host
             :chat-model "gemma3:latest"
             :embedding-model "gemma3:latest"))
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)))
