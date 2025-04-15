;;; my-ai-agent.el --- my-ai-agent integration -*- lexical-binding: t -*-

;;; Commentary:

;; Integration of https://github.com/berquerant/my-ai-agent

;;; Code:

(my-macro-template-thread-buffer-chat "my-ai-agent")

(defcustom my-ai-agent-command "my_ai_agent"
  "my-ai-agent command."
  :type 'string)

(defcustom my-ai-agent-use-external nil
  "Use external AI."
  :type 'bool)

(defcustom my-ai-agent-external-model "gpt-4o-mini"
  "AI model (external)."
  :type 'string)

(defcustom my-ai-agent-internal-model "mistral-nemo"
  "AI model (internal)."
  :type 'string)

(defun my-ai-agent-model ()
  "AI model."
  (if my-ai-agent-use-external
      my-ai-agent-external-model
    my-ai-agent-internal-model))

(defcustom my-ai-agent-internal-base-url (concat "http://" (my-getenv "OLLAMA_HOST") "/v1")
  "base url of internal API."
  :type 'string)

(defun my-ai-agent-base-url ()
  "base url of API."
  (if my-ai-agent-use-external
      nil
    my-ai-agent-internal-base-url))

(defcustom my-ai-agent-instructions nil
  "instructions."
  :type 'string)

(defun my-ai-agent--gen-arg-when-value (key value)
  (when value (list key value)))

(defun my-ai-agent--additional-args ()
  (append (my-ai-agent--gen-arg-when-value "--base_url" (my-ai-agent-base-url))
          (my-ai-agent--gen-arg-when-value "--instructions" my-ai-agent-instructions)))

(defun my-ai-agent--command (txt)
  (append `(,my-ai-agent-command
            "--debug"
            "--model" ,(my-ai-agent-model))
          (my-ai-agent--additional-args)))

(defun my-ai-agent--input (txt) txt)

(provide 'my-ai-agent)
;;; my-ai-agent.el ends here
