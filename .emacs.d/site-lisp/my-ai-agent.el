;;; my-ai-agent.el --- my-ai-agent integration -*- lexical-binding: t -*-

;;; Commentary:

;; Integration of https://github.com/berquerant/my-ai-agent

;;; Code:

(require 'thread-buffer-chat) ; https://github.com/berquerant/emacs-thread-buffer-chat

(defgroup my-ai-agent nil
  "My my-ai-agent integration."
  :prefix "my-ai-agent-")

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

(defcustom my-ai-agent-timeout 300
  "timeout seconds."
  :type 'number)

(defcustom my-ai-agent-tool-timeout 300
  "tool timeout seconds."
  :type 'number)

(defcustom my-ai-agent-tools nil
  "external tool executables."
  :type '(list string))

(defcustom my-ai-agent-buffer-template "*my-ai-agent-%d*"
  "Buffer name template."
  :type 'string)

(defcustom my-ai-agent-buffer-regex "\\*my-ai-agent-[0-9]+\\*"
  "Buffer name regex."
  :type 'string)

(defcustom my-ai-agent-err-buffer "*my-ai-agent-err*"
  "Buffer name for stderr."
  :type 'string)

(defun my-ai-agent--additional-args ()
  (let ((args nil))
    (when (my-ai-agent-base-url)
      (setq args (append (list "--base_url" (my-ai-agent-base-url)) args)))
    (when my-ai-agent-tools
      (setq args (append (list "--tool") my-ai-agent-tools args)))))

(defun my-ai-agent--command ()
  (append `(,my-ai-agent-command
            "--debug"
            "--model" ,(my-ai-agent-model)
            "--tool_timeout" ,(number-to-string my-ai-agent-tool-timeout))
          (my-ai-agent--additional-args)))

(defun my-ai-agent-start (txt)
  "Send TXT to `my-ai-agent-command'."
  (thread-buffer-chat-start (my-ai-agent--command)
                            txt
                            :timeout my-ai-agent-timeout
                            :stderr my-ai-agent-err-buffer
                            :buffer-template my-ai-agent-buffer-template
                            :buffer-regex my-ai-agent-buffer-regex))

(provide 'my-ai-agent)
;;; my-ai-agent.el ends here
