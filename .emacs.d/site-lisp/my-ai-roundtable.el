;;; my-ai-roundtable.el --- My ai-roundtable integration -*- lexical-binding: t -*-

;;; Commentary:

;; Integration of https://github.com/berquerant/ai-roundtable

;;; Code:

(require 'thread-buffer-chat) ; https://github.com/berquerant/emacs-thread-buffer-chat

(defgroup my-ai-roundtable nil
  "My ai-roundtable integration."
  :prefix "my-ai-roundtable-")

(defcustom my-ai-roundtable-command "ai_roundtable"
  "ai-roundtable command."
  :type 'string)

(defcustom my-ai-roundtable-config "ai_roundtable.yml"
  "Config file."
  :type 'string)

(defcustom my-ai-roundtable-use-external nil
  "Use external AI."
  :type 'bool)

(defcustom my-ai-roundtable-external-model "gpt-4o-mini"
  "AI model (external)."
  :type 'string)

(defcustom my-ai-roundtable-internal-model "mistral-nemo"
  "AI model (internal)."
  :type 'string)

(defun my-ai-roundtable-model ()
  "AI model."
  (if my-ai-roundtable-use-external
      my-ai-roundtable-external-model
    my-ai-roundtable-internal-model))

(defcustom my-ai-roundtable-max-turns 8
  "max_turns."
  :type 'number)

(defcustom my-ai-roundtable-eval-messages 5
  "eval_messages."
  :type 'number)

(defcustom my-ai-roundtable-skip-eval 0
  "skip_eval."
  :type 'number)

(defcustom my-ai-roundtable-timeout 300
  "reply timeout."
  :type 'number)

(defcustom my-ai-roundtable-internal-base-url (concat "http://" (my-getenv "OLLAMA_HOST") "/v1")
  "base url of internal API."
  :type 'string)

(defun my-ai-roundtable-base-url ()
  "base url of API."
  (if my-ai-roundtable-use-external
      nil
    my-ai-roundtable-internal-base-url))

(defcustom my-ai-roundtable-buffer-template "*ai-roundtable-%d*"
  "Buffer name template."
  :type 'string)

(defcustom my-ai-roundtable-buffer-regex "\\*ai-roundtable-[0-9]+\\*"
  "Buffer name regex."
  :type 'string)

(defcustom my-ai-roundtable-err-buffer "*ai-roundtable-err*"
  "Buffer name for stderr."
  :type 'string)

(defun my-ai-roundtable--rerun? ()
  (string-match-p my-ai-roundtable-buffer-regex (buffer-name)))

(defun my-ai-roundtable--subcommand ()
  (if (my-ai-roundtable--rerun?) "rerun" "start"))

(defun my-ai-roundtable--additional-args ()
  (let ((args nil))
    (when (my-ai-roundtable-base-url)
      (setq args (append (list "--base_url" (my-ai-roundtable-base-url)) args)))
    args))

(defun my-ai-roundtable--command ()
  (append `(,my-ai-roundtable-command
            ,(my-ai-roundtable--subcommand)
            "--debug"
            "--config" ,my-ai-roundtable-config
            "--model" ,(my-ai-roundtable-model)
            "--max_turns" ,(number-to-string my-ai-roundtable-max-turns)
            "--eval_messages" ,(number-to-string my-ai-roundtable-eval-messages)
            "--skip_eval" ,(number-to-string my-ai-roundtable-skip-eval))
          (my-ai-roundtable--additional-args)))

(defun my-ai-roundtable-start (txt)
  "Send TXT to `my-ai-roundtable-command'."
  (thread-buffer-chat-start (my-ai-roundtable--command)
                            txt
                            :timeout my-ai-roundtable-timeout
                            :stderr my-ai-roundtable-err-buffer
                            :buffer-template my-ai-roundtable-buffer-template
                            :buffer-regex my-ai-roundtable-buffer-regex))

(provide 'my-ai-roundtable)
;;; my-ai-roundtable.el ends here
