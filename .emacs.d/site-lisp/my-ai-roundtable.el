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

(defcustom my-ai-roundtable-model "gpt-4o-mini"
  "AI model."
  :type 'string)

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
  "Reply timeout."
  :type 'number)

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

(defun my-ai-roundtable--command ()
  `(,my-ai-roundtable-command
    ,(my-ai-roundtable--subcommand)
    "--config" ,my-ai-roundtable-config
    "--model" ,my-ai-roundtable-model
    "--max_turns" ,(number-to-string my-ai-roundtable-max-turns)
    "--eval_messages" ,(number-to-string my-ai-roundtable-eval-messages)
    "--skip_eval" ,(number-to-string my-ai-roundtable-skip-eval)))

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
