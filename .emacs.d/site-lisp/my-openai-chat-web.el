;;; my-openai-chat-web.el --- chat with web search -*- lexical-binding: t -*-

;;; Code:

(require 'thread-buffer-chat) ; https://github.com/berquerant/emacs-thread-buffer-chat

(defgroup my-openai-chat-web nil
  "My openai-chat-web integration.")

(defcustom my-openai-chat-web-command "openai_chat_web.sh"
  "openai-chat-web command."
  :type 'string
  :group 'my-openai-chat-web)

(defcustom my-openai-chat-web-timeout 120
  "Reply timeout."
  :type 'number
  :group 'my-openai-chat-web)

(defcustom my-openai-chat-web-buffer-template "*openai-chat-web-%d*"
  "Buffer name template."
  :type 'string
  :group 'my-openai-chat-web)

(defcustom my-openai-chat-web-buffer-regex "\\*openai-chat-web-[0-9]+\\*"
  "Buffer name regex."
  :type 'string
  :group 'my-openai-chat-web)

(defcustom my-openai-chat-web-err-buffer "*openai-chat-web-err*"
  "Buffer name for error."
  :type 'string
  :group 'my-openai-chat-web)

(defun my-openai-chat-web--command ()
  `(,my-openai-chat-web-command
    "chat"
    "--chat_model"
    "gpt-3.5-turbo"
    "--language"
    "Japanese"))

(defun my-openai-chat-web-start (txt)
  "Send whole buffer or region to `my-openai-chat-web-command'."
  (interactive
   (list (cond
          ((use-region-p) (buffer-substring (region-beginning) (region-end)))
          (t (buffer-substring (point-min) (point-max))))))
  (thread-buffer-chat-start (my-openai-chat-web--command)
                            txt
                            :timeout my-openai-chat-web-timeout
                            :stderr my-openai-chat-web-err-buffer
                            :buffer-template my-openai-chat-web-buffer-template
                            :buffer-regex my-openai-chat-web-buffer-regex))

(provide 'my-openai-chat-web)
;;; my-openai-chat-web.el ends here
