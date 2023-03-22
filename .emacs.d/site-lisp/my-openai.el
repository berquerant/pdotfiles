;;; my-openai.el --- translation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 's)

(defgroup my-openai nil
  "My openai chat."
  :prefix "my-openai-")

(defcustom my-openai-command "python -m open_ai_chat.cli chat"
  "chat command."
  :type 'string)

(defcustom my-openai-command-options nil
  "chat command options."
  :type (repeat 'string)))

(defun my-openai--command ()
  (format "%s %s" my-openai-command (s-join " " my-openai-command-options)))

(defcustom my-openai-process-name "my-openai"
  "openai chat process name."
  :type 'string)

(defcustom my-openai-output-buffer-name "*my-openai-output*"
  "chat output buffer name."
  :type 'string)

(defun my-openai--insert-output-buffer (input)
  "Insert INPUT into buffer `my-openai-output-buffer-name'."
  (with-current-buffer (get-buffer-create my-openai-output-buffer-name)
    (insert input)))

(defun my-openai--openai-output-filter (p output)
  "openai output process filter."
  (my-openai--insert-output-buffer output))

;;;###autoload
(defun my-openai-chat (src)
  "Create chat and receive response."
  (my-openai--insert-output-buffer (format "[send]\n%s\n[end]\n" src))
  (little-async-start-process (my-openai--command)
                              :input src
                              :process-name my-openai-process-name
                              :buffer-name my-openai-output-buffer-name
                              :filter 'my-openai--openai-output-filter
                              :timeout 60000))

(provide 'my-openai)
;;; my-openai.el ends here
