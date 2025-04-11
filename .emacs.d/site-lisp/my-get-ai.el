;;; my-get-ai.el --- get-ai.sh integration -*- lexical-binding: t -*-

;;; Commentary:

;; Integration of get-ai.sh

;;; Code:

(require 's)
(my-macro-template-thread-buffer-chat "my-get-ai")

(defcustom my-get-ai-command "get-ai.sh"
  "get-ai.sh command."
  :type 'string)

(defcustom my-get-ai-use-external nil
  "Use external AI."
  :type 'bool)

(defcustom my-get-ai-external-model "gpt-4o-mini"
  "AI model (external)."
  :type 'string)

(defcustom my-get-ai-internal-model "gemma3:12b"
  "AI model (internal)."
  :type 'string)

(defun my-get-ai-model ()
  "AI model."
  (if my-get-ai-use-external
      my-get-ai-external-model
    my-get-ai-internal-model))

(defcustom my-get-ai-base-url (concat "http://" (my-getenv "OLLAMA_HOST") "/v1")
  "base url of API."
  :type 'string)

(defun my-get-ai--command (txt)
  (let* ((ss (s-split-up-to "\n" txt 1))
         (url (car ss)))
    (list my-get-ai-command url (my-get-ai-model) my-get-ai-base-url)))

(defun my-get-ai--input (txt)
  (let ((ss (s-split-up-to "\n" txt 1)))
    (cadr ss)))

(provide 'my-get-ai)
;;; my-get-ai.el ends here
