;;; my-external.el --- functions to be called by external processes -*- lexical-binding: t -*-
;;; Code:

(require 'my-straight)

(defun my-external-write (text)
  "Write TEXT to stdout."
  (append-to-file text nil "/dev/stdout"))

(defun my-external-straight-info ()
  (my-external-write (json-encode (my-straight-info))))

(provide 'my-external)
;;; my-external.el ends here
