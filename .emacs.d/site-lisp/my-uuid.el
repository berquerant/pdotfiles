;;; my-uuid.el --- my uuid utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'my-proc)

(defgroup my-uuid nil
  "My uuid utilities."
  :prefix "my-uuid-")

(defun my-uuid-gen ()
  "Create a new random UUID."
  (s-downcase (my-proc-shell-command-to-string "uuidgen")))

(provide 'my-uuid)
;;; my-uuid.el ends here
