;;; my-external.el --- functions to be called by external processes -*- lexical-binding: t -*-
;;; Code:

(require 'my-straight)

(defun my-external-write (text)
  "Write TEXT to stdout."
  (append-to-file text nil "/dev/stdout"))

(defun my-external-straight-list-packages ()
  (cl-loop for name in (my-straight-list-packages)
           do (my-external-write (format "%s\n" name))))

(defun my-external-straight-list-directories ()
  (cl-loop for name in (my-straight-list-package-directories)
           do (my-external-write (format "%s\n" name))))

(defun my-external-straight-update-package (pkg)
  (straight-pull-package-and-deps pkg)
  (straight-rebuild-package pkg))

(defun my-external-straight-freeze ()
  (straight-freeze-versions))

(provide 'my-external)
;;; my-external.el ends here
