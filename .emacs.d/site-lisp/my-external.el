;;; my-external.el --- functions to be called by external processes -*- lexical-binding: t -*-
;;; Code:

(require 'my-straight)

(defun my-external-write (text)
  "Write TEXT to stdout."
  (append-to-file text nil "/dev/stdout"))

(defun my-external-straight-dependencies ()
  (let ((h (make-hash-table)))
    (cl-loop for name in (my-straight-list-packages)
             do (puthash name (straight-dependencies name) h))
    (my-external-write (json-encode h))))

(defun my-external-straight-dependents ()
  (let ((h (make-hash-table)))
    (cl-loop for name in (my-straight-list-packages)
             do (puthash name (straight-dependents name) h))
    (my-external-write (json-encode h))))

(defun my-external-straight-list-packages ()
  (cl-loop for name in (my-straight-list-packages)
           do (my-external-write (format "%s\n" name))))

(defun my-external-straight-list-directories ()
  (cl-loop for name in (my-straight-list-package-directories)
           do (my-external-write (format "%s\n" name))))

(defun my-external-straight-update-packages (&rest pkgs)
  (cl-loop for pkg in pkgs
           do (my-external-straight-update-package pkg)))

(defun my-external-straight-update-package (pkg)
  (straight-pull-recipe-repositories)
  (straight-pull-package-and-deps pkg)
  (straight-rebuild-package pkg))

(defun my-external-straight-freeze ()
  (straight-check-all)
  (straight-freeze-versions))

(defun my-exrernal-straight-check-all ()
  (straight-check-all))

(defun my-external-straight-read-profile ()
  (my-external-write (json-encode (my-straight-read-profile))))

(provide 'my-external)
;;; my-external.el ends here
