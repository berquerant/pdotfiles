;;; my-straight.el --- my straight extensions -*- lexical-binding: t -*-

;;; Code:

(require 's)
(require 'cl)

(defgroup my-straight nil
  "My straight extensions."
  :prefix "my-straight-")

(defcustom my-straight-profile-path (concat user-emacs-directory "straight-default.el")
  "Straight-default.el."
  :type 'string)

(defcustom my-straight-dir-path (concat user-emacs-directory "straight")
  "Straight directory."
  :type 'string)

(defun my-straight-read-profile ()
  "Read `my-straight-profile-path'."
    (with-temp-buffer
      (insert-file-contents my-straight-profile-path)
      (read (buffer-string))))

(defun my-straight-list-packages ()
  (cl-loop for profile in (my-straight-read-profile)
           collect (car profile)))

(defun my-straight-list-package-directories ()
  (cl-loop for name in (my-straight-list-packages)
           collect (format "%s/repos/%s" my-straight-dir-path name)))

(defun my-straight-dependencies ()
  (let ((h (make-hash-table)))
    (cl-loop for name in (my-straight-list-packages)
             do (puthash name (straight-dependencies name) h))
    h))

(defun my-straight-dependents ()
  (let ((h (make-hash-table)))
    (cl-loop for name in (my-straight-list-packages)
             do (puthash name (straight-dependents name) h))
    h))

(defun my-straight-info ()
  `(("profile" . ,(my-straight-read-profile))
    ("packages" . ,(my-straight-list-packages))
    ("directories" . ,(my-straight-list-package-directories))
    ("dependencies" . ,(my-straight-dependencies))
    ("dependents" . ,(my-straight-dependents))))

(provide 'my-straight)
;;; my-straight.el ends here
