;;; my-path.el --- my path utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 's)
(require 'my-proc)

(defgroup my-path nil
  "My path utilities."
  :prefix "my-path-")

(defun my-path--buffer-file-name ()
  (let ((p (buffer-file-name)))
    (when p
      (shell-quote-argument p))))

(defun my-path--current-path ()
  (my-path--buffer-file-name))

;;;###autoload
(defun my-path-current-path ()
  "Store current buffer file path to kill ring."
  (interactive)
  (let ((p (my-path--current-path)))
    (when p
      (message "Stored: %s" p)
      (kill-new p))))

(defun my-path--git-relative-path ()
  (let ((p (my-path--buffer-file-name)))
    (cond ((not p) (message "Not visiting file.") nil)
          (t (my-proc-shell-command-to-string-secure "git" "ls-files" "--full-name" p)))))

;;;###autoload
(defun my-path-git-relative-path ()
  "Store current buffer file path as git relpath to kill ring."
  (interactive)
  (let ((p (my-path--git-relative-path)))
    (when p
      (message "Stored: %s" p)
      (kill-new p))))

(provide 'my-path)
;;; my-path.el ends here
