;;; my-buffer-change.el --- buffer change hook -*- lexical-binding: t -*-

;;; Code:

(defvar my-buffer-change--last-buffer nil)

(defun my-buffer-change--buffer-changed? ()
  (eq (current-buffer) my-buffer-change--last-buffer))

(defun my-buffer-change--save-current-buffer ()
  (setq my-buffer-change--last-buffer (current-buffer)))

(defvar my-buffer-change-hook nil)

(defun my-buffer-change--run-hook ()
  (when (my-buffer-change--buffer-changed?)
    (my-buffer-change--save-current-buffer)
    (run-hooks 'my-buffer-change-hook)))

(defun my-buffer-change-setup ()
  "Setup `my-buffer-change'."
  (add-hook 'post-command-hook 'my-buffer-change--run-hook))

(provide 'my-buffer-change)
;;; my-buffer-change.el ends here
