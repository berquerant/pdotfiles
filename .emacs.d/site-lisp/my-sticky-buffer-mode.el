;;; my-sticky-buffer-mode.el --- make sticky buffers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;;###autoload
(define-minor-mode my-sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) my-sticky-buffer-mode))

(defun my-sticky-buffer-mode--delete-other-windows (window)
  "Delete other windows except WINDOW and `my-sticky-buffer-mode' enabled windows."
  (cl-loop for w in (window-list)
        when (not (eq w window))
        do (progn
             (select-window w)
             (unless my-sticky-buffer-mode
               (delete-window w))))
  (select-window window))

(defun my-sticky-buffer-mode-delete-other-windows (&optional window)
  "Delete other windows except WINDOW and `my-sticky-buffer-mode' enabled windows."
  (interactive)
  (my-sticky-buffer-mode--delete-other-windows (or window (get-buffer-window))))

(provide 'my-sticky-buffer-mode)
;;; my-sticky-buffer-mode.el ends here
