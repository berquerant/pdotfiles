;;; my-show-indentation.el --- show indentation in margin -*- lexical-binding: t -*-

;;; Code:

(defcustom my-show-indentation-margin-width 3
  "Width of the margin.")

(defun my-show-indentation--show ()
  "Show `current-indentation' in the left margin."
  (set-window-margins (selected-window) my-show-indentation-margin-width)
  (remove-overlays (point-min) (point-max) 'type 'margin-indent)
  (save-excursion
    (goto-char (window-start))
    (while (< (point) (window-end))
      (let* ((value (current-indentation))
             (ov (make-overlay (line-beginning-position) (line-beginning-position))))
        (overlay-put ov 'before-string
                     (propertize " " 'display `((margin left-margin) ,(format "%3d" value))))
        (overlay-put ov 'type 'margin-indent)
        (forward-line 1)))))

;;;###autoload
(defun my-show-indentation-show ()
  "Show `current-indentation' in the left margin."
  (interactive)
  (my-show-indentation--show))

(defun my-show-indentation--disable ()
  (remove-hook 'post-command-hook 'my-show-indentation--show t)
  (set-window-margins (selected-window) 0)
  (remove-overlays (point-min) (point-max) 'type 'margin-indent))

(defun my-show-indentation--enable ()
  (add-hook 'post-command-hook 'my-show-indentation--show nil t)
  (my-show-indentation--show))

;;;###autoload
(defun my-show-indentation-toggle ()
  "If `my-show-indentation--show' is not in `post-command-hook', add it, else remove it."
  (interactive)
  (if (member 'my-show-indentation--show post-command-hook) (my-show-indentation--disable)
    (my-show-indentation--enable)))

;;;###autoload
(defun my-show-indentation-enable ()
  "Add `my-show-indentation--show' to `post-command-hook'."
  (interactive)
  (my-show-indentation--enable))

;;;###autoload
(defun my-show-indentation-disable ()
  "Remove `my-show-indentation--show' from `post-command-hook'."
  (interactive)
  (my-show-indentation--disable))

;;;###autoload
(defun my-show-indentation-global-toggle ()
  "If `my-show-indentation--enable' is not in `buffer-list-update-hook', add it, else remove it."
  (interactive)
  (if (member 'my-show-indentation--enable buffer-list-update-hook) (my-show-indentation-global--disable)
    (my-show-indentation-global--enable)))

(defun my-show-indentation-global--clear-hook ()
  (remove-hook 'buffer-list-update-hook 'my-show-indentation--enable)
  (remove-hook 'buffer-list-update-hook 'my-show-indentation--disable))

(defun my-show-indentation-global--disable ()
  (my-show-indentation-global--clear-hook)
  (add-hook 'buffer-list-update-hook 'my-show-indentation--disable)
  (my-show-indentation--disable))

(defun my-show-indentation-global--enable ()
  (my-show-indentation-global--clear-hook)
  (add-hook 'buffer-list-update-hook 'my-show-indentation--enable)
  (my-show-indentation--enable))

(provide 'my-show-indentation)
;;; my-show-indentation.el ends here
