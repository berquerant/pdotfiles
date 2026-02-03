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
  (my-show-indentation--disable-cleanup))

(defun my-show-indentation--disable-cleanup ()
  (set-window-margins (selected-window) 0)
  (remove-overlays (point-min) (point-max) 'type 'margin-indent))

(defun my-show-indentation--enable ()
  (add-hook 'post-command-hook 'my-show-indentation--show nil t)
  (my-show-indentation--enable-setup))

(defun my-show-indentation--enable-setup ()
  (my-show-indentation--show))

(defun my-show-indentation--enabled? ()
  (member 'my-show-indentation--show post-command-hook))

;;;###autoload
(defun my-show-indentation-toggle ()
  "If `my-show-indentation--show' is not in `post-command-hook', add it, else remove it."
  (interactive)
  (if (my-show-indentation--enabled?) (my-show-indentation--disable)
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

(defun my-show-indentation-global--enabled? ()
  ;; check global post-command-hook
  (member 'my-show-indentation--show (default-value 'post-command-hook)))

;;;###autoload
(defun my-show-indentation-global-toggle ()
  "If `my-show-indentation--show' is not in `post-command-hook', add it, else remove it."
  (interactive)
  (if (my-show-indentation-global--enabled?) (my-show-indentation-global--disable)
    (my-show-indentation-global--enable)))

(defun my-show-indentation-global--disable ()
  (remove-hook 'post-command-hook 'my-show-indentation--show)
  (my-show-indentation--disable-cleanup))

(defun my-show-indentation-global--enable ()
  (add-hook 'post-command-hook 'my-show-indentation--show)
  (my-show-indentation--enable-setup))

(provide 'my-show-indentation)
;;; my-show-indentation.el ends here
