;;; my-rpath.el --- my rpath wrappers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'idle-timer)

(defun my-rpath--category ()
  (case major-mode
    ('yaml-mode "yaml")
    ('json-mode "json")))

(defun my-rpath--call (&optional verbose)
  (let* ((path (file-name-nondirectory (buffer-file-name)))
         (linum (line-number-at-pos))
         (colnum (+ 1 (current-column)))
         (line-flag (format "-line %d" linum))
         (column-flag (format "-column %d" colnum))
         (args `("rpath" ,line-flag ,column-flag))
         (cmd (format "%s %s %s"
                      (s-join " " (if verbose (append args '("-verbose"))
                                    args))
                      (my-rpath--category)
                      path)))
    (let ((result (shell-command-to-string cmd)))
      (if (s-blank? result) (message "my-rpath: failed %s" cmd)
        (message "my-rpath: %s -> %s" cmd result)
        (popup-tip result)))))

;;;###autoload
(defun my-rpath-call (arg)
  "Display path of yaml or json element.

C-u   : verbose output
(nil) : default output

Requires https://github.com/berquerant/rpath"
  (interactive "p")
  (my-rpath--call (case arg
                    (4 t)
                    (t nil))))

(defun my-rpath-run ()
  (my-rpath--call))

(idle-timer-define-minor-mode my-rpath-run 3)
;;;###autoload
(defun my-rpath-mode-toggle ()
  (interactive)
  (if my-rpath-run-idle-timer-mode (my-rpath-run-idle-timer-mode -1)
    (my-rpath-run-idle-timer-mode 1)))

(provide 'my-rpath)
;;; my-rpath.el ends here
