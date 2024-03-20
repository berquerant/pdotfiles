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

(defcustom my-straight-grinfo "grinfo"
  "grinfo command.
https://github.com/berquerant/grinfo"
  :type 'string)

(defcustom my-straight-grinfo-worker 8
  "grinfo worker num."
  :type 'number)

(defcustom my-straight-grinfo-buffer 200
  "grinfo input/output buffer size."
  :type 'number)

(defun my-straight--message (fmt &rest args)
  (message (format "my-straight: %s" (apply 'format fmt args))))

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

(defun my-straight-display-profile-command ()
  `(,my-straight-grinfo
    "-worker"
    ,(number-to-string my-straight-grinfo-worker)
    "-buffer"
    ,(number-to-string my-straight-grinfo-buffer)))

(defun my-straight-display-profile ()
  "Display installed package git info."
  (interactive)
  (let ((directories (s-join "\n" (my-straight-list-package-directories))))
    (little-async-start-process (my-straight-display-profile-command)
                                :process-name "grinfo"
                                :buffer-name "*my-straight-display-profile*"
                                :stderr "*my-straight-display-profile-err*"
                                :timeout (* 300 1000) ; 300 seconds
                                :input directories)))

(provide 'my-straight)
;;; my-straight.el ends here
