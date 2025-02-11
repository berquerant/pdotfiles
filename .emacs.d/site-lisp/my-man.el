;;; my-man.el --- my man wrappers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'little-async)

(defgroup my-man nil
  "My man wrappers."
  :prefix "my-man-")

(defconst my-man-buffer-name "*man-browse*"
  "Buffer to man browse.")

(defconst my-man-dman-command (my-getenv-join "DOTFILES_ROOT" "bin" "dman.sh"))
(defconst my-man-hman-command (my-getenv-join "DOTFILES_ROOT" "bin" "hman.sh"))

;;;###autoload
(defun my-man-dman (arg)
  "Invoke dman."
  (little-async-start-process `(,my-man-dman-command "debian" ,arg)
                              :process-name "dman"
                              :buffer-name my-man-buffer-name))

;;;###autoload
(defun my-man-hman (arg)
  "Invoke hman."
  (little-async-start-process `(,my-man-hman-command ,arg)
                              :process-name "hman"
                              :buffer-name my-man-buffer-name))

(provide 'my-man)
;;; my-man.el ends here
