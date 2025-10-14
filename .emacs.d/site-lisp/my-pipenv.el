;;; my-pipenv.el --- my pipenv wrappers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'my-proc)

(defgroup my-pipenv nil
  "My pipenv wrappers."
  :prefix "my-pipenv-")

(defcustom my-pipenv-get-path-command "pipenv-get-path"
  "Command to get path from module name."
  :type 'string)

;;;###autoload
(defun my-pipenv-get-path (target)
  "Get path of TARGET module."
  (my-proc-shell-command-to-string-secure my-pipenv-get-path-command target))

(provide 'my-pipenv)
;;; my-pipenv.el ends here
