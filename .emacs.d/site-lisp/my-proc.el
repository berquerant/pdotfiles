;;; my-proc.el --- my process utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup my-proc nil
  "My process utilities."
  :prefix "my-proc-")

(defun my-proc-shell-command-to-string-secure (&rest args)
  (s-chomp (shell-command-to-string (mapconcat #'shell-quote-argument args " "))))

(provide 'my-proc)
;;; my-proc.el ends here
