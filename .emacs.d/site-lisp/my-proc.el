;;; my-proc.el --- my process utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup my-proc nil
  "My process utilities."
  :prefix "my-proc-")

(defconst my-proc-call-process-buffer "*my-proc-call-process*"
  "Where `my-proc-call-process' write output.")

(defun my-proc-call-process--insert-buffer (msg)
  (with-current-buffer (get-buffer-create my-proc-call-process-buffer)
    (goto-char (point-max))
    (insert msg)))

(defun my-proc-call-process (program &optional dry &rest args)
  "Call PROGRAM synchronously in separate process."
  (my-proc-call-process--insert-buffer
   (if dry (format "my-proc-call-process:dry:%s %s\n" program (s-join " " args))
     (format "my-proc-call-process:%s %s=> %d\n"
             program
             (s-join " " args)
             (apply 'call-process program nil my-proc-call-process-buffer nil args))))
  nil)

(defun my-proc-shell-command-to-string (command)
  (s-chomp (shell-command-to-string command)))

(provide 'my-proc)
;;; my-proc.el ends here
