;;; my-path.el --- my path utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 's)

(defgroup my-path nil
  "My path utilities."
  :prefix "my-path-")

(defun my-path--current-path (cut)
  (let ((p (buffer-file-name)))
    (cond ((not p) (message "Not visiting file.") nil)
          ((or (not cut) (eql cut 0)) p)
          (t (let ((ss (s-split "/" p)))
               (s-join "/"
                       (nthcdr (if (>= cut 0) (+ cut 1)
                                 (+ (length ss) cut))
                               (s-split "/" p))))))))

;;;###autoload
(defun my-path-current-path (cut)
  "Store current buffer file path to kill ring.
If CUT, trim CUT parent directories.
e.g. path is /a/b/c and CUT is 1 then stores b/c
  -1 then stores c
  -2 then stores b/c"
  (interactive (list (read-string (format "Cut index (%s): " (buffer-file-name) nil))))
  (let ((p (my-path--current-path
            (if cut (string-to-number cut)
              nil))))
    (when p
      (message "Stored: %s" p)
      (kill-new p))))

(defun my-path-pwd ()
  (nth 1 (s-split " " (pwd))))

(defun my-path--cd (directory)
  (cd directory)
  nil)

(defmacro my-path-with-cd (directory current &rest body)
  "Change directory to DIRECTORY, and evaluate BODY."
  `(progn
     (my-path--cd ,directory)
     (unwind-protect
         (progn ,@body)
       (my-path--cd ,current))))

(defun my-path-copy-file (src dst)
  "Copy SRC to DST."
  (with-temp-buffer
    (insert-file-contents src)
    (mm-write-region (point-min) (point-max) dst)))

(defun my-path-clear-file (path)
  "Clear PATH."
  (when (file-writable-p path)
    (with-temp-file path)))

(provide 'my-path)
;;; my-path.el ends here
