;;; my-open-file --- -*- lexical-binding: t -*-

(require 's)
(require 'f)

(defun my-open-file--read (path)
  (if (f-exists? path)
      (s-trim (f-read-text path))
    ""))

(defun my-open-file--open (s)
  (cond ((s-blank? s) (message "[my-open-file] target is empty!"))
        ((s-contains? ":" s) (let* ((ss (s-split ":" s))
                                    (p (car ss))
                                    (n (string-to-number (cadr ss))))
                               (find-file p)
                               (goto-line n)))
        (t (find-file s))))

(defcustom my-open-file-target (my-getenv-join "EMACSD" ".my-open-file-target")
  "`my-open-file-find' target file."
  :type 'string)

;;;###autoload
(defun my-open-file-find ()
  "Open the file specified by `my-open-file-target'."
  (interactive)
  (my-open-file--open (my-open-file--read my-open-file-target)))

(provide 'my-open-file)
;;; my-open-file.el ends here
