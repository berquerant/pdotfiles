;;; my-package.el --- my package management -*- lexical-binding: t -*-

;;; Code:

(require 's)
(require 'my-time)
(require 'my-misc)

(defgroup my-package nil
  "My package management."
  :prefix "my-package-")

(defcustom my-package-backupd (concat user-emacs-directory "backup")
  "Where to backup.")

(defcustom my-package-dryrun nil
  "If non-nil, dry run.")

;;;###autoload
(defun my-package-restore (path backup-path)
  "Copy BACKUP-PATH to PATH.
It overwrites PATH and not affects BACKUP-PATH."
  (cond ((my-package--file-p backup-path) (my-package--copy-file-by-redirect backup-path path))
        ((my-package--directory-p backup-path) (my-package--remove path) (my-package--copy-directory backup-path path))
        (t (error "my-package-restore: %s not exist" backup-path))))

;;;###autoload
(defun my-package-new-backup (path &optional remove-origin)
  "Create backup of PATH.
Remove PATH if REMOVE-ORIGIN is not nil.
Return the path of the backup."
  (let ((backup (my-package--backup-path path)))
    (my-package--ensure-backupd)
    (cond ((my-package--file-p path) (my-package--copy-file-by-redirect path backup))
          ((my-package--directory-p path) (my-package--copy-directory path backup))
          (t (error "my-package-new-backup: %s not exist" path)))
    (when remove-origin
      (cond ((my-package--file-p path) (my-package--clear-file path))
            (t (my-package--remove path))))
    backup))

(defun my-package--directory-p (path)
  (file-directory-p path))

(defun my-package--file-p (path)
  (and (file-exists-p path) (not (file-directory-p path))))

(defun my-package--backup-path (path)
  (concat (file-name-as-directory my-package-backupd)
          (file-name-nondirectory path)
          "."
          (my-package--now)))

(defun my-package--now ()
  (format "%d_%s" (my-time-timestamp) (s-replace ":" "-" (s-replace " " "_" (my-time-datetime)))))

(defun my-package--ensure-backupd ()
  "Ensure that `my-package-backupd' exists."
  (my-package--ensured (file-name-as-directory my-package-backupd)))

(defun my-package--message (fmt &rest args)
  (message (format "my-package: %s" (apply 'format fmt args))))

(defun my-package--call-process (program &rest args)
  "`my-misc-call-process' with PROGRAM and `my-package-dryrun'."
  (my-package--message "%s %s" program (s-join " " args))
  (apply 'my-misc-call-process program my-package-dryrun args))

(defun my-package--ensured (path)
  "mkdir -p PATH."
  (my-package--call-process "mkdir" "-p" path))

(defun my-package--remove (path)
  "rm -rf PATH."
  (my-package--call-process "rm" "-rf" path))

(defun my-package--move (src dst)
  "mv SRC DST."
  (my-package--call-process "mv" src dst))

(defun my-package--copy-directory (src dst)
  "cp -r SRC DST."
  (my-package--call-process "cp" "-r" src dst))

(defun my-package--clear-file (path)
  "echo -n > PATH."
  (my-package--message "clear %s" path)
  (unless my-package-dryrun
    (my-misc-clear-file path)))

(defun my-package--copy-file-by-redirect (src dst)
  "cat SRC > DST."
  (my-package--message "copy %s into %s" src dst)
  (unless my-package-dryrun
    (my-misc-copy-file src dst)))

(provide 'my-package)
;;; my-package ends here
