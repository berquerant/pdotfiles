;;; my-git-browse.el --- my git-browse wrappers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'little-async)

(defgroup my-git-browse nil
  "My git-browse wrappers."
  :prefix "my-git-browse-")

(defconst my-git-browse-git-browse-buffer-name "*git-browse*"
  "Buffer to git browse output.")

(defconst my-git-browse-gomod-browse-buffer-name "*gomod-browse*"
  "Buffer to gomod browse output.")

(defun my-git-browse--git-browse (&optional phases)
  "Open the current file committed to git in browser.
Requires https://github.com/berquerant/gbrowse"
    (let* ((path (file-name-nondirectory (buffer-file-name)))
           (linum (line-number-at-pos))
           (location (format "%s:%s" path linum))
           (phase (if phases (format "-phase %s" (s-join "," phases))
                    ""))
           (gbrowse-args (if (s-blank? phase) `("gbrowse" ,location)
                           `("gbrowse" ,phase ,location))))
      (little-async-start-process `("gomodbrowse" ,location)
                                  :process-name "gomod-browse"
                                  :buffer-name my-git-browse-gomod-browse-buffer-name)
      (little-async-start-process gbrowse-args
                                  :process-name "git-browse"
                                  :buffer-name my-git-browse-git-browse-buffer-name)))

;;;###autoload
(defun my-git-browse-git-browse (arg)
  "Open the current file committed to git in browser.

phases:

C-u C-u : default_branch
C-u     : tag,branch
(nil)   : (empty)

Requires https://github.com/berquerant/gbrowse"
  (interactive "p")
  (my-git-browse--git-browse (cl-case arg
                               (16 '("default_branch"))
                               (4 '("tag" "branch"))
                               (t nil))))

(provide 'my-git-browse)
;;; my-git-browse.el ends here
