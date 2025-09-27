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

(defun my-git-browse--git-browse ()
  "Open the current file committed to git in browser.
Requires https://github.com/berquerant/gbrowse"
    (let* ((path (file-name-nondirectory (buffer-file-name)))
           (linum (line-number-at-pos))
           (location (format "%s:%s" path linum)))
      (little-async-start-process `("gomodbrowse" ,location)
                                  :process-name "gomod-browse"
                                  :buffer-name my-git-browse-gomod-browse-buffer-name)
      (little-async-start-process `("gbrowse" ,location)
                                  :process-name "git-browse"
                                  :buffer-name my-git-browse-git-browse-buffer-name)))

;;;###autoload
(defun my-git-browse-git-browse ()
  "Open the current file committed to git in browser.
Requires https://github.com/berquerant/gbrowse"
  (interactive)
  (my-git-browse--git-browse))

(provide 'my-git-browse)
;;; my-git-browse.el ends here
