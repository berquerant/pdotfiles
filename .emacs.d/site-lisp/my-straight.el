;;; my-straight.el --- my straight extensions -*- lexical-binding: t -*-

;;; Code:

(require 's)
(require 'cl)
(require 'my-misc)

(defgroup my-straight nil
  "My straight extensions."
  :prefix "my-straight-")

(defcustom my-straight-profile-path (concat user-emacs-directory "straight-default.el")
  "Straight-default.el."
  :type 'string)

(defcustom my-straight-dir-path (concat user-emacs-directory "straight")
  "Straight directory."
  :type 'string)

(defcustom my-straight-save-path (concat user-emacs-directory "my-straight.json")
  "Save extended straight profile."
  :type 'string)

(defun my-straight--read-profile ()
    (with-temp-buffer
      (insert-file-contents my-straight-profile-path)
      (read (buffer-string))))

(defun my-straight-read-profile ()
  "Read profile `my-straight-profile-path' and add git log information."
  (let ((profile (my-straight--read-profile))
        (current-dir (my-misc-pwd))
        (result nil))
    (while profile
      (let* ((elem (car profile))
             (name (car elem))
             (hash (cdr elem))
             (repo-dir (format "%s/repos/%s" my-straight-dir-path name)))
        (my-misc-with-cd repo-dir current-dir
                         (let* ((origin-url (my-misc-shell-command-to-string
                                             "git config --get remote.origin.url"))
                                ;; https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History
                                (log (my-misc-shell-command-to-string
                                      "git log --pretty=format:'%H|%ad|%ar|%s' --date=format:'%Y-%m-%d %H:%M:%S' -1"))
                                (log-splitted (s-split-up-to "|" log 4))
                                (log-hash (nth 0 log-splitted))
                                (log-date (nth 1 log-splitted))
                                (log-rel-date (nth 2 log-splitted))
                                (log-message (nth 3 log-splitted))
                                (log-obj `(("name" . ,name)
                                           ("hash" . ,hash)
                                           ("repohash" . ,log-hash)
                                           ("url" . ,origin-url)
                                           ("date" . ,log-date)
                                           ("reldate" . ,log-rel-date)
                                           ("message" . ,log-message)))
                                (log-elem `(,name . ,log-obj)))
                           (push log-elem result)))
        (setq profile (cdr profile))))
    result))

(defun my-straight-save ()
  "Save extended straight profile to `my-straight-save-path'."
  (interactive)
  (with-temp-buffer
    (insert (json-encode (my-straight-read-profile)))
    (json-pretty-print-buffer-ordered)
    (write-region nil nil my-straight-save-path nil 'silent)))

(defun my-straight-display-profile (json &rest name-list)
  "Display straight profile.
If JSON is t, as json.
NAME-LIST filters information, available filters are name, hash, repohash, url, date, reldate and message."
  (cl-loop for elem in (my-straight-read-profile)
           do (let ((obj (cl-loop for x in (cdr elem)
                                  when (or (not name-list)
                                           (member (car x) name-list))
                                  collect x)))
                (message "%s" (if json (json-encode obj)
                                obj)))))

(provide 'my-straight)
;;; my-straight ends here
