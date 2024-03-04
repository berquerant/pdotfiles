;;; my-straight.el --- my straight extensions -*- lexical-binding: t -*-

;;; Code:

(require 's)
(require 'cl)
(require 'straight)
(require 'my-time)
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

(defun my-straight--message (fmt &rest args)
  (message (format "my-straight: %s" (apply 'format fmt args))))

(defun my-straight--read-profile ()
    (with-temp-buffer
      (insert-file-contents my-straight-profile-path)
      (read (buffer-string))))

(cl-defstruct my-straight--git-log
  "Git log info."
  (url
   nil
   :read-only t
   :type string)
  (log
   nil
   :read-only t
   :type string)
  (hash
   nil
   :read-only t
   :type string)
  (date
   nil
   :read-only t
   :type string)
  (reldate
   nil
   :read-only t
   :type string)
  (message
   nil
   :read-only t
   :type string))

(defun my-straight--git-log-timestamp (instance)
  (my-time-timestamp (my-straight--git-log-date instance)))

(defun my-straight--git-latest-remote-hash (url)
  (nth 0 (s-split-up-to
          "\t"
          (my-misc-shell-command-to-string
           (format "git ls-remote %s HEAD" url))
          2)))

(defun my-straight--git-fetch (repo-dir current-dir)
  (my-misc-with-cd repo-dir current-dir
                   (my-misc-shell-command-to-string "git fetch")))

(defun my-straight--git-latest-remote-hash-from-dir (repo-dir current-dir)
  (my-misc-with-cd repo-dir current-dir
                   (my-straight--git-latest-remote-hash
                    (my-misc-shell-command-to-string
                     "git config --get remote.origin.url"))))

(defconst my-straight--git-log-base-command
  "git log --pretty=format:'%H|%ad|%ar|%s' --date=format:'%Y-%m-%d %H:%M:%S' -1")

(defun my-straight--git-log-from-raw (url raw-log)
  "Analyze `my-straight--git-log-base-command' result."
  (let* ((log-splitted (s-split-up-to "|" raw-log 4))
         (log-hash (nth 0 log-splitted))
         (log-date (nth 1 log-splitted))
         (log-rel-date (nth 2 log-splitted))
         (log-message (nth 3 log-splitted)))
    (make-my-straight--git-log :url url
                               :log raw-log
                               :hash log-hash
                               :date log-date
                               :reldate log-rel-date
                               :message log-message)))

(defun my-straight--get-git-log (repo-dir current-dir &optional hash)
  (my-misc-with-cd repo-dir current-dir
                   (my-straight--git-log-from-raw
                    (my-misc-shell-command-to-string
                     "git config --get remote.origin.url")
                    (my-misc-shell-command-to-string
                     (format "%s %s"
                             my-straight--git-log-base-command
                             (or hash ""))))))

(defun my-straight--read-latest-remote-profile (repo-dir current-dir)
  (my-straight--git-fetch repo-dir current-dir)
  (let* ((latest-remote-hash (my-straight--git-latest-remote-hash-from-dir repo-dir current-dir))
         (latest-remote-log (my-straight--get-git-log repo-dir current-dir latest-remote-hash)))
    `(("lhash" . ,(my-straight--git-log-hash latest-remote-log))
      ("ldate" . ,(my-straight--git-log-date latest-remote-log))
      ("lts" . ,(my-straight--git-log-timestamp latest-remote-log))
      ("lreldate" . ,(my-straight--git-log-reldate latest-remote-log))
      ("lmessage" . ,(my-straight--git-log-message latest-remote-log)))))

(defun my-straight--read-current-profile (repo-dir current-dir)
  (let ((r (my-straight--get-git-log repo-dir current-dir)))
    `(("repohash" . ,(my-straight--git-log-hash r))
      ("url" . ,(my-straight--git-log-url r))
      ("date" . ,(my-straight--git-log-date r))
      ("ts" . ,(my-straight--git-log-timestamp r))
      ("reldate" . ,(my-straight--git-log-reldate r))
      ("message" . ,(my-straight--git-log-message r)))))

(defun my-straight--read-profile-elem (elem current-dir &optional read-latest-remote)
  (let* ((name (car elem))
         (hash (cdr elem))
         (repo-dir (format "%s/repos/%s" my-straight-dir-path name))
         (current-log-obj (my-straight--read-current-profile
                           repo-dir
                           current-dir))
         (latest-log-obj (if read-latest-remote (my-straight--read-latest-remote-profile
                                                 repo-dir
                                                 current-dir)
                           nil))
         (log-obj (append `(("name" . ,name)
                            ("hash" . ,hash))
                          current-log-obj
                          latest-log-obj)))
    `(,name . ,log-obj)))

(defun my-straight--read-profile-calc-ts-diff (profile-elem)
  (let* ((current (assoc "ts" profile-elem))
         (latest (assoc "lts" profile-elem))
         (current-ts (cdr current))
         (latest-ts (and latest (cdr latest)))
         (ts-diff (if latest-ts (- latest-ts current-ts)
                    0))
         (ts-diff-days (/ ts-diff 60 60 24)))
    `(("tsdiff" . ,ts-diff)
      ("tsdiffdays" . ,ts-diff-days))))

(cl-defun my-straight-read-profile (&key read-latest-remote name-list)
  "Read profile `my-straight-profile-path' and add git log information.
If READ-LATEST-REMOTE is t, add latest commit info.
NAME-LIST filters packages by name."
  (let ((profile (my-straight--read-profile))
        (current-dir (my-misc-pwd))
        (result nil))
    (while profile
      (let ((name (caar profile)))
        (when (or (not name-list)
                  (member name name-list))
          (let* ((r (my-straight--read-profile-elem
                     (car profile)
                     current-dir
                     read-latest-remote))
                 (td (if read-latest-remote (my-straight--read-profile-calc-ts-diff r)
                       nil))
                 (res (append r td)))
            (push res result))))

      (setq profile (cdr profile)))
    result))

(defun my-straight--save ()
  (my-straight--message "start straight-save")
  (my-straight--message "start straight-freeze")
  (straight-freeze-versions t)
  (my-straight--message "start read-profile")
  (with-temp-buffer
    (insert (json-encode (await (my-straight-read-profile))))
    (json-pretty-print-buffer-ordered)
    (write-region nil nil my-straight-save-path nil 'silent))
  (my-straight--message "end straight-save"))

(defun my-straight-save ()
  "Save extended straight profile to `my-straight-save-path'."
  (interactive)
  (my-straight--save))

(defun my-straight--display-profile-filter (elem name-list ts-diff-days)
  (let ((name (cdr (assoc "name" elem)))
        (tdd (cdr (assoc "tsdiffdays" elem))))
    (and (or (not name-list)
             (member name name-list))
         (or (not ts-diff-days)
             (> tdd ts-diff-days)))))

(cl-defun my-straight--display-profile (&key json name-list ts-diff-days col-list)
  "Display straight profile.
If JSON is t, as json.
NAME-LIST filters packages by name.
TS-DIFF-DAYS excludes those where the difference between the generation time
of the current commit and the generation time of the latest commit is
less than this value.
COL-LIST selects profile keys, available values are:
date, hash, ldate, lhash, lmessage, lreldate, lts, message, name, reldate,
repohash, ts, tsdiff, tsdiffdays, url."
  (my-straight--message "start display-profile")
  (let* ((profile (my-straight-read-profile :read-latest-remote t
                                            :name-list name-list))
         (result (cl-loop for elem in profile
                          when (my-straight--display-profile-filter
                                elem
                                name-list
                                ts-diff-days)
                          collect (cl-loop for x in (cdr elem)
                                           when (or (not col-list)
                                                    (member (car x) col-list))
                                           collect x))))
    (my-straight--message "end display-profile: %s" (if json (json-encode result)
                                                      result))))

(defun my-straight-display-profile (name-list ts-diff-days col-list)
  "Display straight profile.
See `my-straight-display-profile'."
  (interactive "sNAME-LIST: \nnTS-DIFF-DAYS: \nsCOL-LIST: ")
  (let ((nlist (s-split " " name-list t))
        (clist (s-split " " col-list t)))
    (my-straight--display-profile :json t
                                  :name-list nlist
                                  :ts-diff-days ts-diff-days
                                  :col-list clist)))

(provide 'my-straight)
;;; my-straight ends here
