;;; my-misc.el --- my miscellaneous utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'cl-lib)

(defgroup my-misc nil
  "My miscellaneous utilities."
  :prefix "my-misc-")

(defun my-misc--matches-only-spaces (s)
  "Is S the empty string or contain spaces only?"
  (or (s-equals? s "") (s-matches? "^\s *$" s)))

(defvar my-misc--command-repeated-count 0
  "Counter of repeated command.")

(defun my-misc--command-repeated? ()
  "Return t if command is repeated."
  (eq last-command this-command))

(defun my-misc-command-repeated-count ()
  "Return count of repeated command."
  my-misc--command-repeated-count)

(defun my-misc--command-repeated-hook ()
  "A hook to control `my-misc--command-repeated-count'."
  (setq my-misc--command-repeated-count
        (if (my-misc--command-repeated?)
            (+ my-misc--command-repeated-count 1)
          0)))
(add-hook 'pre-command-hook 'my-misc--command-repeated-hook)

;;;###autoload
(defun my-misc-eval-last-sexp-and-insert ()
    "Insert the preceding sexp with its value."
    (interactive)
    (let ((v (eval (elisp--preceding-sexp))))
      (insert (format "%S" v))))

;;;###autoload
(defun my-misc-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))

;;;###autoload
(defun my-misc-pp-macroexpand-1-last-sexp ()
  "Read sexp before point, output `macroexpand-1`-ed result in pretty format."
  (interactive)
  (pp (macroexpand-1 (elisp--preceding-sexp))))

;;;###autoload
(defun my-misc-pp-macroexpand-all-last-sexp ()
  "Read sexp before point, output `macroexpand-all`-ed result in pretty format."
  (interactive)
  (pp (macroexpand-all (elisp--preceding-sexp))))

(defun my-misc--current-path (cut)
  (let ((p (buffer-file-name)))
    (cond ((not p) (message "Not visiting file.") nil)
          ((not cut) p)
          (t (s-join "/" (nthcdr cut (s-split "/" p)))))))

;;;###autoload
(defun my-misc-current-path (cut)
  "Store current buffer file path to kill ring.
If CUT, trim CUT parent directories.
e.g. path is /a/b/c and CUT is 1 then stores b/c"
  (interactive (list (read-string (format "Cut index (%s): " (buffer-file-name) nil))))
  (let ((p (my-misc--current-path
            (if cut (+ 1 (string-to-number cut))
              nil))))
    (when p
      (message "Stored: %s" p)
      (kill-new p))))

(defcustom my-misc-other-window-prediates nil
  "List of functions: window to bool.
Return t means that the window is selectable by `other-window'."
  :type '(repeat function))

(defun my-misc--other-window-selectable (window)
  (select-window window)
  (cl-loop for f in my-misc-other-window-prediates
           when (not (apply f (list window)))
           return nil
           finally return t))

(defun my-misc--other-window (count &optional window)
  (let ((windows-len (length (window-list)))
        (select-count 0))
    (message "DEBUG [my-misc--other-window] %d window %s" select-count (get-buffer-window))
    (other-window count)
    (while (and (< select-count windows-len)
                     (not (my-misc--other-window-selectable
                           (get-buffer-window))))
      (message "DEBUG [my-misc--other-window] %d window %s" select-count (get-buffer-window))
      (+ select-count 1)
      (other-window count))))

;;;###autoload
(defun my-misc-other-window-predicates-add (f)
  "Add F to `my-misc-other-window-prediates'."
  (add-to-list 'my-misc-other-window-prediates f))

;;;###autoload
(defun my-misc-other-window (&optional window)
  "`other-window' except WINDOW and windows `my-misc-other-window-prediates' return nil."
  (interactive)
  (my-misc--other-window 1 (or window (get-buffer-window))))

;;;###autoload
(defun my-misc-other-window-reverse (&optional window)
  "Reversed `other-window' except WINDOW and windows `my-misc-other-window-prediates' return nil."
  (interactive)
  (my-misc--other-window -1 (or window (get-buffer-window))))

(defcustom my-misc-delete-window-predicates nil
  "List of functions: window to bool.
Return t means that the window is deletable.
When a predicate is called, selected window will be the window of the argument."
  :type '(repeat function))

(defun my-misc--delete-window-deletable (window)
  (select-window window)
  (cl-loop for f in my-misc-delete-window-predicates
           when (not (apply f (list window)))
           return nil
           finally return t))

(defun my-misc--delete-other-windows (&optional window)
  (cl-loop for w in (window-list)
           when (not (eq w window))
           do (when (my-misc--delete-window-deletable w)
                (delete-window w)))
  (select-window window))

;;;###autoload
(defun my-misc-delete-window-predicates-add (f)
  "Add F to `my-misc-delete-window-predicates'."
    (add-to-list 'my-misc-delete-window-predicates f))

;;;###autoload
(defun my-misc-delete-other-windows (&optional window)
  "`delete-other-windows' except WINDOW and windows `my-misc-delete-window-predicates' return nil."
  (interactive)
  (my-misc--delete-other-windows (or window (get-buffer-window))))

(defconst my-misc-git-browse-buffer-name "*git-browse*"
  "Buffer to git browse output.")

(defun my-misc-git-browse--generate-repo-url ()
  (shell-command-to-string "git config --get remote.origin.url | tr ':' '/' | sed 's|git@|https://|' | tr -d '\n'"))

;;;###autoload
(defun my-misc-git-browse ()
  "Open the current file committed to git in browser."
    (interactive)
    (let ((path (file-name-nondirectory (buffer-file-name)))
          (linum (line-number-at-pos))
          (repo-url (my-misc-git-browse--generate-repo-url)))
      (little-async-start-process (format "gh browse %s:%s || open %s" path linum repo-url)
                                  :process-name "github-cli"
                                  :buffer-name my-misc-git-browse-buffer-name)))

(provide 'my-misc)
;;; my-misc.el ends here
