;;; my-misc.el --- my miscellaneous utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'cl-lib)
(require 'little-async)

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
          ((or (not cut) (eql cut 0)) p)
          (t (let ((ss (s-split "/" p)))
               (s-join "/"
                       (nthcdr (if (>= cut 0) (+ cut 1)
                                 (+ (length ss) cut))
                               (s-split "/" p))))))))

;;;###autoload
(defun my-misc-current-path (cut)
  "Store current buffer file path to kill ring.
If CUT, trim CUT parent directories.
e.g. path is /a/b/c and CUT is 1 then stores b/c
  -1 then stores c
  -2 then stores b/c"
  (interactive (list (read-string (format "Cut index (%s): " (buffer-file-name) nil))))
  (let ((p (my-misc--current-path
            (if cut (string-to-number cut)
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

(defconst my-misc-gomod-browse-buffer-name "*gomod-browse*"
  "Buffer to gomod browse output.")

(defun my-misc--git-browse (&optional phases)
  "Open the current file committed to git in browser.
Requires https://github.com/berquerant/gbrowse"
    (let* ((path (file-name-nondirectory (buffer-file-name)))
           (linum (line-number-at-pos))
           (location (format "%s:%s" path linum))
           (phase (if phases (format "-phase %s" (s-join "," phases))
                    ""))
           (args `("gbrowse" ,phase ,location)))
      (little-async-start-process (format "gomodbrowse %s" location)
                                  :process-name "gomod-browse"
                                  :buffer-name my-misc-gomod-browse-buffer-name)
      (little-async-start-process (s-join " " args)
                                  :process-name "git-browse"
                                  :buffer-name my-misc-git-browse-buffer-name)))

;;;###autoload
(defun my-misc-git-browse (arg)
  "Open the current file committed to git in browser.

phases:

C-u C-u : default_branch
C-u     : tag,branch
(nil)   : (empty)

Requires https://github.com/berquerant/gbrowse"
  (interactive "p")
  (my-misc--git-browse (case arg
                         (16 '("default_branch"))
                         (4 '("tag" "branch"))
                         (t nil))))

(defconst my-misc-call-process-buffer "*my-misc-call-process*"
  "Where `my-misc-call-process' write output.")

(defun my-misc-call-process--insert-buffer (msg)
  (with-current-buffer (get-buffer-create my-misc-call-process-buffer)
    (goto-char (point-max))
    (insert msg)))

;;;###autoload
(defun my-misc-call-process (program &optional dry &rest args)
  "Call PROGRAM synchronously in separate process."
  (my-misc-call-process--insert-buffer
   (if dry (format "my-misc-call-process:dry:%s %s\n" program (s-join " " args))
     (format "my-misc-call-process:%s %s=> %d\n"
             program
             (s-join " " args)
             (apply 'call-process program nil my-misc-call-process-buffer nil args))))
  nil)

;;;###autoload
(defun my-misc-copy-file (src dst)
  "Copy SRC to DST."
  (with-temp-buffer
    (insert-file-contents src)
    (mm-write-region (point-min) (point-max) dst)))

;;;###autoload
(defun my-misc-clear-file (path)
  "Clear PATH."
  (when (file-writable-p path)
    (with-temp-file path)))

(defun my-misc-approx-collect-symbols (buffer-or-name)
  "Experimental: Collect symbols in the BUFFER-OR-NAME."
  (let (symbols)
    (with-current-buffer (get-buffer-create buffer-or-name)
      (save-excursion
        (goto-char (point-min))
        (while (not (eq (point) (point-max)))
          (when (not (or (looking-at-p "[[:space:]\n]+$")
                         (and (bolp) (eolp))))
            (let ((symbol (intern (current-word))))
              (add-to-list 'symbols symbol)))
          (forward-word))))
    symbols))

(defun my-misc-describe-symbol (symbol)
  "Return result of `describe-symbol SYMBOL' as string."
  (describe-symbol symbol)
  (with-current-buffer (help-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun my-misc-symbol-introduced-version (symbol)
  "Experimental: Get Emacs version when SYMBOL was introduced."
  (describe-symbol symbol)
  (let ((doc (my-misc-describe-symbol symbol)))
    (and (string-match (format "^%s is" (symbol-name symbol)) doc) ; SYMBOL is...
         (string-match "Probably introduced at or before Emacs version \\(.*\\).$" doc)
         (match-string 1 doc))))

(defun my-misc-approx-collect-symbols-introduced-version (buffer-or-name)
  "Experimental: Get the symbols in the BUFFER-OR-NAME and Emacs version in which they were introduced.
Maybe more useful to search from https://www.gnu.org/software/emacs/news/"
  (let ((result nil))
    (cl-loop for symbol in (my-misc-approx-collect-symbols buffer-or-name)
             when (not (assoc symbol result))
             do (let ((version (my-misc-symbol-introduced-version symbol)))
                  (when version
                    (add-to-list 'result `(,symbol . ,version)))))
    result))

(defun my-misc--cd (directory)
  (cd directory)
  nil)

(defun my-misc-pwd ()
  (nth 1 (s-split " " (pwd))))

(defun my-misc-shell-command-to-string (command)
  (s-chomp (shell-command-to-string command)))

(defmacro my-misc-with-cd (directory current &rest body)
  "Change directory to DIRECTORY, and evaluate BODY."
  `(progn
     (my-misc--cd ,directory)
     (unwind-protect
         (progn ,@body)
       (my-misc--cd ,current))))

(provide 'my-misc)
;;; my-misc.el ends here
