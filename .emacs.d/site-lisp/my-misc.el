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

(provide 'my-misc)
;;; my-misc.el ends here
