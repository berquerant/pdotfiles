;;; my-misc.el --- my miscellaneous utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup my-misc nil
  "My miscellaneous utilities."
  :prefix "my-misc-")

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

(defvar my-misc-other-window-prediates nil
  "List of functions: window to bool.
Return t means that the window is selectable by `other-window'.")

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

(defun my-misc--delete-other-frames (&optional frame)
  (cl-loop for f in (frame-list)
           when (not (eq f frame))
           do (delete-frame f))
  (select-frame frame))

;;;###autoload
(defun my-misc-delete-other-frames (&optional frame)
  "`delete-frame' except FRAME."
  (interactive)
  (my-misc--delete-other-frames (or frame (selected-frame))))

(provide 'my-misc)
;;; my-misc.el ends here
