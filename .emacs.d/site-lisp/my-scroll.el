;;; my-scroll.el --- my utilities for scrolling -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup my-scroll nil
  "My utilities for scrolling."
  :prefix "my-scroll-")

(defcustom my-scroll-medium-lines 10
  "Define medium distance."
  :type 'integer)

;;;###autoload
(defun my-scroll-scroll-down (&optional arg)
  "Scroll down ARG lines.
default ARG is 1."
  (interactive "P")
  (let ((x (prefix-numeric-value arg)))
    (scroll-down x)))

;;;###autoload
(defun my-scroll-scroll-up (&optional arg)
  "Scroll up ARG lines.
default ARG is 1."
  (interactive "P")
  (let ((x (prefix-numeric-value arg)))
    (scroll-up x)))

;;;###autoload
(defun my-scroll-scroll-down-relationally (&optional arg)
  "Scroll down ARG lines while keeping relative location of cursor on buffer.
default ARG is 1."
  (interactive "P")
  (let ((x (prefix-numeric-value arg)))
    (scroll-down x)
    (forward-line (- x))))

;;;###autoload
(defun my-scroll-scroll-up-relationally (&optional arg)
  "Scroll up ARG lines while keeping relative location of cursor on buffer.
default ARG is 1."
  (interactive "P")
  (let ((x (prefix-numeric-value arg)))
    (scroll-up x)
    (forward-line x)))

(defmacro my-scroll--medium (f)
  "Define `my-scroll-scroll-XXX-medium' functions."
  (let ((fname (symbol-name f))
        (name (format "%s-medium" (symbol-name f)))
        (doc-string (format "Call `%s' `my-scroll-medium-lines' times." (symbol-name f))))
    `(progn
       (defun ,(read name)
           ()
         ,doc-string
         (interactive)
         (,f my-scroll-medium-lines))
       (autoload ',(read name) "my-scroll"))))

(my-scroll--medium my-scroll-scroll-up)
(my-scroll--medium my-scroll-scroll-down)
(my-scroll--medium my-scroll-scroll-up-relationally)
(my-scroll--medium my-scroll-scroll-down-relationally)

(provide 'my-scroll)
;;; my-scroll.el ends here
