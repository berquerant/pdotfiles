;;; my-scroll.el --- my utilities for scrolling -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup my-scroll nil
  "My utilities for scrolling."
  :prefix "my-scroll-")

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

(provide 'my-scroll)
;;; my-scroll.el ends here
