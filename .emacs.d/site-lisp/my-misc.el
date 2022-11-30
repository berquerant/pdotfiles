;;; my-misc.el --- my miscellaneous utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 's)

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

(provide 'my-misc)
;;; my-misc.el ends here
