;;; my-command-repeated.el --- my command repeated utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup my-command-repeated nil
  "My command repeated utilities."
  :prefix "my-command-repeated-")

(defvar my-command-repeated--command-repeated-count 0
  "Counter of repeated command.")

(defun my-command-repeated--command-repeated? ()
  "Return t if command is repeated."
  (eq last-command this-command))

(defun my-command-repeated-command-repeated-count ()
  "Return count of repeated command."
  my-command-repeated--command-repeated-count)

(defun my-command-repeated--command-repeated-hook ()
  "A hook to control `my-command-repeated--command-repeated-count'."
  (setq my-command-repeated--command-repeated-count
        (if (my-command-repeated--command-repeated?)
            (+ my-command-repeated--command-repeated-count 1)
          0)))

(defun my-command-repeated-setup ()
  "Enable `my-command-repeated--command-repeated-hook'."
  (add-hook 'pre-command-hook 'my-command-repeated--command-repeated-hook))

(provide 'my-command-repeated)
;;; my-command-repeated.el ends here
