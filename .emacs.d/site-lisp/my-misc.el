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
(defun my-misc-backward-delete-char-untabify ()
  "Do `w/kill-whole-line' if current line contain spaces only before `backward-delete-char-untabify'."
  (interactive)
  (if (my-misc--matches-only-spaces
       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
      (progn
        (kill-whole-line)
        (previous-line)
        (end-of-line))
    (backward-delete-char-untabify 1)))

;;;###autoload
(defun my-misc-eval-last-sexp-and-insert ()
    "Insert the preceding sexp with its value."
    (interactive)
    (let ((v (eval (elisp--preceding-sexp))))
      (insert (format "%S" v))))

;;;###autoload
(defun my-misc-tig-blame-current-buffer ()
  "Run tig blame on current buffer into new tmux window."
  (interactive)
  (shell-command
   (format "bash -c \"tmux new-window -n %s -c %s 'tig blame +%s %s'\""
           (my-getenv "TMUX_TIG_WINDOW")
           (file-name-directory buffer-file-name)
           (line-number-at-pos)
           (file-name-nondirectory buffer-file-name))))

(provide 'my-misc)
;;; my-misc.el ends here
