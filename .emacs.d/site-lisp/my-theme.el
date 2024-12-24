;;; my-theme.el --- my theme loading -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package solarized
  :straight (solarized :host github :repo "bbatsov/solarized-emacs"))

(use-package modus-themes
  :straight (modus-themes :host gitlab :repo "protesilaos/modus-themes")
  :ensure t
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-subtle-line-numbers t)
  (modus-themes-hl-line  '(accented))
  (modus-themes-paren-match '(bold))
  (modus-themes-region '(bg-only no-extend accented))
  :config
  (my-macro-ring-hook "my-themes" '(modus-vivendi solarized-dark manoj-dark solarized-light))
  (defun my-theme--init-basic-appearance ()
    (set-cursor-color "green")
    (setq cursor-type 'box))
  (defun my-theme--rotate (current-theme next-theme)
    (disable-theme current-theme)
    (disable-theme next-theme)
    (load-theme next-theme t)
    (my-theme--init-basic-appearance))
  (add-to-list 'my-themes-ring-hook 'my-theme--rotate)
  (my-themes-ring-hook-rotate)
  (bind-key "M-s 1" 'my-themes-ring-hook-rotate))

(provide 'my-theme)
;;; my-theme.el ends here
