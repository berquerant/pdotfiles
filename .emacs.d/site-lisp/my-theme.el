;;; my-theme.el --- my theme loading -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package solarized
  :straight (solarized :host github :repo "bbatsov/solarized-emacs")
  :custom
  (solarized-distinct-fringe-background t)
  (solarized-use-variable-pitch t)
  (solarized-high-contrast-mode-line t)
  (solarized-use-less-bold nil)
  (solarized-use-more-italic t)
  (solarized-emphasize-indicators t)
  (solarized-scale-markdown-headlines t)
  (solarized-highlight-numbers t)
  (solarized-distinct-doc-face t))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  :config
  (my-macro-ring-hook "my-themes" '(manoj-dark modus-vivendi-tinted solarized-dark))
  (defun my-theme--init-basic-appearance ()
    (set-cursor-color "green")
    (setq cursor-type 'box))
  (defun my-theme--rotate (current-theme next-theme)
    (message "Rotate theme: %s -> %s" current-theme next-theme)
    (disable-theme current-theme)
    (disable-theme next-theme)
    (load-theme next-theme t)
    (my-theme--init-basic-appearance))
  (add-to-list 'my-themes-ring-hook 'my-theme--rotate)
  (my-themes-ring-hook-rotate)
  (bind-key "M-s 1" 'my-themes-ring-hook-rotate))

(provide 'my-theme)
;;; my-theme.el ends here
