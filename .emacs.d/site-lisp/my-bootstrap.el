;;; my-bootstrap.el --- My Emacs bootstrapping -*- lexical-binding: t -*-

;;; Commentary:

;; This should be loaded with `demand t'.
;;
;; - Fundamental settings
;; - Load my local packages
;;   - my-macro
;;   - my-misc
;;   - my-scroll
;;   - my-sticky-buffer-mode

;;; Code:

(setq inhibit-default-init t) ; ignore default.el
(repeat-mode t)
(setq-default ispell-program-name "aspell")

(use-package minimal-init
  :demand t
  :straight (minimal-init :host github
                          :repo "berquerant/emacs-minimal-init")
  :config
  (minimal-init-setup))

(defun other-window-back ()
  "Reverse `other-window'."
  (interactive)
  (other-window -1))

(defun my--revert-buffer ()
  (revert-buffer t t))

(defun my-revert-buffer ()
  (interactive)
  (my--revert-buffer))

(bind-keys :map global-map
           ([?\¥] . [?\\])
           ([?\C-¥] . [?\C-\\])
           ([?\M-¥] . [?\M-\\])
           ([?\C-\M-¥] . [?\C-\M-\\])
           ("M-s z" . my-revert-buffer)
           ("C-M-s" . isearch-forward-thing-at-point)
           ("C-s" . isearch-forward)
           ("C-t" . other-window)
           ("C-T" . other-window-back)
           ("C-<tab>" . tab-next)
           ("C-S-<tab>" . tab-previous)
           ;; TODO: find workaround. delete-frame crashes emacs. (delete-frame (selected-frame) nil) too
           ;; https://github.com/syl20bnr/spacemacs/issues/6301
           ;; ("C-x f 0" . delete-frame)
           ;; ("C-x f 1" . delete-other-frames)
           ("C-x C-t" . toggle-truncate-lines)
           ("C-x C-r" . read-only-mode)
           ("C-x w w" . overwrite-mode)
           ("M-SPC" . cycle-spacing) ; space conversions
           ("M-s e" . shell-command)
           ("C-h o" . describe-symbol)
           ("M-s x" . repeat)
           ("M-s y" . repeat-complex-command)
           ("M-s c" . compile)
           ("M-g ." . eldoc)
           ("M-z" . repeat)
           ("M-s q" . text-scale-adjust))

(use-package my-macro
  :demand t
  :straight (my-macro :type built-in)
  :config
  (my-macro-defun-toggle debug-on-error)
  (my-macro-defun-toggle debug-on-quit)
  (defconst my-init-el (format "%s/init.el" (my-getenv "EMACSD"))
    "init.el location.")
  (defconst my-zshrc (format "%s/.zshrc" (my-getenv "HOME"))
    ".zshrc location.")
  (my-macro-handle-file find-file my-init-el)
  (my-macro-handle-file load-file my-init-el)
  (my-macro-handle-file find-file my-zshrc)
  (my-macro-handle-buffer switch-to-buffer "*scratch*")
  (my-macro-handle-buffer switch-to-buffer-other-window "*scratch*")
  (my-macro-handle-buffer switch-to-buffer-other-tab "*scratch*")
  (my-macro-handle-buffer switch-to-buffer-other-frame "*scratch*")
  (bind-key "M-s 0" 'find-file-initel)
  (bind-key "M-s 9" 'find-file-zshrc)
  (bind-key "M-s 8" 'switch-to-buffer-scratch)
  (bind-key "M-s 7" 'switch-to-buffer-other-tab-scratch)
  (bind-key "M-s 6" 'switch-to-buffer-other-window-scratch)
  (bind-key "M-s 5" 'switch-to-buffer-other-frame-scratch))

(use-package modus-themes
  :ensure t
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-subtle-line-numbers t)
  (modus-themes-hl-line  '(accented))
  (modus-themes-paren-match '(bold))
  (modus-themes-region '(bg-only no-extend accented))
  :config
  (my-macro-ring-hook "my-themes" '(manoj-dark modus-vivendi))
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

;; important utilities

(use-package my-misc
  :straight (my-misc :type built-in)
  :commands my-misc-delete-window-predicates-add
  :bind
  ("C-x 1" . my-misc-delete-other-windows)
  ("M-s p p" . my-misc-current-path)
  ("M-s C-e" . my-misc-pp-macroexpand-1-last-sexp)
  ("M-s C-M-e" . my-misc-pp-macroexpand-all-last-sexp)
  ("C-x C-M-e" . my-misc-eval-last-sexp-and-insert)
  ("C-x C-x" . my-misc-exchange-point-and-mark))

(use-package my-scroll
  :straight (my-scroll :type built-in)
  :bind
  ("M-u" . my-scroll-scroll-up-relationally-medium)
  ("M-i" . my-scroll-scroll-down-relationally-medium)
  ("M-n" . my-scroll-scroll-up-relationally)
  ("M-p" . my-scroll-scroll-down-relationally)
  ("M-N" . my-scroll-scroll-up)
  ("M-P" . my-scroll-scroll-down))

(use-package my-sticky-buffer-mode
  :straight (my-sticky-buffer-mode :type built-in)
  :config
  (defun my-sticky-buffer-mode-delete-window-predicate (window)
    "Prevent WINDOW from `delete-other-windows'."
    (not my-sticky-buffer-mode))
  (my-misc-delete-window-predicates-add 'my-sticky-buffer-mode-delete-window-predicate)
  :bind
  ("M-s M-t" . my-sticky-buffer-mode))

(use-package switch-buffer-functions
  :bind
  ("M-s C-r" . read-only-mode-thyristor-toggle)
  :config
  (defun my-switch-buffer-functions--flexible-window-size-hook (prev cur)
    "Make window size flexible."
    (setq window-size-fixed nil))

  (my-macro-thyristor read-only-mode)
  (read-only-mode-thyristor-set nil)
  (defvar my-read-only-hook-exclude-regex
    "Minibuf")
  (defun my-switch-buffer-functions--read-only-hook (prev cur)
    "Enable `read-only-mode' when buffer switched.
Disable the function by setting `read-only-mode-thyristor-flag' to nil."
    (unless (string-match-p my-read-only-hook-exclude-regex (buffer-name cur))
      (read-only-mode-thyristor)))

  (dolist (f '(my-switch-buffer-functions--flexible-window-size-hook
               my-switch-buffer-functions--read-only-hook))
    (add-to-list 'switch-buffer-functions f)))

(use-package emacs-little-async
  :commands little-async-start-process
  :straight (emacs-little-async :host github
                                :repo "berquerant/emacs-little-async"))
(provide 'my-bootstrap)
;;; my-bootstrap.el ends here.
