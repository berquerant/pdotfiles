;;; my-bootstrap.el --- My Emacs bootstrapping -*- lexical-binding: t -*-

;;; Commentary:

;; This should be loaded with `demand t'.
;;
;; - Fundamental settings
;; - Load my local packages

;;; Code:

(setq inhibit-default-init t) ; ignore default.el
(repeat-mode t)
(setq-default ispell-program-name "aspell")
(setq default-frame-alist
      '((width . 180)
        (height . 60)
        (top . 0)
        (left . 0)
        (font . "-*-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))

(use-package minimal-init
  :demand t
  :straight (minimal-init :host github :repo "berquerant/emacs-minimal-init")
  :custom
  (minimal-init-font-size 120)
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

(defalias 'binary-mode 'hexl-mode)

(bind-keys :map global-map
           ([?\¥] . [?\\])
           ([?\C-¥] . [?\C-\\])
           ([?\M-¥] . [?\M-\\])
           ([?\C-\M-¥] . [?\C-\M-\\])
           ("M-s z" . my-revert-buffer)
           ("C-M-s" . isearch-forward-thing-at-point)
           ("C-s" . isearch-forward)
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

(use-package my-uuid
  :straight (my-uuid :type built-in))

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

;; important utilities

;; popup window manager
(use-package popwin
  :demand t
  :config
  (popwin-mode t)
  (setq special-display-function 'popwin:special-display-popup-window)
  (setq display-buffer-function 'popwin:display-buffer))

(use-package my-path
  :straight (my-path :type built-in)
  :bind
  ("M-s p p" . my-path-current-path))

(use-package my-proc
  :straight (my-proc :type built-in)
  :commands (my-proc-call-process))

(use-package my-git-browse
  :straight (my-git-browse :type built-in)
  :bind
  ("M-s 4" . my-git-browse-git-browse))

(use-package my-misc
  :straight (my-misc :type built-in)
  :commands (my-misc-delete-window-predicates-add
             my-misc-other-window-predicates-add)
  :bind
  ("C-t" . my-misc-other-window)
  ("C-M-t" . my-misc-other-window-reverse)
  ("C-x 1" . my-misc-delete-other-windows)
  ("M-s C-e" . my-misc-pp-macroexpand-1-last-sexp)
  ("M-s C-M-e" . my-misc-pp-macroexpand-all-last-sexp)
  ("C-x C-M-e" . my-misc-eval-last-sexp-and-insert)
  ("C-x C-x" . my-misc-exchange-point-and-mark))

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
  (defun my-kick-out-fundamental-mode-hook (&rest args)
    "Disable `fundamental-mode' at all times and enable `text-mode' instead."
    (when (eq major-mode 'fundamental-mode)
      (text-mode)))
  (add-hook 'after-change-major-mode-hook 'my-kick-out-fundamental-mode-hook)

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
               my-switch-buffer-functions--read-only-hook
               my-kick-out-fundamental-mode-hook))
    (add-to-list 'switch-buffer-functions f)))

(use-package little-async
  :demand t
  :commands little-async-start-process
  :straight (emacs-little-async :host github :repo "berquerant/emacs-little-async"))

(use-package scroll-util
  :straight (emacs-scroll-util :host github :repo "berquerant/emacs-scroll-util")
  :bind
  ("M-u" . scroll-util-scroll-up-relationally-medium)
  ("M-i" . scroll-util-scroll-down-relationally-medium)
  ("M-n" . scroll-util-scroll-up-relationally)
  ("M-p" . scroll-util-scroll-down-relationally)
  ("M-N" . scroll-util-scroll-up)
  ("M-P" . scroll-util-scroll-down))

(use-package my-time
  :straight (my-time :type built-in))

(use-package my-command-repeated
  :straight (my-command-repeated :type built-in)
  :config
  (my-command-repeated-setup))

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here.
