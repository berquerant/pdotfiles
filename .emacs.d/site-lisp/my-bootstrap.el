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
  (bind-key "M-s 5" 'switch-to-buffer-scratch)
  (bind-key "M-s 6" 'switch-to-buffer-other-window-scratch)
  (bind-key "M-s 7" 'switch-to-buffer-other-tab-scratch)
  (bind-key "M-s 8" 'switch-to-buffer-other-frame-scratch))

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

(menu-bar-mode 0) ; no menu bar
(tool-bar-mode 0) ; no tool bar
(toggle-scroll-bar nil) ; no scroll bar
(toggle-horizontal-scroll-bar nil)
(setcar mode-line-position '(:eval (format "%d" (count-lines (point-max) (point-min)))))
(fset 'yes-or-no-p 'y-or-n-p) ; translate yes/no query into y/n query
(global-auto-revert-mode t) ; revert buffer when edited except emacs
(global-hl-line-mode t) ; highlight cursor line
(show-paren-mode t) ; highlight pair parens
(delete-selection-mode t) ; overwrite when type after selecting region
(savehist-mode t) ; save minibuffer history
(winner-mode 1) ; save window configurations
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")) ; ignore spell check for Japanese
(line-number-mode t)
(column-number-mode t)
(global-display-line-numbers-mode t)
(repeat-mode t)
(display-time) ; display time on modeline
(set-face-attribute 'default nil :height 100) ; initial font size
(setq-default major-mode 'text-mode       ; use text-mode instead of fundamental-mode
              bidi-display-reordering nil ; ignore the writing direction: right to left
              tab-width 2
              indent-tabs-mode nil ; disable tab
              ispell-program-name "aspell")
(setq initial-major-mode 'text-mode
      gc-cons-threshold (* 100 (expt 2 20)) ; reduce gc frequency
      gc-cons-percentage 1
      garbage-collection-messages nil
      inhibit-default-init t      ; ignore default.el
      inhibit-splash-screen t     ; no splash screen
      inhibit-startup-message t   ; no startup
      initial-scratch-message nil ; empty scratch
      ring-bell-function 'ignore  ; no beep
      frame-title-format (format "%%f - Emacs@%s" (system-name))
      completion-ignore-case t
      make-backup-files nil ; no *.~ files
      auto-save-default nil ; no .#* files
      create-lockfiles nil  ; no lock files
      history-delete-duplicates t
      history-length 1000
      message-log-max 10000
      kill-ring-max 200
      compilation-scroll-output t ; tail when compile
      confirm-kill-emacs 'y-or-n-p
      vc-follow-symlinks t
      default-directory "~/"
      yank-excluded-properties t    ; ignore text properties when paste
      set-mark-command-repeat-pop t ; pop-global-mark like smartrep, for C-u C-SPC
      file-name-coding-system 'utf-8
      locale-coding-system 'utf-8
      line-number-display-limit 100000
      line-number-display-limit-width 50
      split-height-threshold 120
      eval-expression-print-length nil
      eval-expression-print-level nil
      display-time-interval 1 ; display time every second
      display-time-string-forms '((format-time-string "%Y-%m-%d %H:%M:%S" now))
      eval-expression-print-maximum-character nil
      package-native-compile t
      read-process-output-max (* 1024 1024)
      max-lisp-eval-depth 1500
      max-specpdl-size 3000)

(defun other-window-back ()
  "Reverse `other-window'."
  (interactive)
  (other-window -1))

(bind-keys :map global-map
           ([?\¥] . [?\\])
           ([?\C-¥] . [?\C-\\])
           ([?\M-¥] . [?\M-\\])
           ([?\C-\M-¥] . [?\C-\M-\\])
           ("C-s" . isearch-forward-thing-at-point)
           ("C-r" . isearch-backward-thing-at-point)
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
           ("M-SPC" . cycle-spacing)       ; space conversions
           ("M-s e" . shell-command)
           ("C-h o" . describe-symbol)
           ("M-s x" . repeat)
           ("M-s y" . repeat-complex-command)
           ("M-s c" . compile)
           ("M-g ." . eldoc)
           ("M-z" . repeat)
           ("M-s q" . text-scale-adjust))

;; copy and paste

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx
      save-interprogram-paste-before-kill t)

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
  (my-macro-thyristor read-only-mode)
  (read-only-mode-thyristor-set nil)
  (defvar my-read-only-hook-exclude-regex
    "Minibuf")
  (defun my-switch-buffer-functions--read-only-hook (prev cur)
    "Enable `read-only-mode' when buffer switched.
Disable the function by setting `read-only-mode-thyristor-flag' to nil."
    (unless (string-match-p my-read-only-hook-exclude-regex (buffer-name cur))
      (read-only-mode-thyristor)))
  (add-to-list 'switch-buffer-functions 'my-switch-buffer-functions--read-only-hook))

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here.
