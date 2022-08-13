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
  (bind-key "M-s 0" 'find-file-initel)
  (bind-key "M-s 9" 'find-file-zshrc))

(use-package modus-themes
  :ensure
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-hl-line  '(accented)
        modus-themes-paren-match '(bold)
        modus-themes-region '(bg-only no-extend accented))
  (modus-themes-load-themes)
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
           ("C-t" . other-window)
           ("C-T" . other-window-back)
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

(defun my-get-current-path ()
  "Store current buffer file name to kill ring."
  (interactive)
  (let ((p (buffer-file-name)))
    (when p
      (message "Stored: %s" p)
      (kill-new p))))

(bind-key "M-s p p" 'my-get-current-path)

(use-package my-misc
  :straight (my-misc :type built-in)
  :bind
  ("C-x C-M-e" . my-misc-eval-last-sexp-and-insert)
  ("C-x g t" . my-misc-tig-blame-current-buffer)
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
  :bind
  ("M-s M-t" . my-sticky-buffer-mode)
  ("C-x 1" . my-sticky-buffer-mode-delete-other-windows))

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here.
