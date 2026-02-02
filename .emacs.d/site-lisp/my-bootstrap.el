;;; my-bootstrap.el --- My Emacs bootstrapping -*- lexical-binding: t -*-

;;; Commentary:

;; This should be loaded with `demand t'.
;;
;; - Fundamental settings
;; - Load my local packages

;;; Code:

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
  (minimal-init-quiet t)
  :config
  (minimal-init-setup))

(use-package dictionary
  :straight (dictionary :type built-in)
  :bind
  ("M-g d" . dictionary-search)
  :custom
  ;(dictionary-search-interface 'help)
  (dictionary-default-strategy "prefix")
  (dictionary-server "dict.org"))

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
           ("C-s" . isearch-forward-thing-at-point)
           ("C-M-s" . isearch-forward)
           ("C-r" . isearch-backward)
           ("C-<tab>" . tab-next)
           ("C-S-<tab>" . tab-previous)
           ("C-x <" . previous-buffer)
           ("C-x >" . next-buffer)
           ;; TODO: find workaround. delete-frame crashes emacs. (delete-frame (selected-frame) nil) too
           ;; https://github.com/syl20bnr/spacemacs/issues/6301
           ;; ("C-x f 0" . delete-frame)
           ;; ("C-x f 1" . delete-other-frames)
           ("C-x C-t" . toggle-truncate-lines)
           ("C-x C-r" . read-only-mode)
           ("M-SPC" . cycle-spacing) ; space conversions
           ("M-s e" . shell-command)
           ("C-h o" . describe-symbol)
           ("M-s c" . compile)
           ("M-g ." . eldoc))

(use-package my-macro
  :demand t
  :straight (my-macro :type built-in)
  :config
  (my-macro-defun-toggle debug-on-error)
  (my-macro-defun-toggle debug-on-quit)
  (defconst my-init-el (my-getenv-join "EMACSD" "init.el")
    "init.el location.")
  (defconst my-zshrc (my-getenv-join "HOME" ".zshrc")
    ".zshrc location.")
  (defconst my-zshrc2 (my-getenv-join "HOME" ".zshrc2")
    ".zshrc2 location.")
  (my-macro-handle-file find-file my-init-el)
  (my-macro-handle-file load-file my-init-el)
  (my-macro-handle-file find-file my-zshrc)
  (my-macro-handle-file find-file my-zshrc2)
  (my-macro-handle-buffer switch-to-buffer "*scratch*")
  (my-macro-handle-buffer switch-to-buffer-other-window "*scratch*")
  (my-macro-handle-buffer switch-to-buffer-other-tab "*scratch*")
  (my-macro-handle-buffer switch-to-buffer-other-frame "*scratch*")
  (bind-key "M-s 0" 'find-file-initel)
  (bind-key "M-s 9 0" 'find-file-zshrc)
  (bind-key "M-s 9 1" 'find-file-zshrc2)
  (bind-key "M-s 8" 'switch-to-buffer-scratch)
  (bind-key "M-s 7" 'switch-to-buffer-other-tab-scratch)
  (bind-key "M-s 6" 'switch-to-buffer-other-window-scratch)
  (bind-key "M-s 5" 'switch-to-buffer-other-frame-scratch)
  (bind-key "C-x w h" 'shrink-window-horizontally)
  (bind-key "C-x w j" 'shrink-window)
  (bind-key "C-x w k" 'enlarge-window)
  (bind-key "C-x w l" 'enlarge-window-horizontally)
  (my-macro-setup-repeat-commands (("h" shrink-window-horizontally)
                                   ("j" shurink-window)
                                   ("k" enlarge-window)
                                   ("l" enlarge-window-horizontally)))
  (my-macro-setup-repeat-commands ((">" next-buffer)
                                   ("<" previous-buffer))))

;; important utilities

;; popup window manager
(use-package popwin
  :demand t
  :config
  (popwin-mode t)
  (setq special-display-function 'popwin:special-display-popup-window)
  (setq display-buffer-function 'popwin:display-buffer))

(use-package my-proc
  :straight (my-proc :type built-in)
  :commands (my-proc-call-process))

(use-package my-path
  :straight (my-path :type built-in)
  :bind
  ("M-s M-s x" . my-path-current-path)
  ("M-s M-s M-x" . my-path-git-relative-path))

(use-package my-git-browse
  :straight (my-git-browse :type built-in)
  :bind
  ("M-g G" . my-git-browse-git-browse))

(use-package idle-timer
  :demand t
  :straight (idle-timer :host github :repo "berquerant/emacs-idle-timer"))
(use-package my-rpath
  :straight (my-rpath :type built-in)
  :bind
  ("M-s p" . my-rpath-call)
  ("M-s C-p" . my-rpath-mode-toggle))

(use-package my-show-indentation
  :demand t
  :straight (my-show-indentation :type built-in)
  :bind
  ("M-s M-i" . my-show-indentation-toggle))

(use-package my-misc
  :straight (my-misc :type built-in)
  :commands (my-misc-delete-window-predicates-add
             my-misc-other-window-predicates-add)
  :bind
  ("M-s M-s 0" . delete-frame)
  ("M-s M-s 1" . my-misc-delete-other-frames)
  ("M-s M-s 2" . clone-frame)
  ("C-t" . my-misc-other-window)
  ("C-T" . my-misc-other-window-reverse)
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
  ("C-x M-r" . my-sticky-buffer-mode))

(use-package my-buffer-change
  :demand t
  :straight (my-buffer-change :type built-in)
  :config
  (defun my-kick-out-fundamental-mode-hook ()
    "Disable `fundamental-mode' at all times and enable `text-mode' instead."
    (when (eq major-mode 'fundamental-mode)
      (text-mode)))
  (defun my-switch-buffer-functions--flexible-window-size-hook ()
    "Make window size flexible."
    (setq window-size-fixed nil))
  (dolist (f '(my-switch-buffer-functions--flexible-window-size-hook
               my-kick-out-fundamental-mode-hook))
    (add-to-list 'my-buffer-change-hook f))
  (my-buffer-change-setup))

(use-package little-async
  :demand t
  :commands little-async-start-process
  :straight (emacs-little-async :host github :repo "berquerant/emacs-little-async")
  :config
  (defun my-open-link (arg)
    "Open ARG as a link."
    (little-async-start-process `("open" ,arg)
                                :process-name "my-open-link"
                                :buffer-name "*my-open-link*"))
  (defun my-google-this (arg)
    "Open ARG by google."
    (little-async-start-process (list "open" (format "https://www.google.com/search?q=%s" arg))
                                :process-name "my-google-this"
                                :buffer-name "*my-google-this*"
                                :timeout (* 10 1000)))
  (my-macro-region-or-at-point my-open-link "open-link> ")
  (my-macro-region-or-at-point my-google-this "google> ")
  (bind-key "M-g 0" 'my-open-link-region-or-at-point)
  (bind-key "M-g M-0" 'my-google-this-region-or-at-point))

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

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here.
