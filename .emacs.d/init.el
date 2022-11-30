;;; init.el --- Initialize Emacs

;;; Commentary:

;; for macOS

;;; Code:

(defun display-emacs-init-time()
  "Display the elapsed time to initialize."
  (message "init time: %s" (emacs-init-time)))
(add-hook 'after-init-hook 'display-emacs-init-time)
;; install and initialize package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package) ; use-package integration

(setq use-package-verbose t                  ; enable logs in *Message* buffer
      use-package-minimum-reported-time 0.2
      use-package-compute-statistics t       ; see use-package-report
      straight-use-package-by-default t      ; use-package integration
      byte-compile-warnings '(cl-functions)  ; ignore package cl is deprecated
      warning-suppress-types '((comp)))      ; do not display comp warnings immediately

(use-package bind-key) ; easy key bindings
(use-package diminish) ; diminished mode-line
(use-package cl-lib)
(use-package s)
(use-package exec-path-from-shell
  :demand t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(defun my-getenv (&optional arg)
  "Get environment variable ARG or $HOME."
  (or (getenv (or arg "HOME"))
      (progn (message (format "[my-getenv] not found %s" arg))
             nil)))

(setq straight-profiles `((nil . ,(format "%s/dotfiles/.emacs.d/straight-default.el" (my-getenv "HOME")))))

;; for prefix
(unbind-key "M-s w")
(unbind-key "M-m")
(unbind-key "C-]")
(unbind-key "C-x m")
(unbind-key "M-t")
(unbind-key "M-j")

(add-to-list 'load-path (format "%s/site-lisp" (my-getenv "EMACSD")))
(add-to-list 'load-path (format "%s/external-site-lisp" (my-getenv "EMACSD")))

(use-package my-bootstrap
  :demand t
  :straight (my-bootstrap :type built-in))

(use-package my-load-built-in
  :straight (my-load-built-in :type built-in))

(use-package vterm ; libvterm libtool
  :bind
  ("M-s s" . vterm)
  :custom
  (vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "C-v" "M-v" "C-y" "M-y" "M-s" "C-t" "M-j"))
  (vterm-max-scrollback 10000)
  (vterm-always-compile-module t))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-line-or-code)
  ("C-e" . mwim-end-of-line-or-code))

(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2)
  :config
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(use-package which-key
  :demand t
  :diminish (which-key-mode . "")
  :bind
  ("M-s w k" . which-key-show-full-keymap)
  ("M-s w t" . which-key-show-top-level)
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-side-window-right)
  (which-key-mode t))

(use-package rainbow-mode
  :diminish (rainbow-mode . "")
  :hook
  ((prog-mode text-mode) . rainbow-mode)
  :custom
  (rainbow-ansi-colors t)
  (rainbow-html-colors t)
  (rainbow-x-colors t)
  (rainbow-latex-colors t))

(use-package rainbow-delimiters
  :hook
  ((prog-mode text-mode) . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "Red"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "Orange"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "Yellow"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "Green"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "Blue"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "Purple"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "Magenta"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "Brown")))))

;; cursor with beacon
(use-package beacon
  :custom
  (beacon-color "Yellow")
  :config
  (beacon-mode 1))

;; highlight focused buffer
(use-package dimmer
  :custom
  (dimmer-fraction 0.10)
  (dimmer-exclusion-regexp-list '("^\\*scratch"
                                  "^\\*[hH]elm"
                                  "^\\which-key"
                                  "^\\*Minibuf"
                                  "^\\*Echo"))
  :config
  (dimmer-mode))

(use-package google-this
  :demand t
  :bind
  ("M-j j" . google-this)
  :config
  (my-macro-state-hook google-this browse-url-browser-function browse-url-default-browser)
  (google-this-state-hook-generator eww-browse-url)
  (google-this-state-hook-generator xwidget-webkit-browse-url)
  (bind-key "M-j e" 'google-this-state-hook-eww-browse-url)
  (bind-key "M-j w" 'google-this-state-hook-xwidget-webkit-browse-url))

(use-package format-all
  :bind
  ("M-s f" . format-all-buffer))

(use-package magit
  :bind
  ("C-x g s" . magit-status)
  ("C-x g b" . magit-blame))

(use-package treemacs
  :bind
  ("M-t M-t" . treemacs-select-window)
  ("M-t M-n" . treemacs-next-workspace)
  ("M-t M-f" . treemacs-find-file)
  :custom
  (treemacs-indentation 1)
  (treemacs-indentation-string "|")
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-hide-gitignored-files-mode nil)
  (treemacs-fringe-indicator-mode 'always))

(use-package shut-up)

;; popup window manager
(use-package popwin
  :demand t
  :config
  (popwin-mode t)
  (setq special-display-function 'popwin:special-display-popup-window)
  (setq display-buffer-function 'popwin:display-buffer))

(use-package posframe)

;; search with romaji
(use-package migemo
  :custom
  (migemo-dictionary (my-getenv "MIGEMO_DICT"))
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init))

;; framework for completion
(use-package helm
  :diminish (helm-mode . "")
  :after migemo
  :bind
  (("C-x C-b" . helm-for-files)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x t o" . helm-occur)
   ("C-x t b" . helm-bookmarks)
   ("C-x t m" . helm-all-mark-rings)
   ("C-x t d" . helm-dabbrev)
   ("C-x t f" . helm-multi-files)
   ("C-x t i" . helm-semantic-or-imenu)
   ("C-x t a" . helm-apropos)
   ("C-x C-q" . helm-resume)
   :map helm-map
   ("C-h" . delete-backward-char)
   ("C-l" . helm-select-action)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action))
  :custom
  (helm-input-idle-delay 0.3)
  (helm-M-x-fuzzy-match t)
  (helm-buffers-fuzzy-matching nil)
  (helm-ff-fuzzy-matching nil)
  (helm-buffer-details-flag nil)
  (helm-apropos-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-follow-mode-persistent t)
  (helm-delete-minibuffer-contents-from-point t) ; kill by C-k
  (helm-display-function #'display-buffer)
  (helm-recentf-fuzzy-match nil)
  (helm-split-window-inside-p t)
  (helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-buffer-not-found))
  (helm-for-files-preferred-list '(helm-source-buffers-list
                                   helm-source-bookmarks
                                   helm-source-recentf
                                   helm-source-file-cache
                                   helm-source-files-in-current-dir
                                   helm-source-bookmark-set
                                   helm-source-locate))
  :config
  (helm-mode t)
  (helm-migemo-mode 1)
  (diminish 'helm-migemo-mode)
  (setq helm-idle-delay 0.3))

(use-package helm-descbinds
  :bind
  ("C-h b" .  helm-descbinds)
  :config
  (helm-descbinds-mode))

(use-package projectile
  :demand t
  :commands my-project-p
  :diminish (projectile-mode . "P")
  :config
  (defun my-project-p ()
    "Check if current directory is a project."
    (and (buffer-file-name)
         (s-starts-with-p (projectile-project-root) (file-truename (buffer-file-name)))))
  (projectile-mode)
  (setq projectile-mode-line-lighter "")
  (setq projectile-mode-line "")
  (defun my-advice-project-root (orig-func &rest args)
    (let ((r (apply orig-func args)))
      (if (not (equal r (format "%s/" (my-getenv "HOME"))))
          r
        (format "%s/" (my-getenv "EMACSD")))))
  (advice-add 'projectile-project-root :around 'my-advice-project-root)
  :custom
  (projectile-use-git-grep t)
  (projectile-dynamic-mode-line nil)
  (projectile-completion-system 'helm))

;; https://github.com/x-motemen/ghq
(use-package helm-ghq
  :demand t
  :bind
  ("C-x c g" . helm-ghq))

(use-package helm-projectile
  :demand t
  :after (projectile helm-ghq)
  :hook
  (projectile-mode . helm-projectile-on)
  :custom
  (helm-projectile-fuzzy-match nil)
  :config
  (my-macro-fallback-interactively my-project-p helm-projectile-find-file helm-ghq)
  (my-macro-fallback-interactively my-project-p helm-projectile-recentf helm-ghq)
  (bind-keys :map projectile-mode-map
             ("C-x g f" . helm-projectile-find-file-fallback-to-helm-ghq)
             ("C-x g h" . helm-projectile-recentf-fallback-to-helm-ghq)))

;; key bindings when region activated
(use-package selected
  :demand t
  :diminish (selected-minor-mode . "")
  :commands (shell-command-on-region-and-insert
             shell-command-on-region-and-replace
             my-sql-format-on-region)
  :bind
  (:map selected-keymap
        ("@" . rectangle-mark-mode)
        ("c" . count-words-region)
        ("Q" . query-replace)
        ("1" . sort-lines)
        ("2" . reverse-region)
        ("i" . indent-rigidly)
        ("b" . shell-command-on-region)
        ("C-b" . shell-command-on-region-and-insert)
        ("B" . shell-command-on-region-and-replace)
        ("l" . my-sql-format-on-region))
  :config
  (defun shell-command-on-region-and-insert (command)
    "Execute COMMAND with specified region and insert result into current buffer."
    (interactive (list (read-string "Shell command on region*: ")))
    (when (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) command t)))
  (defun shell-command-on-region-and-replace (command)
    "Execute COMMAND with specified region and replace region with result."
    (interactive (list (read-string "Shell command on region**: ")))
    (when (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) command t t)))
  (setq selected-minor-mode-override t)
  (selected-global-mode 1)
  (defun my-sql-format-on-region ()
    "Format sql on region, sql-formatter required.
https://github.com/zeroturnaround/sql-formatter"
    (interactive)
    (shell-command-on-region-and-insert "sql-formatter -u")))

(use-package my-trans
  :demand t
  :straight (my-trans :type built-in)
  :config
  (add-to-list 'special-display-buffer-names my-trans-output-buffer-name)
  (my-macro-region-or-at-point my-trans-into-ja "To ja: ")
  (my-macro-region-or-at-point my-trans-into-en "To en: ")
  (bind-key "M-j t j" 'my-trans-into-ja-region-or-at-point)
  (bind-key "M-j t e" 'my-trans-into-en-region-or-at-point))

(use-package helm-selected
  :after selected
  :bind
  (:map selected-keymap
        ("h" . helm-selected)))

;; omit continuous command
(use-package smartrep
  :demand t
  :config
  (smartrep-define-key global-map "C-x w"
    '(("h" . shrink-window-horizontally)
      ("j" . shrink-window)
      ("k" . enlarge-window)
      ("l" . enlarge-window-horizontally)
      ("/" . winner-undo)
      ("_" . winner-redo))))

;; strong isearch
(use-package helm-swoop
  :demand t
  :bind
  (("M-s o" . helm-multi-swoop)
   :map isearch-mode-map
   ("C-o" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("C-c C-e" . helm-swoop-edit)
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)
   ("C-o" . helm-multi-swoop-current-mode-from-helm-swoop)
   :map helm-multi-swoop-map
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line))
  :custom
  (helm-swoop-use-fuzzy-match nil)
  (helm-swoop-move-to-line-cycle t)
  :config
  (defun my-helm-swoop-or-occur-p ()
    "Return T if do swoop."
    (< (buffer-size) (* 10 (expt 2 10))))
  (my-macro-fallback-interactively my-helm-swoop-or-occur-p helm-swoop helm-occur)
  (bind-key "C-x o" 'helm-swoop-fallback-to-helm-occur))

(use-package helm-git-grep
  :after (helm-projectile helm-ghq)
  :demand t
  :bind
  (:map isearch-mode-map
   ("C-0" . helm-git-grep-from-isearch)
   :map helm-map
   ("C-0" . helm-git-grep-from-helm))
  :config
  (my-macro-fallback-interactively my-project-p helm-git-grep-at-point helm-ghq)
  (bind-key "C-x g g" 'helm-git-grep-at-point-fallback-to-helm-ghq))

(use-package deadgrep
  :demand t
  :commands (my-deadgrep-hidden my-deadgrep-with-path)
  :bind
  (("C-x a o" . deadgrep)
   ("C-x a a" . my-deadgrep-hidden)
   ("C-x a p" . my-deadgrep-with-path))
  :config
  (add-to-list 'deadgrep-project-root-overrides `("~/" . ,(concat (my-getenv "DOTFILES_ROOT") "/"))) ; deny searching at the home directory
  (my-macro-thyristor my-deadgrep-specify-path)
  (my-deadgrep-specify-path-thyristor-set nil)
  (defvar my-deadgrep-specify-path--path (concat (my-getenv "DOTFILES_ROOT") "/"))
  (defun my-advice-deadgrep--lookup-override (orig-func &rest args)
    (if my-deadgrep-specify-path-thyristor-flag
        (progn
          (my-deadgrep-specify-path-thyristor-set nil)
          my-deadgrep-specify-path--path)
      (apply orig-func args)))
  (advice-add 'deadgrep--lookup-override :around 'my-advice-deadgrep--lookup-override)
  (defun my-deadgrep-with-path (path)
    "Do `deadgrep' on `path'."
    (interactive "fPath: ")
    (setq my-deadgrep-specify-path--path path)
    (my-deadgrep-specify-path-thyristor-set t)
    (call-interactively 'deadgrep))
  (my-macro-thyristor my-deadgrep-set-search-hidden-files)
  (my-deadgrep-set-search-hidden-files-thyristor-set nil)
  (defun my-advice-deadgrep--arguments (orig-func &rest args)
    (let ((result (apply orig-func args)))
      (when my-deadgrep-set-search-hidden-files-thyristor-flag
        (push "--hidden" result)
        (my-deadgrep-set-search-hidden-files-thyristor-set nil))
      result))
  (advice-add 'deadgrep--arguments :around 'my-advice-deadgrep--arguments)
  (defun my-deadgrep-hidden ()
    "Do `deadgrep' with rg --hidden."
    (interactive)
    (my-deadgrep-set-search-hidden-files-thyristor-set t)
    (call-interactively 'deadgrep)))

(use-package dumb-jump
  :demand t
  :after (deadgrep projectile)
  :bind
  ("M-g ?" . xref-find-references)
  ("M-g b" . xref-go-back)
  ("M-g h" . xref-find-apropos)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (my-macro-fallback-interactively my-project-p xref-find-definitions deadgrep)
  (bind-key "M-g g" 'xref-find-definitions-fallback-to-deadgrep)
  :custom
  (dumb-jump-default-project (my-getenv "DOTFILES_ROOT"))
  (dumb-jump-selector 'helm)
  (dumb-jump-force-searcher 'rg))

(use-package git-gutter
  :demand t
  :after (projectile goto-chg)
  :diminish ((global-git-gutter-mode . "")
             (git-gutter-mode . ""))
  :bind
  (("M-s l" . git-gutter))
  :custom
  (git-gutter:update-commands nil)
  (git-gutter:update-windows-commands nil)
  (git-gutter:update-hooks '(after-save-hook
                             after-revert-hook))
  :custom-face
  (git-gutter:added ((t (:foreground "green"))))
  (git-gutter:deleted ((t (:foreground "red"))))
  (git-gutter:modified ((t (:foreground "yellow"))))
  :config
  (global-git-gutter-mode t))

(use-package helm-bm
  :demand t
  :after smartrep
  :commands bm-toggle-or-helm
  :bind
  (("M-0" . bm-toggle-or-helm))
  :config
  (require 'bm)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (defun save-bm-before-kill-emacs ()
    (bm-buffer-save-all)
    (bm-repository-save))
  (add-hook 'kill-emacs-hook 'save-bm-before-kill-emacs)
  (setq bookmark-save-flag 1)
  (setq bookmark-use-annotations t)
  (setq bookmark-automatically-show-annotations t)
  (defun bm-toggle-or-helm ()
    (interactive)
    (bm-toggle)
    (when (eq last-command 'bm-toggle-or-helm)
      (helm-bm)))
  (push '(migemo) helm-source-bm)
  (setq helm-source-bm (delete '(multiline) helm-source-bm))
  (smartrep-define-key global-map "C-x j" '(("b" . bm-previous)
                                            ("f" . bm-next))))

;; snippet management
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package auto-yasnippet
  :bind
  ("M-z" . aya-expand)
  ("M-Z" . aya-create)
  :custom
  (aya-create-with-newline t))

;; completions
(use-package company
  :demand t
  :diminish (company-mode . "")
  :bind
  (("C-x C-i" . company-complete)
   ("TAB" . indent-for-tab-command)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("TAB" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :custom-face
  (company-tooltip ((t (:foreground "Black" :background "DimGray"))))
  (company-tooltip-common ((t (:foreground "Black" :background "DimGray"))))
  (company-tooltip-common-selection ((t (:foreground "White" :background "SteelBlue"))))
  (company-tooltip-selection ((t (:foreground "Black" :background "SteelBlue"))))
  (company-preview-common ((t (:foreground "DimGray" :background nil :underline t))))
  (company-scrollbar-fg ((t (:background "Orange"))))
  (company-scrollbar-bg ((t (:background "Gray"))))
  :custom
  (company-tooltip-maximum-width 50)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.5)
  (company-echo-delay 0.1)
  (company-transformers '(company-sort-by-backend-importance))
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
  (company-backends '(company-bbdb
                      company-eclim
                      company-semantic
                      company-clang
                      company-xcode
                      company-cmake
                      (company-capf company-dabbrev) ; capf needs yas-minor-mode
                      company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)
                      company-oddmuse))
  :config
  (global-company-mode))

(use-package company-statistics
  :hook
  (company-mode . company-statistics-mode)
  :custom
  (company-transformers '(company-sort-by-statistics
                          company-sort-by-backend-importance)))

(use-package company-quickhelp
  :hook
  (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-max-lines 10))

(use-package git-complete
  :straight (git-complete :host github
                          :repo "zk-phi/git-complete")
  :bind
  ("M-q" . git-complete)
  :custom
  (git-complete-enable-autopair t)
  (git-complete-ignore-case t))

(use-package dabbrev
  :custom
  (abbrev-file-name (format "%s/abbrev_defs" (my-getenv "EMACSD")))
  (save-abbrevs t)
  :config
  (quietly-read-abbrev-file))

;; jump by char
(use-package avy
  :demand t
  :bind
  ("M-g M-g" . avy-goto-line)
  :custom
  (avy-timeout-seconds nil))

(use-package avy-migemo
  :demand t
  :bind
  (("C-j" . avy-migemo-goto-char-timer)
   :map isearch-mode-map
   ("C-j" . avy-migemo-isearch))
  :config
  (avy-migemo-mode t))

;; memo
(use-package open-junk-file
  :after deadgrep
  :bind
  (("C-x j o" . open-junk-file)
   ("C-x j a" . my-deadgrep-junk-files))
  :config
  (defun my-deadgrep-junk-files ()
    "Do `deadgrep' junk files."
    (interactive)
    (my-deadgrep-with-path (concat (my-getenv "EMACSD") "/junk/")))
  (setq open-junk-file-format (concat (my-getenv "EMACSD") "/junk/%Y-%m%d-%H%M%S.")))

(use-package expand-region
  :after selected
  :bind
  (:map selected-keymap
        ("j" . er/contract-region)
        ("k" . er/expand-region))
  :config
  (transient-mark-mode t))

(use-package multiple-cursors
  :config
  (smartrep-define-key global-map "C-x m"
      '(("." . mc/edit-lines)
        (">" . mc/mark-next-like-this)
        ("<" . mc/mark-previous-like-this))))

;; isearch from region
(use-package phi-search-migemo
  :after (selected migemo)
  :bind
  (:map selected-keymap
        ("s" . phi-search-migemo)
        ("r" . phi-search-migemo-backward)))

(use-package replace-from-region
  :after selected
  :bind
  (:map selected-keymap
        ("q" . query-replace-from-region)
        ("C-q" . query-replace-regexp-from-region)))

;; search assist
(use-package anzu
  :custom
  (anzu-deactivate-region t)
  (anzu-mode-lighter "")
  (anzu-search-threshold 100)
  :config
  (global-anzu-mode +1))

(use-package ez-query-replace
  :bind
  ("M-%" . ez-query-replace))

(use-package smartparens
  :diminish (smartparens-mode . "")
  :demand t
  :after smartrep
  :config
  (smartrep-define-key global-map "C-]"
    '(("f" . sp-forward-sexp)
      ("b" . sp-backward-sexp)
      ("," . sp-backward-barf-sexp)
      ("." . sp-backward-slurp-sexp)
      ("n" . sp-up-sexp)
      ("p" . sp-down-sexp)
      ("k" . sp-kill-sexp)
      ("u" . sp-splice-sexp)
      ("w" . sp-rewrap-sexp)
      ("s" . sp-split-sexp)
      ("j" . sp-join-sexp)))
  (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
  (ad-activate 'delete-backward-char)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil) ; disable right ' completion
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil) ; disable right ` completion
  (smartparens-global-mode t))

(use-package highlight-symbol
  :after smartrep
  :config
  (smartrep-define-key global-map "M-]"
    `(("n" . highlight-symbol-next)
      ("p" . highlight-symbol-prev)
      ("." . highlight-symbol-at-point)
      ("q" . highlight-symbol-query-replace)
      ("r" . highlight-symbol-remove-all)
      ("l" . highlight-symbol-list-all)
      ("o" . highlight-symbol-occur)
      ("h" . highlight-symbol-mode)))
  :custom
  (highlight-symbol-colors
   '("Green"
     "Magenta"
     "Blue"
     "Orange"
     "Purple"
     "Brown"
     "selectedMenuItemColor")))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :bind
  (:map gfm-mode-map
        ("C-c m >" . markdown-follow-thing-at-point))
  :config
  (defun markdown-mode-before-save-hook ()
    "Disable `delete-trailing-whitespace' if `major-mode' is `markdown-mode' or `gfm-mode'."
    (delete-trailing-whitespace-thyristor-set (not (memq major-mode '(markdown-mode gfm-mode)))))
  (add-hook 'before-save-hook 'markdown-mode-before-save-hook)
  (my-macro-state-hook markdown-preview browse-url-browser-function browse-url-default-browser)
  (markdown-preview-state-hook-generator xwidget-webkit-browse-url)
  (bind-key "C-c C-c C-p" 'markdown-preview-state-hook-xwidget-webkit-browse-url gfm-mode-map)
  :custom
  (markdown-command "github-markup")
  (markdown-command-needs-filename t)
  (markdown-content-type "application/xhtml+xml")
  (markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  (markdown-xhtml-header-content "
<script>
document.addEventListener('DOMContentLoaded', (event) => {
  document.body.classList.add('markdown-body');
});
</script>") ; github-markdown.min.css is applied to .markdown-body
  (markdown-fontify-code-blocks-natively t))

;; spell check
(use-package flyspell
  :diminish (flyspell-mode . "Fs")
  :bind
  (:map flyspell-mode-map
   ("M-s a b" . flyspell-buffer))
  :config
  ; for backward-forward
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map)
  (my-macro-thyristor flyspell-mode)
  (defun flyspell-prog-mode-switch-thyristor ()
    (flyspell-mode-off)
    (when flyspell-mode-thyristor-flag
      (flyspell-prog-mode)))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode-switch-thyristor)
  (add-hook 'text-mode-hook 'flyspell-mode-thyristor-2n)
  (add-hook 'markdown-mode-hook 'flyspell-mode-thyristor-2n)
  (add-hook 'generic-mode-hook 'flyspell-mode-thyristor-2n))

(use-package cc-mode
  :mode
  ("\\.h\\'" . c++-mode)
  :hook
  (c-mode-common . (lambda ()
                     (setq c-default-style "k&r")
                     (setq indent-tabs-mode nil)
                     (setq c-basic-offset 2))))

(use-package add-node-modules-path
  :hook
  ((js-mode . add-node-modules-path)
   (js2-mode . add-node-modules-path)
   (web-mode . add-node-modules-path)))

(use-package web-mode
  :commands (web-mode web-mode-indent)
  :after flycheck
  :mode
  (("\\.(dj|p)?html?\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.gsp\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.xml\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.vuex?\\'" . web-mode)
   ("\\.twig\\'" . web-mode))
  :hook
  (web-mode . (lambda ()
                (setq web-mode-attr-indent-offset nil)
                (setq indent-tabs-mode nil)))
  :config
  (defun web-mode-indent (n)
    "Set variables for `web-mode' to set indent tab width."
      (interactive "nTab-width: ")
      (setq web-mode-markup-indent-offset n)
      (setq web-mode-css-indent-offset n)
      (setq web-mode-style-padding n)
      (setq web-mode-code-indent-offset n)
      (setq web-mode-script-padding n)
      (setq web-mode-javascript-indentation n)
      (setq web-mode-block-padding n))
  (setq web-mode-enable-auto-pairing nil) ; due to smartparens
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-tag-auto-close-style t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (web-mode-indent 2)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

(use-package go-mode
  :custom
  (gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package flycheck-golangci-lint
  :demand t
  :after (go-mode flycheck)
  :hook (go-mode . flycheck-golangci-lint-setup)
  :custom
  (flycheck-golangci-lint-config "~/golangci.yml"))

(use-package go-playground
  :custom
  (go-playground-basedir (format "%s/github.com/%s/go-playground/"
                                 (my-getenv "GHQ_ROOT")
                                 (my-getenv "GIT_USER")))
  (go-playground-init-command "go mod init"))


(use-package tern
  :custom
  (tern-command '("tern" "--no-port-file"))) ; no .tern-port

(use-package js2-mode
  :hook
  (js-mode . (lambda ()
               (make-local-variable 'js-indent-level)
               (setq js-indent-level 2)
               (setq indent-tabs-mode nil)
               (setq c-basic-offset 2)
               (setq js2-basic-offset 2)
               (setq tab-width 2)))
  :mode
  (("\\.js\\'" . js2-mode)
   ("\\.jsx$" . web-mode)))

(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . (lambda ()
                       (setq typescript-indent-level 2)
                       (setq flycheck-check-syntax-automatically '(save mode-enabled))))
  :mode
  ("\\.ts\\'" . typescript-mode)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . (lambda ()
                   (setq indent-tabs-mode nil)
                   (setq python-indent 4)))
  :init
  (add-to-list 'exec-path (format "%s/shims" (my-getenv "PYENV_ROOT")))
  :custom
  (python-shell-interpreter (format "%s/shims/python" (my-getenv "PYENV_ROOT")))
  :config
  (setq py-python-command (format "%s/shims/python" (my-getenv "PYENV_ROOT"))))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :custom
  (ruby-deep-indent-paren-style nil)
  (ruby-indent-tabs-mode nil))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(use-package php-mode
  :hook
  (php-mode . (lambda ()
                (setq c-basic-offset 4)))
  :mode ("\\.php\\'" . php-mode)
  :config
  (php-enable-default-coding-style)
  (subword-mode 1))

(use-package php-eldoc
  :after php-mode
  :hook
  (php-mode . php-eldoc-enable))

(use-package php-cs-fixer
  :after php-mode
  :init
  (my-macro-thyristor php-cs-fixer-before-save)
  (defun add-php-before-save-local-hook ()
    (add-hook 'before-save-hook 'php-cs-fixer-before-save-thyristor nil t))
  (add-hook 'php-mode 'add-php-before-save-local-hook))

(use-package flycheck-phpstan
  :after (php-mode flycheck))

(use-package scala-mode
  :mode "^\w+\\.s\\(cala\\|bt\\)$")

(use-package ascii
  :bind
  (("M-s M-s M-a" . ascii-on)
   ("M-s M-s a" . ascii-off)))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :bind
  (:map json-mode-map
        ("M-s f" . json-mode-beautify))
  :config
  (unbind-key "C-c :" json-mode-map)
  (unbind-key "C-c C-r" json-mode-map))

(use-package yaml-mode
  :mode
  (("\\.yaml\\'" . yaml-mode)
   ("\\.yml\\'" . yaml-mode)
   ("\\.dig\\'" . yaml-mode))
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)))

(use-package haml-mode
  :mode
  (("\\.haml\\'" . haml-mode)))

(use-package sass-mode
  :mode
  (("\\.sass\\'" . sass-mode)))

(use-package scss-mode
  :mode
  (("\\.scss\\'" . scss-mode))
  :config
  (setq scss-compile-at-save nil))

(use-package dockerfile-mode
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

(use-package emmet-mode
  :hook
  ((web-mode . emmet-mode)
   (html-mode . emmet-mode)
   (css-mode . emmet-mode)
   (sgml-mode . emmet-mode))
  :config
  (unbind-key "C-j" emmet-mode-keymap)
  (bind-key "C-c :" 'emmet-expand-line emmet-mode-keymap))

(use-package rust-mode
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :custom
  (rust-format-on-save t))

(use-package rust-playground
  :custom
  (rust-playground-basedir (format "%s/github.com/%s/rust-playground"
                                   (my-getenv "GHQ_ROOT")
                                   (my-getenv "GIT_USER"))))

(use-package flycheck-haskell
  :after (haskell-mode flycheck)
  :hook
  (haskell-mode . flycheck-haskell-setup))

(use-package haskell-mode
  :mode
  ("\\.hs$" . haskell-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(defalias 'perl-mode 'cperl-mode)

(use-package clojure-mode)

(use-package cider)

(use-package cypher-mode
  :mode (("\\.cql\\'" . cypher-mode)))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package zig-mode
  :mode "\\.zig\\'")

(use-package svelte-mode)

;; syntax checkers
(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :commands flycheck-pos-tip-mode)

(use-package flycheck-popup-tip
  :after flycheck
  :commands flycheck-popup-tip-mode)

(use-package flycheck
  :demand t
  :commands (flycheck-mode flycheck-mode-thyristor-2n)
  :mode
  (("\\.proto\\'" . flycheck-mode-thyristor-2n))
  :hook
  ((go-mode
    dart-mode
    cperl-mode
    scala-mode
    python-mode
    c-mode
    sh-mode
    emacs-lisp-mode
    haskell-mode
    php-mode
    js2-mode) . flycheck-mode-thyristor-2n)
  :bind
  (("M-s M-s M-f" . flycheck-mode))
  :custom
  (flycheck-idle-change-delay 5)
  (flycheck-display-errors-delay 1)
  (flycheck-highlighting-mode 'lines)
  (flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
  (flycheck-python-flake8-executable (format "%s/shims/python" (my-getenv "PYENV_ROOT")))
  (flycheck-flake8-maximum-line-length 120)
  :config
  (my-macro-thyristor flycheck-mode)
  (smartrep-define-key global-map "C-c f"
    '(("f" . flycheck-list-errors)
      ("." . flycheck-display-error-at-point)
      ("n" . flycheck-next-error)
      ("p" . flycheck-previous-error)
      ("e" . flycheck-buffer)
      ("w" . flycheck-copy-errors-as-kill)))
  (if (display-graphic-p)
      (flycheck-pos-tip-mode)
    (flycheck-popup-tip-mode)))

;; syntax highlighting
(use-package tree-sitter-langs)
(use-package tree-sitter
  :diminish  (tree-sitter-mode . "")
  :after tree-sitter-langs
  :demand t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescript-tsx-mode . tsx)))

(use-package eglot
  :hook
  (((python-mode
     go-mode
     ruby-mode
     c-mode
     c++-mode) . eglot-ensure)
   (eglot-managed-mode . (lambda () (flymake-mode 0)))
   (eglot-managed-mode . my-eglot-before-save-hook))
  :bind
  (("M-s M-s M-e" . eglot)
   :map eglot-mode-map
   ("C-x p m r" . eglot-reconnect)
   ("C-x p f" . eglot-format-buffer)
   ("C-x p a" . eglot-code-actions)
   ("C-x p r" . eglot-rename)
   ("M-s M-s M-e" . eglot-shutdown))
  :config
  (my-macro-ring-hook "my-eglot" '(my-eglot-imports-and-format
                                   my-eglot-noop
                                   my-eglot-format
                                   my-eglot-imports))
  (defun my-eglot-ring-echo (cur next)
    (message "[my-eglot] change before save hook to %s" next))
  (add-to-list 'my-eglot-ring-hook 'my-eglot-ring-echo)
  (defun my-eglot-format-buffer ()
    (when (or (eq (my-eglot-ring-hook-get-state) 'my-eglot-format)
              (eq (my-eglot-ring-hook-get-state) 'my-eglot-imports-and-format))
      (call-interactively 'eglot-format-buffer)))
  (defun my-eglot-code-action-organize-imports ()
    (when (or (eq (my-eglot-ring-hook-get-state) 'my-eglot-imports)
              (eq (my-eglot-ring-hook-get-state) 'my-eglot-imports-and-format))
      (call-interactively 'eglot-code-action-organize-imports)))
  (defun my-eglot-format-and-imports ()
    (unless (eq major-mode 'go-mode) ; use gofmt-before-save
      (my-eglot-code-action-organize-imports)
      (my-eglot-format-buffer)))
  (defun my-eglot-before-save-hook ()
    (add-hook 'before-save-hook #'my-eglot-format-and-imports))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp" "-v" "--tcp" "--host" "127.0.0.1" "--port" :autoport)))
  :custom
  (eglot-autoreconnect nil)
  (eglot-connect-timeout 5)
  (eglot-send-changes-idle-time 5)
  (eglot-confirm-server-initiated-edits nil))

(use-package lsp-mode
  :hook
  (((typescript-mode
     typescript-tsx-mode
     svelte-mode
     rust-mode
     clojure-mode
     clojurescript-mode
     clojurec-mode
     zig-mode
     css-mode
     terraform-mode
     html-mode) . lsp-deferred)
   (lsp-mode . lsp-lens-mode)
   (zig-mode . flymake-mode))
  :bind
  (("M-s M-s M-l" . lsp)
   :map lsp-mode-map
   ("M-s M-s M-l" . lsp-disconnect)
   ("C-c p w" . lsp-workspace-folders-add)
   ("M-s M-s l" . lsp-workspace-restart)
   ("C-c p C-w" . lsp-workspace-shutdown)
   ("C-c p m d" . lsp-describe-session)
   ("C-c p ." . lsp-describe-thing-at-point)
   ("C-c p a" . lsp-execute-code-action)
   ("C-c p r" . lsp-rename))
  :custom
  (lsp-disabled-clients '(tfls))
  (lsp-auto-guess-root t)
  (lsp-prefer-capf t)
  (lsp-prefer-flymake nil)
  (lsp-signature-auto-activate t)
  (lsp-print-performance nil)
  (lsp-log-io t)
  (lsp-trace nil)
  (lsp-restart 'ignore)
  (lsp-idle-delay 0.6)
  (lsp-file-watch-threshold 50)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-indentation nil)
  (lsp-enable-completion-at-point t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-text-document-color t)
  (lsp-enable-file-watchers t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-diagnostic-package nil)
  (lsp-document-sync-method lsp--sync-incremental)
  (lsp-response-timeout 5)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t)
  (lsp-terraform-enable-logging nil))

(use-package lsp-ui
  :commands (ladicle/toggle-lsp-ui-doc)
  :bind
  (:map lsp-mode-map
        ("C-c p ?" . lsp-ui-peek-find-references)
        ("C-c p >" . lsp-ui-peek-find-definitions)
        ("C-c p i" . lsp-ui-peek-find-implementation)
        ("C-c p s" . lsp-ui-sideline-mode)
        ("C-c p d" . ladicle/toggle-lsp-ui-doc))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-delay 0.3)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-imenu-enable nil)
  :config
  (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1))))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-mode-map
        ("C-c t e" . lsp-treemacs-errors-list)
        ("C-c t l" . lsp-treemacs-symbols)
        ("C-c t ?" . lsp-treemacs-references)
        ("C-c t i" . lsp-treemacs-implementations)
        ("C-c t c" . lsp-treemacs-call-hierarchy)
        ("C-c t t" . lsp-treemacs-type-hierarchy)))


(use-package goto-chg
  :demand t
  :bind
  ("M-," . goto-last-change-reverse)
  ("M-." . goto-last-change))

(use-package backward-forward
  :demand t
  :bind
  ("C-," . backward-forward-previous-location)
  ("C-." . backward-forward-next-location)
  :config
  (cl-loop for x in '(helm-bookmarks
                      helm-for-files
                      helm-find-files
                      helm-all-mark-rings
                      helm-ghq
                      helm-projectile-find-file
                      helm-projectile-recentf
                      helm-swoop
                      helm-occur
                      helm-git-grep-at-point
                      deadgrep
                      xref-find-references
                      xref-find-definitions)
           do (advice-add x :before #'backward-forward-push-mark-wrapper))
  (backward-forward-mode t))

(use-package command-log
  :straight (command-log :host github
                         :repo "berquerant/emacs-command-log")
  :custom
  (command-log-histfile (my-getenv "EMACS_HISTFILE"))
  :config
  (command-log-setup))

(use-package recentf-ext
  :custom
  (recentf-save-file (format "%s/.recentf" (my-getenv "EMACSD")))
  (recentf-exclude '(".recentf"))
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 'never)
  :config
  (run-with-idle-timer 60 t (lambda () (shut-up (recentf-save-list)))))

;;; init.el ends here
